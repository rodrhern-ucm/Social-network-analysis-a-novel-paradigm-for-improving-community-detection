########## Version Codigo para iterar por parametros parrillas de redes
library(igraph)
library(rvest)
library(readr)
library(hydra)
library(igraphdata)
library(visNetwork)
library(combinat)
library(dplyr)
######### SET WORKING DIRECTORY
setwd("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Funciones R/igraph")
source("Functions.R")
set.seed(12345)
######### NETWORKS
folder_path <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Nuevas redes sinteticas"
file_list <- list.files(folder_path, pattern = "\\.graphml$", full.names = TRUE)

# Function to compute and store adjacency matrices
precompute_matrices <- function(graph_list) {
  matrices_list <- list()
  
  for (file in graph_list) {
    cat("Precomputing:", file, "\n")
    g <- read_graph(file, format = "graphml")
    adj_matrix <- as_adjacency_matrix(g, sparse = FALSE)
    
    # Compute squared and cubed adjacency matrices
    adj_squared <- adj_matrix %*% adj_matrix
    adj_cubed <- adj_squared %*% adj_matrix
    
    # Store in list
    matrices_list[[file]] <- list(A1 = adj_matrix, A2 = adj_squared, A3 = adj_cubed)
  }
  
  return(matrices_list)
}

# Run once before processing networks
precomputed_matrices <- precompute_matrices(file_list)

# Function to generate weight combinations that sum to 1
generate_weight_combinations <- function(num_weights, step = 0.01) {
  seq_vals <- seq(0, 1, by = step)
  combinations <- expand.grid(rep(list(seq_vals), num_weights))
  valid_combinations <- combinations[rowSums(combinations) == 1, ]
  return(as.matrix(valid_combinations))
}

# Generate weight combinations
weight_combinations <- generate_weight_combinations(3, step = 0.01)

combine_weighted_walk_matrices <- function(graph_name, weights, matrices_list) {
  matrices <- matrices_list[[graph_name]]
  
  # Initialize with weighted A^1
  combined_matrix <- weights[1] * matrices$A1 + 
    weights[2] * matrices$A2 + 
    weights[3] * matrices$A3
  
  return(combined_matrix)
}

process_network <- function(graph_name, weight_combinations, algorithm, matrices_list) {
  g <- read_graph(graph_name, format = "graphml")
  g <- as.undirected(g)
  # Check if the graph has a weight attribute
  if ("weight" %in% edge_attr_names(g)) {
    # Remove the weight attribute
    g <- delete_edge_attr(g, "weight")
  }
  results <- data.frame()
  
  # Ensure Fast Greedy works without multi-edges or loops
  if (algorithm == 'fast_greedy') {
    g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  }
  
  # Select the algorithm
  cluster_function <- switch(algorithm,
                             'louvain' = cluster_louvain,
                             'leiden' = function(g) cluster_leiden(g, objective_function = "modularity"),
                             'walktrap' = function(g) cluster_walktrap(g),
                             'infomap' = cluster_infomap,
                             'fast_greedy' = cluster_fast_greedy
  )
  
  original <- cluster_function(g)
  original_modularity <- modularity(g, original$membership)
  
  for (i in 1:nrow(weight_combinations)) {
    weights <- weight_combinations[i, ]
    
    start <- Sys.time()
    
    # Use precomputed matrices
    composite_matrix <- combine_weighted_walk_matrices(graph_name, weights, matrices_list)
    
    # Create new graph
    new_graph <- graph_from_adjacency_matrix(composite_matrix, mode = "undirected")

    # Fix for Fast Greedy: remove multi-edges and loops
    if (algorithm == 'fast_greedy') {
      new_graph <- simplify(new_graph, remove.multiple = TRUE, remove.loops = TRUE)
    }
    
    composite_result <- cluster_function(new_graph)
    modularity_result <- modularity(new_graph, composite_result$membership)
    
    end <- Sys.time()
    
    # Store results
    results <- rbind(
      results, 
      cbind(
        t(weights), 
        modularity = modularity_result,
        partition = paste0(composite_result$membership, collapse = ","),
        time = end - start,
        algorithm = algorithm
      )
    )
  }
  
  return(results)
}

gc()
network_list <- file_list[1:20]

for (file in network_list) {
  final_results <- data.frame()
  cat("Processing:", file, "\n")
  
  # Remove weights if present
  g <- read_graph(file, format = "graphml")
  if ("weight" %in% edge_attr_names(g)) {
    g <- delete_edge_attr(g, "weight")
  }
  
  network_name <- basename(file)
  
  # Run different algorithms
  cat("Processing: louvain")
  louvain <- process_network(file, weight_combinations, 'louvain', precomputed_matrices)
  louvain$network <- network_name
  
  cat("Processing: leiden")
  leiden <- process_network(file, weight_combinations, 'leiden', precomputed_matrices)
  leiden$network <- network_name
  
  cat("Processing: walktrap")
  walktrap <- process_network(file, weight_combinations, 'walktrap', precomputed_matrices)
  walktrap$network <- network_name
  
  cat("Processing: infomap")
  infomap <- process_network(file, weight_combinations, 'infomap', precomputed_matrices)
  infomap$network <- network_name
  
  cat("Processing: fast_greedy")
  fast_greedy <- process_network(file, weight_combinations, 'fast_greedy', precomputed_matrices)
  fast_greedy$network <- network_name
  
  # Combine and save results
  final_results <- rbind(final_results, louvain, leiden, walktrap, infomap, fast_greedy)
  final_results$modularity <- ifelse(is.nan(final_results$modularity), 0, final_results$modularity)
  
  write.csv(final_results, paste0("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Tabla Modularidades/Nueva Funcion resultados/", substr(network_name, 1, nchar(network_name) - 8), "_modularity_results.csv"), row.names = FALSE)
}
