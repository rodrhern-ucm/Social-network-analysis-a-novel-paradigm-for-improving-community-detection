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
folder_path <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes"
file_list <- list.files(folder_path, pattern = "\\.graphml$", full.names = TRUE)


# Function to generate weight combinations that sum to 1
generate_weight_combinations <- function(num_weights, step = 0.01) {
  seq_vals <- seq(0, 1, by = step)
  combinations <- expand.grid(rep(list(seq_vals), num_weights))
  valid_combinations <- combinations[rowSums(combinations) == 1, ]
  return(as.matrix(valid_combinations))
}

combine_weighted_walk_matrices <- function(graph, max_power, weights) {
  adj_matrix <- as_adjacency_matrix(graph, sparse = FALSE)
  
  # Initialize the combined matrix with the weighted original adjacency matrix (p=1)
  combined_matrix <- weights[1] * adj_matrix
  
  # Ensure weights vector has the appropriate length
  if (length(weights) < max_power) {
    stop("Length of weights must match max_power.")
  }
  
  # Add weighted matrices for p=2 to max_power
  current_matrix <- adj_matrix
  for (power in 2:max_power) {
    current_matrix <- current_matrix %*% adj_matrix
    combined_matrix <- combined_matrix + weights[power] * current_matrix
  }
  
  return(combined_matrix)
}

# Generate weight combinations
weight_combinations <- generate_weight_combinations(3, step = 0.01)


# Function to process a network
process_network <- function(graph, weight_combinations,algorithm = 'louvain') {
  if(algorithm == 'louvain'){
    start <- Sys.time()
    max_power <- 3
    results <- data.frame()
    
    original <- cluster_louvain(as.undirected(graph))
    original_modularity <- original$modularity[length(original$modularity)]
    
    for (i in 1:nrow(weight_combinations)) {
      weights <- weight_combinations[i, ]
      composite_matrix <- combine_weighted_walk_matrices(as.undirected(graph), max_power, weights)
      
      # Create new graph and calculate modularity
      new_graph <- graph_from_adjacency_matrix(composite_matrix, mode = "undirected")
      composite_result <- cluster_louvain(as.undirected(new_graph, mode = "collapse"))
      modularity_result <- max(composite_result$modularity)
      end <- Sys.time()
      time <- end - start
      # Store results
      results <- rbind(
        results, 
        cbind(
          t(weights), 
          modularity = modularity_result,
          partition = paste0(composite_result$membership,collapse = ","),
          time = time,
          algorithm = 'louvain'
        )
      )

    } 
  }
  if(algorithm == 'leiden'){
    start <- Sys.time()
    max_power <- 3
    results <- data.frame()
    
    original <- cluster_leiden(as.undirected(graph), objective_function = "modularity")
    original_modularity <- modularity(as.undirected(graph), original$membership)
    
    for (i in 1:nrow(weight_combinations)) {
      weights <- weight_combinations[i, ]
      composite_matrix <- combine_weighted_walk_matrices(as.undirected(graph), max_power, weights)
      
      # Create new graph and calculate modularity
      new_graph <- graph_from_adjacency_matrix(composite_matrix, mode = "undirected")
      composite_result <- cluster_leiden(as.undirected(new_graph, mode = "collapse"), objective_function = "modularity")
      modularity_result <- modularity(as.undirected(graph), composite_result$membership)
      end <- Sys.time()
      time <- end - start
      # Store results
      results <- rbind(
        results, 
        cbind(
          t(weights), 
          modularity = modularity_result,
          partition = paste0(composite_result$membership,collapse = ","),
          time = time,
          algorithm = 'leiden'
        )
      )
    } 
  }
  if(algorithm == 'walktrap'){
    start <- Sys.time()
    max_power <- 3
    results <- data.frame()
    
    original <- cluster_walktrap(as.undirected(graph))
    original_modularity <- max(original$modularity)
    
    for (i in 1:nrow(weight_combinations)) {
      weights <- weight_combinations[i, ]
      composite_matrix <- combine_weighted_walk_matrices(as.undirected(graph), max_power, weights)
      
      # Create new graph and calculate modularity
      new_graph <- graph_from_adjacency_matrix(composite_matrix, mode = "undirected")
      composite_result <- cluster_walktrap(as.undirected(new_graph, mode = "collapse"))
      modularity_result <- max(composite_result$modularity)
      original
      end <- Sys.time()
      time <- end - start
      # Store results
      results <- rbind(
        results, 
        cbind(
          t(weights), 
          modularity = modularity_result,
          partition = paste0(composite_result$membership,collapse = ","),
          time = time,
          algorithm = 'walktrap'
        )
      )
    } 
  }
  if(algorithm == 'infomap'){
    start <- Sys.time()
    max_power <- 3
    results <- data.frame()
    
    original <- cluster_infomap(as.undirected(graph))
    original_modularity <- modularity(as.undirected(graph), original$membership)
    
    for (i in 1:nrow(weight_combinations)) {
      weights <- weight_combinations[i, ]
      composite_matrix <- combine_weighted_walk_matrices(as.undirected(graph), max_power, weights)
      
      # Create new graph and calculate modularity
      new_graph <- graph_from_adjacency_matrix(composite_matrix, mode = "undirected")
      composite_result <- cluster_infomap(as.undirected(new_graph, mode = "collapse"))
      modularity_result <- composite_result$modularity
      end <- Sys.time()
      time <- end - start
      # Store results
      results <- rbind(
        results, 
        cbind(
          t(weights), 
          modularity = modularity_result,
          partition = paste0(composite_result$membership,collapse = ","),
          time = time,
          algorithm = 'infomap'
        )
      )
    } 
  }
  if(algorithm == 'fast_greedy'){
    start <- Sys.time()
    max_power <- 3
    results <- data.frame()
    graph <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
    original <- cluster_fast_greedy(as.undirected(graph))
    original_modularity <- max(modularity(as.undirected(graph), original$membership))
    
    for (i in 1:nrow(weight_combinations)) {
      weights <- weight_combinations[i, ]
      composite_matrix <- combine_weighted_walk_matrices(as.undirected(graph), max_power, weights)
      
      # Create new graph and calculate modularity
      new_graph <- graph_from_adjacency_matrix(composite_matrix, mode = "undirected")
      new_graph <- simplify(new_graph,remove.multiple = TRUE, remove.loops = TRUE)
      composite_result <- cluster_fast_greedy(as.undirected(new_graph, mode = "collapse"))
      modularity_result <- max(composite_result$modularity)
      end <- Sys.time()
      time <- end - start
      # Store results
      results <- rbind(
        results, 
        cbind(
          t(weights), 
          modularity = modularity_result,
          partition = paste0(composite_result$membership,collapse = ","),
          time = time,
          algorithm = 'fast_greedy'
        )
      )
    } 
  }  
  return(results)
}



# Initialize results table
network_list <-read.csv("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/networks_used.csv")
network_list$file <- gsub("_modularity_results.csv",".graphml",network_list$file)
network_list$file <- gsub("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output","C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes",network_list$file)
# network_list <- sample(file_list,90)
#network_list <- c("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/moreno_mac.graphml",
#                  "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/moreno_seventh.graphml",
#                  "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/moreno_sheep.graphml",
#                  "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/moreno_taro.graphml",
#                  "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/moreno_zebra.graphml",
#                  "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/opsahl-southernwomen.graphml",
#                  "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/subelj_euroroad.graphml",
#                  "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/ucidata-gama.graphml")
# Iterate over files
# brunson corporate/ brunson south africa /subelj_euroroad/
# moreno_innovation/brunson_club-membership falta 
# moreno_zebra / moreno_hens / moreno_taro/ brunson_revolution/ 
# brunson_southern-women
#Error in cluster_walktrap(as.undirected(graph)) : 
#At core/community/walktrap/walktrap_graph.cpp:211 : Vertex with zero strength found: all vertices must have positive strength for walktrap, Invalid value
for (file in network_list$file) {
  final_results <- data.frame()
  cat("Processing:", file, "\n")
  g <- read_graph(file, format = "graphml")
  # Check if the graph has a weight attribute
  if ("weight" %in% edge_attr_names(g)) {
    # Remove the weight attribute
    g <- delete_edge_attr(g, "weight")
  }
  
  network_name <- basename(file)
  #cat("Processing: louvain")
  #louvain <- process_network(g, weight_combinations, algorithm = 'louvain')
  #louvain$network <- network_name
  
  cat("Processing: leiden")
  leiden <- process_network(g, weight_combinations, algorithm = 'leiden')
  leiden$network <- network_name

  cat("Processing: walktrap")
  walktrap <- process_network(g, weight_combinations, algorithm = 'walktrap')
  walktrap$network <- network_name

  cat("Processing: infomap")
  infomap <- process_network(g, weight_combinations, algorithm = 'infomap')
  infomap$network <- network_name

  cat("Processing: fast_greedy")
  fast_greedy <- process_network(g, weight_combinations, algorithm = 'fast_greedy')
  fast_greedy$network <- network_name
  
  # Save results
  already_run <- read.csv(paste0("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/Nueva ejecucion/",substr(network_name,1,nchar(network_name)-8),"_modularity_results.csv"))
  already_run <- subset(already_run, algorithm == 'louvain')
  final_results <- rbind(already_run,leiden,walktrap,infomap,fast_greedy)
  final_results$modularity <- ifelse(is.nan(final_results$modularity), 0, final_results$modularity)
  
  write.csv(final_results, paste0("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/Nueva ejecucion/",substr(network_name,1,nchar(network_name)-8),"_modularity_results.csv"), row.names = FALSE)
}


