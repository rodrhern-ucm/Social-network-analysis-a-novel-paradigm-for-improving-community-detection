################################## FUNCTIONS LOUVAIN ############################################################################
create_community_adj_matrix <- function(original_graph, communities) {
  # Check for NA values in communities vector
  if(any(is.na(communities))) {
    stop("NA values found in community assignments.")
  }
  
  # Create an empty graph with nodes representing communities
  num_communities <- length(unique(communities))
  community_graph <- make_empty_graph(num_communities)
  
  # Iterate over each edge in the original graph
  for(e in E(original_graph)) {
    # Get the community assignments of the nodes connected by the edge
    ends <- ends(original_graph, e)
    comm1 <- communities[ends[1]]
    comm2 <- communities[ends[2]]
    
    # Add an edge in the community graph if the communities are different
    if (!is.na(comm1) && !is.na(comm2) && comm1 != comm2) {
      community_graph <- add_edges(community_graph, c(comm1, comm2))
    }
  }
  
  # Remove duplicate edges and create the adjacency matrix
  community_graph <- simplify(community_graph)
  adj_matrix <- as_adjacency_matrix(community_graph)
  return(adj_matrix)
}


run_louvain <- function(graph, resolution_range) {
  num_resolutions <- length(resolution_range)
  community_lists <- vector("list", num_resolutions)
  modularities <- numeric(num_resolutions)
  community_adj_matrices <- vector("list", num_resolutions)
  
  for (i in seq_along(resolution_range)) {
    res <- resolution_range[i]
    # Running Louvain algorithm with varying resolution
    communities <- cluster_louvain(graph, resolution = res)
    community_lists[[i]] <- communities$membership
    modularities[i] <- modularity(communities)
    
    # Create the community adjacency matrix for this resolution
    community_adj_matrices[[i]] <- create_community_adj_matrix(graph, communities$membership)
  }
  
  return(list(community_matrices = community_lists, 
              modularities = modularities, 
              community_adj_matrices = community_adj_matrices))
}



#create_combined_graph <- function(original_graph, louvain_results) {
#  # Extracting unique communities across all resolutions
#  all_communities <- unique(unlist(louvain_results$community_matrices))
#  num_communities <- length(all_communities)
#  num_original_nodes <- vcount(original_graph)
#  
#  # Create a combined graph with original nodes and community nodes
#  combined_graph <- make_empty_graph(n = num_original_nodes + num_communities)
#  
#  # Set names for original and community nodes
#  V(combined_graph)$name <- c(V(original_graph)$name, 
#                              paste0("Community", seq_len(num_communities)))
#  
#  # Add original edges
#  combined_graph <- add_edges(combined_graph, get.edgelist(original_graph))
#  
#  # Add edges between nodes and their communities
#  for (i in 1:num_original_nodes) {
#    for (j in seq_along(louvain_results$community_matrices)) {
#      community <- louvain_results$community_matrices[[j]][i]
#      community_node_index <- num_original_nodes + community
#      combined_graph <- add_edges(combined_graph, c(i, community_node_index))
#    }
#  }
#  
#  return(combined_graph)
#}



#plot_communities <- function(graph, membership_vector) {
#  # Create a color palette with a unique color for each community
#  unique_communities <- sort(unique(membership_vector))
#  colors <- rainbow(length(unique_communities))
#  
#  # Map community IDs to colors
#  color_map <- setNames(colors, unique_communities)
#  vertex_colors <- color_map[as.character(membership_vector)]
#  
#  # Assign community membership as an attribute to each node
#  V(graph)$community <- membership_vector
#  
#  # Plotting the graph with nodes colored by their community
#  plot(graph, vertex.color=vertex_colors,
#       main="Community Detection Results",
#       vertex.size=7, vertex.label.cex=0.8,
#       edge.arrow.size=0.5, margin=-0.2)
#}


#aggregate_communities <- function(community_lists) {
#  # Check if the community_lists is not empty and contains vectors
#  if (length(community_lists) == 0 || !is.vector(community_lists[[1]])) {
#    stop("Error: community_lists is empty or not properly formatted.")
#  }
#  
#  num_nodes <- length(community_lists[[1]])
#  composite_membership <- numeric(num_nodes)
#  
#  for (node in 1:num_nodes) {
#    # Extract community assignments for this node across all runs
#    node_communities <- sapply(community_lists, function(x) x[node])
#    
#    # Find the most frequent community assignment (mode)
#    composite_membership[node] <- as.numeric(names(which.max(table(node_communities))))
#  }
#  
#  return(composite_membership)
#}
#
#aggregate_communities_weighted <- function(community_lists) {
#  num_nodes <- length(community_lists[[1]])
#  composite_membership <- numeric(num_nodes)
#  
#  for (node in 1:num_nodes) {
#    # Extract and weight community assignments for this node across all runs
#    node_communities <- table(unlist(lapply(community_lists, function(x) x[node])))
#    weighted_communities <- node_communities * (1:length(node_communities))
#    
#    # Assign the community with the highest weighted score
#    composite_membership[node] <- as.numeric(names(which.max(weighted_communities)))
#  }
#  
#  return(composite_membership)
#}
#
#aggregate_communities_hierarchical <- function(community_lists) {
#  # Ensure community_lists is not empty and contains vectors
#  if (length(community_lists) == 0 || !is.vector(community_lists[[1]])) {
#    stop("Error: community_lists is empty or not properly formatted.")
#  }
#  
#  # Convert the list of community vectors into a matrix
#  community_matrix <- do.call(rbind, community_lists)
#  
#  # Perform hierarchical clustering on the transposed matrix
#  hc <- hclust(dist(t(community_matrix)), method = "average")
#  
#  # Cut the dendrogram to form clusters (communities)
#  # The number of clusters can be chosen based on specific criteria
#  num_clusters <- max(unlist(community_lists))
#  clustered_communities <- cutree(hc, k = num_clusters)
#  
#  return(clustered_communities)
#}

#create_graph_from_aggregated_communities <- function(original_graph, composite_membership) {
#  num_nodes <- length(composite_membership)
#  
#  # Create an empty graph
#  new_graph <- make_empty_graph(n = num_nodes)
#  
#  # Add edges between nodes that are in the same community
#  for (node1 in 1:(num_nodes-1)) {
#    for (node2 in (node1 + 1):num_nodes) {
#      if (composite_membership[node1] == composite_membership[node2]) {
#        new_graph <- add_edges(new_graph, c(node1, node2))
#      }
#    }
#  }
#  
#  return(new_graph)
#}

#create_graph_from_aggregated_communities_sparse <- function(original_graph, composite_membership) {
#  num_nodes <- length(composite_membership)
#  # Create a sparse matrix with dimensions num_nodes x num_nodes
#  sparse_matrix <- sparseMatrix(i = integer(0), j = integer(0), dims = c(num_nodes, num_nodes))
#  
#  community_nodes <- split(1:num_nodes, composite_membership)
#  
#  for (community in community_nodes) {
#    if (length(community) > 1) {
#      # Creating edges within the community
#      community_edges <- combn(community, 2, simplify = TRUE)
#      
#      # Adding edges to the sparse matrix
#      sparse_matrix[community_edges[1, ], community_edges[2, ]] <- 1
#      sparse_matrix[community_edges[2, ], community_edges[1, ]] <- 1  # For undirected graph
#    }
#  }
#  
#  # Convert the sparse matrix to an igraph object
#  new_graph <- graph_from_adjacency_matrix(sparse_matrix, mode = "undirected", diag = FALSE)
#  return(new_graph)
#}

#transform_community_vectors <- function(community_vectors, num_nodes) {
#  community_matrices <- lapply(community_vectors, function(community_vector) {
#    # Initialize a matrix of zeros
#    community_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
#    
#    # Set matrix element to 1 if nodes are in the same community
#    for (i in 1:(num_nodes - 1)) {
#      for (j in (i + 1):num_nodes) {
#        if (community_vector[i] == community_vector[j]) {
#          community_matrix[i, j] <- 1
#          community_matrix[j, i] <- 1  # For symmetry
#        }
#      }
#    }
#    
#    return(community_matrix)
#  })
#  
#  return(community_matrices)
#}

transform_community_vectors <- function(community_vectors, original_graph) {
  num_nodes <- vcount(original_graph)
  # Extract the original adjacency matrix
  original_adj_matrix <- as_adjacency_matrix(original_graph, sparse = FALSE)
  
  # Initialize the list with the original adjacency matrix
  community_matrices <- list(original_adj_matrix)
  
  for (vec in community_vectors) {
    # Initialize a matrix of zeros
    community_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
    
    # Set matrix element to 1 if nodes are in the same community
    for (i in 1:(num_nodes - 1)) {
      for (j in (i + 1):num_nodes) {
        if (vec[i] == vec[j]) {
          community_matrix[i, j] <- 1
          community_matrix[j, i] <- 1  # For symmetry
        }
      }
    }
    
    # Append the community matrix to the list
    community_matrices <- c(community_matrices, list(community_matrix))
  }
  
  return(community_matrices)
}


aggregate_communities_matrix <- function(community_matrices) {
  num_nodes <- nrow(community_matrices[[1]])
  aggregated_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
  
  for (i in 1:(num_nodes - 1)) {
    for (j in (i + 1):num_nodes) {
      # Count how many matrices have nodes i and j in the same community
      same_community_count <- sum(sapply(community_matrices, function(m) m[i, j]))
      
      # Majority voting
      if (same_community_count > length(community_matrices) / 2) {
        aggregated_matrix[i, j] <- 1
        aggregated_matrix[j, i] <- 1
      }
    }
  }
  
  return(aggregated_matrix)
}

aggregate_communities_weighted_matrix <- function(community_matrices, weights) {
  if (length(weights) != length(community_matrices)) {
    stop("Number of weights must match the number of community matrices.")
  }
  
  num_nodes <- nrow(community_matrices[[1]])
  aggregated_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
  
  for (i in 1:(num_nodes - 1)) {
    for (j in (i + 1):num_nodes) {
      # Weighted sum
      weighted_sum <- sum(sapply(1:length(community_matrices), function(k) community_matrices[[k]][i, j] * weights[k]))
      
      # Determine if i and j are in the same community based on weighted sum
      if (weighted_sum > sum(weights) / 2) {
        aggregated_matrix[i, j] <- 1
        aggregated_matrix[j, i] <- 1
      }
    }
  }
  
  return(aggregated_matrix)
}

aggregate_communities_percentile <- function(community_matrices, percentile_threshold) {
  num_nodes <- nrow(community_matrices[[1]])
  aggregated_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
  num_runs <- length(community_matrices)
  threshold_runs <- ceiling(percentile_threshold / 100 * num_runs)
  
  for (i in 1:(num_nodes - 1)) {
    for (j in (i + 1):num_nodes) {
      # Count the number of matrices where nodes i and j are in the same community
      same_community_count <- sum(sapply(community_matrices, function(m) m[i, j] == 1))
      
      # Check if the count exceeds the threshold
      if (same_community_count >= threshold_runs) {
        aggregated_matrix[i, j] <- 1
        aggregated_matrix[j, i] <- 1
      }
    }
  }
  
  return(aggregated_matrix)
}

aggregate_communities_combined <- function(community_matrices, method = "weighted", weights = NULL, percentile_threshold = NULL) {
  if (method == "weighted") {
    # Check if weights are provided
    if (is.null(weights) || length(weights) != length(community_matrices)) {
      stop("For weighted method, a valid 'weights' vector must be provided.")
    }
    return(aggregate_communities_weighted_matrix(community_matrices, weights))
    
  } else if (method == "percentile") {
    # Check if percentile_threshold is provided
    if (is.null(percentile_threshold)) {
      stop("For percentile method, 'percentile_threshold' must be provided.")
    }
    return(aggregate_communities_percentile(community_matrices, percentile_threshold))
    
  } else {
    stop("Invalid method specified. Choose 'weighted' or 'percentile'.")
  }
}


raise_adjacency_matrix <- function(graph, power) {
  adj_matrix <- as_adjacency_matrix(graph, sparse = FALSE)
  
  result_matrix <- adj_matrix
  
  if (power > 1) {
    for (i in 2:power) {
      result_matrix <- result_matrix %*% adj_matrix
    }
  }
  
  return(result_matrix)
}

visualize_communities <- function(graph, community_vector_1, community_vector_2) {
  # Convert graph to nodes and edges data frames
  nodes_df <- get.data.frame(graph, what = "vertices")
  edges_df <- get.data.frame(graph, what = "edges")
  
  # Adjust node IDs in nodes_df to start from 0
  nodes_df$id <- as.numeric(rownames(nodes_df)) - 1
  edges_df$from <- edges_df$from - 1
  edges_df$to <- edges_df$to - 1
  
  # Prepare extended color mappings for each set of community assignments
  # Create a function to generate a repeating color palette
  get_colors <- function(num_communities, palette_name) {
    palette <- brewer.pal(min(num_communities, length(brewer.pal.info[palette_name, "maxn"])), palette_name)
    # Repeat the palette to cover all communities
    return(rep(palette, length.out = num_communities))
  }
  
  colors_1 <- get_colors(length(unique(community_vector_1)), "Set1")
  colors_2 <- get_colors(length(unique(community_vector_2)), "Set2")
  
  color_map_1 <- setNames(colors_1, unique(community_vector_1))
  color_map_2 <- setNames(colors_2, unique(community_vector_2))
  
  nodes_df$color_1 <- color_map_1[as.character(community_vector_1)]
  nodes_df$color_2 <- color_map_2[as.character(community_vector_2)]
  
  # Plot with community 1
  net1 <- visNetwork(nodes_df, edges_df) %>%
    visNodes(color = list(background = nodes_df$color_1)) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLegend()
  
  # Plot with community 2
  net2 <- visNetwork(nodes_df, edges_df) %>%
    visNodes(color = list(background = nodes_df$color_2)) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visLegend()
  
  # Return both networks
  list(Community1 = net1, Community2 = net2)
}

combine_walk_matrices <- function(graph, max_power) {
  adj_matrix <- as_adjacency_matrix(graph, sparse = FALSE)
  
  # Initialize the combined matrix with the original adjacency matrix (p=1)
  combined_matrix <- adj_matrix
  
  # Add matrices for p=2 to max_power
  current_matrix <- adj_matrix
  for (power in 2:max_power) {
    current_matrix <- current_matrix %*% adj_matrix
    combined_matrix <- combined_matrix + current_matrix
  }
  
  return(combined_matrix)
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

