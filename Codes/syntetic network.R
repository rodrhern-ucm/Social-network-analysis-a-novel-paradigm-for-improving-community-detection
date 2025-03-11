library(igraph)

# Function to generate a random hierarchical network
generate_random_hierarchical_network <- function() {
  n <- sample(25:200, 1)  # Random number of nodes between 50 and 200
  levels <- sample(2:5, 1)  # Random number of hierarchical levels
  block_sizes <- sample(5:30, levels, replace = TRUE)  # Random community sizes
  n <- sum(block_sizes)  # Adjust total nodes based on block sizes
  
  p_in <- runif(1, 0.2, 0.5)  # Random intra-community connection probability
  p_out <- runif(1, 0.01, 0.1)  # Random inter-community connection probability
  
  P <- matrix(p_out, nrow = levels, ncol = levels)
  diag(P) <- p_in  # Higher probability within communities
  
  g <- sample_sbm(n, pref.matrix = P, block.sizes = block_sizes, directed = FALSE)
  return(g)
}

# Create and save 20 networks with randomness
set.seed(12345)  # For reproducibility
output_dir <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Nuevas redes sinteticas"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

for (i in 1:20) {
  g <- generate_random_hierarchical_network()
  filename <- file.path(output_dir, paste0("hierarchical_graph_", i, ".graphml"))
  write_graph(g, filename, format = "graphml")
}

cat("All 20 random networks saved in the 'synthetic_networks' folder as GraphML files.\n")
