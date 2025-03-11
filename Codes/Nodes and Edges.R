library(igraph)
# Define the directory containing the .graphml files
directory <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Tabla Modularidades/Nueva Funcion redes"  
directory_sintetica <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Nuevas redes sinteticas"

# Get the list of all .graphml files in the directory
files <- list.files(directory, pattern = "\\.graphml$", full.names = TRUE)
files_sintetica <- list.files(directory_sintetica, pattern = "\\.graphml$", full.names = TRUE)
files <- c(files, files_sintetica)

# Initialize an empty data frame to store results
results <- data.frame(File = character(), TotalEdges = numeric(), stringsAsFactors = FALSE)

# Loop through each file, read the graph, and compute the number of edges
for (file in files) {
  graph <- read_graph(file, format = "graphml")
  num_nodes <- vcount(graph)  # Get the total number of nodes
  num_edges <- gsize(graph)  # Get the total number of edges
  results <- rbind(results, data.frame(File = basename(file), TotalNodes = num_nodes, TotalEdges = num_edges))
}
results$File <- gsub("network","barabasi_albert",results$File)

library(ggplot2)
plot <- ggplot(results, aes(x = TotalNodes, y = TotalEdges)) +
  geom_point(color = "blue") +
  #geom_label(data = top_3, aes(label = File), size = 4, fill = "white", color = "black", label.padding = unit(0.25, "lines")) +
  labs(title = "Scatterplot of Nodes vs Edges", x = "Number of Nodes", y = "Number of Edges") +
  theme_minimal()
print(plot)

write.csv2(results,"C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Tabla Modularidades/Nueva Funcion resultados/Nodes and Edges.csv")
