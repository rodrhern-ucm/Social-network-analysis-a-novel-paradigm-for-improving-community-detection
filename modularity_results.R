########## Modularity extraction
library(igraph)
library(rvest)
library(readr)
library(hydra)
library(igraphdata)
library(visNetwork)
library(combinat)
set.seed(12345)
modularity_results <- data.frame()
# Ruta de la carpeta con los CSV
folder_path <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes"

# Obtener lista de archivos CSV en la carpeta
file_list <- data.frame(file = list.files(path = folder_path, pattern = "\\.graphml$", full.names = TRUE))
file_list$network <- substr(file_list$file,76,nchar(file_list$file)-8)
network_list <-read.csv("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/networks_used.csv")
network_list$network <- substr(network_list$file,83,nchar(network_list$file)-23)

file_list <- subset(file_list,network %in% network_list$network)
file = "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/moreno_hens.graphml"

for (file in file_list$file) {
  cat("Processing:", file, "\n")
  g <- read_graph(file, format = "graphml")
  network_name <- basename(file)
  
  if ("weight" %in% edge_attr_names(g)) {
    # Remove the weight attribute
    g <- delete_edge_attr(g, "weight")
  }
  
  original_louvain <- cluster_louvain(as.undirected(g))
  original_louvain_modularity <- data.frame(modularity = original_louvain$modularity[length(original_louvain$modularity)])
  original_louvain_modularity$algorithm <- 'louvain'
  original_louvain_modularity$network <- network_name
  
  original_leiden <- cluster_leiden(as.undirected(g), objective_function = "modularity")
  original_leiden_modularity <- data.frame(modularity = modularity(as.undirected(g),original_leiden$membership))
  original_leiden_modularity$algorithm <- 'leiden'
  original_leiden_modularity$network <- network_name

  original_walktrap <- cluster_walktrap(as.undirected(g))
  original_walktrap_modularity <- data.frame(modularity = max(original_walktrap$modularity))
  original_walktrap_modularity$algorithm <- 'walktrap'
  original_walktrap_modularity$network <- network_name  

  original_infomap <- cluster_infomap(as.undirected(g, mode = "collapse"))
  original_infomap_modularity <- data.frame(modularity = original_infomap$modularity)
  original_infomap_modularity$algorithm <- 'infomap'
  original_infomap_modularity$network <- network_name  

  original_fast_greedy <- cluster_fast_greedy(as.undirected(g))
  original_fast_greedy_modularity <- data.frame(modularity = max(original_fast_greedy$modularity))
  original_fast_greedy_modularity$algorithm <- 'fast_greedy'
  original_fast_greedy_modularity$network <- network_name  
  
  modularity_results <- rbind(modularity_results,original_louvain_modularity,original_leiden_modularity,original_walktrap_modularity,original_infomap_modularity,original_fast_greedy_modularity)
}
surprise_modularity <- read.csv("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/results from all networks/surprise_modularity_results.csv")
modularity_results <- rbind(modularity_results,surprise_modularity %>% select(modularity,algorithm,network))
write.csv(modularity_results,"C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/augmented_modularity_results.csv",row.names = FALSE)
