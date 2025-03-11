library(dplyr)
data <- arrow::read_parquet("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Tabla Modularidades/ml_input_data.parquet")
data <- data %>% select(algorithm, Var1, Var2, improvement) %>% rename(alpha1 = Var1,alpha2 = Var2)

library(ggplot2)
# Create heatmap
#data_melt <- reshape2::melt(data %>% filter(algorithm == 'infomap') %>% select(-algorithm) %>% group_by(alpha1,alpha2) %>% summarise(improvement = sum(improvement,na.rm =TRUE)*100/n()), id.vars = c('alpha1', 'alpha2'))
data_melt <- reshape2::melt(data %>% select(-algorithm) %>% group_by(alpha1,alpha2) %>% summarise(improvement = sum(improvement,na.rm =TRUE)*100/n()), id.vars = c('alpha1', 'alpha2'))

# Define the limits and breaks for the legend
legend_limits <- c(0, 100)  # Adjust these values as needed
legend_breaks <- seq(0, 100, by = 20)  # Adjust the step as needed

# Generate the plot with fixed legend values
ggplot(data_melt, aes(x = alpha1, y = alpha2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", limits = legend_limits, breaks = legend_breaks) +
  labs(title = "Heatmap of Improvements All Algorithms", 
       x = bquote(alpha[1]), 
       y = bquote(alpha[2]), 
       fill = "Improvement") +
  theme_minimal()


# Ruta de la carpeta con los CSV
folder_path <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Tabla Modularidades/Nueva Funcion resultados"

# Obtener lista de archivos CSV en la carpeta
file_list <- list.files(path = folder_path, pattern = "\\_modularity_results.csv$", full.names = TRUE)

time_base <- data.frame()
for(file in file_list){
  case_base_time <- read.csv(file)
  case_base_time <- case_base_time %>% filter(Var1 == 1, Var2 == 0, Var3 == 0) %>% 
                    select(time,algorithm,network) %>%
                    mutate(network = substr(network,1,nchar(network) - 8))
  time_base <- rbind(time_base,case_base_time)
}
time_base$network <- gsub("network_","barabasi_albert_",time_base$network)
results <- list()
# Process each file
for (file in file_list) {
  message(paste("Processing file:", file))
  data <- read.csv(file) %>%
    mutate(network = sub("_modularity_results\\.csv$", "", basename(file))) %>% 
    filter(!(Var1 == 1 & Var2 == 0 & Var3 == 0)) %>%
    group_by(network,algorithm) %>%
    summarise(avge_time = mean(time))
  
  # Merge with base modularity and calculate metrics
  data <- data %>% left_join(time_base, by = c("network", "algorithm")) %>% mutate(increase_time = (avge_time/time)-1 )
  
  results[[file]] <- data
}

# Combine all results into a single data frame
combined_data <- bind_rows(results)

combined_data %>% 
  filter(!(is.na(increase_time))) %>% 
  filter(!(network %in% c('arenas_jazz'))) %>%
  ggplot(aes(x = factor(algorithm), y = increase_time, fill = factor(algorithm))) +
  geom_boxplot() +
  labs(
    title = "Computational times",
    x = "Algorithm",
    y = "Increase Percentage (%)",
    fill = "Algorithm"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1", labels = tools::toTitleCase) +  # Capitalize legend labels
  scale_x_discrete(labels = tools::toTitleCase) +  # Capitalize x-axis labels
  theme(
    plot.title = element_text(hjust = 0.5),  # Center title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )




optimal_case <- data.frame()

for(file in file_list){
  data <- read.csv(file) %>% group_by(network,algorithm) %>% 
          mutate(optimal = max(modularity)) %>% filter(modularity == optimal) %>%
          select(algorithm, Var1,Var2) %>% mutate(improvement = 1)
  optimal_case <- rbind(optimal_case,data)

}
data_melt <- reshape2::melt(optimal_case  %>% rename(alpha1 = Var1,alpha2 = Var2) %>% group_by(alpha1,alpha2) %>% summarise(improvement = sum(improvement,na.rm =TRUE)), id.vars = c('alpha1', 'alpha2'))

ggplot(data_melt , aes(x = alpha1, y = alpha2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of optimal cases (all algorithms)", x = "alpha1", y = "alpha2", fill = "Improvement") +
  theme_minimal()


