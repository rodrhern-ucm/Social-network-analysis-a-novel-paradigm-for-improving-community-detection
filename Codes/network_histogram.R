library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Function to generate and split plots
process_and_split_plots <- function(combined_data, output_folder) {
  # Ensure the output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Validate data
  if (nrow(combined_data) == 0) {
    stop("The combined data is empty. Please check the input data processing.")
  }
  
  # --- Plot 1: Split by Algorithm ---
  pdf(file.path(output_folder, "plot1_improvement_by_algorithm.pdf"), width = 10, height = 8)
  algorithms <- unique(combined_data$algorithm)
  for (alg in algorithms) {
    message(paste("Generating plot for algorithm:", alg))
    data_subset <- combined_data %>% filter(algorithm == alg)
    if (nrow(data_subset) == 0) {
      next
    }
    p <- ggplot(data_subset, aes(x = improvement_perc_adjusted)) +
      geom_histogram(binwidth = 0.05, color = "black", fill = "blue", alpha = 0.7) +
      scale_x_continuous(
        breaks = c(-0.05, seq(0.05, 1, by = 0.1)),
        labels = c("Improvement~0.01", seq(0.05, 1, by = 0.1))
      ) +
      labs(
        title = paste("Histogram of Improvement Percentage for Algorithm:", alg),
        x = "Improvement Percentage",
        y = "Number of Combinations"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    print(p)
  }
  dev.off()
  
  # --- Plot 2: Split by Network ---
  pdf(file.path(output_folder, "plot2_improvement_by_network.pdf"), width = 10, height = 8)
  networks <- unique(combined_data$network)
  for (net in networks) {
    message(paste("Generating plot for network:", net))
    data_subset <- combined_data %>% filter(network == net)
    if (nrow(data_subset) == 0) {
      next
    }
    p <- ggplot(data_subset, aes(x = improvement_perc_adjusted)) +
      geom_histogram(binwidth = 0.05, color = "black", fill = "green", alpha = 0.7) +
      scale_x_continuous(
        breaks = c(-0.05, seq(0.05, 1, by = 0.1)),
        labels = c("Improvement~0.01", seq(0.05, 1, by = 0.1))
      ) +
      labs(
        title = paste("Histogram of Improvement Percentage for Network:", net),
        x = "Improvement Percentage",
        y = "Number of Combinations"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    print(p)
  }
  dev.off()
  
  # --- Plot 3: Combined by Network and Algorithm ---
  pdf(file.path(output_folder, "plot3_combined_improvement.pdf"), width = 10, height = 8)
  for (net in networks) {
    message(paste("Generating combined plot for network:", net))
    data_subset <- combined_data %>% filter(network == net)
    if (nrow(data_subset) == 0) {
      next
    }
    p <- ggplot(data_subset, aes(x = improvement_perc_adjusted, fill = factor(algorithm))) +
      geom_histogram(binwidth = 0.05, color = "black", alpha = 0.7, position = "identity") +
      scale_x_continuous(
        breaks = c(-0.05, seq(0.05, 1, by = 0.1)),
        labels = c("Improvement~0.01", seq(0.05, 1, by = 0.1))
      ) +
      labs(
        title = paste("Combined Histogram for Network:", net),
        x = "Improvement Percentage",
        y = "Number of Combinations",
        fill = "Algorithm"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    print(p)
  }
  dev.off()
}

# Main processing and plotting function
process_and_plot <- function(folder_path, base_modularity_file, output_folder) {
  # Ensure output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Load base modularity data
  case_base_data <- read.csv(base_modularity_file) %>%
    select(network, algorithm, modularity) %>%
    rename(base_modularity = modularity) %>%
    mutate(network = substr(network,1,nchar(network) - 8))
  
  # Load all CSV files
  file_list <- list.files(path = folder_path, pattern = "\\_modularity_results.csv$", full.names = TRUE)
  results <- list()
  
  # Process each file
  for (file in file_list) {
    message(paste("Processing file:", file))
    data <- read.csv(file) %>%
      mutate(network = sub("_modularity_results\\.csv$", "", basename(file)))
    
    # Merge with base modularity and calculate metrics
    data <- data %>% left_join(case_base_data, by = c("network", "algorithm")) %>%
      mutate(time = as.numeric(time), 
             improvement = ifelse(modularity >= base_modularity,1,0),
             improvement_perc = modularity-base_modularity)
    
    results[[file]] <- data
  }
  
  # Combine all results into a single data frame
  combined_data <- bind_rows(results)
  
  #Conjunto de datos de entrada problema machine learning: 
  #incluye todas las combinaciones alfa y beta de pesos con 5 columnas: 
  #network, algoritmo, alfa, beta, mejora o no (0/1)
  
  # Add adjustment for non-improvement cases
  combined_data <- combined_data %>%
    mutate(
      improvement_perc_adjusted = ifelse(improvement == 1, improvement_perc, -0.05),
      improvement_perc_adjusted = as.numeric(improvement_perc_adjusted)
    ) 
  ml_export_data <- combined_data %>% 
    select(network, algorithm, Var1, Var2, Var3, improvement, improvement_perc) %>% 
    #filter(Var1 != 1 & Var2 != 0 & Var3 != 0) %>% 
    distinct()
  
  arrow::write_parquet(ml_export_data,"C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/ml_input_data.parquet")
  # Generate and split plots
  process_and_split_plots(combined_data, output_folder)
}

# Define input paths and call the function
folder_path <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/Nueva ejecucion"
base_modularity_file <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/results from all networks/augmented_modularity_results.csv"
output_folder <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/results from all networks/"

process_and_plot(folder_path, base_modularity_file, output_folder)
