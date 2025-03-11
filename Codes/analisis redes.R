library(igraph)
library(rvest)
library(readr)
library(hydra)
library(igraphdata)
library(visNetwork)
library(combinat)
library(dplyr)
library(ggplot2)
library(tidyr)

# Ruta de la carpeta con los CSV
folder_path <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Tabla Modularidades/Nueva Funcion resultados"

# Obtener lista de archivos CSV en la carpeta
file_list <- list.files(path = folder_path, pattern = "\\_modularity_results.csv$", full.names = TRUE)
file_list <- subset(file_list, file_list != "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/Nueva ejecucion/augmented_modularity_results.csv")


# Inicializar lista para almacenar resultados
results <- list()

# Leer el archivo de casos base
#case_base_data <- read.csv("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/results from all networks/augmented_modularity_results.csv")
#case_base_data <- case_base_data %>% rename(base_modularity = modularity)

# Función actualizada para calcular métricas
calculate_metrics_algorithm <- function(data) {
  case_base_data <- data %>% filter(Var1 == 1 & Var2== 0 & Var3 == 0) %>% select(network,algorithm,modularity) %>% rename(base_modularity = modularity)
  # Unir el caso base con los datos actuales por network y algoritmo
  data <- data %>% left_join(case_base_data, by = c("network", "algorithm")) %>%
                   mutate(time = as.numeric(time), 
                          improvement = ifelse(modularity >= base_modularity ,1,0),
                          improvement_perc = modularity-base_modularity)
        
  
  # Calcular métricas por algoritmo
  metrics <- data %>%
    group_by(algorithm) %>%
    summarize(
      improvement_cases = mean(improvement,na.rm = TRUE) * 100,
      improvement_percentage_avg = mean(improvement_perc[improvement == 1], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(metrics)
}


# Procesar cada archivo
for (file in file_list) {
  # Leer el archivo
  data <-  read.csv(file)
  
  # Calcular métricas
  metrics <- calculate_metrics_algorithm(data)
  
  # Agregar nombre del archivo
  metrics <- metrics %>%
    mutate(file_name = basename(file))
  
  # Guardar en la lista de resultados
  results[[file]] <- metrics
}

# Combinar todos los resultados en una tabla final
final_results <- bind_rows(results)
final_results$improvement_percentage_avg <- ifelse(is.nan(final_results$improvement_percentage_avg),0,final_results$improvement_percentage_avg)

# Pivotar para obtener formato deseado
final_table <- final_results %>%
  pivot_wider(
    names_from = algorithm,
    values_from = c(improvement_cases, improvement_percentage_avg),
    names_sep = "_"
  )

final_table <- final_table %>%
  mutate(
    network = stringr::str_remove(file_name, "_modularity_results\\.csv$")
  )
final_table <- final_table %>% select(-file_name)
#openxlsx::write.xlsx(final_table,"C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/results from all networks/resultados_redes_algoritmos.xlsx")


# Gráfico de caja para comparar algoritmos globalmente
final_results %>%
  ggplot(aes(x = factor(toupper(algorithm)), y = improvement_cases, fill = factor(toupper(algorithm)))) +
  geom_boxplot() +
  labs(
    title = "Algorithm Improvement Distribution",
    x = "Algorithm",
    y = "Improvement Percentage (%)",
    fill = "Algorithm"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  scale_fill_brewer(palette = "Set1")
