install.packages(c("sf", "ggplot2", "dplyr", "gridExtra", "rmarkdown"))
library(sf)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)
library(gridExtra)
library(rmarkdown)

# Load Spain map
spain_map <- ne_states(country = "Spain", returnclass = "sf")

# Filter out Canary Islands and reposition them
peninsula <- spain_map %>% filter(!region %in% c("Canary Is."))
canarias <- spain_map %>% filter(region %in% c("Canary Is."))
st_geometry(canarias) <- st_geometry(canarias) + c(6.5, 6.5)

peninsula <- st_transform(peninsula, crs = 4326)
canarias <- st_set_crs(canarias, 4326)
spain_map_adjusted <- rbind(peninsula, canarias)

# Load monthly partition files
monthly_partitions <- read_xlsx("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Tabla Modularidades/Case study/monthly partitions.xlsx")
monthly_louvain <- read_xlsx("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/Tabla Modularidades/Case study/monthly_partitions_louvain.xlsx")

# Ensure column names match for merging
monthly_partitions <- monthly_partitions %>% rename(region = Node)
monthly_louvain <- monthly_louvain %>% rename(region = Node)

# Define colors
colores <- c("#E74C3C", "#3498DB", "#33FF57", "#F1C40F", "#8E44AD")

# Create a PDF to store the plots
pdf("monthly_maps_comparison.pdf", width = 14, height = 8)

# Loop through months and generate plots
for (month in unique(monthly_partitions$Month)) {
  partition_data <- filter(monthly_partitions, Month == month)
  louvain_data <- filter(monthly_louvain, Month == month)
  
  map1 <- left_join(spain_map_adjusted, partition_data, by = "region")
  map2 <- left_join(spain_map_adjusted, louvain_data, by = "region")
  
  plot1 <- ggplot(data = map1) +
    geom_sf(aes(fill = factor(Partition)), color = "black") +
    scale_fill_manual(values = setNames(colores, unique(map1$Partition))) +
    theme_minimal() +
    labs(title = paste("MCG-CDP -", month), fill = "Partition") +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  plot2 <- ggplot(data = map2) +
    geom_sf(aes(fill = factor(Louvain)), color = "black") +
    scale_fill_manual(values = setNames(colores, unique(map2$Louvain))) +
    theme_minimal() +
    labs(title = paste("Louvain -", month), fill = "Louvain Community") +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  grid.arrange(plot1, plot2, ncol = 2)
}

# Close the PDF
dev.off()
