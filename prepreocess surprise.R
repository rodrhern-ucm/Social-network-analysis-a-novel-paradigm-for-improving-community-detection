library(dplyr)
folder_path <- "C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output"
files <- list.files(folder_path, pattern = "\\_modularity_results.csv$", full.names = TRUE)

for (file in files[2:length(files)]) {
  print(paste("Processing", file, "..."))
  
  # Read the CSV file
  df <- read.csv(file)
  
  # Replace NA values in Var1, Var2, Var3 with values from w1, w2, w3
  df$Var1[is.na(df$Var1)] <- df$w1[is.na(df$Var1)]
  df$Var2[is.na(df$Var2)] <- df$w2[is.na(df$Var2)]
  df$Var3[is.na(df$Var3)] <- df$w3[is.na(df$Var3)]
  df <- df %>% dplyr::select(-w1,-w2, -w3)
  # Save the updated DataFrame back to the same file
  write.csv(df, file, row.names = FALSE)
  print(paste("Finished processing", file))
}
