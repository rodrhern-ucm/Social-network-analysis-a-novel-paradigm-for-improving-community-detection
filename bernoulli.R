# Load required libraries
library(dplyr)

# Load data
data <- arrow::read_parquet("C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/ml_input_data.parquet")
data[is.na(data)] <- 0 
write.csv(data,"C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/ml_input_data.csv")
#data <- na.omit(data)
# Define the critical value for a 95% confidence interval
z <- 1.96

# Function to calculate confidence intervals 1st option
write.csv2(
rbind(
data %>%
group_by(algorithm) %>%
summarise(
n_k = n(),
p = mean(improvement),
margin_of_error = z * sqrt(p * (1 - p) / n_k),
lower_bound = p - margin_of_error,
upper_bound = p + margin_of_error
) %>%
ungroup()
,
data %>%
summarise(
n_k = n(),
p = mean(improvement),
margin_of_error = z * sqrt(p * (1 - p) / n_k),
lower_bound = p - margin_of_error,
upper_bound = p + margin_of_error
) %>% mutate(algorithm = "Total") %>% select(algorithm,n_k,p,margin_of_error,lower_bound,upper_bound) %>%
ungroup()
),"C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/results from all networks/bernoulli_1.csv")


# Function to calculate confidence intervals 2nd option
write.csv2(data %>% 
    group_by(network,algorithm) %>% 
    summarise(p = any(improvement == 1)) %>% 
    group_by(algorithm) %>%
    summarise(
      n_k = n(),  # Número de casos en cada grupo
      p = mean(p),  # 1 si existe al menos una mejora
      margin_of_error = z * sqrt(p * (1 - p) / n_k),
      lower_bound = p - margin_of_error,
      upper_bound = p + margin_of_error
    ) %>% 
    ungroup(),"C:/Users/rodri/OneDrive/Documentos/DOCTORADO/Paper Flins Inske Datos/Redes/output/results from all networks/bernoulli_2.csv")





