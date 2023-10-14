#Tarea 3 
#Josefa Anselmo - Gabriel Sotomayor
library(tidyverse)
library(openxlsx)

unzip("data.zip")

tarea3<- function(dir) {

complete_dfs <- list()
incomplete_info <- data.frame(area = character(),
                              missing_percentage = numeric(),
                              stringsAsFactors = FALSE)

for (i in dir(path = dir)) {
  data <- read.csv(paste0(dir, i)) 
  colnames(data) <- tolower(colnames(data))

  data <- data %>% 
    select(-x) %>% 
    pivot_longer(
      cols = starts_with("y"),
      names_to = "year",
      values_to = "temperature_change"
    ) %>%
    mutate(year = as.integer(str_replace(year, "y", ""))) %>%
    mutate(date = as.Date(paste(year, sprintf("%02d", match(months, month.name)), "15", sep = "-")))
  
  missing <- sum(is.na(data$temperature_change))
  total <- nrow(data)
  
  if (missing == 0 & all(1961:2019 %in% data$year)) {
    complete_dfs[[length(complete_dfs) + 1]] <- data
  } else {
    missing_porc <- (missing/ total) * 100
    incomplete_info <- rbind(incomplete_info, data.frame(area = gsub(".csv", "", i),
                                                         missing_percentage = missing_porc))
  }
}

# Unir los dataframes completos en un solo dataframe
df <- bind_rows(complete_dfs)

# Guardar el dataframe final como df_final.csv
write.csv(df, "df_final.csv", row.names = FALSE)

# Guardar la tabla de información incompleta como tabla_missing.xlsx
write.xlsx(incomplete_info, "tabla_missing.xlsx")
}


tarea3("data/")
