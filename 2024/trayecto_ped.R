library(tidyverse)


data_trayecto_tbl <- read_csv("2024/data/trayecto_ped.csv")


data_trayecto_depto_norm <- data_trayecto_tbl |> 
  mutate(provincia = case_when(
    provincia=="Cordoba" ~ "Córdoba",
    TRUE ~ provincia
  )) |> 
  mutate(departamento = case_when(
    departamento == "Pte. Roque Saenz Peña" ~ "Presidente Roque Sáenz Peña",
    departamento == "Rio Cuarto" ~ "Río Cuarto",
    departamento == "Rio Primero" ~ "Río Primero",
    departamento == "Marcos Juarez" ~ "Marcos Juárez",
    departamento == "General San Martin" ~ "General San Martín",
    departamento == "Ischilin" ~ "Ischilín",
    departamento == "Juarez Celman" ~ "Juárez Celman",
    departamento == "Rio Seco" ~ "Río Seco",
    departamento == "Santa Maria" ~ "Santa María",
    departamento == "Union" ~ "Unión",
    departamento == "Cruz Del Eje" ~ "Cruz del Eje",
    departamento == "Colon" ~ "Colón",
    departamento == "Rio Segundo" ~ "Río Segundo",
    TRUE ~ departamento
  ))


data_trayecto_depto_norm |> write_csv("2024/output/trayecto_ped_norm.csv")
