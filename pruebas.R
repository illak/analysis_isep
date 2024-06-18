library(tidyverse)


data_tbl <- read_tsv("https://isep-cba.edu.ar/autogestion/uploads/dataeval/outputs/query_fd_coordi_uc_cursada_output.tsv")



data_fechainicio_as_Date_tbl <- data_tbl %>% 
  filter(carrera_siglas=="TRAYECTO_PED") %>% 
  mutate(fechainicio = fechainicio %>% as.Date()) %>% 
  arrange(fechainicio)


data_fechainicio_as_Date_tbl$fechainicio %>% class()

data_fechainicio_as_Date_tbl %>% View()
