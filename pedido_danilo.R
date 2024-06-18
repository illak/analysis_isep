library(tidyverse)


datos_fm_tbl <- read_csv("data/fm_30_3_2022.csv") |> 
  filter(!carrera_siglas %in% c('MOD_INDIVIDUALES','FORM_ADULTOS')) |> 
  mutate(tipo = case_when(
    tipo == 'Seminario' ~ 'Curso',
    tipo == 'Trayecto' ~ 'Postítulo',
    tipo == 'Profesorado' ~ tipo,
    TRUE ~ 'Formación académica')
  )



carreras_por_anio <- datos_fm_tbl |> 
  group_by(carrera, tipo) |> 
  summarise(cohortes = min(cohorte_anio))


carreras_por_anio |> View()

write_csv(carreras_por_anio, "output/carreras_por_anio.csv")
