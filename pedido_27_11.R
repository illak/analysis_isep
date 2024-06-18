library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lubridate) # Make Dealing with Dates a Little Easier
library(showtext) # Using Fonts More Easily in R Graphs # Using Fonts More Easily in R Graphs
library(ggbeeswarm) # Categorical Scatter (Violin Point) Plots
library(googlesheets4) # Access Google Sheets using the Sheets API V4
library(scales) # Scale Functions for Visualization
library(patchwork) # The Composer of Plots
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(ggnewscale) # Multiple Fill and Colour Scales in 'ggplot2'
library(sf) # Simple Features for R
library(ggchicklet) # Create 'Chicklet' (Rounded Segmented Column) Charts
library(waffle) # Create Waffle Chart Visualizations



# Carga de datos de la Fuente Maestra ----
query_fm <- c(
  "https://isep-cba.edu.ar/autogestion/uploads/dataeval/outputs/query_dataeval-001_output.tsv"
)


datos_fm <- read_tsv(query_fm) |> 
  mutate(prematriculados	= as.numeric(prematriculados),
                  inscriptos = as.numeric(inscriptos),
                  aprobados_mi = as.numeric(aprobados_mi),
                  inician_cursada_seminario = as.numeric(inician_cursada_seminario),
                  cursantes_t3 = as.numeric(cursantes_t3),
                  cursando_hoy = as.numeric(cursando_hoy),
                  egresados_intermedio = as.numeric(egresados_intermedio),
                  egresados = as.numeric(egresados),
                  matricula_inicial = as.numeric(matricula_inicial),
                  activos_RAI = as.numeric(activos_RAI),
                  aprobados_sem = as.numeric(aprobados_sem))


# "read_sheet" es una función de la libreria "googlesheets4"
# datos_fm <- read_sheet(fuente_maestra_URL) %>% 
#   filter(!carrera_siglas %in% c('MOD_INDIVIDUALES','FORM_ADULTOS'))


# Además unifico cohortes "ISEP"s
datos_fm <- datos_fm |> 
  filter(!carrera_siglas %in% c('MOD_INDIVIDUALES','FORM_ADULTOS')) %>% 
  mutate(
    idcohorte = case_when(
      idcohorte==201 ~ 199,
      idcohorte==202 ~ 200,
      TRUE ~ idcohorte
    )
  )


datos_fm |> 
  mutate(egresados = egresados + egresados_intermedio + aprobados_sem) |> 
  summarise(total = sum(egresados, na.rm = T))




cohortes_egresados_tbl <- datos_fm |> 
  mutate(egresados_suma = egresados + egresados_intermedio + aprobados_sem) |> 
  group_by(carrera) |> 
  summarise(
    cohortes = n_distinct(idcohorte),
    egresados_historicos = sum(egresados_suma, na.rm = TRUE)
  ) |> 
  ungroup()

cohortes_egresados_tbl  

write_csv(cohortes_egresados_tbl, "output/cohortes_egresados_historicos.csv")  
  


datos_fm |> 
  select(ifda, ifda_siglas) |> 
  distinct() |>
  write_csv("output/ifds.csv")



matricula_inicial_tipo <- datos_fm |> 
  filter(anio <= 2023) |> 
  group_by(tipo,anio) |> 
  summarise(matricula_inicial_total = sum(matricula_inicial, na.rm = TRUE)) |> 
  ungroup()

write_csv(matricula_inicial_tipo, "output/tabla_matricula_inicial_tipo.csv")

matricula_inicial_ifda <- datos_fm |> 
  filter(anio <= 2023) |> 
  group_by(ifda, anio) |> 
  summarise(matricula_inicial_total = sum(matricula_inicial, na.rm = TRUE)) |> 
  mutate(acumulado = cumsum(matricula_inicial_total)) |> 
  ungroup()

write_csv(matricula_inicial_ifda, "output/tabla_matricula_inicial_ifda.csv")

matricula_inicial_lf <- datos_fm |> 
  filter(anio <= 2023) |> 
  group_by(linea_formativa, anio) |> 
  summarise(matricula_inicial_total = sum(matricula_inicial, na.rm = TRUE)) |> 
  mutate(acumulado = cumsum(matricula_inicial_total)) |> 
  ungroup()

write_csv(matricula_inicial_lf, "output/tabla_matricula_inicial_lf.csv")




matricula_ini_mara <- datos_fm |> 
  filter(anio == "2022", vigencia_prop == "Si") %>%
  group_by(anio) %>% 
  summarise(cohortes_vigentes = n_distinct(idcohorte),
            inscriptos = sum(inscriptos, na.rm = T),
            matricula_inicial = sum(matricula_inicial, na.rm = T),
            cursantes = sum(cursantes_t3, na.rm = T))



resumen <- datos_fm |> 
  ##filter(anio <= 2023) |> 
  filter(carrera_siglas != "FORM_ADULTOS") %>%
  mutate(egresados = egresados_intermedio + egresados + aprobados_sem) |> 
  group_by(anio) |> 
  summarise(
    propuestas = n_distinct(propuesta[vigencia_prop=="Si"]),
    inscriptos = sum(inscriptos, na.rm = T),
    matricula_inicial = sum(matricula_inicial, na.rm = T),
    cursantes = sum(cursantes_t3, na.rm = T),
    egresados = sum(egresados, na.rm = T)
  ) |> 
  pivot_longer(cols= -anio, names_to = "metrica", values_to = "total") |> 
  pivot_wider(names_from = anio, values_from = total)


resumen

write_csv(resumen,"output/resumen_2016_2023.csv")



resumen <- datos_fm |> 
  filter(anio <= 2023) |> 
  filter(anio != 2024,
         carrera_siglas != "FORM_ADULTOS") %>%
  mutate(egresados = egresados_intermedio + egresados + aprobados_sem) |> 
  group_by(linea_formativa) |> 
  summarise(
    propuestas = n_distinct(propuesta[vigencia_prop=="Si"]),
    inscriptos = sum(inscriptos, na.rm = T),
    matricula_inicial = sum(matricula_inicial, na.rm = T),
    cursantes = sum(cursantes_t3, na.rm = T),
    egresados = sum(egresados, na.rm = T)
  )


datos_fm |> filter(is.na(linea_formativa) | linea_formativa == "NULL") |> View()




cohortes <- read_tsv("https://isep-cba.edu.ar/autogestion/uploads/dataeval/outputs/query_carreras_cohortes_output.tsv")

prop_nuevas_23 <- cohortes |> 
  group_by(linea_formativa, carrera) |> 
  summarise(min_cohorte = min(min_cohorte_anio)) |> 
  filter(min_cohorte==2023)

prop_nuevas_23 |> View()
