library(tidyverse)


# El pedido corresponde a listado de personas de cohorte Ago2023 de la diplo
# que tengan aprobadas materias de cohortes 2020/2021, para equivalencias

diplo_cyg_2023_tbl <- read_csv("data/datos_cyg_v2.csv")

my_flatten <- function(x) {
  str_flatten(x, collapse = "-")
}

diplo_cyg_tidy_tbl <- diplo_cyg_2023_tbl %>% 
  select(dni, nombre, apellido, carrera_siglas, carrera_siglas_20_21,
         cohorte, cohorte_20_21, estado, modulo_20_21, estado_20_21, nota) %>% 
  mutate(
    tiene_aprobadas = case_when(
      modulo_20_21 != "NULL" & estado_20_21 %in% c("Aprobado","Equivalencia") ~ "sí",
      TRUE ~ "no"
    )
  ) %>% 
  mutate(texto = str_c(carrera_siglas_20_21, cohorte_20_21, estado_20_21, nota, sep = "/")) %>% 
  select(dni, nombre, apellido, carrera_siglas, tiene_aprobadas, modulo_20_21, cohorte, texto) %>% 
  pivot_wider(names_from = modulo_20_21, values_from = texto, 
              values_fn = my_flatten) %>% 
  select(-`NULL`)


diplo_cyg_tidy_tbl %>% View()

write_csv(diplo_cyg_tidy_tbl, "output/diplo_cyg_tidy.csv")



cyg_equivalencias_tbl <- read_csv("data/cyg_equivalencias_v3.csv")


cyg_equivalencias_tramo <- cyg_equivalencias_tbl %>% 
  filter(carrera_siglas %in% c("GESTIONINI","GESTIONPRI","GESTIONSEC")) %>% 
  select(carrera, cohorte, tipo_titulo, estado_diplo, dni, nombre, apellido, modulo_siglas, calificacion_virtual) %>% 
  pivot_wider(names_from = modulo_siglas, values_from = calificacion_virtual)

write_csv(cyg_equivalencias_tramo,"output/cyg_equivalencias_tramo.csv")

cyg_equivalencias_espe <- cyg_equivalencias_tbl %>% 
  filter(carrera_siglas %in% c("GESTIONINI_ESP","GESTIONPRI_ESP","GESTIONSEC_ESP","GESTIONSUP_ESP")) %>% 
  select(carrera, cohorte, tipo_titulo, estado_diplo, dni, nombre, apellido, modulo_siglas, calificacion_virtual) %>% 
  pivot_wider(names_from = modulo_siglas, values_from = calificacion_virtual)

write_csv(cyg_equivalencias_espe,"output/cyg_equivalencias_espe.csv")

cyg_equivalencias_act <- cyg_equivalencias_tbl %>% 
  filter(carrera_siglas %in% c("GESTIONINI_ACT","GESTIONPRI_ACT","GESTIONSEC_ACT","GESTIONSUP_ACT")) %>% 
  select(carrera, cohorte, tipo_titulo, estado_diplo, dni, nombre, apellido, modulo_siglas, calificacion_virtual) %>% 
  pivot_wider(names_from = modulo_siglas, values_from = calificacion_virtual)

write_csv(cyg_equivalencias_act,"output/cyg_equivalencias_act.csv")



# Revisión final 16/08/2023 ----
cyg_equivalencias_tbl <- read_csv("data/cyg_equivalencias_v4.csv")


cyg_equivalencias_tramo <- cyg_equivalencias_tbl %>% 
  filter(carrera_siglas %in% c("GESTIONINI","GESTIONPRI","GESTIONSEC")) %>% 
  select(idmatriculacion, carrera, cohorte, tipo_titulo, fechafinalizacion, estado_diplo, dni, nombre, apellido, 
         modulo_siglas, calificacion_virtual, calificacion_fecha, estado_final_cursada=estado, ifda_modulo) %>% 
  group_by(idmatriculacion) %>% 
  mutate(promedio_final = mean(calificacion_virtual, na.rm = TRUE))

write_csv(cyg_equivalencias_tramo,"output/cyg_equivalencias_tramo.csv")


cyg_equivalencias_espe <- cyg_equivalencias_tbl %>% 
  filter(carrera_siglas %in% c("GESTIONINI_ESP","GESTIONPRI_ESP","GESTIONSEC_ESP","GESTIONSUP_ESP")) %>% 
  select(idmatriculacion, carrera, cohorte, tipo_titulo, fechafinalizacion, estado_diplo, dni, nombre, apellido, 
         modulo_siglas, calificacion_virtual, calificacion_fecha, estado_final_cursada=estado, ifda_modulo) %>% 
  group_by(idmatriculacion) %>% 
  mutate(promedio_final = mean(calificacion_virtual, na.rm = TRUE))

write_csv(cyg_equivalencias_espe,"output/cyg_equivalencias_espe.csv")



cyg_equivalencias_act <- cyg_equivalencias_tbl %>% 
  filter(carrera_siglas %in% c("GESTIONINI_ACT","GESTIONPRI_ACT","GESTIONSEC_ACT","GESTIONSUP_ACT")) %>% 
  select(carrera, cohorte, tipo_titulo, estado_diplo, dni, nombre, apellido, 
         modulo_siglas, calificacion_virtual, calificacion_fecha, estado_final_cursada=estado, ifda_modulo)

write_csv(cyg_equivalencias_act,"output/cyg_equivalencias_act.csv")



cyg_diplo_equivalencias_tbl <- read_csv("data/cyg_diplo_equivalencias.csv")

cyg_diplo_final_tbl <- cyg_diplo_equivalencias_tbl %>% 
  select(idmatriculacion, carrera, cohorte, tipo_titulo, fechafinalizacion, dni, nombre, apellido, 
         modulo_siglas, calificacion_virtual, calificacion_fecha, estado_final_cursada=estado, ifda_modulo) %>% 
  group_by(idmatriculacion) %>% 
  mutate(promedio_final = mean(calificacion_virtual, na.rm = TRUE))

write_csv(cyg_diplo_final_tbl,"output/cyg_equivalencias_diplo.csv")




# Revisión final v2 23/08/2023 ----
cyg_equivalencias_tbl <- read_csv("data/cyg_v5.csv")


cyg_equivalencias_tramo <- cyg_equivalencias_tbl %>% 
  filter(carrera_siglas %in% c("GESTIONINI","GESTIONPRI","GESTIONSEC")) %>% 
  select(idmatriculacion, carrera, cohorte, ifda, validez, tipo_titulo, fechafinalizacion, estado_diplo, dni, nombre, apellido, 
         modulo_siglas, calificacion_virtual, calificacion_fecha, estado_final_cursada=estado, ifda_modulo) %>% 
  group_by(idmatriculacion) %>% 
  mutate(promedio_final = mean(calificacion_virtual, na.rm = TRUE))

write_csv(cyg_equivalencias_tramo,"output/cyg_equivalencias_tramo_v2.csv")


cyg_equivalencias_espe <- cyg_equivalencias_tbl %>% 
  filter(carrera_siglas %in% c("GESTIONINI_ESP","GESTIONPRI_ESP","GESTIONSEC_ESP","GESTIONSUP_ESP")) %>% 
  select(idmatriculacion, carrera, cohorte, ifda, validez, tipo_titulo, fechafinalizacion, estado_diplo, dni, nombre, apellido, 
         modulo_siglas, calificacion_virtual, calificacion_fecha, estado_final_cursada=estado, ifda_modulo) %>% 
  group_by(idmatriculacion) %>% 
  mutate(promedio_final = mean(calificacion_virtual, na.rm = TRUE))

write_csv(cyg_equivalencias_espe,"output/cyg_equivalencias_espe_v2.csv")



cyg_equivalencias_act <- cyg_equivalencias_tbl %>% 
  filter(carrera_siglas %in% c("GESTIONINI_ACT","GESTIONPRI_ACT","GESTIONSEC_ACT","GESTIONSUP_ACT")) %>% 
  select(carrera, cohorte, validez, tipo_titulo, estado_diplo, dni, nombre, apellido, 
         modulo_siglas, calificacion_virtual, calificacion_fecha, estado_final_cursada=estado, ifda_modulo)

write_csv(cyg_equivalencias_act,"output/cyg_equivalencias_act.csv")



cyg_diplo_equivalencias_tbl <- read_csv("data/cyg_diplo_equivalencias.csv")

cyg_diplo_final_tbl <- cyg_diplo_equivalencias_tbl %>% 
  select(idmatriculacion, carrera, cohorte, ifda, tipo_titulo, fechafinalizacion, dni, nombre, apellido, 
         modulo_siglas, calificacion_virtual, calificacion_fecha, estado_final_cursada=estado, ifda_modulo) %>% 
  group_by(idmatriculacion) %>% 
  mutate(promedio_final = mean(calificacion_virtual, na.rm = TRUE))

write_csv(cyg_diplo_final_tbl,"output/cyg_equivalencias_diplo.csv")
