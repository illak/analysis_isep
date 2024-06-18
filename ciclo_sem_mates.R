library(tidyverse)


ciclo_sem_mates_tbl <- read_csv("data/ciclo_sem_mates.csv")



ciclo_sem_mates_tidy <- ciclo_sem_mates_tbl %>% 
  group_by(dni) %>% 
  mutate(ifda = paste0(ifda, collapse = ",")) %>% 
  ungroup() %>% 
  select(dni, nombre, apellido, emails, email_gapp, localidad_res, departamento_res,
          ifda,seminario, estado) %>% 
  pivot_wider(names_from = seminario, values_from = estado)

write_csv(ciclo_sem_mates_tidy, "output/ciclo_sem_mates_tidy.csv")




ciclo_sem_mates_tidy <- ciclo_sem_mates_tbl %>% 
  mutate(nota_ifd = paste0(estado, "/", ifda)) %>% 
  mutate(ifda_seminario = case_when(
    seminario=="SEMINARIO1GEOMETRIA" ~ "IFDA_SEMINARIO1GEOMETRIA",
    seminario=="SEMINARIONUMEROS" ~ "IFDA_SEMINARIONUMEROS",
    TRUE ~ "IFDA_SEMINARIONUMNATURALES"
  )) %>% 
  select(dni, nombre, apellido, emails, email_gapp, localidad_res, departamento_res,
         ifda, seminario, estado) %>% 
  pivot_wider(names_from = c(seminario), values_from = c(estado,ifda)) %>% 
  relocate(estado_SEMINARIO1GEOMETRIA, ifda_SEMINARIO1GEOMETRIA,
         estado_SEMINARIONUMEROS, ifda_SEMINARIONUMEROS,
         estado_SEMINARIONUMNATURALES, ifda_SEMINARIONUMNATURALES, .after = everything())

write_csv(ciclo_sem_mates_tidy, "output/ciclo_sem_mates_tidy.csv")
