library(tidyverse)
library(stringr)


data_tbl <- read_csv("data/output.csv")


names(data_tbl) <- c("PATH", "Existe", "idpersona", "dni", "cohorte", "condicion_docum")


data_tbl |> glimpse()


# Resumen
resumen <- data_tbl |> 
  group_by(Existe) |> 
  summarise(
    total = n(),
    personas = n_distinct(idpersona)
  )


resumen


data_tbl_NO_EXISTE <- data_tbl |> 
  filter(Existe == "NO EXISTE") 

data_tbl_NO_EXISTE

datos_no_existe_reconstruido <- data_tbl_NO_EXISTE |> 
  mutate(
    idmatricula = str_split_i(PATH, "/", -3) |> as.numeric(),
    idcond = str_split_i(PATH, "/", -2) |> as.numeric()
  ) 


datos_no_existe_reconstruido_unicos <- datos_no_existe_reconstruido |> 
  distinct(idpersona, idmatricula, cohorte, dni)


write_csv(datos_no_existe_reconstruido_unicos, "casos_no_existen.csv")


