library(tidyverse)

campanias_tbl <- read_csv("data/campanias_2023/mailing_destinatarios.csv")

prematriculados_tbl <- read_csv("data/campanias_2023/prematriculados_2024.csv")


prematriculados_cuenta_isep_tbl <- prematriculados_tbl |> 
  filter(!is.na(email_gapp))

campanias_cuenta_isep_tbl <- campanias_tbl |> 
  filter(!is.na(email_gapp))

datos_alcance_cuenta_isep_tbl <- campanias_cuenta_isep_tbl |> 
  left_join(prematriculados_cuenta_isep_tbl, by = c("email_gapp"="email_gapp"))


names(datos_alcance_cuenta_isep_tbl)

datos_alcance_cuenta_isep_tbl |> 
  filter(prematriculaciones_2024 >= 0) |> 
  mutate(prematriculaciones_anteriores = as.double(prematriculaciones_anteriores)) |> 
  replace_na(list(prematriculaciones_anteriores = 0)) |> 
  ggplot(aes(x = prematriculaciones_anteriores)) +
  geom_histogram() +
  labs(
    x = "# prematriculaciones anteriores",
    y = "total",
    title = ""
  )


datos_cuenta_isep_resumen <- datos_alcance_cuenta_isep_tbl |> 
  mutate(prematriculado_2024 = case_when(
    prematriculaciones_2024 > 0 ~ "sí",
    TRUE ~ "no"
  )) |> 
  count(prematriculado_2024)
  



