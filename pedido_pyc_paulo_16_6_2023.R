library(tidyverse)
library(tidyquant)


data_pyc <- read_csv("data/datos_pyc_16_6.csv")

glimpse(data_pyc)


data_pyc_analysis <- data_pyc %>% 
  group_by(idpersona) %>% 
  summarise(min_anio = min(cohorte_anio)) %>% 
  ungroup() %>% 
  mutate(min_anio = as.factor(min_anio)) %>% 
  group_by(min_anio) %>% 
  summarise(cursantes_1ra_vez = n_distinct(idpersona)) %>% 
  ungroup() %>% 
  ggplot(aes(x = min_anio, y = cursantes_1ra_vez)) +
  geom_col(fill = palette_light()[1], width = .7) +
  geom_label(aes(label = cursantes_1ra_vez), size = 6, vjust = 1.1, alpha = .7) +
  labs(
    title = "Cantidad de personas que cursan por primera vez el ciclo PYC",
    subtitle = str_glue(
      "Se consideran quienes han tenido o tienen alguno de los siguientes estados:
      (Cursando, Cursando condicional, Reprobado, Abandonó, Aprobado)"),
    x = NULL,
    y = NULL
  ) +
  theme_tq(base_size = 12) +
  theme(
    plot.title.position = "plot",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15)
  )


data_pyc_analysis
ggsave("output/pyc_recurrencia.png", data_pyc_analysis)

library(lubridate)
library(ggrepel)

data_pyc %>% 
  distinct(fechainicio)

plot_recurrencia_x_seminario <- data_pyc %>% 
  filter(fechainicio != "NULL") %>%
  mutate(fechainicio = ymd(fechainicio)) %>% 
  mutate(mes = month(fechainicio, label = TRUE),
         anio = year(fechainicio)) %>%
  mutate(edicion = str_c(mes, anio),
         edicion = factor(edicion) %>% fct_reorder(fechainicio)) %>% 
  select(idpersona, fechainicio, mes, anio, carrera_siglas, edicion) %>% 
  # Nos quedamos con la fila de datos con fechainicio mínima para cada persona
  slice_min(by = idpersona, order_by = fechainicio) %>%
  group_by(edicion, carrera_siglas) %>% 
  summarise(total = n_distinct(idpersona)) %>% 
  mutate(pct = total / sum(total)) %>% 
  ungroup() %>% 
  # Visualización
  ggplot(aes(x = edicion, y = total, fill = carrera_siglas)) +
  geom_col() +
  geom_text(
    aes(label = pct %>% scales::label_percent(accuracy = 1)()),
    position = position_stack(vjust = .5),
    size = 3
  ) +
  scale_fill_tq() +
  # Quitamos el "espacio" entre datos y axis
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  labs(
    title = "Cantidad de personas nuevas en el ciclo de seminarios de PYC",
    subtitle = str_glue("Se consideran los estados (Cursando, Cursando condicional, Reprobado, Abandonó, Aprobado)"),
    x = "Edición",
    y = "Total",
    fill = "Seminario"
  ) +
  theme_tq(base_size = 12) +
  theme(
    plot.title.position = "plot"
  )

ggsave("output/recurrencia_x_seminario.png", plot_recurrencia_x_seminario)  

plot_recurrencia_x_seminario_facetted <- data_pyc %>% 
  filter(fechainicio != "NULL") %>%
  mutate(fechainicio = ymd(fechainicio)) %>% 
  mutate(mes = month(fechainicio, label = TRUE),
         anio = year(fechainicio)) %>%
  mutate(edicion = str_c(mes, anio),
         edicion = factor(edicion) %>% fct_reorder(fechainicio)) %>% 
  select(idpersona, fechainicio, mes, anio, carrera_siglas, edicion) %>% 
  # Nos quedamos con la fila de datos con fechainicio mínima para cada persona
  slice_min(by = idpersona, order_by = fechainicio) %>%
  group_by(edicion, carrera_siglas) %>% 
  summarise(total = n_distinct(idpersona)) %>% 
  mutate(pct = total / sum(total)) %>% 
  ungroup() %>% 
  # Visualización
  ggplot(aes(x = edicion, y = total, fill = carrera_siglas)) +
  geom_col() +
  geom_text(
    aes(label = pct %>% scales::label_percent(accuracy = 1)()),
    size = 4,
    vjust = -.2
  ) +
  scale_fill_tq() +
  # Quitamos el "espacio" entre datos y axis
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  labs(
    title = "Cantidad de personas nuevas en el ciclo de seminarios de PYC",
    subtitle = str_glue("Se consideran los estados (Cursando, Cursando condicional, Reprobado, Abandonó, Aprobado).\nPorcentajes respecto al total de cursantes de la edición."),
    x = "Edición",
    y = "Total",
    fill = "Seminario"
  ) +
  guides(fill = "none") +
  theme_tq(base_size = 12) +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  facet_wrap(vars(carrera_siglas))

plot_recurrencia_x_seminario_facetted

ggsave("output/recurrencia_x_seminario_facetted.png", plot_recurrencia_x_seminario_facetted, width = 15, height = 10)
