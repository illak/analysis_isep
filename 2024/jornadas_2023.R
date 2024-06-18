library(tidyverse)
library(tidyquant)


data_jornadas_tbl <- read_csv("2024/data/datos_jornada_4ta_2v.csv")

data_jornadas_tbl |> glimpse()

data_jornadas_no_isep <- data_jornadas_tbl |> 
  filter(sexo == "NULL", idpersona_au=="NULL")

data_jornadas_no_isep |> 
  write_csv("presentes_no_isep_listado.csv")


data_jornadas_isep <- data_jornadas_tbl |> 
  filter(idpersona_au!="NULL")


data_jornadas_isep_b <- data_jornadas_tbl |> 
  filter(idpersona_au=="NULL", sexo!="NULL")

data_jornadas_isep_b |> glimpse()


data_jornadas_isep_plot <- data_jornadas_isep |> 
  group_by(sexo, asistencia) |> 
  summarise(total = n_distinct(idpersona_au)) |> 
  ungroup() |> 
  mutate(pct = total / sum(total)) |> 
  mutate(asistencia = case_when(
    asistencia==1 ~ "Presente",
    TRUE ~ "Ausente"
  )) |> 
  ggplot(aes(x = sexo, y = total, fill = sexo)) +
  geom_col(width = .5) +
  geom_label(aes(label = paste0(total, " (",pct |> scales::label_percent()(),")")),
            vjust = -.5, size=4, color="white") +
  scale_fill_tq() +
  expand_limits(y=c(0,280)) +
  guides(fill = "none") +
  labs(y = "Total inscriptos", x = NULL,
       title = "Distribución de asistencia a evento: 4ta Jornada de Estudios",
       subtitle = "Personas que registran interacción con ISEP (al menos cursó una propuesta)"
  ) +
  facet_wrap(vars(asistencia)) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(size=11),
    strip.text = element_text(size=11, hjust = 0, face = "bold"),
    #strip.background = element_rect(fill="grey")
  )

data_jornadas_isep_plot

ggsave("asistencia_isep.png", data_jornadas_isep_plot, width = 7, height = 6)

data_jornadas_no_isep_plot <- data_jornadas_no_isep |> 
  filter(dni!="NULL") |> 
  distinct(dni, asistencia) |>
  select(dni, asistencia) |>
  group_by(asistencia) |>
  summarise(total = n()) |> 
  ungroup() |> 
  mutate(pct = total / sum(total)) |> 
  mutate(asistencia = case_when(
    asistencia==1 ~ "Presente",
    TRUE ~ "Ausente"
  )) |> 
  ggplot(aes(x = asistencia, y = total, fill = asistencia)) +
  geom_col(width = .5) +
  geom_label(aes(label = paste0(total, " (",pct |> scales::label_percent()(),")")),
            vjust = -.5, size=5, color="white") +
  scale_fill_tq() +
  expand_limits(y=c(0,60)) +
  guides(fill = "none") +
  labs(y = "Total inscriptos", x = NULL,
       title = "Distribución de asistencia a evento: 4ta Jornada de Estudios",
       subtitle = "Personas que no registran interacción previa con ISEP (no tenemos datos extra)") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(size=15)
  )

data_jornadas_no_isep_plot
ggsave("asistencia_no_isep.png", data_jornadas_no_isep_plot, width = 6, height = 6)






data_jornadas_isep_b_plot <- data_jornadas_isep_b |> 
  group_by(sexo, asistencia) |> 
  summarise(total = n()) |> 
  ungroup() |> 
  mutate(pct = total / sum(total)) |> 
  mutate(asistencia = case_when(
    asistencia==1 ~ "Presente",
    TRUE ~ "Ausente"
  )) |> 
  ggplot(aes(x = sexo, y = total, fill = sexo)) +
  geom_col(width = .5) +
  geom_label(aes(label = paste0(total, " (",pct |> scales::label_percent()(),")")),
             vjust = -.5, size=4, color="white") +
  scale_fill_tq() +
  expand_limits(y=c(0,75)) +
  guides(fill = "none") +
  labs(y = "Total inscriptos", x = NULL,
       title = "Distribución de asistencia a evento: 4ta Jornada de Estudios",
       subtitle = "Personas que registran interacción con ISEP (pero no fueron destinatarias)"
  ) +
  facet_wrap(vars(asistencia)) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    axis.text.x = element_text(size=11),
    strip.text = element_text(size=11, hjust = 0, face = "bold"),
    #strip.background = element_rect(fill="grey")
  )

data_jornadas_isep_b_plot
ggsave("asistencia_isep_b.png", data_jornadas_isep_b_plot, width = 6, height = 6)
