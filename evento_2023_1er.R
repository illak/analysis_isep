library(tidyverse)
library(tidyquant)

raw_masiva_tbl <- read_csv("data/eventos_2023/fuente_masiva.csv")
raw_masiva_tbl <- read_csv("data/eventos_2023/fuente_masiva_v2.csv")

raw_formulario_tbl <- read_csv("data/eventos_2023/formulario.csv")

raw_evento_insc_tbl <- read_csv("data/eventos_2023/eventos_inscripciones.csv")

raw_formulario_tbl <- raw_formulario_tbl %>% 
  filter(!is.na(`Marca temporal`)) %>% 
  filter(DNI != 35018128)

raw_formulario_tbl %>% View()

formulario_clean_tbl <- raw_formulario_tbl %>% 
  rename(email = "Dirección de correo electrónico",
         modalidad = "¿Desea asistir de manera presencial, en Córdoba Capital, o participar de manera virtual?",
         momento = "Indique, a continuación, a qué momento de la jornada desea inscribirse",
         bloque1 = "En el bloque de las 14 a las 15, ¿en qué mesa de diálogo quiere anotarse? Tenga en cuenta que puede inscribirse sólo a una.",
         bloque2 = "En el bloque de 15.30 a 16.30, ¿en qué mesa de diálogo desea inscribirse?  Tenga en cuenta que puede inscribirse sólo a una.")



formulario_clean_tbl %>% View()


formulario_metadata_tbl <- formulario_clean_tbl %>% 
  left_join(raw_masiva_tbl, by = c("email"="email"))


formulario_metadata_tbl %>% View()

formulario_metadata_tbl %>% 
  filter(!is.na(suscripcion)) %>% 
  count(tiene_cuenta_institucional)

plot_cant_niveles <- formulario_metadata_tbl %>% 
  select(email, inicial, primaria, secundaria, superior) %>% 
  pivot_longer(cols = inicial:superior, names_to = "nivel", values_to = "valor") %>% 
  group_by(nivel) %>% 
  summarise(total = sum(valor, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(nivel, total), y = total)) +
  geom_col(fill = tidyquant::palette_light()[1]) +
  geom_label(aes(label = total),
            size = 9, color = "black") +
  labs(x = "Nivel", y = "Total",
       title = "Cantidad de docentes que se desempeñan en los distintos niveles",
       subtitle = "Un docente puede desempeñarse en más de un nivel") +
  theme_tq(base_size = 15) +
  theme(
    plot.title.position = "plot"
  )


ggsave(filename="output/evento_2023_1er/niveles_total.jpg", plot_cant_niveles, width = 8)

plot_prop_niveles <- formulario_metadata_tbl %>% 
  select(email, inicial, primaria, secundaria, superior) %>% 
  pivot_longer(cols = inicial:superior, names_to = "nivel", values_to = "valor") %>% 
  group_by(nivel) %>% 
  summarise(total = sum(valor, na.rm = TRUE)) %>% 
  mutate(prop = total / sum(total)) %>% 
  ungroup() %>% 
  mutate(text = str_c(nivel %>% str_to_title(), "\n", prop %>% scales::label_percent()())) %>% 
  mutate(nivel = fct_reorder(nivel, prop)) %>% 
  ggplot(aes(x="", y=prop, fill=nivel)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(label = text), position = position_stack(vjust = 0.5),
            color = "white", size=6) +
  scale_fill_tq() +
  labs(title = "El 67% de los participantes registrados se desempeñan mayormente\nen los niveles de primaria y secundaria ") +
  theme(
    plot.title.position = "plot"
  )
  

ggsave(filename="output/evento_2023_1er/niveles_prop.jpg", plot_prop_niveles)





plot_niveles_modalidad <- formulario_metadata_tbl %>% 
  select(email, modalidad, inicial, primaria, secundaria, superior) %>% 
  pivot_longer(cols = inicial:superior, names_to = "nivel", values_to = "valor") %>% 
  group_by(nivel, modalidad) %>% 
  summarise(total = sum(valor, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(x = fct_reorder(nivel, total), y = total)) +
  geom_col(fill = tidyquant::palette_light()[1]) +
  geom_label(aes(label = total),
             size = 9, color = "black") +
  labs(x = "Nivel", y = "Total",
       title = "Cantidad de docentes que se desempeñan en los distintos niveles",
       subtitle = "Un docente puede desempeñarse en más de un nivel") +
  facet_wrap(vars(modalidad)) +
  theme_tq(base_size = 15) +
  theme(
    plot.title.position = "plot"
  )


ggsave(filename="output/evento_2023_1er/niveles_modalidad.jpg", plot_niveles_modalidad, width = 9)







plot_cuentas_isep <- formulario_metadata_tbl %>% 
  mutate(cuenta_institucional = case_when(
         tiene_cuenta_institucional==1 ~ "Posee cuenta ISEP",
         tiene_cuenta_institucional==0 ~ "No posee cuenta ISEP",
         TRUE ~ "Sin dato")) %>% 
  count(cuenta_institucional) %>%
  mutate(prop = n / sum(n)) %>% 
  mutate(cuenta_institucional = fct_reorder(cuenta_institucional, prop)) %>% 
  mutate(text = str_c(cuenta_institucional, "\n", prop %>% scales::label_percent()())) %>% 
  ggplot(aes(x="", y=prop, fill=cuenta_institucional)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(label = text), position = position_stack(vjust = 0.5),
            color = "white", size=5) +
  scale_fill_tq() +
  labs(title = "Del total de participantes, apróximadamente el 40% posee una cuenta institucional",
       subtitle = "Esto indica que el alrededor del 40% de los participantes han interactuado con nuestra oferta académica")

ggsave(filename="output/evento_2023_1er/cuentas_isep.jpg", plot_cuentas_isep, width = 9)

plot_cuentas


formulario_metadata_tbl %>% 
  filter(tiene_cuenta_institucional == 1) %>% 
  count(estado_ISEP)



plot_cuentas_origen <- formulario_metadata_tbl %>% 
  mutate(fuente = case_when(
    is.na(fuente) ~ "Sin dato",
    str_detect(fuente, "au") ~ "Fuente Autogestión",
    TRUE ~ "Otras fuentes"
  )) %>% 
  count(fuente) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(fuente = fct_reorder(fuente, prop)) %>% 
  mutate(text = str_c(fuente, "\n", prop %>% scales::label_percent(accuracy=.01)())) %>% 
  ggplot(aes(x="", y=prop, fill=fuente)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(label = text), position = position_stack(vjust = 0.5),
            color = "white", size=5) +
  scale_fill_tq() +
  labs(title = "Del total de cuentas de correo registradas, ~46% son institucionales",
       subtitle = "~22% provienen de otras fuentes y ~33% no se encuentran en la base de datos")

plot_cuentas_origen
ggsave(filename="output/evento_2023_1er/cuentas_origen.jpg", plot_cuentas_origen, width = 9)




plot_evento <- formulario_metadata_tbl %>% 
  filter(modalidad == "Presencial") %>% 
  select(email, inicial:superior, momento, bloque1, bloque2) %>% 
  pivot_longer(cols = momento:bloque2, names_to = "bloque", values_to = "nombre") %>% 
  select(-bloque) %>% 
  filter(!is.na(nombre)) %>% 
  filter(nombre != "Para la jornada completa") %>% 
  filter(nombre != "Sólo la jornada de la tarde") %>% 
  filter(nombre != "No deseo anotarme para asistir a ninguna mesa de diálogo en este horario.") %>% 
  pivot_longer(cols = inicial:superior, names_to = "nivel", values_to = "valor") %>% 
  group_by(nombre, nivel) %>% 
  summarise(total = sum(valor, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    nombre = gsub("\\|.*", "", nombre),
    nombre = str_wrap(nombre, width = 40)) %>% 
  ggplot(aes(x = nivel, y = total)) +
  geom_col(fill = "#6A3D9A") +
  geom_label(aes(label = total)) +
  facet_wrap(vars(nombre)) +
  theme_tq()


ggsave(filename="output/evento_2023_1er/plot_evento.jpg", width = 9, height = 7)




formulario_metadata_final_tbl <- formulario_metadata_tbl %>% 
  mutate(email = email %>% str_to_lower()) %>% 
  left_join(raw_evento_insc_tbl %>% mutate(email = email %>% str_to_lower()),
            by = c("email"="email"))

formulario_metadata_final_tbl %>% View()


plot_final <- formulario_metadata_final_tbl %>% 
  filter(modalidad == "Presencial") %>% 
  select(email, asistencia, evento = nombre_corto, inicial:superior) %>% 
  mutate(sin_dato = ifelse(inicial + primaria + secundaria + superior==0, 1, 0)) %>% 
  pivot_longer(cols = inicial:sin_dato, names_to = "nivel", values_to = "valor") %>% 
  mutate(asistencia = case_when(
    is.na(asistencia) ~ "No",
    asistencia=="NULL" ~ "No",
    TRUE ~ "Si"
  )) %>% 
  filter(!is.na(evento)) %>% 
  mutate(asistencia = as.factor(asistencia)) %>% 
  mutate(nivel = factor(nivel, levels = c("inicial",
                                          "primaria",
                                          "secundaria",
                                          "superior",
                                          "sin_dato"))) %>% 
  mutate(evento = str_wrap(evento, width = 40)) %>% 
  group_by(evento, nivel, asistencia) %>% 
  summarise(total = sum(valor, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(x = nivel, y = total, fill = fct_rev(asistencia))) +
  geom_col() +
  facet_wrap(vars(evento), scales = "free_x") +
  scale_fill_tq() +
  labs(fill = "Asistencia") +
  theme_tq()

plot_final

ggsave(filename="output/evento_2023_1er/plot_evento_asistencia.jpg", 
       plot_final, width = 12, height = 10)






plot_asistencia <- formulario_metadata_final_tbl %>% 
  #filter(modalidad == "Presencial") %>% 
  mutate(asistencia = case_when(
    asistencia == 1 ~ "Presente",
    TRUE ~ "Ausente"
  )) %>% 
  select(email, asistencia, evento = nombre_corto) %>% 
  filter(!is.na(evento)) %>% 
  group_by(evento, asistencia) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(asistencia = fct_reorder(asistencia, prop)) %>% 
  mutate(text = str_c(asistencia, "\n", prop %>% scales::label_percent()())) %>% 
  mutate(evento = str_wrap(evento, width = 40)) %>% 
  ggplot(aes(x="", y=prop, fill=asistencia)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(label = text), position = position_stack(vjust = 0.5),
            color = "white", size=4) +
  scale_fill_tq() +
  labs(title = "",
       subtitle = "") +
  facet_wrap(vars(evento))

plot_asistencia

ggsave(filename="output/evento_2023_1er/cuentas_isep.jpg", plot_asistencia, width = 9)


asistencia_presenciales_raw <- read_csv("data/eventos_2023/asistencia_presenciales.csv")

asistencia_presenciales_tbl <- asistencia_presenciales_raw %>% 
  mutate(faltas = total - asistencia) %>% 
  select(evento, asistencia, faltas) %>% 
  pivot_longer(cols = asistencia:faltas, names_to = "asistencia", values_to = "valor") %>% 
  mutate(asistencia = case_when(
    asistencia == "asistencia" ~ "Presente",
    TRUE ~ "Ausente"
  )) %>% 
  group_by(evento) %>% 
  mutate(total = sum(valor)) %>% 
  ungroup() %>% 
  mutate(prop = valor / total)


asistencia_presenciales_plot <- asistencia_presenciales_tbl %>% 
  mutate(text = str_c(asistencia, "\n", prop %>% scales::label_percent()())) %>% 
  mutate(asistencia = fct_rev(asistencia)) %>% 
  mutate(evento = str_wrap(evento, width = 40)) %>% 
  ggplot(aes(x="", y=prop, fill=asistencia)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(label = text), position = position_stack(vjust = 0.5),
            color = "white", size=4) +
  scale_fill_tq() +
  labs(title = "",
       subtitle = "") +
  facet_wrap(vars(evento))

asistencia_presenciales_plot

ggsave(filename="output/evento_2023_1er/asist_presenciales.jpg", 
       asistencia_presenciales_plot, width = 12, height = 9)


