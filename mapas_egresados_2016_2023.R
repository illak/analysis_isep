library(tidyverse)
library(sf)


personas <- read_csv("data/egresados_16_19/egresados.csv")

# Departamentos de CBA
data_esp <- st_read("https://raw.githubusercontent.com/mgaitan/departamentos_argentina/master/departamentos-cordoba.topojson",
                    stringsAsFactors = FALSE) %>% 
  mutate(id = as.numeric(id))

# Mapeo de IDs de departamentos
data_deptos_isep <- read_csv("data/egresados_16_19/deptos_cba.csv") 


data_deptos_fix <- read_csv("data/egresados_16_19/deptos_fix.csv")

personas_cba <- personas %>% 
  filter(provincia %in% c("Córdoba","Cordoba")) %>% 
  mutate(departamento = case_when(
    departamento == "Pte. Roque Saenz Peña" ~ "Presidente Roque Sáenz Peña",
    departamento == "Rio Cuarto" ~ "Río Cuarto",
    departamento == "Rio Primero" ~ "Río Primero",
    departamento == "Marcos Juarez" ~ "Marcos Juárez",
    departamento == "General San Martin" ~ "General San Martín",
    departamento == "Ischilin" ~ "Ischilín",
    departamento == "Juarez Celman" ~ "Juárez Celman",
    departamento == "Rio Seco" ~ "Río Seco",
    departamento == "Santa Maria" ~ "Santa María",
    departamento == "Union" ~ "Unión",
    departamento == "Cruz Del Eje" ~ "Cruz del Eje",
    departamento == "Colon" ~ "Colón",
    departamento == "Rio Segundo" ~ "Río Segundo",
    TRUE ~ departamento
  )) %>% 
  filter(departamento != "Sin Asignar") %>% 
  group_by(anio,departamento) %>% 
  summarise(total = sum(total, na.rm = T)) %>% 
  mutate(pct = total / sum(total)) %>% 
  ungroup() %>% 
  complete(departamento, anio, fill = list(0)) %>% 
  left_join(data_deptos_fix, by = c("departamento"="departamento_A")) %>% 
  mutate(anio = as.factor(anio))

  #mutate(pct = total / sum(total))


data_map <- data_esp %>% 
  left_join(personas_cba, by = c("id"="ID_MAPA"))

puntos_mapa_personas <- data_map %>% 
  filter(pct > .3) %>% 
  mutate(geometry = st_point_on_surface(geometry))

mi_percent <- function(x){
  scales::percent(x, accuracy = 1)
}


# plot
# plot_mapa_personas <- ggplot(data_map) +
#   geom_sf(aes(fill = pct), size = .2) +
#   geom_sf(data = puntos_mapa_personas,
#           color = "black") +
#   ggrepel::geom_label_repel(
#     data = data_map %>% 
#       filter(pct > .03),
#     aes(label = paste0(departamento.x," - ",scales::percent(pct, accuracy = 1)),
#         geometry = geometry),
#     stat = "sf_coordinates",
#     alpha = .6,
#     size = 2.5
#   ) +
#   scale_fill_distiller(palette = "Blues", direction = 1, labels = mi_percent) +
#   theme_void() +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank()
#   )

plot_mapa_personas <- ggplot(data_map) +
  geom_sf(aes(fill = pct), size = .2, color = "grey30") +
  #geom_sf(data = puntos_mapa_personas,
  #       color = "black") +
  ggrepel::geom_label_repel(
    data = data_map %>% filter(pct > .04),
    aes(label = paste0(departamento.x %>% str_to_title(),
                       ": ",scales::percent(pct, accuracy = .1)),
        geometry = geometry),
    stat = "sf_coordinates",
    alpha = .7,
    size = 3.0
  ) +
  scale_fill_distiller(palette = "Blues", direction = 1, labels = mi_percent) +
  facet_wrap(vars(anio), ncol = 4) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", size = 12, hjust = 0)
  )

plot_mapa_personas

ggsave("output/mapa_personas_egresados_2016_2023_todos.png", plot_mapa_personas, 
       width = 10, height = 7, dpi = 320)



mi_number <- function(x){
  scales::label_number_si()(x)
}

plot_mapa_personas <- ggplot(data_map) +
  geom_sf(aes(fill = total), size = .2, color = "grey30") +
  #geom_sf(data = puntos_mapa_personas,
  #       color = "black") +
  ggrepel::geom_label_repel(
    data = data_map,
    aes(label = paste0(departamento.x %>% str_to_title(),
                       ": ", total),
        geometry = geometry),
    stat = "sf_coordinates",
    alpha = .5,
    size = 3.1
  ) +
  scale_fill_distiller(palette = "Blues", direction = 1, 
                       labels = mi_number
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

plot_mapa_personas

ggsave("output/mapa_personas_egresados_total.png", plot_mapa_personas, width = 5, height = 7, dpi = 320)






plot_x_anio <- function(anio){
  
  data_map_temp <- data_map %>% 
    filter(anio == {{anio}})
  
  plot_mapa_personas <- ggplot(data_map_temp) +
    geom_sf(aes(fill = pct), size = .2, color = "grey30") +
    #geom_sf(data = puntos_mapa_personas,
    #       color = "black") +
    ggrepel::geom_label_repel(
      data = data_map_temp %>% 
        filter(pct > .03),
      aes(label = paste0(departamento.x %>% str_to_title(),
                         ": ",scales::percent(pct, accuracy = .1)),
          geometry = geometry),
      stat = "sf_coordinates",
      alpha = .7,
      size = 3.4
    ) +
    scale_fill_distiller(palette = "Blues", direction = 1, labels = mi_percent) +
    #facet_wrap(vars(anio), ncol = 4) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold", size = 12, hjust = 0)
    )
  
  ggsave(paste0("output/mapas/mapa_personas_egresados_total_",anio,".png"), 
         plot_mapa_personas, width = 6, height = 7, dpi = 320)
  
}

walk(c(2016:2023), plot_x_anio)



data_map_white <- data_map %>% 
  filter(anio == 2022)

plot_mapa_white <- ggplot(data_map_white) +
  geom_sf(aes(fill = pct), 
          fill = "grey90",size = .2, color = "grey30") +
  scale_fill_distiller(palette = "Blues", direction = 1, labels = mi_percent) +
  #facet_wrap(vars(anio), ncol = 4) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", size = 12, hjust = 0)
  )

ggsave(paste0("output/mapas/mapa_sedes.png"), 
       plot_mapa_white, width = 6, height = 7, dpi = 320)




# Crecimiento porcentual ----

data_tbl <- personas %>% 
  filter(provincia %in% c("Córdoba","Cordoba")) %>% 
  mutate(departamento = case_when(
    departamento == "Pte. Roque Saenz Peña" ~ "Presidente Roque Sáenz Peña",
    departamento == "Rio Cuarto" ~ "Río Cuarto",
    departamento == "Rio Primero" ~ "Río Primero",
    departamento == "Marcos Juarez" ~ "Marcos Juárez",
    departamento == "General San Martin" ~ "General San Martín",
    departamento == "Ischilin" ~ "Ischilín",
    departamento == "Juarez Celman" ~ "Juárez Celman",
    departamento == "Rio Seco" ~ "Río Seco",
    departamento == "Santa Maria" ~ "Santa María",
    departamento == "Union" ~ "Unión",
    departamento == "Cruz Del Eje" ~ "Cruz del Eje",
    departamento == "Colon" ~ "Colón",
    departamento == "Rio Segundo" ~ "Río Segundo",
    TRUE ~ departamento
  )) %>% 
  filter(departamento != "Sin Asignar") %>% 
  group_by(departamento) %>% 
  mutate(pct_change = (total-lag(total))/lag(total))


library(gt)
library(gtExtras)

gt_sparkline_tab <- data_tbl %>%
  group_by(departamento) %>%
  # must end up with list of data for each row in the input dataframe
  summarize(crecimiento_pct = list(pct_change),
            total = sum(total, na.rm=TRUE), 
            .groups = "drop") %>%
  arrange(desc(total)) %>% 
  gt() %>%
  gt_plt_sparkline(crecimiento_pct)

gt_sparkline_tab



# Egresados x IFDA ----
# Carga de datos de la Fuente Maestra
query_fm <- c(
  "https://isep-cba.edu.ar/autogestion/uploads/dataeval/outputs/query_dataeval-001_output.tsv"
)

query_fm <- c(
  "https://isep-cba.edu.ar/autogestion/uploads/dataeval/outputs/query_fm_original_output.tsv"
)

datos_fm <- read_tsv(query_fm) |> 
  mutate(inscriptos	= as.numeric(inscriptos),
         matriculados_aceptados = as.numeric(matriculados_aceptados),
         aprobados_mi = as.numeric(aprobados_mi),
         inician_cursada_seminario = as.numeric(inician_cursada_seminario),
         cursantes_t3 = as.numeric(cursantes_t3),
         cursando_hoy = as.numeric(cursando_hoy),
         egresados_intermedio = as.numeric(egresados_intermedio),
         egresados = as.numeric(egresados),
         aprobados_1er_uc = as.numeric(aprobados_1er_uc),
         activos_RAI = as.numeric(activos_RAI),
         aprobados_sem = as.numeric(aprobados_sem))


# "read_sheet" es una función de la libreria "googlesheets4"
# datos_fm <- read_sheet(fuente_maestra_URL) %>% 
#   filter(!carrera_siglas %in% c('MOD_INDIVIDUALES','FORM_ADULTOS'))


# Además unifico cohortes "ISEP"
datos_fm <- datos_fm |> 
  filter(!carrera_siglas %in% c('MOD_INDIVIDUALES','FORM_ADULTOS')) %>% 
  mutate(
    idcohorte = case_when(
      idcohorte==201 ~ 199,
      idcohorte==202 ~ 200,
      TRUE ~ idcohorte
    )
  )


egresados_x_ifda_x_anio <- datos_fm %>%
  mutate(anio = as.factor(anio)) %>% 
  mutate(tipo = case_when(
    tipo == "Seminario" ~ "Curso",
    tipo == "Trayecto" ~ "Postítulo",
    tipo == "Profesorado" ~ "Profesorado",
    TRUE ~ "Formación académica"
  )) %>% 
  mutate(
    egresados = ifelse(is.na(egresados), 0, egresados),
    egresados_intermedio = ifelse(is.na(egresados_intermedio), 0, egresados_intermedio),
    aprobados_sem = ifelse(is.na(aprobados_sem),0,aprobados_sem),
    anio = as.factor(anio)
  ) %>% 
  mutate(egresados_totales = case_when(
    tipo == "Curso" ~ aprobados_sem,
    TRUE ~ egresados_intermedio + egresados
  )) %>% 
  group_by(ifda_siglas, ifda, anio) %>% 
  summarise(egresados = sum(egresados_totales, na.rm=TRUE)) %>% 
  ungroup()

library(tidyquant)

egresados_x_ifda_x_anio_plot <- egresados_x_ifda_x_anio %>% 
  filter(egresados > 0) %>% 
  mutate(ifda = str_wrap(ifda, width = 50)) %>% 
  mutate(ifda = ifda %>% fct_reorder(egresados, .fun=sum, .desc = TRUE)) %>% 
  ggplot(aes(x = anio, y = egresados)) +
  geom_col(fill = tidyquant::palette_light()[6],
            width = .8) +
  geom_text(aes(label = egresados), vjust = -.2) +
  expand_limits(y = c(0,2300)) +
  labs(y = NULL, x = NULL) +
  facet_wrap(ifda ~ ., strip.position = "left", ncol = 1) +
  theme_tq() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    strip.text.y.left = element_text(angle = 0, size = 12),
    panel.grid = element_blank(), 
    panel.spacing.y = unit(1, "mm")
  )

egresados_x_ifda_x_anio_plot  


egresados_x_ifda_plot <- datos_fm %>%   
  mutate(anio = as.factor(anio)) %>% 
  mutate(tipo = case_when(
    tipo == "Seminario" ~ "Curso",
    tipo == "Trayecto" ~ "Postítulo",
    tipo == "Profesorado" ~ "Profesorado",
    TRUE ~ "Formación académica"
  )) %>% 
  mutate(
    egresados = ifelse(is.na(egresados), 0, egresados),
    egresados_intermedio = ifelse(is.na(egresados_intermedio), 0, egresados_intermedio),
    aprobados_sem = ifelse(is.na(aprobados_sem),0,aprobados_sem),
    anio = as.factor(anio)
  ) %>% 
  mutate(egresados_totales = case_when(
    tipo == "Curso" ~ aprobados_sem,
    TRUE ~ egresados_intermedio + egresados
  )) %>% 
  group_by(ifda_siglas, ifda) %>% 
  summarise(egresados = sum(egresados_totales, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(egresados > 0) %>% 
  mutate(ifda = str_wrap(ifda, width = 45)) %>%
  mutate(ifda = ifda %>% fct_reorder(egresados, .fun=sum, .desc = TRUE)) %>% 
  ggplot(aes(y = "a", x = 1)) +
  geom_point(aes(size = egresados), alpha = .2) +
  geom_text(aes(label = egresados), hjust = -1) +
  scale_size(range = c(8, 18)) +
  facet_wrap(ifda ~ ., ncol = 1) +
  labs(y = NULL, x = NULL) +
  guides(size = "none") +
  theme_void() +
  theme(
    strip.text = element_blank()
  )

egresados_x_ifda_plot


library(patchwork)

plot_final <- egresados_x_ifda_x_anio_plot + egresados_x_ifda_plot +
    plot_layout(widths = c(2,1))

plot_final

ggsave("output/egresos_x_ifda_x_anio.png", plot_final,
       width = 10, height = 9, dpi = 320)



# Datos para tabla ----


datos_a_tabla <- egresados_x_ifda_x_anio %>% 
  filter(egresados > 0) %>% 
  pivot_wider(names_from = anio, values_from = egresados, values_fill = 0)



datos_a_tabla

write_csv(datos_a_tabla, "output/egresados_ifda_tabla.csv")
  










# mapa egresados 2016-2023 ------------------------------------------------

personas <- read_csv("data/egresados_2016_2023_v2.csv")

# Departamentos de CBA
data_esp <- st_read("https://raw.githubusercontent.com/mgaitan/departamentos_argentina/master/departamentos-cordoba.topojson",
                    stringsAsFactors = FALSE) %>% 
  mutate(id = as.numeric(id))

# Mapeo de IDs de departamentos
data_deptos_isep <- read_csv("data/egresados_16_19/deptos_cba.csv") 


data_deptos_fix <- read_csv("data/egresados_16_19/deptos_fix.csv")

personas_cba <- personas %>% 
  filter(provincia %in% c("Córdoba","Cordoba")) %>% 
  mutate(departamento = case_when(
    departamento == "Pte. Roque Saenz Peña" ~ "Presidente Roque Sáenz Peña",
    departamento == "Rio Cuarto" ~ "Río Cuarto",
    departamento == "Rio Primero" ~ "Río Primero",
    departamento == "Marcos Juarez" ~ "Marcos Juárez",
    departamento == "General San Martin" ~ "General San Martín",
    departamento == "Ischilin" ~ "Ischilín",
    departamento == "Juarez Celman" ~ "Juárez Celman",
    departamento == "Rio Seco" ~ "Río Seco",
    departamento == "Santa Maria" ~ "Santa María",
    departamento == "Union" ~ "Unión",
    departamento == "Cruz Del Eje" ~ "Cruz del Eje",
    departamento == "Colon" ~ "Colón",
    departamento == "Rio Segundo" ~ "Río Segundo",
    TRUE ~ departamento
  )) %>% 
  filter(departamento != "Sin Asignar") %>% 
  group_by(departamento) %>% 
  summarise(total = sum(total, na.rm = T)) %>% 
  mutate(pct = total / sum(total)) %>% 
  ungroup() %>% 
  #complete(departamento, anio, fill = list(0)) %>% 
  left_join(data_deptos_fix, by = c("departamento"="departamento_A"))
  #mutate(anio = as.factor(anio))

#mutate(pct = total / sum(total))


data_map <- data_esp %>% 
  left_join(personas_cba, by = c("id"="ID_MAPA"))

puntos_mapa_personas <- data_map %>% 
  filter(pct > .3) %>% 
  mutate(geometry = st_point_on_surface(geometry))

mi_percent <- function(x){
  scales::percent(x, accuracy = 1)
}


data_map <- data_map |> 
  mutate(label_color = case_when(
    departamento.x == "CAPITAL" ~ "white",
    TRUE ~ "grey30"
  ))

# plot
# plot_mapa_personas <- ggplot(data_map) +
#   geom_sf(aes(fill = pct), size = .2) +
#   geom_sf(data = puntos_mapa_personas,
#           color = "black") +
#   ggrepel::geom_label_repel(
#     data = data_map %>% 
#       filter(pct > .03),
#     aes(label = paste0(departamento.x," - ",scales::percent(pct, accuracy = 1)),
#         geometry = geometry),
#     stat = "sf_coordinates",
#     alpha = .6,
#     size = 2.5
#   ) +
#   scale_fill_distiller(palette = "Blues", direction = 1, labels = mi_percent) +
#   theme_void() +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank()
#   )

library(ggtext)
library(glue)

plot_mapa_personas <- ggplot(data_map) +
  geom_sf(aes(fill = pct), linewidth = .4, color = "white") +
  #geom_sf(data = puntos_mapa_personas,
  #       color = "black") +
  #ggrepel::geom_label_repel(
  
  # geom_sf_text(
  #   aes(label = paste0(departamento.x |> str_wrap(width = 10),
  #                      "\n",total),
  #       geometry = geometry,
  #       color = label_color),
  #   stat = "sf_coordinates",
  #   alpha = 1,
  #   size = 1.5,
  #   #color = "grey40",
  #   fontface = "bold"
  # ) +

  ggtext::geom_richtext(
    aes(label = glue("<span style='font-size:5.9pt;'>{departamento.y |> str_wrap(width = 10)}</span>
                     <br><span style='font-size:6.2pt;'>{total}</span>
                     <br><span style='font-size:5.2pt;'>{pct |> 
                      scales::label_percent(accuracy=.1)()}</span>"),
        geometry = geometry,
        color = label_color),
      stat = "sf_coordinates",
      alpha = 1,
      size = 1.7,
      #color = "grey40",
      label.color = NA,
      fill = NA,
      fontface = "bold"
  ) +

  scale_fill_distiller(palette = "Blues", direction = 1, labels = mi_percent) +
  scale_color_identity() +
  #facet_wrap(vars(anio), ncol = 4) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

plot_mapa_personas

ggsave("output/mapa_personas_egresados_2016_2023_todos.png", plot_mapa_personas, 
       width = 6, height = 7, dpi = 320)



mi_number <- function(x){
  scales::label_number_si()(x)
}

plot_mapa_personas <- ggplot(data_map) +
  geom_sf(aes(fill = total), size = .2, color = "grey30") +
  #geom_sf(data = puntos_mapa_personas,
  #       color = "black") +
  ggrepel::geom_label_repel(
    data = data_map,
    aes(label = paste0(departamento.x %>% str_to_title(),
                       ": ", total),
        geometry = geometry),
    stat = "sf_coordinates",
    alpha = .5,
    size = 3.1
  ) +
  scale_fill_distiller(palette = "Blues", direction = 1, 
                       labels = mi_number
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

plot_mapa_personas

ggsave("output/mapa_personas_egresados_total.png", plot_mapa_personas, width = 5, height = 7, dpi = 320)





# Revisión 2024 ----

datos_fm <- read_tsv(query_fm) |> 
  mutate(
         prematriculados = as.numeric(prematriculados),
         inscriptos	= as.numeric(inscriptos),
         matricula_inicial = as.numeric(matricula_inicial),
         aprobados_mi = as.numeric(aprobados_mi),
         inician_cursada_seminario = as.numeric(inician_cursada_seminario),
         cursantes_t3 = as.numeric(cursantes_t3),
         cursando_hoy = as.numeric(cursando_hoy),
         egresados_intermedio = as.numeric(egresados_intermedio),
         egresados = as.numeric(egresados),
         #aprobados_1er_uc = as.numeric(aprobados_1er_uc),
         activos_RAI = as.numeric(activos_RAI),
         aprobados_sem = as.numeric(aprobados_sem))

# Además unifico cohortes "ISEP"
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
  mutate(egresados_total = egresados + egresados_intermedio + aprobados_sem) |> 
  group_by(anio) |> 
  summarise(
    matricula_inicial_tot = sum(matricula_inicial, na.rm = T),
    cursantes_t3_tot = sum(cursantes_t3, na.rm=T),
    egresados_tot = sum(egresados_total, na.rm=T)
  ) |> View()


egresados_x_ifda_x_anio <- datos_fm %>%
  mutate(anio = as.factor(anio)) %>% 
  mutate(tipo = case_when(
    tipo == "Seminario" ~ "Curso",
    tipo == "Trayecto" ~ "Postítulo",
    tipo == "Profesorado" ~ "Profesorado",
    TRUE ~ "Formación académica"
  )) %>% 
  mutate(
    egresados = ifelse(is.na(egresados), 0, egresados),
    egresados_intermedio = ifelse(is.na(egresados_intermedio), 0, egresados_intermedio),
    aprobados_sem = ifelse(is.na(aprobados_sem),0,aprobados_sem),
    anio = as.factor(anio)
  ) %>% 
  mutate(egresados_totales = case_when(
    tipo == "Curso" ~ aprobados_sem,
    TRUE ~ egresados_intermedio + egresados
  )) %>% 
  group_by(ifda_siglas, ifda, anio) %>% 
  summarise(egresados = sum(egresados_totales[vigencia_prop=="Si"], na.rm=TRUE)) %>% 
  ungroup()


egresados_x_ifda_x_anio |> filter(anio==2020) |> pull(egresados) |> sum()



datos_fm |> 
  filter(tipo=="Curso", anio <= 2023, aprobados_sem > 0) |> 
  group_by(anio, idcohorte) |> 
  summarise(
    aprobados_sem_total = sum(aprobados_sem, na.rm = T),
    matricula_inicial_total = sum(matricula_inicial, na.rm = T),
    tasa_egreso = aprobados_sem_total / matricula_inicial_total,
  ) |> 
  ungroup() |> 
  group_by(anio) |> 
  summarise(
    tasa_egreso_promedio_MI = mean(tasa_egreso),
  ) |> 
  mutate(
    tasa_egreso_promedio_MI = scales::label_percent(accuracy=.1)(tasa_egreso_promedio_MI)
  ) |> 
  pivot_wider(names_from = anio, values_from = tasa_egreso_promedio_MI) |> 
  write_csv("tasa_egreso_cursos_2016_2023.csv")


datos_fm |> 
  filter(tipo=="Curso", anio <= 2023, aprobados_sem > 0) |> 
  group_by(anio, idcohorte) |> 
  summarise(
    matricula_inicial_tot = sum(matricula_inicial, na.rm = T),
    aprobados_sem_total = sum(aprobados_sem, na.rm=T),
    tasa_egreso = aprobados_sem_total / matricula_inicial_tot
  ) |> View()


datos_fm |> 
  filter(linea_formativa=="Formación docente complementaria") |> 
  mutate(egresados_totales = egresados_intermedio+egresados+aprobados_sem) |>
  group_by(anio) |> 
  summarise(
    egresados_totales = sum(egresados_totales, na.rm=T)
  ) |> 
  pivot_wider(names_from = anio, values_from = egresados_totales) |> 
  write_csv("egresados_totales_FDC_2016_2023.csv")

