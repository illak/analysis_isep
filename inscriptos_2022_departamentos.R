library(tidyverse)
library(sf)
library(glue)


personas <- read_csv("data/inscriptos_2022/inscriptos_2022.csv")


personas_cba <- personas |> 
  filter(provincia %in% c("Córdoba","Cordoba")) |> 
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
  )) |> 
  group_by(departamento) |> 
  summarise(total = sum(total, na.rm = T)) |>
  ungroup() |> 
  mutate(pct = total / sum(total)) |> 
  mutate(pct_label = pct |> scales::label_percent()()) |> 
  arrange(desc(total)) |> 
  left_join(data_deptos_fix, by = c("departamento"="departamento_A"))
  

personas_cba

write_csv(personas_cba, "output/personas_unicas_inscriptas_2022.csv")

# Departamentos de CBA
data_esp <- st_read("https://raw.githubusercontent.com/mgaitan/departamentos_argentina/master/departamentos-cordoba.topojson",
                    stringsAsFactors = FALSE) %>% 
  mutate(id = as.numeric(id))

# Mapeo de IDs de departamentos
data_deptos_isep <- read_csv("data/egresados_16_19/deptos_cba.csv") 


data_deptos_fix <- read_csv("data/egresados_16_19/deptos_fix.csv")




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


data_map <- data_map |> 
  mutate(label_color = case_when(
    departamento.x == "CAPITAL" ~ "white",
    TRUE ~ "grey30"
  ))



plot_mapa_personas <- ggplot(data_map) +
  geom_sf(aes(fill = pct), linewidth = .5, color = "white") +
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
    aes(label = glue("{departamento.y |> str_wrap(width = 10)}
                     <br><span style='font-size:6pt;'>{pct |> scales::percent(accuracy=.01)}</span>"),
        geometry = geometry,
        color = label_color),
      stat = "sf_coordinates",
      alpha = 1,
      size = 1.9,
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

ggsave("output/mapa_personas_inscriptos_2022.png", plot_mapa_personas, 
       width = 6, height = 7, dpi = 320)



mi_number <- function(x){
  #scales::label_number_si()(x)
  scales::label_number()(x)
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


# inscriptos 2023 ---------------------------------------------------------



personas <- read_csv("data/inscriptos/inscriptos_2024.csv")


personas_cba <- personas |> 
  filter(provincia %in% c("Córdoba","Cordoba")) |> 
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
  )) |> 
  group_by(departamento) |> 
  summarise(total = sum(total, na.rm = T)) |>
  ungroup() |> 
  mutate(pct = total / sum(total)) |> 
  mutate(pct_label = pct |> scales::label_percent()()) |> 
  arrange(desc(total)) |> 
  left_join(data_deptos_fix, by = c("departamento"="departamento_A"))


personas_cba

write_csv(personas_cba, "output/personas_unicas_inscriptas_2024.csv")

# Departamentos de CBA
data_esp <- st_read("https://raw.githubusercontent.com/mgaitan/departamentos_argentina/master/departamentos-cordoba.topojson",
                    stringsAsFactors = FALSE) %>% 
  mutate(id = as.numeric(id))

# Mapeo de IDs de departamentos
data_deptos_isep <- read_csv("data/egresados_16_19/deptos_cba.csv") 


data_deptos_fix <- read_csv("data/egresados_16_19/deptos_fix.csv")




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


data_map <- data_map |> 
  mutate(label_color = case_when(
    departamento.x == "CAPITAL" ~ "white",
    TRUE ~ "grey30"
  ))



plot_mapa_personas <- ggplot(data_map) +
  geom_sf(aes(fill = pct), linewidth = .5, color = "white") +
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
  aes(label = glue("{departamento.y |> str_wrap(width = 10)}
                     <br><span style='font-size:6pt;'>{pct |> scales::percent(accuracy=.01)}</span>"),
      geometry = geometry,
      color = label_color),
  stat = "sf_coordinates",
  alpha = 1,
  size = 1.9,
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

ggsave("output/mapa_personas_inscriptos_2024.png", plot_mapa_personas, 
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


