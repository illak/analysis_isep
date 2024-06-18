library(tidyverse)
library(tidyquant)


tickets_tbl <- read_csv("data/ticketera/tickets.csv")

names(tickets_tbl)

tickets_tbl <- tickets_tbl %>% 
  mutate(dias = as.integer(dias)) %>% 
  filter(!is.na(dias)) %>% 
  filter(dias >= 0)


plot_tickets <- tickets_tbl %>% 
  group_by(tipologia) %>% 
  summarise(tickets_generados = n()) %>% 
  ungroup() %>% 
  filter(tickets_generados > 3) %>% 
  ggplot(aes(x = fct_reorder(tipologia, tickets_generados),
             y = tickets_generados)) +
  geom_col(fill = "#CAB2D6", width = .7) +
  geom_text(aes(label = tickets_generados),
            hjust = "inward", size = 8, ) +
  coord_flip() +
  labs(x = NULL,
       y = NULL) +
  theme_tq()


plot_tickets


plot_tickets2 <- tickets_tbl %>%
  filter( tipologia %in% c("Errores / casos inesperados.",
                           "Caso diario: matriculación tardía (trayecto).",
                           "Caso diario: inscripción tardía (módulo).",
                           "Caso diario: inactivación.",
                           "Soporte.")) %>% 
  ggplot(aes(x = dias, fill = tipologia)) +
  geom_histogram(bins = 10,
                 binwidth = 5,
                 center = -2.5,
                 color = "white") +
  facet_wrap(vars(tipologia)) +
  scale_fill_tq() +
  labs(y = NULL) +
  guides(fill = "none") +
  theme_tq()


plot_tickets2

ggsave("output/tickets_dias.png", width = 9, height = 4)






plot_tickets_evol <- tickets_tbl %>% 
  filter(tipologia == "Errores / casos inesperados.") %>% 
  mutate(mes = month(ts_gen, label = TRUE)) %>% 
  group_by(mes) %>% 
  summarise(mediana_dias = mean(dias, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(x = mes,
             y = mediana_dias)) +
  geom_line(aes(group = "a")) +
  labs(y = "Promedio días",
       x = NULL) +
  theme_tq()

plot_tickets_evol

ggsave("output/tickets_dias_promedio.png", width = 9, height = 4)    
