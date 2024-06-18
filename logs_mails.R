library(tidyverse)
library(lubridate)


logs <- read_csv("data/logs_2.csv")

logs_1 <- read_csv("data/logs.csv")

logs |> 
  mutate(start = ymd_hms(`Start date`, locale = Sys.getlocale("LC_TIME"))) |>
  summarise(max = max(start),
            min = min(start))

# Veo las distintas fechas que figuran en el log
logs |> 
  mutate(date = date(`Start date`)) |> 
  distinct(date)


# Cantidad de emails (id único) con el error especificado
logs |> 
  filter(`Event status` == "Transient Error") |> 
  summarise(total = n_distinct(`Message ID`))

  
#filter(`Message ID` == "<CAKJX9HG7FSCHPxnMaPeTLsLJiJouZUp1aPhTAmr-ujcXMBaEAg@mail.gmail.com>") |> 


# Genero grupos por estado de evento y obtengo el timestamp minimo en que ocurre
# Particularmente el de error
min_t_error <- logs |> 
  mutate(date = date(`Start date`)) |> 
  mutate(start = ymd_hms(`Start date`, locale = Sys.getlocale("LC_TIME"))) |>
  mutate(start_loc = with_tz(start, tzone = "America/Buenos_Aires")) |> 
  filter(date == "2023-04-05") |> 
  group_by(`Event status`) |> 
  summarise(ids = n_distinct(`Message ID`),
            min_start = min(start)) |> 
  ungroup() |> 
  filter(`Event status` == "Transient Error") |> 
  pull(min_start)
  
min_t_error


# Filtro los registros anteriores a ese primer timestamp y cuento
# la totalidad de mails únicos registrados
logs |> 
  mutate(date = date(`Start date`)) |> 
  mutate(start = ymd_hms(`Start date`, locale = Sys.getlocale("LC_TIME"))) |>
  mutate(start_loc = with_tz(start, tzone = "America/Buenos_Aires")) |> 
  filter(date == "2023-04-05") |> 
  filter(start  < min_t_error) |>
  summarise(total = n_distinct(`Message ID`))


  

