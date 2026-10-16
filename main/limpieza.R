library(tidyverse)
library(tsibble)
library(here)


datos <- read_csv(here("data", "query.csv"))


sismos_ts <- datos %>%
  mutate(fecha_hora = as_datetime(time)) %>%
  mutate(mes = yearmonth(fecha_hora)) %>%   #estaba puesto mounth y era month de ahi todo bien 
  group_by(mes) %>%
  summarise(
    total_sismos = n(),
    mag_maxima   = max(mag, na.rm = TRUE),
    mag_promedio = mean(mag, na.rm = TRUE)
  ) %>%
  as_tsibble(index = mes)


print(sismos_ts)

