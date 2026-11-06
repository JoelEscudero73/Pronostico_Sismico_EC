library(tidyverse)
library(lubridate)
library(tsibble)
library(slider)
library(here)

# 1. Lectura y limpieza 
datos <- read_csv(here("data", "query.csv"))

sismos_ts <- datos %>%
  mutate(fecha_hora = as_datetime(time)) %>%
  mutate(mes = yearmonth(fecha_hora)) %>%   
  group_by(mes) %>%
  summarise(
    total_sismos = n(),
    mag_maxima   = max(mag, na.rm = TRUE),
    mag_promedio = mean(mag, na.rm = TRUE)
  ) %>%
  as_tsibble(index = mes) %>%
  fill_gaps(total_sismos = 0)

# Convertir a tibble para hacer la descomposición "manual" 
d <- sismos_ts |> as_tibble()

# 2. Descomposición manual clásica aditiva para datos mensuales (periodo m = 12)
# Al ser 12 un número par, aplicamos la tendencia 2 x 12-MA 
d <- d |>
  mutate(
    # Paso a: Media móvil de orden 12 (.before = 5, .after = 6)
    ma_12 = slide_dbl(total_sismos, mean, .before = 5, .after = 6, .complete = TRUE),
    # Paso b: Media móvil de orden 2 para centrarla (Tt final)
    Tt    = slide_dbl(ma_12, mean, .before = 1, .after = 0, .complete = TRUE),
    yt_Tt = total_sismos - Tt,
    n_mes = month(as.Date(mes)) # Extraer el número del mes (1 al 12) para la estacionalidad
  )

# Paso c: Calcular el componente estacional (St) promediando por cada mes del año
St_tab <- d |>
  group_by(n_mes) |>
  summarise(S = mean(yt_Tt, na.rm = TRUE), .groups = "drop") |>
  mutate(St = S - mean(S)) |>
  select(n_mes, St)

# Paso d: Unir la estacionalidad y calcular el residuo (Rt)
d <- d |>
  left_join(St_tab, by = "n_mes") |>
  mutate(Rt = yt_Tt - St)

# ---  GRÁFICAS  ---

# Gráfica 1: Serie original y tendencia (2x12-MA)
ggplot(d, aes(x = as.Date(mes))) +
  geom_line(aes(y = total_sismos), colour = "gray70") +
  geom_line(aes(y = Tt), colour = "#D55E00", linewidth = 1) +
  labs(x = NULL, y = "Total de sismos por mes",
       title = "Serie original y tendencia (2x12-MA) - Sismos")

# Gráfica 2: Componente estacional mensual
ggplot(d, aes(x = as.Date(mes), St)) +
  geom_line(colour = "#0072B2") +
  labs(x = NULL, y = "St",
       title = "Componente estacional mensual - Sismos")

# Gráfica 3: Residuo (componente aleatorio)
ggplot(d, aes(x = as.Date(mes), Rt)) +
  geom_line(colour = "#009E73") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Rt",
       title = "Componente aleatorio (residuo) - Sismos")

