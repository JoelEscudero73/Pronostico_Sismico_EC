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
#----------------------------------------------------------------#
#Auditoria:
#EL codigo compila y muestra las graficas de la descomposicion 
#de la serie
#Ahora vamos a ver si lo que hace el codigo es correcto de acuerdo
#al modelo de descomposicion integrado en R

#Agregamos estas dos librerias
library(feasts)
library(fabletools)


# grafica: descomposicion manual en 4 paneles

d_largo <- d |>
  mutate(fecha = as.Date(mes)) |>
  select(fecha,
         `yt (serie original)` = total_sismos,
         `Tt (tendencia)`      = Tt,
         `St (estacional)`     = St,
         `Rt (residuo)`        = Rt) |>
  pivot_longer(-fecha, names_to = "componente", values_to = "valor") |>
  mutate(componente = factor(componente,
                             levels = c("yt (serie original)", "Tt (tendencia)",
                                        "St (estacional)", "Rt (residuo)")))

ggplot(d_largo, aes(fecha, valor)) +
  geom_line(aes(colour = componente), show.legend = FALSE) +
  facet_wrap(~componente, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("gray40", "#D55E00", "#0072B2", "#009E73")) +
  labs(x = NULL, y = NULL,
       title = "Descomposición clásica aditiva MANUAL - Sismos",
       subtitle = "yt = Tt + St + Rt")

# grafica del modelo de R (classical_decomposition)

modelo_comp <- sismos_ts |>
  model(classical_decomposition(total_sismos ~ season(12), type = "additive")) |>
  components()

modelo_comp |>
  autoplot() +
  labs(title = "Descomposicion clásica aditiva - Sismos")

#comprobacion: manual-modelo (debe dar cercano a 0)
comparacion <- d |>
  left_join(
    modelo_comp |> as_tibble() |> select(mes, trend, seasonal, random),
    by = "mes"
  ) |>
  mutate(
    dif_Tt = Tt - trend,
    dif_St = St - seasonal,
    dif_Rt = Rt - random
  )

# Máxima diferencia absoluta por componente
comparacion |>
  summarise(
    max_dif_Tt = max(abs(dif_Tt), na.rm = TRUE),
    max_dif_St = max(abs(dif_St), na.rm = TRUE),
    max_dif_Rt = max(abs(dif_Rt), na.rm = TRUE)
  )

# TRUE si coinciden dentro de la tolerancia numerica
all(abs(c(comparacion$dif_Tt, comparacion$dif_St, comparacion$dif_Rt)) < 1e-10,
    na.rm = TRUE)

# Verificación interna: yt=Tt+St+Rt
d |>
  mutate(chequeo = Tt + St + Rt - total_sismos) |>
  summarise(max_error = max(abs(chequeo), na.rm = TRUE))

#Como se muestra el valor de la diferencia es cercano a 0
#por el error de la maquina, que no toma todos los decimales,
#no se aprecia el 0. 
