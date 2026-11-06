library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(slider)
library(here)


archivo <- here("Data", "Datos_limpios.csv")

primera <- readLines(archivo, n = 1)
sep <- if (grepl("\t", primera)) "\t" else if (grepl(";", primera)) ";" else ","

datos_raw <- read_delim(archivo, delim = sep, show_col_types = FALSE)

# Convertir la fecha para tsibble

meses <- c(ene = 1, feb = 2, mar = 3, abr = 4, may = 5, jun = 6,
           jul = 7, ago = 8, sep = 9, oct = 10, nov = 11, dic = 12)

variable <- "total_sismos"     # también: "mag_maxima" o "mag_promedio"

ts_datos <- datos_raw |>
  mutate(
    anio    = as.integer(str_extract(mes, "\\d{4}")),
    mes_abb = str_sub(str_to_lower(str_remove_all(mes, "[^A-Za-z]")), 1, 3),
    Mes     = yearmonth(make_date(anio, meses[mes_abb], 1)),
    y       = .data[[variable]]
  ) |>
  select(Mes, y) |>
  as_tsibble(index = Mes)

has_gaps(ts_datos)    # debe dar FALSE (sin meses faltantes)
nrow(ts_datos)
min(ts_datos$y) > 0   # la multiplicativa exige valores > 0

#Descomposicion multiplicativa manual con m=12
# yt = Tt * St * Rt

d <- ts_datos |> as_tibble()

# paso 1: Tt con 2x12-MA (m par)
d <- d |>
  mutate(
    MA12 = slide_dbl(y, mean, .before = 5, .after = 6, .complete = TRUE),
    Tt   = slide_dbl(MA12, mean, .before = 1, .after = 0, .complete = TRUE)
  )

# paso 2: serie sin tendencia: yt/Tt
d <- d |> mutate(yt_Tt = y / Tt, mes_num = month(Mes))

# paso 3: indices estacionales
# a) media de (yt/Tt) por mes del año (12 valores)
# b) ajustar para que sumen 12
St_tab <- d |>
  group_by(mes_num) |>
  summarise(S = mean(yt_Tt, na.rm = TRUE), .groups = "drop") |>
  mutate(St = S * 12 / sum(S)) |>
  select(mes_num, St)

d <- d |> left_join(St_tab, by = "mes_num")

#paso 4: Rt=yt/(Tt*St)
d <- d |> mutate(Rt=y/(Tt*St))

# Comprobaciones internas
sum(St_tab$St)                              
d |> summarise(max_error = max(abs(Tt * St * Rt - y), na.rm = TRUE))

# fecha en formato Date para graficar
d <- d |> mutate(fecha = as.Date(Mes))


# Grafica 1: serie original y tendencia
ggplot(d, aes(fecha)) +
  geom_line(aes(y = y), colour = "gray60") +
  geom_line(aes(y = Tt), colour = "#D55E00", linewidth = 1) +
  labs(x = NULL, y = variable,
       title = paste("Serie original y tendencia (2x12-MA)", variable))

# Grafica 2: componente estacional (indices estacionales)
ggplot(d, aes(fecha, St)) +
  geom_line(colour = "#0072B2") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = NULL, y = "St",
       title = paste("Componente estacional multiplicativo", variable))

# Grafica 3: residuo (se centra en 1)
ggplot(d, aes(fecha, Rt)) +
  geom_line(colour = "#009E73") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = NULL, y = "Rt",
       title = paste("Componente aleatorio multiplicativo", variable))

# Grafica del modelo multiplicativo 
modelo_comp <- ts_datos |>
  model(classical_decomposition(y ~ season(12), type = "multiplicative")) |>
  components()

modelo_comp |>
  autoplot() +
  labs(title = paste("Descomposicion clasica multiplicativa", variable))

#Comprobacion: manual-modelo (debe dar cercano a 0)

comparacion <- d |>
  left_join(
    modelo_comp |> as_tibble() |> select(Mes, trend, seasonal, random),
    by = "Mes"
  ) |>
  mutate(
    dif_Tt = Tt - trend,
    dif_St = St - seasonal,
    dif_Rt = Rt - random
  )

comparacion |>
  summarise(
    max_dif_Tt = max(abs(dif_Tt), na.rm = TRUE),
    max_dif_St = max(abs(dif_St), na.rm = TRUE),
    max_dif_Rt = max(abs(dif_Rt), na.rm = TRUE)
  )


all(abs(c(comparacion$dif_Tt, comparacion$dif_St, comparacion$dif_Rt)) < 1e-10,
    na.rm = TRUE)