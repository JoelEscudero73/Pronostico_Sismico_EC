library(tidyverse)
library(tsibble)
library(here)

#Leer los datos limpios
datos <- read_csv(here("data", "Datos_limpios.csv"), show_col_types = FALSE)

#Diccionario de meses
meses_es <- c("ene"=1, "feb"=2, "mar"=3, "abr"=4, "may"=5, "jun"=6, 
              "jul"=7, "ago"=8, "sep"=9, "oct"=10, "nov"=11, "dic"=12)

# Convertir el texto a fecha real y en tsibble
df <- datos %>%
  mutate(
    anio    = as.integer(str_sub(mes, 1, 4)),
    mes_txt = tolower(str_sub(mes, 6, 8)),
    mes_num = unname(meses_es[mes_txt]),
    fecha   = make_date(anio, mes_num, 1),
    mes     = yearmonth(fecha)
  )

sismos_ts <- as_tsibble(df, index = mes)

print(sismos_ts)


#Graficas

# 1. Serie Temporal General
# Descripcion: Mide la cantidad total de sismos registrados 
# en el eje Y frente al transcurso del tiempo (meses y años) 
# en el eje X.
# Comienza el primer punto en octubre 2020, cada punto es el mes siguiente

p1 <- ggplot(df, aes(x = fecha, y = total_sismos)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.2) +
  labs(
    title = "Serie temporal de sismos mensuales",
    x = "Fecha (Año)",
    y = "Total de sismos"
  ) +
  theme_minimal()
print(p1)


# 2. Comparación Mensual por Año
# Descripcion: Mide el número de sismos en el eje Y frente 
# a los doce meses del año (Ene-Dic) en el eje X, donde cada 
# línea de color representa un año distinto.

p2 <- ggplot(df, aes(x = mes_num, y = total_sismos, color = factor(anio), group = anio)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = 1:12, 
    labels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
  ) +
  labs(
    title = "Comportamiento mensual de sismos por año",
    x = "Mes del año",
    y = "Total de sismos",
    color = "Año"
  ) +
  theme_minimal()
print(p2)


# 3. Función de Autocorrelación (ACF)
# Descripcion: Mide el grado de relación o dependencia en el eje Y
# frente al número de meses de rezago (desfase) en el eje X, 
# delimitado por bandas azules de significancia.

serie_ts <- ts(df$total_sismos, frequency = 12)
acf_calc <- acf(serie_ts, lag.max = 24, plot = FALSE)
ci       <- qnorm(0.975) / sqrt(length(serie_ts))

acf_df <- tibble(lag = as.numeric(acf_calc$lag), acf = as.numeric(acf_calc$acf))

p3 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "blue") +
  geom_hline(yintercept = 0, color = "black") +
  geom_segment(aes(xend = lag, yend = 0), color = "gray20", linewidth = 0.8) +
  geom_point(color = "firebrick", size = 2) +
  labs(
    title = "Función de Autocorrelación (ACF)",
    x = "Rezago (Meses)",
    y = "Autocorrelación"
  ) +
  theme_minimal()
print(p3)




#Comenbtario:MC
# 1. Tendencia
# Tendencia creciente: el número de sismos aumenta con el tiempo.
#2. Estacionalidad
# Se analiza si existe un patrón que se repite cada 12 meses.
#  Estacionalidad fuerte: determinados meses presentan repetidamente
#  cantidades altas o bajas de sismos.
# 3. Ciclos
#  Sin ciclos evidentes: predominan fluctuaciones irregulares
#   o patrones estacionales, sin ciclos prolongados claramente definidos.
# 4. Estacionariedad
# Estacionariedad débil:
#  La media y la varianza permanecen constantes y la autocovarianza
#  depende únicamente del rezago
