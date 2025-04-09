library(tsibble)
library(readxl)
library(tidyverse)
library(tsibbledata)
library(fable)
library(fpp3)
library(forecast)
library(ggplot2)
library(ggfortify)
library(tseries)
library(imputeTS)
library(tidyr)
library(dplyr)
library(zoo)

temp <- read_excel("C:/.../tempmedia.xlsx")
datosts <- ts(temp$TempMedia, start = c(2015,1), frequency = 12)
datostsNA <- datosts

Box.test(datosts, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
           "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

temp <- temp %>%
  mutate(
    Mes = tolower(Mes),
    Mes_num = match(Mes, meses)
  )

temp <- temp %>%
  mutate(Fecha = as.yearmon(paste(Año, Mes_num), "%Y %m"))

ggplot(temp, aes(x = Fecha, y = TempMedia)) +
  geom_line(linewidth = 1) +
  geom_point(color = "#1874CD", size = 2.5) +
  labs(
    title = "Gráfico 4.2.1: Temperatura media de la ciudad de Rosario desde el 2015 al 2025",
    x = "Año",
    y = "Temperatura media (°C)"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20) 
  ) +
  scale_x_yearmon(
    limits = c(2015,2025.2),
    format = "%Y",  
    n = 12  
  )

ggAcf(datosts, plot=TRUE,  lag.max=120) +
  labs(
    title = "Gráfico 4.2.2: Función de autocorrelación muestral",
    x = "Rezago",
    y = "FACM"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 26, face = "bold"),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20) 
  ) +
  scale_y_continuous(
    limits = c(-1,1)
  )

ggPacf(datosts, plot=TRUE, lag.max=120) +
  labs(
    title = "Gráfico 4.2.3: Función de autocorrelación parcial muestral",
    x = "Rezago",
    y = "FACPM"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 26, face = "bold"),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20) 
  ) +
  scale_y_continuous(
    limits = c(-1,1)
  )

ggseasonplot(datosts, year.labels = TRUE, xlab="Mes", ylab="Temperatura media (°C)") +
  ggtitle("Gráfico 4.2.4: Comportamiento anual de la temperatura media de la ciudad de Rosario") +
  geom_line(size = 2) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)
  )

ggsubseriesplot(datosts) + 
  ggtitle("Gráfico 4.2.5: Comportamiento mensual de la temperatura media de la ciudad de Rosario") +
  xlab("Mes") + 
  ylab("Temperatura media (°C)") +
  geom_line(size = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)
  )

ggplot(temp, aes(x = factor(Año), y = TempMedia)) +
  geom_boxplot(fill = "#4F94CD") +
  labs(title = "Gráfico 4.2.6: Box-plot comparativo por año de la temperatura media en Rosario",
       x = "Año",
       y = "Temperatura Media (°C)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_x_discrete(limits = c("2015", "2016", "2017", "2018", "2019", "2020", "2021",
                              "2022", "2023", "2024", "2025")) 

errores_imputacion <- function(datosts, datostsNA, indices_na) {
  datosts_mean <- na_mean(datostsNA, option = "mean")
  rmse_media <- sqrt(mean((datosts_mean[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_media <- mean(abs(datosts_mean[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_media <- mean(abs(datosts_mean[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_median <- na_mean(datostsNA, option = "median")
  rmse_mediana <- sqrt(mean((datosts_median[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_mediana <- mean(abs(datosts_median[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_mediana <- mean(abs(datosts_median[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_mode <- na_mean(datostsNA, option = "mode")
  rmse_mode <- sqrt(mean((datosts_mode[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_mode <- mean(abs(datosts_mode[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_mode <- mean(abs(datosts_mode[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_replace <- na_replace(datostsNA)
  rmse_replace <- sqrt(mean((datosts_replace[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_replace <- mean(abs(datosts_replace[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_replace <- mean(abs(datosts_replace[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_random <- na_random(datostsNA)
  rmse_random <- sqrt(mean((datosts_random[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_random <- mean(abs(datosts_random[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_random <- mean(abs(datosts_random[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_ilinear <- na_interpolation(datostsNA, option = "linear")
  rmse_ilinear <- sqrt(mean((datosts_ilinear[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_ilinear <- mean(abs(datosts_ilinear[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_ilinear <- mean(abs(datosts_ilinear[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_ispline <- na_interpolation(datostsNA, option = "spline")
  rmse_ispline <- sqrt(mean((datosts_ispline[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_ispline <- mean(abs(datosts_ispline[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_ispline <- mean(abs(datosts_ispline[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_istine <- na_interpolation(datostsNA, option = "stine")
  rmse_istine <- sqrt(mean((datosts_istine[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_istine <- mean(abs(datosts_istine[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_istine <- mean(abs(datosts_istine[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_kalmans <- na_kalman(datostsNA, model = "StructTS")
  rmse_kalmans <- sqrt(mean((datosts_kalmans[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_kalmans <- mean(abs(datosts_kalmans[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_kalmans <- mean(abs(datosts_kalmans[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_kalmana <- na_kalman(datostsNA, model = "auto.arima")
  rmse_kalmana <- sqrt(mean((datosts_kalmana[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_kalmana <- mean(abs(datosts_kalmana[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_kalmana <- mean(abs(datosts_kalmana[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_locf <- na_locf(datostsNA, option = "locf")
  rmse_locf <- sqrt(mean((datosts_locf[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_locf <- mean(abs(datosts_locf[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_locf <- mean(abs(datosts_locf[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_nocb <- na_locf(datostsNA, option = "nocb")
  rmse_nocb <- sqrt(mean((datosts_nocb[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_nocb <- mean(abs(datosts_nocb[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_nocb <- mean(abs(datosts_nocb[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_masimple <- na_ma(datostsNA, weighting = "simple")
  rmse_masimple <- sqrt(mean((datosts_masimple[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_masimple <- mean(abs(datosts_masimple[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_masimple <- mean(abs(datosts_masimple[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_malinear <- na_ma(datostsNA, weighting = "linear")
  rmse_malinear <- sqrt(mean((datosts_malinear[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_malinear <- mean(abs(datosts_malinear[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_malinear <- mean(abs(datosts_malinear[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_maexponential <- na_ma(datostsNA, weighting = "exponential")
  rmse_maexponential <- sqrt(mean((datosts_maexponential[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_maexponential <- mean(abs(datosts_maexponential[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_maexponential <- mean(abs(datosts_maexponential[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_seadec <- na_seadec(datostsNA, algorithm = "interpolation")
  rmse_seadec <- sqrt(mean((datosts_seadec[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_seadec <- mean(abs(datosts_seadec[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_seadec <- mean(abs(datosts_seadec[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_seasplit <- na_seasplit(datostsNA)
  rmse_seasplit <- sqrt(mean((datosts_seasplit[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_seasplit <- mean(abs(datosts_seasplit[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_seasplit <- mean(abs(datosts_seasplit[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  rmse_resultados = c(rmse_random, rmse_replace, rmse_media, rmse_mediana, rmse_mode, rmse_ilinear, rmse_ispline, rmse_istine,
                      rmse_masimple, rmse_malinear, rmse_maexponential, rmse_locf, rmse_nocb, rmse_seadec, rmse_seasplit,
                      rmse_kalmans, rmse_kalmana)
  mae_resultados = c(mae_random, mae_replace, mae_media, mae_mediana, mae_mode, mae_ilinear, mae_ispline, mae_istine,
                     mae_masimple, mae_malinear, mae_maexponential, mae_locf, mae_nocb, mae_seadec, mae_seasplit,
                     mae_kalmans, mae_kalmana)
  mape_resultados = c(mape_random, mape_replace, mape_media, mape_mediana, mape_mode, mape_ilinear, mape_ispline, mape_istine,
                      mape_masimple, mape_malinear, mape_maexponential, mape_locf, mape_nocb, mape_seadec, mape_seasplit,
                      mape_kalmans, mape_kalmana)
  
  resultados <- list(rmse = rmse_resultados, mae = mae_resultados, mape = mape_resultados)
  return(resultados)
}

################################### Mismo porcentaje ##################################

porcentaje_faltantes <- 0.1
n_iteraciones <- 100

acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)

set.seed(123)

for (i in 1:n_iteraciones) {
  indices <- sample(1:length(datosts), size = round(length(datosts) * porcentaje_faltantes))
  datostsNA <- datosts
  datostsNA[indices] <- NA
  
  resultados <- errores_imputacion(datosts, datostsNA, indices)
  
  acumulador_rmse <- acumulador_rmse + resultados$rmse
  acumulador_mae <- acumulador_mae + resultados$mae
  acumulador_mape <- acumulador_mape + resultados$mape
}

promedio_rmse <- acumulador_rmse / n_iteraciones
promedio_mae <- acumulador_mae / n_iteraciones
promedio_mape <- acumulador_mape / n_iteraciones

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

porcentaje_faltantes <- 0.2
n_iteraciones <- 100

acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)

set.seed(123)

for (i in 1:n_iteraciones) {
  indices <- sample(1:length(datosts), size = round(length(datosts) * porcentaje_faltantes))
  datostsNA <- datosts
  datostsNA[indices] <- NA
  
  resultados <- errores_imputacion(datosts, datostsNA, indices)
  
  acumulador_rmse <- acumulador_rmse + resultados$rmse
  acumulador_mae <- acumulador_mae + resultados$mae
  acumulador_mape <- acumulador_mape + resultados$mape
}

promedio_rmse <- acumulador_rmse / n_iteraciones
promedio_mae <- acumulador_mae / n_iteraciones
promedio_mape <- acumulador_mape / n_iteraciones

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

porcentaje_faltantes <- 0.3
n_iteraciones <- 100

acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)

set.seed(123)

for (i in 1:n_iteraciones) {
  indices <- sample(1:length(datosts), size = round(length(datosts) * porcentaje_faltantes))
  datostsNA <- datosts
  datostsNA[indices] <- NA
  
  resultados <- errores_imputacion(datosts, datostsNA, indices)
  
  acumulador_rmse <- acumulador_rmse + resultados$rmse
  acumulador_mae <- acumulador_mae + resultados$mae
  acumulador_mape <- acumulador_mape + resultados$mape
}

promedio_rmse <- acumulador_rmse / n_iteraciones
promedio_mae <- acumulador_mae / n_iteraciones
promedio_mape <- acumulador_mape / n_iteraciones

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

porcentaje_faltantes <- 0.4
n_iteraciones_objetivo <- 100
acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)
iteraciones_exitosas <- 0

set.seed(123)
while (iteraciones_exitosas < n_iteraciones_objetivo) {
  indices <- sample(1:length(datosts), size = round(length(datosts) * porcentaje_faltantes))
  datostsNA <- datosts
  datostsNA[indices] <- NA
  
  tryCatch({
    resultados <- errores_imputacion(datosts, datostsNA, indices)
    
    acumulador_rmse <- acumulador_rmse + resultados$rmse
    acumulador_mae <- acumulador_mae + resultados$mae
    acumulador_mape <- acumulador_mape + resultados$mape
    
    iteraciones_exitosas <- iteraciones_exitosas + 1  
    cat("Iteración exitosa", iteraciones_exitosas, "\n") 
  }, error = function(e) {
    cat("Error en la iteración:", e$message, "\n")
  })
}

promedio_rmse <- acumulador_rmse / n_iteraciones_objetivo
promedio_mae <- acumulador_mae / n_iteraciones_objetivo
promedio_mape <- acumulador_mape / n_iteraciones_objetivo

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

porcentaje_faltantes <- 0.5
n_iteraciones_objetivo <- 100
acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)
iteraciones_exitosas <- 0

set.seed(123)
while (iteraciones_exitosas < n_iteraciones_objetivo) {
  indices <- sample(1:length(datosts), size = round(length(datosts) * porcentaje_faltantes))
  datostsNA <- datosts
  datostsNA[indices] <- NA
  
  tryCatch({
    resultados <- errores_imputacion(datosts, datostsNA, indices)
    
    acumulador_rmse <- acumulador_rmse + resultados$rmse
    acumulador_mae <- acumulador_mae + resultados$mae
    acumulador_mape <- acumulador_mape + resultados$mape
    
    iteraciones_exitosas <- iteraciones_exitosas + 1  
    cat("Iteración exitosa", iteraciones_exitosas, "\n") 
  }, error = function(e) {
    cat("Error en la iteración:", e$message, "\n")
  })
}

promedio_rmse <- acumulador_rmse / n_iteraciones_objetivo
promedio_mae <- acumulador_mae / n_iteraciones_objetivo
promedio_mape <- acumulador_mape / n_iteraciones_objetivo

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

############################### Distintos porcentajes ##################################

longitud <- length(datosts)
proporciones <- c(rep(0.05, floor(longitud * 0.30)),
                  rep(0.15, floor(longitud * 0.40)),
                  rep(0.25, longitud - floor(longitud * 0.30) - floor(longitud * 0.40)))

faltantes_mcar_variable <- sapply(1:longitud, function(i) runif(1) < proporciones[i])
dolar_ts_mcar_variable <- datosts
dolar_ts_mcar_variable[faltantes_mcar_variable] <- NA

plot(dolar_ts_mcar_variable, main = "Serie con valores faltantes MCAR (Proporción variable)", ylab = "Precio", xlab = "Año")


n_iteraciones <- 100
acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)

set.seed(123)

for (i in 1:n_iteraciones) {
  faltantes_mcar_variable <- sapply(1:longitud, function(i) runif(1) < proporciones[i])
  dolar_ts_mcar_variable <- datosts
  dolar_ts_mcar_variable[faltantes_mcar_variable] <- NA
  
  resultados <- errores_imputacion(datosts, dolar_ts_mcar_variable, faltantes_mcar_variable)
  
  acumulador_rmse <- acumulador_rmse + resultados$rmse
  acumulador_mae <- acumulador_mae + resultados$mae
  acumulador_mape <- acumulador_mape + resultados$mape
}

promedio_rmse <- acumulador_rmse / n_iteraciones
promedio_mae <- acumulador_mae / n_iteraciones
promedio_mape <- acumulador_mape / n_iteraciones

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)


longitud <- length(datosts)
proporciones <- c(rep(0.25, floor(longitud * 0.30)),
                  rep(0.05, floor(longitud * 0.40)),
                  rep(0.15, longitud - floor(longitud * 0.30) - floor(longitud * 0.40)))

faltantes_mcar_variable <- sapply(1:longitud, function(i) runif(1) < proporciones[i])
dolar_ts_mcar_variable <- datosts
dolar_ts_mcar_variable[faltantes_mcar_variable] <- NA

plot(dolar_ts_mcar_variable, main = "Serie con valores faltantes MCAR (Proporción variable)", ylab = "Precio", xlab = "Año")


n_iteraciones <- 100
acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)

set.seed(123)

for (i in 1:n_iteraciones) {
  faltantes_mcar_variable <- sapply(1:longitud, function(i) runif(1) < proporciones[i])
  dolar_ts_mcar_variable <- datosts
  dolar_ts_mcar_variable[faltantes_mcar_variable] <- NA
  
  resultados <- errores_imputacion(datosts, dolar_ts_mcar_variable, faltantes_mcar_variable)
  
  acumulador_rmse <- acumulador_rmse + resultados$rmse
  acumulador_mae <- acumulador_mae + resultados$mae
  acumulador_mape <- acumulador_mape + resultados$mape
}

promedio_rmse <- acumulador_rmse / n_iteraciones
promedio_mae <- acumulador_mae / n_iteraciones
promedio_mape <- acumulador_mape / n_iteraciones

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

##################################### distribución exponencial #############################
lambda <- 0.5

probabilidad_faltante_exponencial <- rexp(length(datosts), rate = lambda)
probabilidad_faltante_exponencial <- pmin(1, probabilidad_faltante_exponencial / max(probabilidad_faltante_exponencial))

faltantes_mcar_exponencial <- runif(length(datosts)) < probabilidad_faltante_exponencial
temp_ts_mcar_exponencial <- datosts
temp_ts_mcar_exponencial[faltantes_mcar_exponencial] <- NA

plot(temp_ts_mcar_exponencial, main = "Serie con valores faltantes MCAR (Distribución Exponencial)", 
     ylab = "Precio", xlab = "Año")

cat("Cantidad de valores observados después de introducir NA:", sum(!is.na(temp_ts_mcar_exponencial)), "\n")

n_iteraciones <- 100
num_metodos <- 17

acumulador_rmse <- numeric(num_metodos)
acumulador_mae <- numeric(num_metodos)
acumulador_mape <- numeric(num_metodos)

set.seed(123)

for (i in 1:n_iteraciones) {
  faltantes_mcar_exponencial <- runif(length(datosts)) < probabilidad_faltante_exponencial
  temp_ts_mcar_exponencial <- datosts
  temp_ts_mcar_exponencial[faltantes_mcar_exponencial] <- NA
  
  resultados <- tryCatch({
    errores_imputacion(datosts, temp_ts_mcar_exponencial, faltantes_mcar_exponencial)
  }, error = function(e) {
    cat("Error en errores_imputacion:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(resultados)) next
  
  acumulador_rmse <- acumulador_rmse + resultados$rmse
  acumulador_mae <- acumulador_mae + resultados$mae
  acumulador_mape <- acumulador_mape + resultados$mape
}

promedio_rmse <- acumulador_rmse / n_iteraciones
promedio_mae <- acumulador_mae / n_iteraciones
promedio_mape <- acumulador_mape / n_iteraciones

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

lambda <- 0.7

probabilidad_faltante_exponencial <- rexp(length(datosts), rate = lambda)
probabilidad_faltante_exponencial <- pmin(1, probabilidad_faltante_exponencial / max(probabilidad_faltante_exponencial))

faltantes_mcar_exponencial <- runif(length(datosts)) < probabilidad_faltante_exponencial
temp_ts_mcar_exponencial <- datosts
temp_ts_mcar_exponencial[faltantes_mcar_exponencial] <- NA

plot(temp_ts_mcar_exponencial, main = "Serie con valores faltantes MCAR (Distribución Exponencial)", 
     ylab = "Precio", xlab = "Año")

cat("Cantidad de valores observados después de introducir NA:", sum(!is.na(temp_ts_mcar_exponencial)), "\n")

n_iteraciones <- 100
num_metodos <- 17

acumulador_rmse <- numeric(num_metodos)
acumulador_mae <- numeric(num_metodos)
acumulador_mape <- numeric(num_metodos)

set.seed(123)

for (i in 1:n_iteraciones) {
  faltantes_mcar_exponencial <- runif(length(datosts)) < probabilidad_faltante_exponencial
  temp_ts_mcar_exponencial <- datosts
  temp_ts_mcar_exponencial[faltantes_mcar_exponencial] <- NA
  
  resultados <- tryCatch({
    errores_imputacion(datosts, temp_ts_mcar_exponencial, faltantes_mcar_exponencial)
  }, error = function(e) {
    cat("Error en errores_imputacion:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(resultados)) next
  
  acumulador_rmse <- acumulador_rmse + resultados$rmse
  acumulador_mae <- acumulador_mae + resultados$mae
  acumulador_mape <- acumulador_mape + resultados$mape
}

promedio_rmse <- acumulador_rmse / n_iteraciones
promedio_mae <- acumulador_mae / n_iteraciones
promedio_mape <- acumulador_mape / n_iteraciones

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

lambda <- 1.5

probabilidad_faltante_exponencial <- rexp(length(datosts), rate = lambda)
probabilidad_faltante_exponencial <- pmin(1, probabilidad_faltante_exponencial / max(probabilidad_faltante_exponencial))

faltantes_mcar_exponencial <- runif(length(datosts)) < probabilidad_faltante_exponencial

dolar_ts_mcar_exponencial <- datosts
dolar_ts_mcar_exponencial[faltantes_mcar_exponencial] <- NA

plot(dolar_ts_mcar_exponencial, main = "Serie con valores faltantes MCAR (Distribución Exponencial)", ylab = "Precio", xlab = "Año")

n_iteraciones_objetivo <- 100
acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)
iteraciones_exitosas <- 0

set.seed(123)

while (iteraciones_exitosas < n_iteraciones_objetivo) {
  faltantes_mcar_exponencial <- runif(length(datosts)) < probabilidad_faltante_exponencial
  dolar_ts_mcar_exponencial <- datosts
  dolar_ts_mcar_exponencial[faltantes_mcar_exponencial] <- NA
  
  tryCatch({
    resultados <- errores_imputacion(datosts, dolar_ts_mcar_exponencial, faltantes_mcar_exponencial)
    
    acumulador_rmse <- acumulador_rmse + resultados$rmse
    acumulador_mae <- acumulador_mae + resultados$mae
    acumulador_mape <- acumulador_mape + resultados$mape
    
    iteraciones_exitosas <- iteraciones_exitosas + 1  
    cat("Iteración exitosa", iteraciones_exitosas, "\n") 
  }, error = function(e) {
    cat("Error en la iteración:", e$message, "\n")
  })
}

promedio_rmse <- acumulador_rmse / n_iteraciones_objetivo
promedio_mae <- acumulador_mae / n_iteraciones_objetivo
promedio_mape <- acumulador_mape / n_iteraciones_objetivo

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

#################################### CASO MNAR #######################################
  
n_iteraciones_objetivo <- 100
acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)
iteraciones_exitosas <- 0

set.seed(123)
while (iteraciones_exitosas < n_iteraciones_objetivo) {
  alpha <- 0.5
  probabilidad_falta_mnar <- pmin(0.5, alpha * abs(datosts - mean(datosts)) / sd(datosts))
  faltantes_mnar <- runif(length(datosts)) < probabilidad_falta_mnar
  dolar_ts_mnar <- datosts
  dolar_ts_mnar[faltantes_mnar] <- NA
  
  tryCatch({
    resultados <- errores_imputacion(datosts, dolar_ts_mnar, faltantes_mnar)
    
    acumulador_rmse <- acumulador_rmse + resultados$rmse
    acumulador_mae <- acumulador_mae + resultados$mae
    acumulador_mape <- acumulador_mape + resultados$mape
    
    iteraciones_exitosas <- iteraciones_exitosas + 1  
    cat("Iteración exitosa", iteraciones_exitosas, "\n") 
  }, error = function(e) {
    cat("Error en la iteración:", e$message, "\n")
  })
}

promedio_rmse <- acumulador_rmse / n_iteraciones_objetivo
promedio_mae <- acumulador_mae / n_iteraciones_objetivo
promedio_mape <- acumulador_mape / n_iteraciones_objetivo

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

print(resultados_promedio)

####################################### grafico resumen #################################

tabla <- read_excel("C:/.../tablas.xlsx", sheet = "Hoja2")

resultados <- tabla %>%
  pivot_longer(cols = c(MAE, RMSE, MAPE), 
               names_to = "Métrica", 
               values_to = "Valor")

ggplot(resultados, aes(x = Porcentaje, y = Valor, color = Método)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1) +
  facet_wrap(~Métrica, scales = "free_y") +  # Crea un panel por cada métrica
  labs(title = "Gráfico 4.2.7: Desempeño de los métodos de imputación
       según la proporción de valores faltantes", 
       x = "Proporción de valores faltantes", 
       y = "Valor de la Métrica", 
       color = "Método de Imputación") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 18),
    legend.background = element_rect(color = "grey", size = 1, linetype = "solid", fill = "white")
  ) +
  guides(color = guide_legend(nrow = 3))
