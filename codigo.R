library(quantmod)
# Esta librería nos permitira importar información acerca de datos financieros


btc <- getSymbols("BTC-USD", src = "yahoo", to = "2024-12-06", auto.assign = FALSE) # Hasta la fecha donde se consideró el proyecto en ese tiempo
# Se importan las series de tiempo para el precio del bitcoin 
# en dolares desde yahoo finance

# Precio ajustado del bitcoin en USD
btc_ajustado <- btc$`BTC-USD.Adjusted`
plot(btc_ajustado, xlab = "Time", ylab = "USD", main = "Historial de precios del bitcoin en USD")

# Rendimientos de la serie de tiempo 
plot(ts(diff(btc_ajustado)[-1]), xlab = "Time", ylab = "Rendimiento")

# Rendimientos de la serie de tiempo con el logaritmo
logret <- ts(diff(log(btc_ajustado))[-1])
plot(logret, xlab = "Time", ylab = "Rendimiento")

# Al observar la serie podemos apreciar que tiene una variabilidad
# que no es permanence constante, sino que cambia en periodos de tiempo

acf(logret, main = "" )

# La función de autocorrelación no me permite observa que existe una
# varianza no constante.
# Realizamos una transformación a la serie de tiempo para observar la 
# con mayor facilidad la variabilidad de esta, por tanto sacamos el valor
# absoluto de la serie 

plot(abs(logret), xlab = "Time", ylab = "Rendimiento")
acf(abs(logret), main = "" )

# Se visualiza con mayor claridad la varianza de la serie por clusters
# Ahora si se observa que existe autocorrelación que unicamente se pudo 
# detectar al tomar el valor absoluto de la serie. Tiene una correlación
# asociada a la varianza de la serie. 

# Ahora se realizará una prueba más formal para determinar si la serie 
# pretenta heterocedásticidad, para esto modelamos la serie 

library(dynlm)
logret_mean <- dynlm(logret ~ 1) 

# Se realiza un regresión con un solo intercepto lo cual equivale a calcular la
# media de los rendimiento logarítmicos. Se modela la serie X_t = mu + error_t

summary(logret_mean)

# Si observamos los resultados podemos notar un p valor de 0.0155 lo cual es 
# significativo.
# Calculamos los resiudales, los elevamos al cuadrado

ehatsq <- ts(resid(logret_mean)^2) # Esta es la varianza

ARCH_m <- dynlm(ehatsq ~ L(ehatsq)) # Se modela como si dependiera de su rezago anterior
# El modelo es tipo ARCH(1): u_t^2 = media + u_{t-1}^2 alpha + v

summary(ARCH_m)

# Los resultados nos dice que el rezago anterior es bastante significativo 
# esto nos da indicios de que al menos tiene efectos ARCH para el rezago 1
# Es decir, su varianza depende de los valores pasados, al menos para un retardo.


acf(ARCH_m$residuals, main="")
acf(abs(ARCH_m$residuals),main="")

# Hasta el momento se ha verificado de forma empírica la posible existe de 
# heterocedasticidad en la serie, sin embargo, para realizar una prueba más formal
# se realiza una prueba de hipótesis para verificar si al menos para un retraso
# existe heterocedasticidad.
# H0: No hay efectos ARCH (alpha = 0)
# Ha: Hay efectos ARCH (alpha != 0)

library(FinTS)
ArchTest(logret, lags= 1, demean = TRUE)

# Los resultados sugieren que hay evidencia significativa de heterocedasticidad condicional 
# en los residuos del modelo, lo que implica que la varianza de los errores cambia con el 
# tiempo y depende de los valores previos dado que se rechaza la hipotesis nula para una significancia
# del 0.01

# Ahora que tenemos evidencia de heterocedásticidad realizamos un modelado al menos con un ARCH(1) 

#################################################################################################################
# Comenzamos ajustando un modelo ARCH(1)

library(rugarch)
library(forecast)

auto.arima(logret) # Se selecciona el mejor modelo que se ajusta a la media, en este caso resulta ser un ARMA(2,0)

# Se establece el modelo que se desea ajustar, en este caso GARCH(1,0), es decir un ARCH(1) con media ARMA(2,0)
model.spec <- ugarchspec(variance.model = list(model = 'sGARCH', # Modelo GARCH simple
                                               garchOrder = c(1,0)), # Orden del modelo GARCH, en este caso (1,0)
              mean.model = list(armaOrder = c(2,0)), # Se establece el modelo de la media ARMA(2,0)
              distribution.model = "std") # Aquí se usa una distribución t-estándar ("std"), que captura colas pesadas en los datos.

# Se entrena el modelo GARCH con los datos 
arch.fit <- ugarchfit(spec = model.spec, data = logret, solver = 'solnp')

# Se extraen los coeficientes
arch.fit@fit$coef

# De acuerdo a los coeficientes el modelo queda de la siguiente forma: X_t = 0.0016 - 0.0527 X_{t-1} - 0.0001 x_{t-2}
#                                                                     h_t^2 = 0.0020 - 0.0001 h_{t-1}^2
boot.garch <- ugarchboot(arch.fit,
                         method = 'Partial',
                         sampling = 'raw', 
                         n.ahead = 1, # simulación en el horizonte
                         n.bootpred = 100000, # numero de simulaciones
                         solver = 'solnp' # Método de optimización para estimar los parámetros
)

# De acuerdo a las prediccioens para el periodo siguiente es que el la media tome el valor de -0.0018 con una minima
# de -0.6518 y una máxima de 0.2754, y la varianza tome un valor mínimo de 0.0626  y máximo de 0.0626 con una media de 0.0626

# Obtener los valores ajustados desde el modelo ARCH/GARCH
fitted_values <- fitted(arch.fit)

# Se crea un data frame que contiene los valores de los retornos y sus volatitilidades
comparison_data <- data.frame(
  Time = time(logret),
  Original = as.numeric(logret),
  Fitted = as.numeric(fitted_values * -(25))
)

# Grafico del logaritmo de los retornos y los estimados por el modelo.
par(mfrow = c(2, 1))
plot(comparison_data$Original, type = "l", col = "blue", main = "Logaritmo de Rendimientos", xlab = "Tiempo", ylab = "Logret")
plot(comparison_data$Fitted, type = "l", col = "red", main = "Valores Ajustados del Modelo", xlab = "Tiempo", ylab = "Fitted Values")
par(mfrow = c(1, 1))

#################################################################################################################
# Comenzamos ajustando un modelo GARCH(0,1)
# Se establece el modelo que se desea ajustar, en este caso GARCH(0,1) con media ARMA(2,0)
model.spec2 <- ugarchspec(variance.model = list(model = 'sGARCH', # Modelo GARCH simple
                                               garchOrder = c(0,1)), # Orden del modelo GARCH, en este caso (0,1)
                         mean.model = list(armaOrder = c(2,0)), # Se establece el modelo de la media ARMA(2,0)
                         distribution.model = "std") # Aquí se usa una distribución t-estándar ("std"), que captura colas pesadas en los datos.

# Se entrena el modelo GARCH con los datos 
arch.fit2 <- ugarchfit(spec = model.spec2, data = logret, solver = 'solnp')

# Se extraen los coeficientes
arch.fit2@fit$coef


# De acuerdo a los coeficientes el modelo queda de la siguiente forma: X_t = 0.00175 - 0.0062 X_{t-1} - 0.00318 x_{t-2}
#                                                                     h_t^2 = 0.0000029 - 0.99 sigma_{t-1}^2

boot.garch2 <- ugarchboot(arch.fit2,
                         method = 'Partial',
                         sampling = 'raw', 
                         n.ahead = 1, # simulación en el horizonte
                         n.bootpred = 100000, # numero de simulaciones
                         solver = 'solnp' # Método de optimización para estimar los parámetros
)

boot.garch2
# Con este modelo se espera que la media tome un valor de -0.0017 como minimo -0.4697 y máximo 0.2379 y la varianza
# se espera que tome un valor de 0.0457 con una minima de 0.0457 y una máxima de 0.0457

# Se obtiene la volatilidad de acuerdo al modelo con los datos de la serie
fitted_values2 <- fitted(arch.fit2)

# Se crea un data frame que contiene los valores de los retornos y sus volatitilidades
comparison_data2 <- data.frame(
  Time = time(logret),
  Original = as.numeric(logret),
  Fitted = as.numeric(fitted_values2 * -5)
)

ggplot(comparison_data2, aes(x = Time)) +
  geom_line(aes(y = Original, color = "Original"), size = 1) +
  geom_line(aes(y = Fitted, color = "Fitted"), size = 1, linetype = "dashed") +
  labs(
    title = "Retornos y su volatilidad estimada",
    x = "Tiempo",
    y = "Log-Retornos",
    color = "Serie"
  ) +
  theme_minimal()

#################################################################################################################
# Modelo GARCH(p,q) óptimo

# Función para obtener el valor óptimo p,q para el modeo GARCH de acuerdo al criterio AIC
Lag_Opt_GARCH <- function(X_t, p_max, q_max) {
  library(rugarch)
  
  # p_max: Resagos máximos a evaluar del componente AR
  # q_max: Resagos máximos a evaluar del componente GARCH
  # X_t: Serie de tiempo modelada
  
  # Inicializamos la matriz para almacenar los resultados
  Criterio_AIC <- matrix(rep(0, 4 * (q_max * p_max)), ncol = 4)
  colnames(Criterio_AIC) <- c("q", "p", "AIC", "Optimo")
  grupo <- 1
  
  # Recorremos todos los posibles valores de q (componente GARCH) y p (componente AR)
  for (i in 1:p_max) {
    for (j in 1:q_max) {
      
      # Guardamos los valores actuales de p y q en la matriz de AIC
      Criterio_AIC[grupo, 1] <- i
      Criterio_AIC[grupo, 2] <- j
      
      # Especificación del modelo GARCH
      model.spec <- ugarchspec(
        variance.model = list(model = 'sGARCH', garchOrder = c(i, j)),
        mean.model = list(armaOrder = c(0, 0)),
        distribution.model = "std"
      )
      
      # Ajustamos el modelo GARCH
      model <- ugarchfit(spec = model.spec, data = X_t)
      
      # Guardamos el AIC del modelo ajustado
      Criterio_AIC[grupo, 3] <- infocriteria(model)[1]  # AIC
      
      grupo <- grupo + 1
    }
  }
  
  # Selección del modelo óptimo (con el AIC más bajo)
  Criterio_AIC[which.min(Criterio_AIC[, 3]), 4] <- "1"
  
  # Devolvemos la tabla de resultados con el modelo óptimo
  return(Criterio_AIC)
}

garch_opt <- Lag_Opt_GARCH(ehatsq,4,4) # El valor optimo es: p=4, q=3.
garch_opt

# Por tanto se ajusta el modelo GARCH con estos parámetros
model.spec3 <- ugarchspec(variance.model = list(model = 'sGARCH', # Modelo GARCH simple
                                                garchOrder = c(4,3)), # Orden del modelo GARCH, en este caso (4,3)
                          mean.model = list(armaOrder = c(2,0)), # Se establece el modelo de la media ARMA(2,0)
                          distribution.model = "std") # Aquí se usa una distribución t-estándar ("std"), que captura colas pesadas en los datos.

# Se entrena el modelo GARCH con los datos 
arch.fit3 <- ugarchfit(spec = model.spec3, data = logret, solver = 'solnp')

# Se extraen los coeficientes
arch.fit3@fit$coef


# De acuerdo a los coeficientes el modelo queda de la siguiente forma: X_t = 0.00133 - 0.0051 X_{t-1} - 0.000721 x_{t-2}
#                                                                     h_t^2 = 0.000034 + 0.169 h_{t-1}^2 + 0.0535 h_{t-2}^2 + 0.00000017 h_{t-3}^2 + 0.0084 h_{t-4}^2
#                                                                             + 0.000006444 sigma_{t-1}^2 + 0.296 sigma_{t-2}^2 + 0.4719 sigma_{t-3}^2 

boot.garch3 <- ugarchboot(arch.fit3,
                          method = 'Partial',
                          sampling = 'raw', 
                          n.ahead = 1, # simulación en el horizonte
                          n.bootpred = 100000, # numero de simulaciones
                          solver = 'solnp' # Método de optimización para estimar los parámetros
)

boot.garch3
# Con este modelo se espera que la media tome un valor de -0.0013 como minimo -0.3848 y máximo 0.3072 y la varianza
# se espera que tome un valor de 0.0325 con una minima de 0.0325 y una máxima de 0.0325

# Se obtiene la volatilidad de acuerdo al modelo con los datos de la serie
fitted_values3 <- fitted(arch.fit3)

# Se crea un data frame que contiene los valores de los retornos y sus volatitilidades
comparison_data3 <- data.frame(
  Time = time(logret),
  Original = as.numeric(logret),
  Fitted = as.numeric(fitted_values3 * -25)
)

# Grafico del logaritmo de los retornos y los estimados por el modelo.
par(mfrow = c(2, 1))
plot(comparison_data3$Original, type = "l", col = "blue", main = "Logaritmo de Rendimientos", xlab = "Tiempo", ylab = "Logret")
plot(comparison_data3$Fitted, type = "l", col = "red", main = "Valores Ajustados del Modelo", xlab = "Tiempo", ylab = "Fitted Values")
par(mfrow = c(1, 1))

####################################################################################################################
####################################################################################################################
####################################################################################################################

# Ahora se aplicará un modelo DCC - GARCH para modelar la volatilidad de las divisas del peso mexicano frente al dolar
# y el euro. 
library(rmgarch)
usd <- getSymbols("MXN=X", src="yahoo", from = "2017-01-01",to = "2024-12-07", auto.assign = FALSE) # Se importa la serie de tiempo de la divisa MXN/USD 
cad <- getSymbols("CADMXN=X", src="yahoo", from = "2017-01-01", to = "2024-12-07",  auto.assign = FALSE) # Se importa la serie de tiempo de la divisa MXN/CAD

# Se realiza una imputación para los datos faltantes
library(zoo)
usd <- na.approx(usd)
cad <- na.approx(cad)

# Grafica las divisas con respecto al tiempo
plot(usd$`MXN=X.Adjusted`, xlab = "Time", ylab = "MXN", main = "Valor del dolar frente al peso mexicano")
plot(cad$`CADMXN=X.Adjusted`, xlab = "Time", ylab = "MXN", main = "Valor del dolar canadiense frente al peso mexicano")

library(tseries)
#library(gtools)
#library(rmgarch)
#library(ccgarch)
library(fPortfolio)

retornos <- cbind(usd$`MXN=X.Adjusted`, cad$`CADMXN=X.Adjusted`) 
colnames(retornos) <- c("USD/MXN", "CAD/MXN")
retornos <- returns(retornos) # Calcula los retornos de las series de tiempo
retornos <- 100 * retornos[,1:ncol(retornos)] # Convierte los retornos en porcentajes multiplicando por 100.
retornos1 <- as.timeSeries(retornos) # Convierte el objeto de retornos a una clase `timeSeries` para análisis financiero.
retornos1 <- na.omit(retornos1) # Elimina cualquier fila con valores faltantes (NA) en los retornos.
head(retornos1)

apply(retornos1, 2, mean) # Calcula la media de cada columna (serie) de los retornos.
mu <- matrix(apply(retornos1,2, mean)) # Convierte las medias en un objeto de tipo matriz.
mu1 <- t(mu) # Transpone la matriz de medias, convirtiéndola en un vector fila.
plot(retornos1, main = "Retornos") # Grafica las series de retornos procesadas. Cada serie se muestra como una línea.

auto.arima(retornos1$`USD/MXN`) # El mejor modelo ARMA es (1,1)
auto.arima(retornos1$`CAD/MXN`) # El mejor modelo ARMA es (3,0)

# Definir especificaciones GARCH para cada serie
usd_garch <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                        variance.model = list(garchOrder = c(1, 1), model = "sGARCH"))

cad_garch <- ugarchspec(mean.model = list(armaOrder = c(3, 0), include.mean = TRUE),
                        variance.model = list(garchOrder = c(1, 1), model = "sGARCH"))

# Crear un multispec con modelos diferentes
multi_garch <- multispec(list(usd_garch, cad_garch))

# Especificar el modelo DCC-GARCH
dcc_garch <- dccspec(uspec = multi_garch, 
                     dccOrder = c(1, 1), 
                     distribution = "mvt")

dcc_garch_model <- dccfit(dcc_garch, retornos1)
dcc_garch_model

infocriteria(dcc_garch_model)[1]

Lag_Opt_GARCH_mul <- function(X_t, p_max, q_max, multi_garch) {
  library(rugarch)
  
  # p_max: Resagos máximos a evaluar del componente AR
  # q_max: Resagos máximos a evaluar del componente GARCH
  # X_t: Serie de tiempo modelada
  
  # Inicializamos la matriz para almacenar los resultados
  Criterio_AIC <- matrix(rep(0, 4 * (q_max * p_max)), ncol = 4)
  colnames(Criterio_AIC) <- c("q", "p", "AIC", "Optimo")
  grupo <- 1
  
  # Recorremos todos los posibles valores de q (componente GARCH) y p (componente AR)
  for (i in 1:p_max) {
    for (j in 1:q_max) {
      
      # Guardamos los valores actuales de p y q en la matriz de AIC
      Criterio_AIC[grupo, 1] <- i
      Criterio_AIC[grupo, 2] <- j
      
      # Especificar el modelo DCC-GARCH
      dcc_garch <- dccspec(uspec = multi_garch, 
                           dccOrder = c(i, j), 
                           distribution = "mvt")
      
      dcc_garch_model <- dccfit(dcc_garch, X_t)
      
      # Guardamos el AIC del modelo ajustado
      Criterio_AIC[grupo, 3] <- infocriteria(dcc_garch_model)[1]  # AIC
      
      grupo <- grupo + 1
    }
  }
  
  # Selección del modelo óptimo (con el AIC más bajo)
  Criterio_AIC[which.min(Criterio_AIC[, 3]), 4] <- "1"
  
  # Devolvemos la tabla de resultados con el modelo óptimo
  return(Criterio_AIC)
}

# Función para obtener el valor óptimo p,q para el modeo DCC - GARCH de acuerdo al criterio AIC
Lag_Opt_GARCH_mul(retornos1, 4, 4, multi_garch) # El mejor modelo resulo ser el (1,1)

# Se ajusta el modelo DCC - GARCH con p=1 y q=1
# Especificar el modelo DCC-GARCH
dcc_garch <- dccspec(uspec = multi_garch, 
                     dccOrder = c(1, 1), 
                     distribution = "mvt")

# Se ajusta el modelo DCC - GARCH a los datos
dcc_garch_model <- dccfit(dcc_garch, retornos1)
dcc_garch_model

# Gráficos 
plot(dcc_garch_model)

predicciones_10 <- (dccforecast(dcc_garch_model, n.ahead = 10))

plot(predicciones_10)

