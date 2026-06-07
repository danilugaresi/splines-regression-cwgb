# ======================================================================= #
#                         Script Trabajo RNPSP                            #
#   Gabriel Fernández Lago, Daniel Lugaresi Palomares, Antía Vega Crego   #
# ======================================================================= #

#### Cargamos las librerías necesarias

library(mboost)
library(ggplot2)

#### Fijamos una semilla para poder reproducir los resultados

set.seed(12345)

#### Generación de datos

n <- 500    # tamaño muestral
p <- 100    # dimensión del vector de variables explicativas

X <- matrix(runif(n*p, -1, 1), n, p) # matriz de covariables
colnames(X) <- paste0("X", 1:p)

# Funciones verdaderas (entran en juego 3 variables con relaciones no lineales)
f1 <- function(x) sin(pi * x)
f2 <- function(x) x^2
f3 <- function(x) x^3

# Variable respuesta (solo 4 variables relevantes)
Y <- f1(X[,1]) + f2(X[,2]) + f3(X[,3]) + rnorm(n, 0, 1)

df <- data.frame(Y, X)

#### Ajustamos el modelo con gamboost()

# Ajuste inicial
mod_boost1 <- gamboost(Y~., baselearner = "bbs", data = df, 
                      control = boost_control(mstop = 500, trace = TRUE))
# baselearner = "bbs" toma un base-learner bbs(x_i) tipo P-Spline
# para cada una de las covariables. Ponemos mstop grande, aunque
# lo seleccionaremos por validación cruzada después

# Validación cruzada para elegir mstop
cv <- cvrisk(mod_boost1)
par(mfrow = c(1,1))
plot(cv, main = "Validación Cruzada") # Función de validación cruzada
m_opt <- mstop(cv)
cat("Número óptimo de iteraciones:", m_opt, "\n")
# cvrisk() da m_opt <- 73, poner para no tener que ejecutar la función de nuevo

# Modelo final
mod_boost1 <- mod_boost1[m_opt] # Acortamos el modelo
names(coef(mod_boost1)) # Variables elegidas por el modelo


#### Gráficas y tablas importantes

# TABLAS:
# Variables elegidas por cada modelo
selecciones <- tabulate(selected(mod_boost1), nbins = p)
names(selecciones) <- colnames(X)
print(sort(selecciones, decreasing = TRUE))
# Elige muy bien, la mayoría de las veces X1,X2 y X3, aunque incorpora algo
# de ruido de otras 4 variables

# GRÁFICAS:
# Contribuciones estimadas de cada variable (se ve cómo el modelo
# aditivo de P-Splines captura el efecto no lineal)

vec <- c(1,2,3)
windows()
par(mfrow = c(2,3))
for (i  in 1:length(vec)){
  plot(mod_boost1, which = vec[i], main = paste0("Contribución estimada de X", vec[i]), ylab = "", xlab = "")
}

f_list <- list(f1, f2, f3)
xgrid <- seq(-1, 1, length = 200)
for (i  in 1:length(vec)){
  plot(xgrid, f_list[[i]](xgrid), col = "red", lwd = 2, type = "l",
       main = paste0("Contribución teórica de X", vec[i]), ylab = "", xlab = "")
}


#### Métricas de adecuación de las predicciones
# MSE, RMSE 
y_hat <- fitted(mod_boost1)

mse <- mean((df$Y - y_hat)^2)
rmse <- sqrt(mse)

mse
rmse
# Prácticamente 1, que es la varianza del error -> Modelo bueno
