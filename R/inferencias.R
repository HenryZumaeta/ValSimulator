#' Calcula la Inferencia Frecuentista (Prueba t Pareada)
#'
#' Esta función realiza una prueba t de Student para muestras pareadas entre las predicciones y las observaciones.
#' Para cada modelo (columna de \code{x} y \code{y}), se calcula el estadístico t, el valor p y el intervalo de confianza
#' para la diferencia media.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param conf.level Nivel de confianza para el intervalo de confianza (por defecto 0.95).
#'
#' @return Una lista con cuatro elementos:
#' \describe{
#'   \item{statistic}{Vector con el estadístico t para cada modelo.}
#'   \item{p.value}{Vector con el valor p para cada modelo.}
#'   \item{lower}{Vector con el límite inferior del intervalo de confianza para cada modelo.}
#'   \item{upper}{Vector con el límite superior del intervalo de confianza para cada modelo.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función realiza una prueba t de Student para
#' muestras pareadas, evaluando si la diferencia media entre las predicciones y las observaciones es significativamente
#' distinta de cero.
#'
#' @author
#' Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
#' LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
#' WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso:
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(10, 11, 13), real2 = c(8, 10, 12))
#'   resultado <- ValFrequentista(pred, obs, conf.level = 0.95)
#'   print(resultado$statistic)
#'   print(resultado$p.value)
#'   print(resultado$lower)
#'   print(resultado$upper)
#' }
#'
#' @importFrom stats t.test
#'
ValFrequentista <- function(x, y, conf.level = 0.95) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    stat <- numeric(m)
    pval <- numeric(m)
    lower <- numeric(m)
    upper <- numeric(m)
    for (i in 1:m) {
        test <- t.test(x[[i]], y[[i]], paired = TRUE, conf.level = conf.level)
        stat[i] <- test$statistic
        pval[i] <- test$p.value
        lower[i] <- test$conf.int[1]
        upper[i] <- test$conf.int[2]
    }
    return(list(statistic = stat, p.value = pval, lower = lower, upper = upper))
}


#' Calcula la Inferencia Bayesiana
#'
#' Esta función calcula la inferencia bayesiana entre las predicciones y las observaciones,
#' asumiendo un prior uniforme sobre los modelos. Para cada modelo (columna de \code{x}), se estima la
#' log-verosimilitud a partir del error cuadrático y se obtiene la probabilidad posterior relativa.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Un vector con las probabilidades posteriores para cada modelo.
#'
#' @details La función realiza los siguientes pasos:
#' \enumerate{
#'   \item Une las observaciones y las predicciones en un solo objeto (omitiendo valores faltantes).
#'   \item Para cada modelo (columna de \code{x}), calcula el error cuadrático (RSS) entre las observaciones y las predicciones.
#'   \item Estima la varianza del error (\eqn{\sigma^2 = \text{RSS} / n}) y define la log-verosimilitud como:
#'   \deqn{\ell L = -\frac{n}{2} \log(\sigma^2),}
#'   \item Calcula la verosimilitud (\eqn{L = \exp(\ell L)}),
#'   \item Aplica un prior uniforme (\eqn{a priori = 1/m}) y finalmente
#'   \deqn{\text{posteriori} = \frac{\text{prior} \times L}{\sum (\text{prior} \times L)}.}
#' }
#'
#' @author
#' Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
#' LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
#' WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso:
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(10, 11, 13))
#'   resultados <- ValBayes(pred, obs)
#'   print(resultados)
#' }
#'
#' @importFrom stats na.omit
#'
ValBayes <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    n <- nrow(y)

    apriori <- rep(1 / m, m)
    lL <- numeric(m)
    sigmas2 <- numeric(m)
    rss <- numeric(m)
    modelo <- na.omit(cbind(y, x))
    n_modelo <- nrow(modelo)

    for (i in 1:m) {
        rss[i] <- sum((modelo[, 1] - modelo[, i + 1])^2)
        sigmas2[i] <- rss[i] / n_modelo
        lL[i] <- (-n_modelo / 2) * log(sigmas2[i])
    }

    L <- exp(lL)
    numerador <- apriori * L
    denominador <- sum(numerador)
    posteriori <- numerador / denominador

    return(posteriori)
}


#' Calcula las Estimaciones de Máxima Verosimilitud (MLE)
#'
#' Esta función calcula las estimaciones de máxima verosimilitud para la diferencia entre las predicciones y las observaciones,
#' asumiendo que dicha diferencia sigue una distribución normal. Para cada modelo (columna en \code{x} y \code{y}),
#' se estima la media (\eqn{\mu}) y la varianza (\eqn{\sigma^2}) de la diferencia.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{mu}{Vector con la media de la diferencia (predicciones - observaciones) para cada modelo.}
#'   \item{sigma2}{Vector con la varianza de la diferencia para cada modelo.}
#' }
#'
#' @details Para cada modelo, la función calcula la diferencia entre las predicciones y las observaciones,
#' y estima la media (\eqn{\mu}) y la varianza (\eqn{\sigma^2}) de dicha diferencia utilizando la función
#' \code{mean(..., na.rm = TRUE)}. Estas estimaciones se basan en la suposición de que la diferencia sigue
#' una distribución normal.
#'
#' @author
#' Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
#' LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
#' WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso:
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(8, 11, 13), real2 = c(10, 10, 15))
#'   resultado <- ValMLE(pred, obs)
#'   print(resultado$mu)
#'   print(resultado$sigma2)
#' }
ValMLE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    mu <- numeric(m)
    sigma2 <- numeric(m)
    for (i in 1:m) {
        diff <- x[[i]] - y[[i]]
        mu[i] <- mean(diff, na.rm = TRUE)
        sigma2[i] <- mean((diff - mu[i])^2, na.rm = TRUE)
    }
    return(list(mu = mu, sigma2 = sigma2))
}
