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


#' Realiza el Remuestreo Bootstrap para la Diferencia de Medias
#'
#' Esta función realiza un remuestreo bootstrap para estimar la diferencia en medias entre las
#' predicciones y las observaciones, así como para calcular su intervalo de confianza al 95%.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param R Número de replicaciones del bootstrap (por defecto 1000).
#'
#' @return Una lista con tres elementos:
#' \describe{
#'   \item{diff_mean}{Vector con la diferencia en medias estimada para cada modelo.}
#'   \item{ci_lower}{Vector con el límite inferior del intervalo de confianza para la diferencia en medias.}
#'   \item{ci_upper}{Vector con el límite superior del intervalo de confianza para la diferencia en medias.}
#' }
#'
#' @details Para cada modelo (columna en \code{x} y \code{y}), la función:
#' \enumerate{
#'   \item Calcula la diferencia \eqn{d = x_i - y_i} y su media.
#'   \item Realiza \code{R} replicaciones, en cada una de las cuales se muestrea de forma aleatoria con
#'   reemplazo la diferencia y se calcula la media.
#'   \item Calcula el intervalo de confianza al 95% a partir de los cuantiles 0.025 y 0.975 de las replicaciones.
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
#'   obs <- data.frame(real1 = c(8, 11, 13), real2 = c(9, 10, 14))
#'   resultado <- ValBootstrap(pred, obs, R = 1000)
#'   print(resultado$diff_mean)
#'   print(resultado$ci_lower)
#'   print(resultado$ci_upper)
#' }
#'
#' @importFrom stats quantile
#'
ValBootstrap <- function(x, y, R = 1000) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    diff_mean <- numeric(m)
    ci_lower <- numeric(m)
    ci_upper <- numeric(m)
    for (i in 1:m) {
        diff <- x[[i]] - y[[i]]
        diff_mean[i] <- mean(diff, na.rm = TRUE)
        boot_est <- numeric(R)
        n <- length(diff)
        for (r in 1:R) {
            idx <- sample(1:n, size = n, replace = TRUE)
            boot_est[r] <- mean(diff[idx], na.rm = TRUE)
        }
        ci_lower[i] <- quantile(boot_est, 0.025, na.rm = TRUE)
        ci_upper[i] <- quantile(boot_est, 0.975, na.rm = TRUE)
    }
    return(list(diff_mean = diff_mean, ci_lower = ci_lower, ci_upper = ci_upper))
}
