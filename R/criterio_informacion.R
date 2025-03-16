#' Calcula el Criterio de Información de Akaike (AIC)
#'
#' Esta función calcula el coeficiente de información de Akaike (AIC) entre las predicciones y las observaciones.
#' El AIC es una medida que equilibra el ajuste del modelo con su complejidad y se define como:
#' \deqn{AIC = -2 \times \ell L + 2k,}
#' donde \eqn{\ell L} es la log-verosimilitud del modelo y \eqn{k} es el número de parámetros.
#' Además, se calcula el AIC corregido (AICc) para muestras pequeñas:
#' \deqn{AICc = AIC + \frac{2k(k+1)}{n - k - 1},}
#' donde \eqn{n} es el tamaño de la muestra.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param k Número de parámetros del modelo. Se utiliza para ajustar el AIC.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{AIC}{Vector con el coeficiente de información de Akaike para cada modelo.}
#'   \item{AICc}{Vector con el coeficiente de información de Akaike corregido para cada modelo.}
#' }
#'
#' @details La función une las observaciones y las predicciones en un solo objeto (omitiendo valores faltantes)
#' y, para cada modelo (columna de \code{x}), calcula el valor del AIC y el AIC corregido (AICc). Si el valor
#' de \code{k} no es compatible con el tamaño de la muestra, se detiene la ejecución.
#'
#' @author
#' Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
#' LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
#' WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso:
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(10, 11, 13))
#'   # Supongamos que el modelo tiene 3 parámetros:
#'   resultado <- ValAIC(pred, obs, k = 3)
#'   print(resultado$AIC)
#'   print(resultado$AICc)
#' }
#' @importFrom stats na.omit
#'
#' @export
#'
ValAIC <- function(x, y, k) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    n <- nrow(y)

    AIC <- numeric(m)
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

    AIC <- -2 * lL + 2 * k
    if (max(k) != n + 1) {
        AICc <- AIC + 2 * k * (k + 1) / (n_modelo - k - 1)
    } else {
        stop("No se puede calcular AICc, no se puede dividir entre cero")
    }

    return(list(AIC = AIC, AICc = AICc))
}


#' Calcula el Criterio de Información Bayesiano (BIC)
#'
#' Esta función calcula el Criterio de Información Bayesiano (BIC) entre las predicciones y las observaciones.
#' El BIC es una medida que equilibra el ajuste del modelo con su complejidad y se define como:
#' \deqn{BIC = -2 \log(L) + \log(n) \times k,}
#' donde \eqn{L} es la verosimilitud del modelo, \eqn{n} es el tamaño de la muestra y \eqn{k} es el número de parámetros.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param k Número de parámetros del modelo.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{BIC}{Vector con el Criterio de Información Bayesiano para cada modelo.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}), la función realiza los siguientes pasos:
#' \enumerate{
#'   \item Une las observaciones y las predicciones en un solo objeto, omitiendo valores faltantes.
#'   \item Calcula el error cuadrático (RSS) entre las observaciones y las predicciones.
#'   \item Estima la varianza del error como \eqn{\sigma^2 = \text{RSS} / n}, donde \eqn{n} es el número de observaciones.
#'   \item Define la log-verosimilitud como:
#'   \deqn{\ell L = -\frac{n}{2} \log(\sigma^2),}
#'   \item Y, por lo tanto, el BIC se calcula como:
#'   \deqn{BIC = -2 \ell L + \log(n) \times k = n \log(\sigma^2) + \log(n) \times k.}
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
#'   # Supongamos que el modelo tiene 3 parámetros:
#'   resultado <- ValBIC(pred, obs, k = 3)
#'   print(resultado$BIC)
#' }
ValBIC <- function(x, y, k) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    n <- nrow(y)

    rss <- numeric(m)
    modelo <- na.omit(cbind(y, x))
    n_modelo <- nrow(modelo)

    BIC <- numeric(m)
    for (i in 1:m) {
        rss[i] <- sum((modelo[, 1] - modelo[, i + 1])^2)
        sigma2 <- rss[i] / n_modelo
        BIC[i] <- n_modelo * log(sigma2) + log(n_modelo) * k
    }

    return(list(BIC = BIC))
}
