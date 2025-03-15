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
