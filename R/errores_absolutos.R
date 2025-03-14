#' Calcula el Error Absoluto Medio (MAE)
#'
#' Esta función calcula el error absoluto medio entre las predicciones y las observaciones.
#' Además, calcula el error porcentual medio (pMAE) como porcentaje respecto a la media de las observaciones.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{mae}{Matriz con los errores absolutos medios.}
#'   \item{pmae}{Matriz con los errores porcentuales medios, expresados en porcentaje.}
#' }
#'
#' @details Para cada modelo (columna de `x`) y cada conjunto de observaciones (columna de `y`), se calcula:
#' \deqn{MAE = \frac{1}{n}\sum_{i=1}^{n}|x_i - y_i|}
#' y
#' \deqn{pMAE = \frac{MAE}{\bar{y}} \times 100.}
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
#'   resultado <- ValMAE(pred, obs)
#'   print(resultado$mae)
#'   print(resultado$pmae)
#' }
ValMAE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    mae <- matrix(nrow = numreal, ncol = numsim,
                  dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    pmae <- matrix(nrow = numreal, ncol = numsim,
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            mae[j, i] <- mean(abs(xx[valid_indices] - yy[valid_indices]))
            pmae[j, i] <- mae[j, i] / mean(yy[valid_indices]) * 100
        }
    }

    return(list(mae = mae, pmae = pmae))
}


#' Calcula el Error Absoluto Mediano (MedAE)
#'
#' Esta función calcula el error absoluto mediano entre las predicciones y las observaciones.
#' La mediana es una medida robusta que no se ve afectada por valores atípicos, lo que puede ser
#' útil para evaluar el desempeño de modelos de simulación en contextos con datos extremos.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{medae}{Matriz con los errores absolutos medianos.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
#' se calcula la mediana de los valores absolutos de las diferencias:
#' \deqn{MedAE = \text{median}(|x_i - y_i|)}
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
#'   resultado <- ValMedAE(pred, obs)
#'   print(resultado$medae)
#' }
ValMedAE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    medae <- matrix(nrow = numreal, ncol = numsim,
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            medae[j, i] <- median(abs(xx[valid_indices] - yy[valid_indices]))
        }
    }

    return(list(medae = medae))
}

