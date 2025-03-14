#' Calcula el Error Cuadrático Medio (MSE)
#'
#' Esta función calcula el error cuadrático medio entre las predicciones y las observaciones.
#' Además, calcula la raíz del error cuadrático medio (RMSE) y el error cuadrático medio
#' relativo (prmse) expresado en porcentaje.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con tres elementos:
#' \describe{
#'   \item{mse}{Matriz con los errores cuadráticos medios.}
#'   \item{rmse}{Matriz con la raíz del error cuadrático medio (RMSE).}
#'   \item{prmse}{Matriz con los errores cuadráticos medios relativos, expresados en porcentaje.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
#' se calculan las siguientes métricas:
#' \deqn{MSE = \frac{1}{n}\sum_{i=1}^{n}(x_i - y_i)^2}
#' \deqn{RMSE = \sqrt{MSE}}
#' \deqn{prmse = \frac{RMSE}{\bar{y}} \times 100}
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
#'   resultado <- ValMSE(pred, obs)
#'   print(resultado$mse)
#'   print(resultado$rmse)
#'   print(resultado$prmse)
#' }
#'
#' @export
#'
ValMSE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    mse <- matrix(nrow = numreal, ncol = numsim,
                  dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    rmse <- matrix(nrow = numreal, ncol = numsim,
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    prmse <- matrix(nrow = numreal, ncol = numsim,
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            mse[j, i] <- mean((xx[valid_indices] - yy[valid_indices])^2)
            rmse[j, i] <- sqrt(mse[j, i])
            prmse[j, i] <- rmse[j, i] / mean(yy[valid_indices]) * 100
        }
    }

    return(list(mse = mse, rmse = rmse, prmse = prmse))
}
