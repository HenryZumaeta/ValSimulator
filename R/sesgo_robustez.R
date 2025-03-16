#' Calcula la Desviación Media del Sesgo (MBD)
#'
#' Esta función calcula la desviación media del sesgo entre las predicciones y las observaciones.
#' El sesgo se mide como la diferencia promedio entre los valores predichos y los observados.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{mbd}{Matriz con la desviación media del sesgo para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función calcula
#' la diferencia \eqn{(x - y)} en cada posición y luego obtiene la media de estas diferencias,
#' proporcionando una medida del sesgo entre las predicciones y las observaciones.
#'
#' @author
#' Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})\cr
#' LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}\cr
#' WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso:
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs  <- data.frame(real1 = c(8, 11, 13), real2 = c(10, 10, 16))
#'   resultado <- ValMBD(pred, obs)
#'   print(resultado$mbd)
#' }
ValMBD <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    mbd <- matrix(nrow = numreal, ncol = numsim,
                  dimnames = list(paste("Observacion", 1:numreal),
                                  paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            mbd[j, i] <- mean(xx[valid_indices] - yy[valid_indices])
        }
    }

    return(list(mbd = mbd))
}
