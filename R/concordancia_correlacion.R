#' Calcula el Índice de Concordancia (d) de Willmott
#'
#' Esta función calcula el índice de concordancia de Willmott entre las predicciones y las observaciones.
#' Este índice evalúa la capacidad del modelo para reproducir el patrón observado, variando entre 0 y 1, donde 1 indica
#' una concordancia perfecta.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{W}{Matriz con el índice de concordancia de Willmott para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
#' se calcula el índice de concordancia de Willmott (\eqn{d}) como:
#' \deqn{d = 1 - \frac{\sum_{i=1}^{n}(x_i - y_i)^2}{\sum_{i=1}^{n}\left(|x_i - \bar{y}| + |y_i - \bar{y}|\right)^2}}
#' donde \eqn{\bar{y}} es la media de las observaciones.
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
#'   resultado <- ValWillmott(pred, obs)
#'   print(resultado$W)
#' }
ValWillmott <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    W <- matrix(nrow = numreal, ncol = numsim,
                dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            numerador <- sum((xx[valid_indices] - yy[valid_indices])^2)
            denominador <- sum((abs(xx[valid_indices] - mean(yy[valid_indices])) + abs(yy[valid_indices] - mean(yy[valid_indices])))^2)
            W[j, i] <- 1 - numerador / denominador
        }
    }

    return(list(W = W))
}
