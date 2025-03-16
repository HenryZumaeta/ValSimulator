#' Calcula la Entropía de Shannon
#'
#' Esta función calcula la entropía de Shannon entre las predicciones y las observaciones.
#' La entropía de Shannon es una medida de la incertidumbre o diversidad en una distribución de datos.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{shannon}{Matriz con la entropía de Shannon para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función:
#' \enumerate{
#'   \item Convierte \code{x} y \code{y} en data frames.
#'   \item Calcula la entropía de Shannon como:
#'   \deqn{H = -\sum_{i=1}^{n} p_i \log(p_i),}
#'   donde \eqn{p_i} representa la proporción o frecuencia relativa de cada valor en las observaciones.
#'   En este caso, se utiliza directamente la suma de \eqn{- y \log(y)} sobre los valores de \code{y} (omitiendo NA).
#' }
#'
#' @note Se asume que los valores de \code{y} son proporciones o frecuencias.
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
#'   pred <- data.frame(modelo1 = c(0.2, 0.3, 0.5), modelo2 = c(0.1, 0.4, 0.5))
#'   obs <- data.frame(real1 = c(0.3, 0.3, 0.4), real2 = c(0.2, 0.3, 0.5))
#'   resultado <- ValShannon(pred, obs)
#'   print(resultado$shannon)
#' }
ValShannon <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    shannon <- matrix(nrow = numreal, ncol = numsim,
                      dimnames = list(paste("Observacion", 1:numreal),
                                      paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            shannon[j, i] <- -sum(yy[valid_indices] * log(yy[valid_indices]))
        }
    }

    return(list(shannon = shannon))
}
