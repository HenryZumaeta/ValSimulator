#' Calcula la Divergencia de Kullback-Leibler
#'
#' Esta función calcula la divergencia de Kullback-Leibler entre las predicciones y las observaciones.
#' La divergencia de Kullback-Leibler es una medida de la discrepancia entre dos distribuciones de probabilidad,
#' y se utiliza para cuantificar la diferencia entre la distribución predicha y la observada.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{kl}{Matriz con la divergencia de Kullback-Leibler para cada combinación de modelo y observacion.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función:
#' \enumerate{
#'   \item Convierte \code{x} e \code{y} en data frames.
#'   \item Calcula la divergencia de Kullback-Leibler usando la fórmula:
#'   \deqn{KL = \sum_{i=1}^{n} y_i \log\left(\frac{y_i}{x_i}\right),}
#'   donde \eqn{x_i} y \eqn{y_i} son los valores de las predicciones y las observaciones respectivamente.
#'   \item Solo se consideran los elementos para los que \code{x} e \code{y} no son NA.
#' }
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
#'   resultado <- ValKullbackLeibler(pred, obs)
#'   print(resultado$kl)
#' }
ValKullbackLeibler <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    kl <- matrix(nrow = numreal, ncol = numsim,
                 dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            kl[j, i] <- sum(yy[valid_indices] * log(yy[valid_indices] / xx[valid_indices]))
        }
    }

    return(list(kl = kl))
}
