#' Calcula el Coeficiente de Gini
#'
#' Esta función calcula el coeficiente de Gini entre las predicciones y las observaciones.
#' El coeficiente de Gini es una medida de desigualdad, donde 0 indica igualdad perfecta y valores mayores
#' indican mayor desigualdad.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones. Se convierte a factor.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{gini}{Matriz con el coeficiente de Gini para cada combinación de modelo y observacion.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función:
#' \enumerate{
#'   \item Convierte \code{x} a data frame y \code{y} a factor.
#'   \item Para cada par, convierte los valores a numéricos y calcula el coeficiente de Gini
#'   utilizando la función \code{gini} del paquete \code{reldist}.
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
#'   pred <- data.frame(modelo1 = c(0.1, 0.2, 0.3), modelo2 = c(0.2, 0.3, 0.4))
#'   obs <- data.frame(real1 = factor(c("A", "B", "A")), real2 = factor(c("B", "B", "A")))
#'   resultado <- ValGini(pred, obs)
#'   print(resultado$gini)
#' }
#'
#' @importFrom reldist gini
#'
ValGini <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.factor(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    ginimat <- matrix(nrow = numreal, ncol = numsim,
                      dimnames = list(paste("Observacion", 1:numreal),
                                      paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- as.numeric(x[[i]])
            yy <- as.numeric(y[[j]])
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            ginimat[j, i] <- reldist::gini(xx[valid_indices])
        }
    }

    return(list(gini = ginimat))
}
