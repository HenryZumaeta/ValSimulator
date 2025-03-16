#' Calcula el Coeficiente de Variación (CV)
#'
#' Esta función calcula el coeficiente de variación (CV) entre las predicciones y las observaciones.
#' El CV se expresa como un porcentaje y es una medida de la dispersión relativa de los datos.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{cv}{Matriz con el coeficiente de variación, expresado en porcentaje, para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función calcula el CV como:
#' \deqn{CV = \frac{sd(x)}{mean(x)} \times 100,}
#' utilizando únicamente las observaciones completas (omitiendo los NA).
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
#'   obs <- data.frame(real1 = c(10, 11, 13), real2 = c(8, 10, 12))
#'   resultado <- ValCV(pred, obs)
#'   print(resultado$cv)
#' }
#'
#' @importFrom stats sd
#'
ValCV <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    cv <- matrix(nrow = numreal, ncol = numsim,
                 dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            cv[j, i] <- (sd(xx[valid_indices]) / mean(xx[valid_indices])) * 100
        }
    }

    return(list(cv = cv))
}
