#' Calcula la Pérdida de Bisagra (Hinge Loss)
#'
#' Esta función calcula la pérdida de bisagra entre las predicciones y las observaciones.
#' La pérdida de bisagra es utilizada en problemas de clasificación, especialmente en métodos de
#' margen como las máquinas de soporte vectorial, donde penaliza los errores de clasificación.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones, que se convertiran a factor.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{hinge_loss}{Matriz con la pérdida de bisagra para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función:
#' \enumerate{
#'   \item Convierte \code{x} en un data frame y \code{y} en un factor.
#'   \item Calcula la pérdida de bisagra para cada par de valores utilizando la expresión:
#'   \deqn{L = \mathrm{mean}\left(\max\left(0, \; 1 - y_i \times x_i\right)\right),}
#'   donde \eqn{y_i} es el valor observado (convertido a factor) y \eqn{x_i} es la predicción.
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
#'   pred <- data.frame(modelo1 = c(0.8, 0.6, 0.9), modelo2 = c(0.7, 0.5, 0.8))
#'   obs <- data.frame(real1 = c("Pos", "Neg", "Pos"), real2 = c("Neg", "Neg", "Pos"))
#'   # Convertimos las observaciones a factor en la función
#'   resultado <- ValHingeLoss(pred, obs)
#'   print(resultado$hinge_loss)
#' }
ValHingeLoss <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.factor(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    hinge_loss <- matrix(nrow = numreal, ncol = numsim,
                         dimnames = list(paste("Observacion", 1:numreal),
                                         paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            hinge_loss[j, i] <- mean(pmax(0, 1 - yy[valid_indices] * xx[valid_indices]))
        }
    }

    return(list(hinge_loss = hinge_loss))
}
