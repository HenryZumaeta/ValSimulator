#' Calcula la Pérdida de Huber (Huber Loss)
#'
#' Esta función calcula la pérdida de Huber entre las predicciones y las observaciones.
#' La pérdida de Huber es una función de pérdida robusta que combina las propiedades del error cuadrático
#' y del error absoluto, siendo menos sensible a valores atípicos.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param delta Umbral para la transición entre el error cuadrático y el error absoluto. Por defecto, \code{delta = 1.0}.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{huber_loss}{Matriz con la pérdida de Huber para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada par de columnas (correspondiente a un modelo) en \code{x} e \code{y}, se calcula
#' la diferencia y se aplica la función de pérdida de Huber definida como:
#' \deqn{L_{\delta}(error) = \begin{cases} 0.5 \times error^2, & \text{si } |error| \leq \delta, \\ \delta \times (|error| - 0.5 \times \delta), & \text{si } |error| > \delta. \end{cases}}
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
#'   obs <- data.frame(real1 = c(8, 11, 13), real2 = c(10, 10, 16))
#'   resultado <- ValHuberLoss(pred, obs, delta = 1.0)
#'   print(resultado$huber_loss)
#' }
ValHuberLoss <- function(x, y, delta = 1.0) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    huber_loss <- matrix(nrow = numreal, ncol = numsim,
                         dimnames = list(paste("Observacion", 1:numreal),
                                         paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            error <- xx[valid_indices] - yy[valid_indices]
            huber_loss[j, i] <- mean(ifelse(abs(error) <= delta,
                                            0.5 * error^2,
                                            delta * (abs(error) - 0.5 * delta)))
        }
    }

    return(list(huber_loss = huber_loss))
}


