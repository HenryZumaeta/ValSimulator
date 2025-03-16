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


#' Calcula la Pérdida Log-Cosh (Log-Cosh Loss)
#'
#' Esta función calcula la pérdida Log-Cosh entre las predicciones y las observaciones.
#' La pérdida Log-Cosh se define como el logaritmo del coseno hiperbólico del error, lo que la hace similar al error cuadrático
#' para errores pequeños, pero se comporta de manera lineal para errores grandes, ofreciendo mayor robustez frente a valores atípicos.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{log_cosh_loss}{Matriz con la pérdida Log-Cosh para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada modelo (columna en \code{x} y en \code{y}), la función:
#' \enumerate{
#'   \item Convierte los datos en data frames.
#'   \item Identifica las posiciones sin valores faltantes.
#'   \item Calcula la pérdida Log-Cosh, definida como:
#'   \deqn{L = \log\left(\cosh\left(x_i - y_i\right)\right),}
#'   y devuelve el promedio de dichos valores para cada modelo.
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
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(8, 11, 13), real2 = c(10, 10, 16))
#'   resultado <- ValLogCoshLoss(pred, obs)
#'   print(resultado$log_cosh_loss)
#' }
ValLogCoshLoss <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    log_cosh_loss <- matrix(nrow = numreal, ncol = numsim,
                            dimnames = list(paste("Observacion", 1:numreal),
                                            paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            log_cosh_loss[j, i] <- mean(log(cosh(xx[valid_indices] - yy[valid_indices])))
        }
    }

    return(list(log_cosh_loss = log_cosh_loss))
}


#' Calcula la Pérdida Cuantílica (Quantile Loss)
#'
#' Esta función calcula la pérdida cuantílica entre las predicciones y las observaciones.
#' La pérdida cuantílica penaliza asimétricamente los errores según un cuantil especificado, lo que permite
#' capturar de forma particular la desviación en una dirección determinada.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param quantile Valor del cuantil a utilizar para el cálculo de la pérdida (por defecto, \code{0.5}).
#'        Un valor de \code{0.5} corresponde a la pérdida cuantílica simétrica (equivalente a la mediana).
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{quantile_loss}{Matriz con la pérdida cuantílica para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada modelo (columna en \code{x} y en \code{y}), la función:
#' \enumerate{
#'   \item Calcula la diferencia \eqn{error = y - x}.
#'   \item Aplica la función de pérdida cuantílica definida como
#'   \deqn{L = \mathrm{mean}\left(\max\left(quantile \times error, (quantile - 1) \times error\right)\right),}
#'   penalizando asimétricamente los errores en función del cuantil.
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
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(8, 11, 13), real2 = c(10, 10, 16))
#'   resultado <- ValQuantileLoss(pred, obs, quantile = 0.5)
#'   print(resultado$quantile_loss)
#' }
ValQuantileLoss <- function(x, y, quantile = 0.5) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    quantile_loss <- matrix(nrow = numreal, ncol = numsim,
                            dimnames = list(paste("Observacion", 1:numreal),
                                            paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            errors <- yy[valid_indices] - xx[valid_indices]
            quantile_loss[j, i] <- mean(pmax(quantile * errors, (quantile - 1) * errors))
        }
    }

    return(list(quantile_loss = quantile_loss))
}
