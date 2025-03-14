#' Calcula el Error Absoluto Medio (MAE)
#'
#' Esta función calcula el error absoluto medio entre las predicciones y las observaciones.
#' Además, calcula el error porcentual medio (pMAE) como porcentaje respecto a la media de las observaciones.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{mae}{Matriz con los errores absolutos medios.}
#'   \item{pmae}{Matriz con los errores porcentuales medios, expresados en porcentaje.}
#' }
#'
#' @details Para cada modelo (columna de `x`) y cada conjunto de observaciones (columna de `y`), se calcula:
#' \deqn{MAE = \frac{1}{n}\sum_{i=1}^{n}|x_i - y_i|}
#' y
#' \deqn{pMAE = \frac{MAE}{\bar{y}} \times 100.}
#'
#' @author
#' Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
#' LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
#' WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
#'
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso:
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(10, 11, 13))
#'   resultado <- ValMAE(pred, obs)
#'   print(resultado$mae)
#'   print(resultado$pmae)
#' }
#'
#' @export
#'
ValMAE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    mae <- matrix(nrow = numreal, ncol = numsim,
                  dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    pmae <- matrix(nrow = numreal, ncol = numsim,
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            mae[j, i] <- mean(abs(xx[valid_indices] - yy[valid_indices]))
            pmae[j, i] <- mae[j, i] / mean(yy[valid_indices]) * 100
        }
    }

    return(list(mae = mae, pmae = pmae))
}


#' Calcula el Error Absoluto Mediano (MedAE)
#'
#' Esta función calcula el error absoluto mediano entre las predicciones y las observaciones.
#' La mediana es una medida robusta que no se ve afectada por valores atípicos, lo que puede ser
#' útil para evaluar el desempeño de modelos de simulación en contextos con datos extremos.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{medae}{Matriz con los errores absolutos medianos.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
#' se calcula la mediana de los valores absolutos de las diferencias:
#' \deqn{MedAE = \text{median}(|x_i - y_i|)}
#'
#' @author
#' Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
#' LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
#' WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
#'
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso:
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(10, 11, 13))
#'   resultado <- ValMedAE(pred, obs)
#'   print(resultado$medae)
#' }
#' @importFrom stats median
#'
#' @export
#'
ValMedAE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    medae <- matrix(nrow = numreal, ncol = numsim,
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            medae[j, i] <- median(abs(xx[valid_indices] - yy[valid_indices]))
        }
    }

    return(list(medae = medae))
}


#' Calcula el Error Absoluto Medio Relativo (RMAE)
#'
#' Esta función calcula el error absoluto medio relativo entre las predicciones y las observaciones,
#' dividiendo el error absoluto medio por la media de las observaciones. Esta métrica permite evaluar la
#' magnitud del error en relación con los valores reales.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{rmae}{Matriz con los errores absolutos medios relativos.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
#' se calcula:
#' \deqn{RMAE = \frac{\text{MAE}}{\bar{y}}}
#'
#' @author
#' Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
#' LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
#' WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
#'
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso:
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(10, 11, 13))
#'   resultado <- ValRMAE(pred, obs)
#'   print(resultado$rmae)
#' }
#'
#' @export
#'
ValRMAE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    rmae <- matrix(nrow = numreal, ncol = numsim,
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            rmae[j, i] <- mean(abs(xx[valid_indices] - yy[valid_indices])) / mean(yy[valid_indices])
        }
    }

    return(list(rmae = rmae))
}


#' Calcula el Error Absoluto Porcentual Medio (MAPE)
#'
#' Esta función calcula el error absoluto porcentual medio entre las predicciones y las observaciones.
#' El MAPE se expresa en porcentaje y es útil para evaluar la precisión de las predicciones
#' en relación con los valores reales.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{mape}{Matriz con los errores absolutos porcentuales medios, expresados en porcentaje.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
#' se calcula:
#' \deqn{MAPE = \frac{1}{n}\sum_{i=1}^{n}\left|\frac{y_i - x_i}{y_i}\right| \times 100}
#'
#' @author
#' Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
#' LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
#' WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
#'
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso:
#'   pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
#'   obs <- data.frame(real1 = c(10, 11, 13))
#'   resultado <- ValMAPE(pred, obs)
#'   print(resultado$mape)
#' }
#'
#' @export
#'
ValMAPE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    mape <- matrix(nrow = numreal, ncol = numsim,
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(yy != 0 & !is.na(xx) & !is.na(yy))
            mape[j, i] <- mean(abs((yy[valid_indices] - xx[valid_indices]) / yy[valid_indices])) * 100
        }
    }

    return(list(mape = mape))
}


#' Calcula el Error Absoluto Porcentual Medio Simétrico (sMAPE)
#'
#' Esta función calcula el error absoluto porcentual medio simétrico entre las predicciones y las observaciones.
#' El sMAPE se expresa en porcentaje y se define como el promedio de la razón simétrica entre la diferencia absoluta
#' y la suma de los valores absolutos, lo cual permite evaluar la precisión de las predicciones de manera simétrica.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{smape}{Matriz con los errores absolutos porcentuales medios simétricos, expresados en porcentaje.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
#' se calcula:
#' \deqn{sMAPE = \frac{1}{n}\sum_{i=1}^{n}\frac{2 \, |x_i - y_i|}{|x_i| + |y_i|} \times 100}
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
#'   resultado <- ValSMAPE(pred, obs)
#'   print(resultado$smape)
#' }
#'
#' @export
#'
ValSMAPE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    smape <- matrix(nrow = numreal, ncol = numsim,
                    dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            smape[j, i] <- mean(2 * abs(xx[valid_indices] - yy[valid_indices]) / (abs(xx[valid_indices]) + abs(yy[valid_indices]))) * 100
        }
    }

    return(list(smape = smape))
}

