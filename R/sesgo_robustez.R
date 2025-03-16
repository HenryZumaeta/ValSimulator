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


#' Calcula el Sesgo Absoluto Medio (MASE)
#'
#' Esta función calcula el sesgo absoluto medio (MASE) entre las predicciones y las observaciones.
#' El MASE se define como el promedio del valor absoluto de la diferencia entre las predicciones y las observaciones,
#' normalizado por el promedio de la diferencia absoluta entre las observaciones y su valor con un desfase (lag = 1).
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{mase}{Matriz con el sesgo absoluto medio para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función:
#' \enumerate{
#'   \item Convierte \code{x} e \code{y} en data frames.
#'   \item Calcula el valor absoluto de la diferencia entre las predicciones y las observaciones.
#'   \item Normaliza este valor dividiéndolo por el promedio de la diferencia absoluta entre las observaciones
#'         y su valor retrasado (lag de 1).
#' }
#'
#' @note La función \code{lag} se utiliza para obtener el valor previo de las observaciones.
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
#'   resultado <- ValMASE(pred, obs)
#'   print(resultado$mase)
#' }
#'
#' @importFrom stats lag
ValMASE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    mase <- matrix(nrow = numreal, ncol = numsim,
                   dimnames = list(paste("Observacion", 1:numreal),
                                   paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            mase[j, i] <- mean(abs(xx[valid_indices] - yy[valid_indices])) /
                mean(abs(yy[valid_indices] - lag(yy[valid_indices], k = 1)))
        }
    }

    return(list(mase = mase))
}


#' Calcula el Sesgo Porcentual (PBIAS)
#'
#' Esta función calcula el sesgo porcentual entre las predicciones y las observaciones.
#' El PBIAS se expresa en porcentaje y mide la tendencia general del modelo a sobreestimar o subestimar
#' los valores observados.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{pbias}{Matriz con el sesgo porcentual para cada combinación de modelo y observación, expresado en porcentaje.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función:
#' \enumerate{
#'   \item Convierte \code{x} e \code{y} a data frames.
#'   \item Calcula la suma de las diferencias entre las predicciones y las observaciones.
#'   \item Divide esta suma entre la suma de las observaciones y multiplica por 100 para obtener el valor en porcentaje.
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
#'   obs <- data.frame(real1 = c(10, 11, 13), real2 = c(8, 10, 12))
#'   resultado <- ValPBIAS(pred, obs)
#'   print(resultado$pbias)
#' }
ValPBIAS <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    pbias <- matrix(nrow = numreal, ncol = numsim,
                    dimnames = list(paste("Observacion", 1:numreal),
                                    paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            valid_indices <- which(!is.na(xx) & !is.na(yy))
            pbias[j, i] <- 100 * (sum(xx[valid_indices] - yy[valid_indices]) / sum(yy[valid_indices]))
        }
    }

    return(list(pbias = pbias))
}
