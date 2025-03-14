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


#' Calcula el Coeficiente de Correlación
#'
#' Esta función calcula el coeficiente de correlación entre las predicciones y las observaciones
#' utilizando los métodos de Pearson, Spearman y Kendall. Para cada par de columnas de \code{x} y \code{y},
#' se obtiene una matriz de correlación para cada método especificado.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param methods Vector de métodos de correlación a utilizar. Los métodos disponibles son \code{"pearson"},
#' \code{"spearman"} y \code{"kendall"}. Por defecto se utilizan los tres: \code{c("pearson", "spearman", "kendall")}.
#'
#' @return Una lista en la que cada elemento es una matriz de correlación correspondiente al método especificado.
#' \describe{
#'   \item{pearson}{Matriz de correlación calculada con el método de Pearson.}
#'   \item{spearman}{Matriz de correlación calculada con el método de Spearman.}
#'   \item{kendall}{Matriz de correlación calculada con el método de Kendall.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
#' la función calcula la correlación utilizando el método indicado y utiliza \code{use = "complete.obs"} para omitir
#' observaciones con valores faltantes.
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
#'   resultados <- ValCorrela(pred, obs)
#'   # Acceder a la matriz de correlación de Pearson:
#'   resultados[["pearson"]]
#'   # Acceder a la matriz de correlación de Spearman:
#'   resultados[["spearman"]]
#'   # Acceder a la matriz de correlación de Kendall:
#'   resultados[["kendall"]]
#' }
#' @importFrom stats cor
#'
#' @export
#'
ValCorrela <- function(x, y, methods = c("pearson", "spearman", "kendall")) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    # Inicializamos una lista para almacenar los resultados por método
    result_list <- list()

    for (method in methods) {
        corr_mat <- matrix(nrow = numreal, ncol = numsim,
                           dimnames = list(paste("Observacion", 1:numreal),
                                           paste("Modelo", 1:numsim)))

        for (i in 1:numsim) {
            for (j in 1:numreal) {
                xx <- x[[i]]
                yy <- y[[j]]
                corr_mat[j, i] <- cor(xx, yy, method = method, use = "complete.obs")
            }
        }

        result_list[[method]] <- corr_mat
    }

    return(result_list)
}


#' Calcula el Coeficiente de Determinación R cuadrado (R²)
#'
#' Esta función calcula el coeficiente de determinación R² entre las predicciones y las observaciones.
#' El R² representa la proporción de la varianza en las observaciones que es explicada por las predicciones.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{rcuadrado}{Matriz con el coeficiente de determinación R² para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada par de columnas (modelo y observación), se calcula:
#' \deqn{R^2 = \left(\mathrm{cor}(x, y, \text{use = "complete.obs"})\right)^2}
#' utilizando únicamente las observaciones completas (omitiendo valores NA).
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
#'   resultado <- ValR2(pred, obs)
#'   print(resultado$rcuadrado)
#' }
#'
#' @export
#'
ValR2 <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    rcuadrado <- matrix(nrow = numreal, ncol = numsim,
                        dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            rcuadrado[j, i] <- cor(xx, yy, use = "complete.obs")^2
        }
    }

    return(list(rcuadrado = rcuadrado))
}

