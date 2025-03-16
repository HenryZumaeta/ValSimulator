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


#' Calcula el Índice de Eficiencia de Nash-Sutcliffe (Nash)
#'
#' Esta función calcula el índice de eficiencia de Nash-Sutcliffe entre las predicciones y las observaciones.
#' El índice de Nash-Sutcliffe (NSE) es una medida que evalúa la capacidad predictiva de un modelo; su valor varía entre
#' \eqn{-\infty} y 1, donde 1 indica una perfecta concordancia entre las predicciones y las observaciones.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{Eficiencia}{Matriz con el índice de eficiencia de Nash-Sutcliffe para cada combinación de modelo y observación.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función calcula el índice de eficiencia de Nash-Sutcliffe
#' utilizando la fórmula:
#' \deqn{NSE = 1 - \frac{\sum_{i=1}^{n} (x_i - y_i)^2}{\sum_{i=1}^{n} (y_i - \bar{y})^2},}
#' donde \eqn{\bar{y}} es la media de las observaciones y \eqn{n} es el número de observaciones.
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
#'   obs <- data.frame(real1 = c(10, 11, 13), real2 = c(8, 10, 12))
#'   resultado <- ValNash(pred, obs)
#'   print(resultado$Eficiencia)
#' }
ValNash <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    EF <- matrix(nrow = numreal, ncol = numsim,
                 dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            mean_yy <- mean(yy, na.rm = TRUE)
            EF[j, i] <- 1 - (sum((xx - yy)^2, na.rm = TRUE) / sum((yy - mean_yy)^2, na.rm = TRUE))
        }
    }

    return(list(Eficiencia = EF))
}
