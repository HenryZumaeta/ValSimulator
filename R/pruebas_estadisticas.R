#' Realiza la Prueba F de Fisher
#'
#' Esta función realiza la prueba F de Fisher entre las predicciones y las observaciones. Para cada modelo
#' (cada par de columnas en \code{x} y \code{y}), se ajusta un modelo lineal simple y se calcula el estadístico F
#' a partir de la suma de cuadrados residuales, evaluando la hipótesis nula de que no hay diferencia entre
#' las variables.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{F}{Matriz con el estadístico F de Fisher para cada combinación de modelo y observación.}
#'   \item{p}{Matriz con los valores p asociados a la prueba F para cada combinación.}
#' }
#'
#' @details Para cada modelo (columna de \code{x} y la correspondiente columna de \code{y}), la función:
#' \enumerate{
#'   \item Ajusta un modelo lineal simple (\code{lm(yy ~ xx)}).
#'   \item Calcula el residuo y, a partir de éste, estima la suma de cuadrados de los errores.
#'   \item Calcula el estadístico F mediante la fórmula:
#'   \deqn{F = \frac{n \, a^2 + 2 a (b - 1) \sum x + (b - 1)^2 \sum x^2}{2 \, s^2_{xy}},}
#'   donde \eqn{a} y \eqn{b} son los coeficientes del modelo, \eqn{n} es el número de observaciones, y
#'   \eqn{s^2_{xy}} es la varianza de los residuos.
#'   \item Calcula el valor p asociado utilizando la distribución F con 2 y \eqn{(n-2)} grados de libertad.
#' }
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
#'   obs  <- data.frame(real1 = c(10, 11, 13), real2 = c(8, 10, 12))
#'   resultado <- ValPruebaF(pred, obs)
#'   print(resultado$F)
#'   print(resultado$p)
#' }
#'
#' @importFrom stats lm pf coef resid
#'
ValPruebaF <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    Famb <- matrix(nrow = numreal, ncol = numsim,
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    ValP <- matrix(nrow = numreal, ncol = numsim,
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            m1 <- lm(yy ~ xx)
            n <- length(xx)
            s2xy <- sum(resid(m1)^2) / (n - 2)
            a <- coef(m1)[1]
            b <- coef(m1)[2]
            est <- (n * a^2 + 2 * a * (b - 1) * sum(xx) + (b - 1)^2 * sum(xx^2)) / (2 * s2xy)
            Famb[j, i] <- est
            ValP[j, i] <- pf(est, 2, n - 2, lower.tail = FALSE)
        }
    }

    return(list(F = Famb, p = ValP))
}
