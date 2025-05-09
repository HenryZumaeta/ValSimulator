#' Calcula la Inferencia Frecuentista (Prueba t Pareada)
#'
#' Esta función realiza una prueba t de Student para muestras pareadas entre las predicciones y las observaciones.
#' Para cada modelo (columna de \code{x} y \code{y}), se calcula el estadístico t, el valor p y el intervalo de confianza
#' para la diferencia media.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param conf.level Nivel de confianza para el intervalo de confianza (por defecto 0.95).
#'
#' @return Una lista con cuatro elementos:
#' \describe{
#'   \item{statistic}{Vector con el estadístico t para cada modelo.}
#'   \item{p.value}{Vector con el valor p para cada modelo.}
#'   \item{lower}{Vector con el límite inferior del intervalo de confianza para cada modelo.}
#'   \item{upper}{Vector con el límite superior del intervalo de confianza para cada modelo.}
#' }
#'
#' @details Para cada par de columnas correspondientes a un modelo, la función realiza una prueba t de Student para
#' muestras pareadas, evaluando si la diferencia media entre las predicciones y las observaciones es significativamente
#' distinta de cero.
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
#'   resultado <- ValFrequentista(pred, obs, conf.level = 0.95)
#'   print(resultado$statistic)
#'   print(resultado$p.value)
#'   print(resultado$lower)
#'   print(resultado$upper)
#' }
#'
#' @importFrom stats t.test
#'
ValFrequentista <- function(x, y, conf.level = 0.95) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    stat <- numeric(m)
    pval <- numeric(m)
    lower <- numeric(m)
    upper <- numeric(m)
    for (i in 1:m) {
        test <- t.test(x[[i]], y[[i]], paired = TRUE, conf.level = conf.level)
        stat[i] <- test$statistic
        pval[i] <- test$p.value
        lower[i] <- test$conf.int[1]
        upper[i] <- test$conf.int[2]
    }
    return(list(statistic = stat, p.value = pval, lower = lower, upper = upper))
}


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


#' Realiza la Prueba t de Student
#'
#' Esta función realiza la prueba t de Student para muestras pareadas entre las predicciones y las observaciones.
#' Para cada par de columnas correspondientes a un modelo, evalúa la hipótesis nula de que la diferencia media es cero,
#' y calcula el estadístico t, el valor p y el intervalo de confianza para la diferencia.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param confidence Nivel de confianza para el intervalo de confianza (por defecto 0.95).
#'
#' @return Una lista con cuatro elementos:
#' \describe{
#'   \item{Estadisticot}{Matriz con el estadístico t para cada modelo.}
#'   \item{p}{Matriz con los valores p asociados a cada prueba.}
#'   \item{limiteinferiorIC}{Matriz con el límite inferior del intervalo de confianza para cada modelo.}
#'   \item{limitesuperiorIC}{Matriz con el límite superior del intervalo de confianza para cada modelo.}
#' }
#'
#' @details La función utiliza \code{t.test(..., paired = TRUE, conf.level = confidence)} para cada par de columnas
#' correspondientes a un modelo y construye matrices para almacenar el estadístico, el valor p y los límites del intervalo de
#' confianza.
#'
#' @importFrom stats t.test
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
#'   resultado <- ValPruebat(pred, obs, confidence = 0.95)
#'   print(resultado$Estadisticot)
#'   print(resultado$p)
#'   print(resultado$limiteinferiorIC)
#'   print(resultado$limitesuperiorIC)
#' }
ValPruebat <- function(x, y, confidence = 0.95) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numreal <- ncol(y)
    numsim <- ncol(x)

    Valt <- matrix(nrow = numreal, ncol = numsim,
                   dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    p <- matrix(nrow = numreal, ncol = numsim,
                dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    confiinf <- matrix(nrow = numreal, ncol = numsim,
                       dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))
    confisup <- matrix(nrow = numreal, ncol = numsim,
                       dimnames = list(paste("Observacion", 1:numreal), paste("Modelo", 1:numsim)))

    for (i in 1:numsim) {
        for (j in 1:numreal) {
            xx <- x[[i]]
            yy <- y[[j]]
            res <- t.test(xx, yy, paired = TRUE, conf.level = confidence)
            Valt[j, i] <- res$statistic
            p[j, i] <- res$p.value
            confiinf[j, i] <- res$conf.int[1]
            confisup[j, i] <- res$conf.int[2]
        }
    }

    return(list(Estadisticot = Valt, p = p, limiteinferiorIC = confiinf, limitesuperiorIC = confisup))
}


#' Realiza la Inferencia Paramétrica con ANOVA
#'
#' Esta función realiza una prueba ANOVA de una vía para comparar las predicciones y las observaciones.
#' Para cada modelo (columna de \code{x} y \code{y}), combina la información en un único data frame, añadiendo
#' un factor que indica si el valor proviene de las predicciones ("Pred") o de las observaciones ("Obs"), y luego
#' realiza la prueba ANOVA para determinar si existen diferencias significativas.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista en la que cada elemento es el resumen de la prueba ANOVA para cada modelo.
#'
#' @details Para cada modelo, la función:
#' \enumerate{
#'   \item Combina los valores de \code{x} y \code{y} en un único data frame.
#'   \item Crea un factor que clasifica los valores en "Pred" y "Obs".
#'   \item Realiza una prueba ANOVA de una vía para evaluar si existen diferencias significativas
#'   entre las predicciones y las observaciones.
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
#'   resultados <- ValANOVA(pred, obs)
#'   # Para ver el resumen de la prueba ANOVA del Modelo 1:
#'   print(resultados[["Modelo 1"]])
#' }
#'
#' @importFrom stats aov
#'
ValANOVA <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    numsim <- ncol(x)

    anova_list <- vector("list", length = numsim)

    for (i in 1:numsim) {
        pred <- x[[i]]
        obs  <- y[[i]]

        df <- data.frame(
            value = c(pred, obs),
            group = factor(rep(c("Pred", "Obs"), each = length(pred)))
        )

        anova_list[[i]] <- summary(aov(value ~ group, data = df))
    }

    names(anova_list) <- paste("Modelo", 1:numsim)

    return(anova_list)
}


#' Realiza la Inferencia No Paramétrica con Wilcoxon
#'
#' Esta función realiza la prueba de Wilcoxon para muestras pareadas entre las predicciones y las observaciones.
#' Se utiliza para evaluar si existe una diferencia significativa en la mediana de las diferencias entre las dos muestras.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{statistic}{Vector con el estadístico de la prueba Wilcoxon para cada modelo.}
#'   \item{p.value}{Vector con el valor p asociado a la prueba para cada modelo.}
#' }
#'
#' @details Para cada par de columnas (un modelo) se realiza la prueba de Wilcoxon para muestras pareadas,
#' evaluando la hipótesis nula de que la mediana de las diferencias es cero.
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
#'   resultado <- ValWilcoxon(pred, obs)
#'   print(resultado$statistic)
#'   print(resultado$p.value)
#' }
#'
#' @importFrom stats wilcox.test
#'
ValWilcoxon <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    stat <- numeric(m)
    pval <- numeric(m)
    for (i in 1:m) {
        test <- wilcox.test(x[[i]], y[[i]], paired = TRUE)
        stat[i] <- test$statistic
        pval[i] <- test$p.value
    }
    return(list(statistic = stat, p.value = pval))
}
