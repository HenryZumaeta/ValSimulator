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


#' Calcula la Inferencia Bayesiana
#'
#' Esta función calcula la inferencia bayesiana entre las predicciones y las observaciones,
#' asumiendo un prior uniforme sobre los modelos. Para cada modelo (columna de \code{x}), se estima la
#' log-verosimilitud a partir del error cuadrático y se obtiene la probabilidad posterior relativa.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Un vector con las probabilidades posteriores para cada modelo.
#'
#' @details La función realiza los siguientes pasos:
#' \enumerate{
#'   \item Une las observaciones y las predicciones en un solo objeto (omitiendo valores faltantes).
#'   \item Para cada modelo (columna de \code{x}), calcula el error cuadrático (RSS) entre las observaciones y las predicciones.
#'   \item Estima la varianza del error (\eqn{\sigma^2 = \text{RSS} / n}) y define la log-verosimilitud como:
#'   \deqn{\ell L = -\frac{n}{2} \log(\sigma^2),}
#'   \item Calcula la verosimilitud (\eqn{L = \exp(\ell L)}),
#'   \item Aplica un prior uniforme (\eqn{a priori = 1/m}) y finalmente
#'   \deqn{\text{posteriori} = \frac{\text{prior} \times L}{\sum (\text{prior} \times L)}.}
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
#'   obs <- data.frame(real1 = c(10, 11, 13))
#'   resultados <- ValBayes(pred, obs)
#'   print(resultados)
#' }
#'
#' @importFrom stats na.omit
#'
ValBayes <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    n <- nrow(y)

    apriori <- rep(1 / m, m)
    lL <- numeric(m)
    sigmas2 <- numeric(m)
    rss <- numeric(m)
    modelo <- na.omit(cbind(y, x))
    n_modelo <- nrow(modelo)

    for (i in 1:m) {
        rss[i] <- sum((modelo[, 1] - modelo[, i + 1])^2)
        sigmas2[i] <- rss[i] / n_modelo
        lL[i] <- (-n_modelo / 2) * log(sigmas2[i])
    }

    L <- exp(lL)
    numerador <- apriori * L
    denominador <- sum(numerador)
    posteriori <- numerador / denominador

    return(posteriori)
}


#' Calcula las Estimaciones de Máxima Verosimilitud (MLE)
#'
#' Esta función calcula las estimaciones de máxima verosimilitud para la diferencia entre las predicciones y las observaciones,
#' asumiendo que dicha diferencia sigue una distribución normal. Para cada modelo (columna en \code{x} y \code{y}),
#' se estima la media (\eqn{\mu}) y la varianza (\eqn{\sigma^2}) de la diferencia.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{mu}{Vector con la media de la diferencia (predicciones - observaciones) para cada modelo.}
#'   \item{sigma2}{Vector con la varianza de la diferencia para cada modelo.}
#' }
#'
#' @details Para cada modelo, la función calcula la diferencia entre las predicciones y las observaciones,
#' y estima la media (\eqn{\mu}) y la varianza (\eqn{\sigma^2}) de dicha diferencia utilizando la función
#' \code{mean(..., na.rm = TRUE)}. Estas estimaciones se basan en la suposición de que la diferencia sigue
#' una distribución normal.
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
#'   obs <- data.frame(real1 = c(8, 11, 13), real2 = c(10, 10, 15))
#'   resultado <- ValMLE(pred, obs)
#'   print(resultado$mu)
#'   print(resultado$sigma2)
#' }
ValMLE <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    mu <- numeric(m)
    sigma2 <- numeric(m)
    for (i in 1:m) {
        diff <- x[[i]] - y[[i]]
        mu[i] <- mean(diff, na.rm = TRUE)
        sigma2[i] <- mean((diff - mu[i])^2, na.rm = TRUE)
    }
    return(list(mu = mu, sigma2 = sigma2))
}


#' Realiza el Remuestreo Bootstrap para la Diferencia de Medias
#'
#' Esta función realiza un remuestreo bootstrap para estimar la diferencia en medias entre las
#' predicciones y las observaciones, así como para calcular su intervalo de confianza al 95%.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param R Número de replicaciones del bootstrap (por defecto 1000).
#'
#' @return Una lista con tres elementos:
#' \describe{
#'   \item{diff_mean}{Vector con la diferencia en medias estimada para cada modelo.}
#'   \item{ci_lower}{Vector con el límite inferior del intervalo de confianza para la diferencia en medias.}
#'   \item{ci_upper}{Vector con el límite superior del intervalo de confianza para la diferencia en medias.}
#' }
#'
#' @details Para cada modelo (columna en \code{x} y \code{y}), la función:
#' \enumerate{
#'   \item Calcula la diferencia \eqn{d = x_i - y_i} y su media.
#'   \item Realiza \code{R} replicaciones, en cada una de las cuales se muestrea de forma aleatoria con
#'   reemplazo la diferencia y se calcula la media.
#'   \item Calcula el intervalo de confianza al 95% a partir de los cuantiles 0.025 y 0.975 de las replicaciones.
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
#'   obs <- data.frame(real1 = c(8, 11, 13), real2 = c(9, 10, 14))
#'   resultado <- ValBootstrap(pred, obs, R = 1000)
#'   print(resultado$diff_mean)
#'   print(resultado$ci_lower)
#'   print(resultado$ci_upper)
#' }
#'
#' @importFrom stats quantile
#'
ValBootstrap <- function(x, y, R = 1000) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    diff_mean <- numeric(m)
    ci_lower <- numeric(m)
    ci_upper <- numeric(m)
    for (i in 1:m) {
        diff <- x[[i]] - y[[i]]
        diff_mean[i] <- mean(diff, na.rm = TRUE)
        boot_est <- numeric(R)
        n <- length(diff)
        for (r in 1:R) {
            idx <- sample(1:n, size = n, replace = TRUE)
            boot_est[r] <- mean(diff[idx], na.rm = TRUE)
        }
        ci_lower[i] <- quantile(boot_est, 0.025, na.rm = TRUE)
        ci_upper[i] <- quantile(boot_est, 0.975, na.rm = TRUE)
    }
    return(list(diff_mean = diff_mean, ci_lower = ci_lower, ci_upper = ci_upper))
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
