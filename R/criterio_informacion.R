#' Calcula el Criterio de Información de Akaike (AIC)
#'
#' Esta función calcula el coeficiente de información de Akaike (AIC) entre las predicciones y las observaciones.
#' El AIC es una medida que equilibra el ajuste del modelo con su complejidad y se define como:
#' \deqn{AIC = -2 \times \ell L + 2k,}
#' donde \eqn{\ell L} es la log-verosimilitud del modelo y \eqn{k} es el número de parámetros.
#' Además, se calcula el AIC corregido (AICc) para muestras pequeñas:
#' \deqn{AICc = AIC + \frac{2k(k+1)}{n - k - 1},}
#' donde \eqn{n} es el tamaño de la muestra.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param k Número de parámetros del modelo. Se utiliza para ajustar el AIC.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{AIC}{Vector con el coeficiente de información de Akaike para cada modelo.}
#'   \item{AICc}{Vector con el coeficiente de información de Akaike corregido para cada modelo.}
#' }
#'
#' @details La función une las observaciones y las predicciones en un solo objeto (omitiendo valores faltantes)
#' y, para cada modelo (columna de \code{x}), calcula el valor del AIC y el AIC corregido (AICc). Si el valor
#' de \code{k} no es compatible con el tamaño de la muestra, se detiene la ejecución.
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
#'   # Supongamos que el modelo tiene 3 parámetros:
#'   resultado <- ValAIC(pred, obs, k = 3)
#'   print(resultado$AIC)
#'   print(resultado$AICc)
#' }
#' @importFrom stats na.omit
#'
#' @export
#'
ValAIC <- function(x, y, k) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    n <- nrow(y)

    AIC <- numeric(m)
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

    AIC <- -2 * lL + 2 * k
    if (max(k) != n + 1) {
        AICc <- AIC + 2 * k * (k + 1) / (n_modelo - k - 1)
    } else {
        stop("No se puede calcular AICc, no se puede dividir entre cero")
    }

    return(list(AIC = AIC, AICc = AICc))
}


#' Calcula el Criterio de Información Bayesiano (BIC)
#'
#' Esta función calcula el Criterio de Información Bayesiano (BIC) entre las predicciones y las observaciones.
#' El BIC es una medida que equilibra el ajuste del modelo con su complejidad y se define como:
#' \deqn{BIC = -2 \log(L) + \log(n) \times k,}
#' donde \eqn{L} es la verosimilitud del modelo, \eqn{n} es el tamaño de la muestra y \eqn{k} es el número de parámetros.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param k Número de parámetros del modelo.
#'
#' @return Una lista con un elemento:
#' \describe{
#'   \item{BIC}{Vector con el Criterio de Información Bayesiano para cada modelo.}
#' }
#'
#' @details Para cada modelo (columna de \code{x}), la función realiza los siguientes pasos:
#' \enumerate{
#'   \item Une las observaciones y las predicciones en un solo objeto, omitiendo valores faltantes.
#'   \item Calcula el error cuadrático (RSS) entre las observaciones y las predicciones.
#'   \item Estima la varianza del error como \eqn{\sigma^2 = \text{RSS} / n}, donde \eqn{n} es el número de observaciones.
#'   \item Define la log-verosimilitud como:
#'   \deqn{\ell L = -\frac{n}{2} \log(\sigma^2),}
#'   \item Y, por lo tanto, el BIC se calcula como:
#'   \deqn{BIC = -2 \ell L + \log(n) \times k = n \log(\sigma^2) + \log(n) \times k.}
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
#'   # Supongamos que el modelo tiene 3 parámetros:
#'   resultado <- ValBIC(pred, obs, k = 3)
#'   print(resultado$BIC)
#' }
ValBIC <- function(x, y, k) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    m <- ncol(x)
    n <- nrow(y)

    rss <- numeric(m)
    modelo <- na.omit(cbind(y, x))
    n_modelo <- nrow(modelo)

    BIC <- numeric(m)
    for (i in 1:m) {
        rss[i] <- sum((modelo[, 1] - modelo[, i + 1])^2)
        sigma2 <- rss[i] / n_modelo
        BIC[i] <- n_modelo * log(sigma2) + log(n_modelo) * k
    }

    return(list(BIC = BIC))
}


#' Realiza el Análisis de Perfil (Profile Analysis)
#'
#' Esta función realiza un análisis de perfil entre las predicciones y las observaciones. Es un método multivariante
#' que prueba la hipótesis de que la trayectoria de los datos reales y la salida del modelo son paralelas, lo cual permite
#' evaluar modelos que simulan el comportamiento de un sistema real a lo largo del tiempo.
#'
#' @param x Data frame, matriz o vector numérico que contiene las predicciones. Cada fila representa un perfil.
#' @param y Data frame, matriz o vector numérico que contiene las observaciones.
#' @param instantetiempo Número de instantes de tiempo (o intervalo) en donde se tomaron las muestras, y que define el perfil.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{T2}{Vector con el estadístico Hotelling's T² para cada perfil.}
#'   \item{valorp}{Vector con los valores p asociados a cada prueba de perfil.}
#' }
#'
#' @details La función realiza los siguientes pasos:
#' \enumerate{
#'   \item Convierte \code{x} y \code{y} en data frames y elimina las filas incompletas de \code{y} mediante \code{na.omit()}.
#'   \item Define el número de subintervalos (\code{var}) como la cantidad de columnas de \code{y} dividida entre \code{instantetiempo}.
#'   \item Para cada perfil (fila de \code{x}), construye una matriz \code{yyy} a partir de los datos de \code{y} en intervalos definidos
#'   por \code{instantetiempo}. Esta matriz se forma calculando las diferencias secuenciales entre columnas consecutivas de \code{y} y
#'   de \code{x}.
#'   \item Calcula el estadístico Hotelling's T² y el valor p utilizando la función \code{HotellingsT2} del paquete \code{ICSNP} (con \code{test = "chi"}).
#' }
#'
#' Además, este método permite evaluar modelos con múltiples variables respuesta y diversos intervalos de tiempo.
#' Una desventaja es que puede requerir un número relativamente grande de réplicas, que se estima con la condición
#' \eqn{n > q(k-1)}, donde \eqn{q} es el número de variables respuesta y \eqn{k} el número de instantes de tiempo.
#'
#' @note Se requiere que el paquete \code{ICSNP} esté instalado, ya que se utiliza la función \code{HotellingsT2}.
#'
#' @references
#' HAEFNER, James W. (2005). \emph{Modeling Biological Systems: Principles and Applications}. Springer.
#' \cr
#' Timm N. H. (1975). \emph{Multivariate Analysis with Applications in Education and Psychology}. Monterey: Brooks/Cole Publishing Company.
#'
#' @seealso \code{\link[ICSNP]{HotellingsT2}} del paquete \code{ICSNP}.
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
#'   # Supongamos que 'x' tiene 5 perfiles (filas) y 'y' tiene datos correspondientes a 10 instantes,
#'   # de modo que instantetiempo = 2 y var = 10 / 2 = 5.
#'   pred <- data.frame(matrix(rnorm(50), nrow = 5))
#'   obs <- data.frame(matrix(rnorm(100), nrow = 10))
#'   resultado <- ValProfile(pred, obs, instantetiempo = 2)
#'   print(resultado$T2)
#'   print(resultado$valorp)
#' }
#'
#' @importFrom ICSNP HotellingsT2
ValProfile <- function(x, y, instantetiempo) {
    x <- as.data.frame(x)
    y <- na.omit(as.data.frame(y))
    ins <- instantetiempo
    temp3 <- ins - 1
    temp4 <- nrow(x)
    var <- ncol(y) / ins
    T2 <- numeric(temp4)
    valorp <- numeric(temp4)

    for (m in 1:temp4) {
        fila <- x[m, ]
        yyy <- matrix(nrow = nrow(y), ncol = temp3 * var)
        k <- 1
        for (i in 1:var) {
            temp1 <- (i - 1) * ins + 1
            temp2 <- ins * i
            yy <- y[temp1:temp2, ]
            xx <- fila[temp1:temp2]
            for (j in 1:temp3) {
                yyy[, k] <- yy[, j] - yy[, j + 1] - xx[j] + xx[j + 1]
                k <- k + 1
            }
        }
        res <- HotellingsT2(yyy, test = "chi")
        T2[m] <- res$statistic
        valorp[m] <- res$p.value
    }

    return(list(T2 = T2, valorp = valorp))
}
