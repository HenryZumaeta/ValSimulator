% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/criterio_informacion.R
\name{ValBIC}
\alias{ValBIC}
\title{Calcula el Criterio de Información Bayesiano (BIC)}
\usage{
ValBIC(x, y, k)
}
\arguments{
\item{x}{Data frame, matriz o vector numérico que contiene las predicciones.}

\item{y}{Data frame, matriz o vector numérico que contiene las observaciones.}

\item{k}{Número de parámetros del modelo.}
}
\value{
Una lista con un elemento:
\describe{
\item{BIC}{Vector con el Criterio de Información Bayesiano para cada modelo.}
}
}
\description{
Esta función calcula el Criterio de Información Bayesiano (BIC) entre las predicciones y las observaciones.
El BIC es una medida que equilibra el ajuste del modelo con su complejidad y se define como:
\deqn{BIC = -2 \log(L) + \log(n) \times k,}
donde \eqn{L} es la verosimilitud del modelo, \eqn{n} es el tamaño de la muestra y \eqn{k} es el número de parámetros.
}
\details{
Para cada modelo (columna de \code{x}), la función realiza los siguientes pasos:
\enumerate{
\item Une las observaciones y las predicciones en un solo objeto, omitiendo valores faltantes.
\item Calcula el error cuadrático (RSS) entre las observaciones y las predicciones.
\item Estima la varianza del error como \eqn{\sigma^2 = \text{RSS} / n}, donde \eqn{n} es el número de observaciones.
\item Define la log-verosimilitud como:
\deqn{\ell L = -\frac{n}{2} \log(\sigma^2),}
\item Y, por lo tanto, el BIC se calcula como:
\deqn{BIC = -2 \ell L + \log(n) \times k = n \log(\sigma^2) + \log(n) \times k.}
}
}
\examples{
\dontrun{
  # Ejemplo de uso:
  pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
  obs <- data.frame(real1 = c(10, 11, 13))
  # Supongamos que el modelo tiene 3 parámetros:
  resultado <- ValBIC(pred, obs, k = 3)
  print(resultado$BIC)
}
}
\author{
Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
}
