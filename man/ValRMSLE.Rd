% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/errores_cuadraticos.R
\name{ValRMSLE}
\alias{ValRMSLE}
\title{Calcula el Error Cuadrático Medio Logarítmico Relativo (RMSLE)}
\usage{
ValRMSLE(x, y)
}
\arguments{
\item{x}{Data frame, matriz o vector numérico que contiene las predicciones.}

\item{y}{Data frame, matriz o vector numérico que contiene las observaciones.}
}
\value{
Una lista con un elemento:
\describe{
\item{rmsle}{Matriz con el error cuadrático medio logarítmico relativo.}
}
}
\description{
Esta función calcula el error cuadrático medio logarítmico relativo entre las predicciones y las observaciones,
dividiendo la raíz del error cuadrático medio logarítmico por la media de las observaciones. Esto permite evaluar la
precisión de las predicciones de forma relativa, considerando la magnitud de los valores observados.
}
\details{
Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
se calcula:
\deqn{RMSLE = \frac{\sqrt{\frac{1}{n}\sum_{i=1}^{n}\left(\log(1+x_i) - \log(1+y_i)\right)^2}}{\bar{y}}}
}
\examples{
\dontrun{
  # Ejemplo de uso:
  pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
  obs <- data.frame(real1 = c(10, 11, 13))
  resultado <- ValRMSLE(pred, obs)
  print(resultado$rmsle)
}

}
\author{
Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
}
