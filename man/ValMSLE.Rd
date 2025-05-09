% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/errores_cuadraticos.R
\name{ValMSLE}
\alias{ValMSLE}
\title{Calcula el Error Cuadrático Medio Logarítmico (MSLE)}
\usage{
ValMSLE(x, y)
}
\arguments{
\item{x}{Data frame, matriz o vector numérico que contiene las predicciones.}

\item{y}{Data frame, matriz o vector numérico que contiene las observaciones.}
}
\value{
Una lista con un elemento:
\describe{
\item{msle}{Matriz con el error cuadrático medio logarítmico.}
}
}
\description{
Esta función calcula el error cuadrático medio logarítmico entre las predicciones y las observaciones.
Se utiliza la transformación logarítmica (log1p) para manejar adecuadamente valores cercanos a cero.
}
\details{
Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
se calcula:
\deqn{MSLE = \frac{1}{n}\sum_{i=1}^{n}\left(\log(1+x_i) - \log(1+y_i)\right)^2}
}
\examples{
\dontrun{
  # Ejemplo de uso:
  pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
  obs <- data.frame(real1 = c(10, 11, 13))
  resultado <- ValMSLE(pred, obs)
  print(resultado$msle)
}

}
\author{
Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
}
