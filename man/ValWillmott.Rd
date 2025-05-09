% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concordancia_correlacion.R
\name{ValWillmott}
\alias{ValWillmott}
\title{Calcula el Índice de Concordancia (d) de Willmott}
\usage{
ValWillmott(x, y)
}
\arguments{
\item{x}{Data frame, matriz o vector numérico que contiene las predicciones.}

\item{y}{Data frame, matriz o vector numérico que contiene las observaciones.}
}
\value{
Una lista con un elemento:
\describe{
\item{W}{Matriz con el índice de concordancia de Willmott para cada combinación de modelo y observación.}
}
}
\description{
Esta función calcula el índice de concordancia de Willmott entre las predicciones y las observaciones.
Este índice evalúa la capacidad del modelo para reproducir el patrón observado, variando entre 0 y 1, donde 1 indica
una concordancia perfecta.
}
\details{
Para cada modelo (columna de \code{x}) y cada conjunto de observaciones (columna de \code{y}),
se calcula el índice de concordancia de Willmott (\eqn{d}) como:
\deqn{d = 1 - \frac{\sum_{i=1}^{n}(x_i - y_i)^2}{\sum_{i=1}^{n}\left(|x_i - \bar{y}| + |y_i - \bar{y}|\right)^2}}
donde \eqn{\bar{y}} es la media de las observaciones.
}
\examples{
\dontrun{
  # Ejemplo de uso:
  pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
  obs <- data.frame(real1 = c(10, 11, 13))
  resultado <- ValWillmott(pred, obs)
  print(resultado$W)
}
}
\author{
Henry P. Zumaeta Lozano (\email{henry.zumaeta.l@uni.pe})
LinkedIn: \href{https://www.linkedin.com/in/henryzumaeta}{henryzumaeta}
WhatsApp: \href{https://wa.me/51963719768}{+51963719768}
}
