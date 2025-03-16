
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ValSimulator

<!-- badges: start -->

[![R-CMD-check](https://github.com/HenryZumaeta/ValSimulator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HenryZumaeta/ValSimulator/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

ValSimulator es un paquete de R que contiene un conjunto de funciones
destinadas a la validación de modelos de simulación.  
Este paquete incluye herramientas para calcular métricas de error (como
MAE, MSE, etc.), realizar pruebas estadísticas, evaluar inferencias, y
otros índices de rendimiento y precisión.

## Instalación

Puedes instalar la versión de desarrollo de ValSimulator desde
[GitHub](https://github.com/) con:

``` r
# install.packages("pak")
pak::pak("HenryZumaeta/ValSimulator")
```

## Uso Básico

Aquí se muestra un ejemplo básico de cómo utilizar algunas de las
funciones del paquete:

``` r
library(ValSimulator)
   
# Ejemplo: Cálculo del Error Absoluto Medio
pred <- data.frame(modelo1 = c(10, 12, 14), modelo2 = c(9, 11, 15))
obs <- data.frame(real1 = c(10, 11, 13))
resultado <- ValMAE(pred, obs)
print(resultado$mae)
print(resultado$pmae)
```

## Funciones del paquete ValSimulator

El paquete incluye funciones agrupadas en los siguientes archivos:

### **`analisis_perfil.R`**

- `ValPerfil` (Análisis de perfil basado en Hotelling’s T²)

### **`clasificacion_precision.R`**

- `ValHingeLoss` (Pérdida de Bisagra para Modelos de Clasificación)

### **`concordancia_correlacion.R`**

- `ValWillmott` (Índice de Concordancia de Willmott)  
- `ValCorrela` (Coeficientes de Correlación de Pearson, Spearman y
  Kendall)  
- `ValR2` (Coeficiente de Determinación $R^2$)

### **`criterio_informacion.R`**

- `ValAIC` (Criterio de Información de Akaike)  
- `ValBIC` (Criterio de Información Bayesiano)

### **`divergencia_dispersion.R`**

- `ValKullbackLeibler` (Divergencia de Kullback-Leibler)

### **`entropia_diversidad.R`**

- `ValShannon` (Entropía de Shannon)

### **`errores_absolutos.R`**

- `ValMAE` (Error Absoluto Medio)  
- `ValMedAE` (Error Absoluto Mediano)  
- `ValRMAE` (Error Absoluto Medio Relativo)  
- `ValMAPE` (Error Absoluto Porcentual Medio)  
- `ValSMAPE` (Error Absoluto Porcentual Medio Simétrico)

### **`errores_cuadraticos.R`**

- `ValMSE` (Error Cuadrático Medio)  
- `ValRMSE` (Raíz del Error Cuadrático Medio)  
- `ValPRMSE` (Error Cuadrático Medio Relativo en porcentaje)  
- `ValRRMSE` (Error Cuadrático Medio Relativo)  
- `ValMSLE` (Error Cuadrático Medio Logarítmico)  
- `ValRMSLE` (Raíz del Error Cuadrático Medio Logarítmico)  
- `ValRMSPE` (Raíz del Error Cuadrático Porcentual Medio)

### **`gini.R`**

- `ValGini` (Coeficiente de Gini)

### **`indices_metricas_compuestas.R`**

- `ValNash` (Índice de Eficiencia de Nash-Sutcliffe)  
- `ValTheil` (Índice de Theil con componentes MC, SC y RC)

### **`inferencias_modelos.R`**

- `ValBayes` (Inferencia Bayesiana)  
- `ValMLE` (Estimaciones de Máxima Verosimilitud)  
- `ValBootstrap` (Remuestreo Bootstrap)

### **`perdida_sesgo.R`**

- `ValHuberLoss` (Pérdida de Huber)  
- `ValLogCoshLoss` (Pérdida Log-Cosh)  
- `ValQuantileLoss` (Pérdida Cuantílica)

### **`pruebas_estadisticas.R`**

- `ValPruebaF` (Prueba F de Fisher)  
- `ValPruebat` (Prueba t de Student)  
- `ValFrequentista` (Inferencia Frecuentista con Prueba t Pareada)  
- `ValWilcoxon` (Prueba de Wilcoxon)  
- `ValANOVA` (Análisis de Varianza)

### **`sesgo_robustez.R`**

- `ValMBD` (Desviación Media del Sesgo)  
- `ValMASE` (Error Absoluto Medio Escalado)  
- `ValPBIAS` (Sesgo Porcentual)

### **`variabilidad.R`**

- `ValCV` (Coeficiente de Variación)

## Contribución

Si deseas contribuir al paquete o reportar problemas, por favor visita
el repositorio en
[GitHub](https://github.com/HenryZumaeta/ValSimulator).

## Licencia

ValSimulator está bajo licencia MIT. Consulta el archivo LICENSE para
más detalles.
