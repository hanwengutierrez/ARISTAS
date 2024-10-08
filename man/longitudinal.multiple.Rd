% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/longitudinal.multiple.R
\name{longitudinal.multiple}
\alias{longitudinal.multiple}
\title{Esta función compara las diferentes ediciones de ARISTAS para más de una variable de corte}
\usage{
longitudinal.multiple(
  datos,
  ediciones,
  y,
  x,
  digit = 3,
  plot = "NULL",
  descarga = FALSE,
  archivo = NULL,
  ...
)
}
\arguments{
\item{datos}{La base de datos que se quiere analizar, debe tener una columna llamada anio indicando a cuál edición corresponde cada fila de la base.}

\item{ediciones}{Vector de caracteres o números indicando cuáles son las ediciones que desea comparar.}

\item{y}{Vector de caracteres indicando el nombre de la variable de interés, ésta variable puede ser de tipo numérico o categórico.}

\item{x}{Vector de caracteres indicando el nombre de las variables de corte.}

\item{digit}{Especificar el número de dígitos decimales, por defecto son 3 dígitos decimales.}

\item{plot}{Las mismas opciones que las de la funci´on \code{longitudinal}.}

\item{descarga}{Indicar si desea descargar los resultados numéricos y la gráfica, debe especificar la ubicación. Al usar la opción \code{descarga = TRUE} se descargará un archivo excel con los resultados numéricos y pdf con gráficas.}

\item{archivo}{Indicar el nombre del archivo excel que se descarga. Las gráficas se descargarán con el mismo nombre y extensión pdf en la misma ubicación que el archivo excel.}

\item{ancho}{El ancho en pulgadas de la gráfica, debe ser un vectro del mismo tamaño que el número de variables de corte.}

\item{alto}{El alto en pulgadas de la gráfica, debe ser un vectro del mismo tamaño que el número de variables de corte.}
}
\value{
\code{longitudinal.multiple}: devuelve una lista de objetos correspondientes a estimacions y resultados de hipótesis de comparación:
\itemize{
\item \code{estimaciones}: La estimación, error estándar, intervalo de confianza de 95\% e información de los NA para la media de la variable de interés para cada valor de la variable de corte \code{x} y para cada edición
\item \code{contrastes_individuales_entre_ediciones}: Los resultados de prueba de hipótesis para la igualdad de medias entre ediciones de la variable de interés para cada valor de cada variable de corte \code{x}.
\item \code{estimaciones.globales}: Las estimaciones, errores estándares, intervalos de confianza de 95\% e información de los NA para la media de la variable de interés para cada edición a nivel global.
\item \code{contraste.global}: Los resultados de prueba de hipótesis para la igualdad de medidas de la variable de interés en diferentes ediciones.
}

Adicionalmente, también la función arrojará la gráfica que usuario haya elegido para cada variable de corte \code{x}
}
\description{
Esta función compara las diferentes ediciones de ARISTAS para más de una variable de corte
}
\details{
Esta función arroja los mismos resultados de la función \code{longitudinal} permitiendo que el usuario ingrese más de una variable de corte.
}
\examples{
# ------------------------------------------------------------------------------------------
# Unificar categorías de las variables
# ------------------------------------------------------------------------------------------
# Unificar las categorías de ARISTAS 2018 según las categorías de 2022
ARISTAS18.matematicas2 <- ARISTAS18.matematicas |>
  dplyr::mutate(regiones = dplyr::case_when(
    regiones == 1 ~ "SUR",
    regiones == 2 ~ "OESTE",
    regiones == 3 ~ "NORTE",
    regiones == 4 ~ "ESTE",
    regiones == 5 ~ "CENTRO",
    TRUE ~ NA_character_  # Para asegurarse de que otros valores no definidos se traten como NA
    )
  )

# Unificar el nombre de la variable theta_MAT_300_50
ARISTAS18.matematicas2 <- ARISTAS18.matematicas2 |>
  dplyr::rename(theta_MAT_300_50 = theta_MAT_E300,
                AlumnoGenero = AlumnoGenero15)

# -------------------------------------------------------------------------------------------------
# Comparación de puntaje de matemáticas
# -------------------------------------------------------------------------------------------------

 # Análisis de la variable theta_MAT_E300 por región y género y edición
 # Unir las bases de las dos ediciones a una sola base de datos
ARISTAS18.22.matematicas <- unionbases.longitudinal(ediciones = c(2018, 2022), bases = c("ARISTAS18.matematicas2", "ARISTAS22.matematicas"), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"))
longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "density")
longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "density", descarga = TRUE)
longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "density", descarga = TRUE, archivo = "theta_MAT_300_50.xlsx")
longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "density", descarga = TRUE, archivo = "theta_MAT_300_50.xlsx", ancho = c(10,4), alto = c(6,9))

# -------------------------------------------------------------------------------------------------
# Comparación de nivel de desempeño de matemáticas
# -------------------------------------------------------------------------------------------------

 # Análisis de la variable theta_MAT_E300 por región y género y edición
 # Unir las bases de las dos ediciones a una sola base de datos
ARISTAS18.22.matematicas <- unionbases.longitudinal(ediciones = c(2018, 2022), bases = c("ARISTAS18.matematicas2", "ARISTAS22.matematicas"), y = "Niveles_MAT", x = c("regiones", "AlumnoGenero"))
longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "Niveles_MAT", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "bar")
longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "Niveles_MAT", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "bar", descarga = TRUE, archivo = "Niveles_MAT.xlsx")
longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "Niveles_MAT", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "bar", descarga = TRUE, archivo = "Niveles_MAT.xlsx", ancho = c(10,4), alto = c(6,9))

}
