#' @title
#' Esta función calcula las estimaciones de una variable de interés para una edición específica de ARISTAS para más de una variable de corte
#'
#' @param data La base de datos que se quiere analizar, puede ser cualquiera de las bases de ARISTAS incluidas en el paquete o cualquiera otra base ARISTAS que no sea de uso público. Tenga en cuenta que las columnas que contengan los pesos replicados deben tener nombres: EST_W_REP_1, EST_W_REP_2, etc.
#' @param y El nombre de la variable de interés, ésta variable puede ser de tipo numérico o categórico.
#' @param x Vector de caracteres indicando el nombre de las variables de corte.
#' @param digit Especificar el número de dígitos decimales, por defecto son 3 dígitos decimales.
#' @param IC Indicar si se muestran las estimaciones e intervalos de confianza sobre la gráfica.
#' @param plot Las mismas opciones que las de la funci´on `transversal`.
#' @param descarga Indicar si desea descargar los resultados numéricos y la gráfica, debe especificar la ubicación. Al usar la opción `descarga = TRUE` se descargará un archivo excel con los resultados numéricos y pdf con las gráficas.
#' @param archivo Indicar el nombre del archivo excel que se descarga. Las gráficas se descargarán con el mismo nombre y extensión pdf en la misma ubicación que el archivo excel.
#' @param ancho El ancho en pulgadas de la gráfica, debe ser un vectro del mismo tamaño que el número de variables de corte.
#' @param alto El alto en pulgadas de la gráfica, debe ser un vectro del mismo tamaño que el número de variables de corte.
#'
#' @return
#' `transversal.multiple` devuelve una lista de objetos correspondientes a estimacions y resultados de hipótesis de comparación:
#' * `estimaciones`: La estimación, error estándar, intervalo de confianza de 95% e información de los NA para la media de la variable de interés para cada valor de la variable de corte `x` y también a nivel global (para todos los datos ingresados en el argumento `data`)
#' * `contrastes_individuales`: Resultados de la prueba de hipótesis para la igualdad de medidas de `y` para cada combinación de 2 categorías de cada variable de corte.
#' * `contraste_global`: Resultados de la prueba de hipótesis para la comparación múltiple de la media de `y` para todas las categorías de cada variable de corte.
#'
#'  Adicionalmente, también la función arrojará la gráfica que usuario haya elegido.
#'
#' @details
#'
#' Esta función arroja los mismos resultados de la función `transversal` permitiendo que el usuario ingrese más de una variable de corte.
#'
#' @importFrom ggplot2 ggsave
#' @export
#'
#' @examples
#'# ------------------------------------------------------------------------------------------------------
#'# Análisis de theta_MAT_E300 con variables de corte regiones y AlumnoGenero15
#'# ------------------------------------------------------------------------------------------------------
#' transversal.multiple(data = ARISTAS18.matematicas, y = "theta_MAT_E300", x = c("regiones","AlumnoGenero15"))
#' transversal.multiple(data = ARISTAS18.matematicas, y = "theta_MAT_E300", x = c("regiones","AlumnoGenero15"), descarga = TRUE)
#' transversal.multiple(data = ARISTAS18.matematicas, y = "theta_MAT_E300", x = c("regiones","AlumnoGenero15"), descarga = TRUE, plot = "density", archivo = "theta_MAT_E300.xlsx")
#' transversal.multiple(data = ARISTAS18.matematicas, y = "theta_MAT_E300", x = c("regiones","AlumnoGenero15"), descarga = TRUE, plot = "Lollipop", archivo = "theta_MAT_E300.xlsx", ancho = c(10,4), alto = c(8,5))
#'
#'# ------------------------------------------------------------------------------------------------------
#'# Análisis de Niveles_MAT con variables de corte regiones y AlumnoGenero15
#'# ------------------------------------------------------------------------------------------------------
#' transversal.multiple(data = ARISTAS18.matematicas, y = "Niveles_MAT", x = c("regiones","AlumnoGenero15"))
#' transversal.multiple(data = ARISTAS18.matematicas, y = "Niveles_MAT", x = c("regiones","AlumnoGenero15"), descarga = TRUE, plot = "bar", archivo = "Niveles_MAT.xlsx")
#' transversal.multiple(data = ARISTAS18.matematicas, y = "Niveles_MAT", x = c("regiones","AlumnoGenero15"), descarga = TRUE, plot = "bar", archivo = "Niveles_MAT.xlsx", ancho = c(10,4), alto = c(6,9))


transversal.multiple <- function(data, y, x, IC = TRUE, digit = 3, plot = NULL, descarga = FALSE, archivo = NULL, ...) {

  args <- list(...)
  options(warn = -1) # Eliminar los warning que produce ggplot

  # Inicializar listas para guardar los resultados
  estimaciones_list <- list()
  contrastes.inviduales_list <- list()
  contrastes.globales_list <- list()
  graficas_list <- list()

  # Capturar mensajes de print
  sink(tempfile())
  on.exit(sink(), add = TRUE)

  # Crear un libro de Excel si se especifica descarga
  if (descarga) {
    if(is.null(archivo)){
      archivo = paste0(y, ".xlsx")
    }
    wb <- openxlsx::createWorkbook()
  }

  # Iterar sobre cada variable en x
  for (i in seq_along(x)) {
    var <- x[i]

    resultado <- transversal(data = data,y = y, x = var, digit = digit, plot = plot)

    # Guardar los resultados
    estimaciones_list[[var]] <- resultado$estimaciones
    if(length(unique(data[[var]])) != 2){
      contrastes.inviduales_list[[var]] <- resultado$contraste.individual
    }
    contrastes.globales_list[[var]] <- resultado$contraste.global

    if (descarga) {
      # Guardar los resultados en una nueva hoja del libro de Excel
      openxlsx::addWorksheet(wb, var)
      openxlsx::writeData(wb, sheet = var, resultado$estimaciones, startRow = 1, startCol = 1)
      openxlsx::writeData(wb, sheet = var, resultado$contraste.global, startRow = nrow(resultado$estimaciones) + 3, startCol = 1)
      if (length(unique(data[[var]])) != 2) {
        openxlsx::writeData(wb, sheet = var, resultado$contraste.individual, startRow = nrow(resultado$estimaciones) + 6, startCol = 1)
      }
      # Guardar la gráfica
      nombre.plot <- paste0(sub("\\.xlsx$", "", archivo), "_", i, ".pdf")
      if (!is.null(args$ancho) && !is.null(args$alto)) {
        ggsave(filename = nombre.plot, plot = resultado$p1, device = "pdf", width = args$ancho[i], height = args$alto[i])
      } else {
        ggsave(filename = nombre.plot, plot = resultado$p1, device = "pdf")
      }
    }
  }

  # Guardar el libro de Excel si se especifica descarga
  if (descarga) {
    openxlsx::saveWorkbook(wb, file = archivo, overwrite = TRUE)
  }

  # Obtener los resultados globales

  resultados_finales <- list(
    estimaciones = ajustarDecimalesFormatoLista(estimaciones_list, digito = digit, decimal.mark = ",") ,
    contrastes_individuales = ajustarDecimalesFormatoLista(contrastes.inviduales_list, digito = digit, decimal.mark = ","),
    contraste_global = contrastes.globales_list
  )

  sink()

  cat("Los siguientes resultados consideran todos los estudiantes para los que se cuenta con ponderador \n",
      "los resultados de estimación y prueba de hipótesis consideran pesos réplica y pesos de estudiantes \n",
      "los conteos de datos y de NA no son ponderados \n")
  print(resultados_finales)
  sink()
  cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1 \n")

  return(invisible(resultados_finales))
}
