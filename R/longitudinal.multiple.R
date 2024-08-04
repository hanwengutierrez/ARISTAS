#' @title
#' Esta función compara las diferentes ediciones de ARISTAS para más de una variable de corte
#'
#' @param datos La base de datos que se quiere analizar, debe tener una columna llamada anio indicando a cuál edición corresponde cada fila de la base.
#' @param y Vector de caracteres indicando el nombre de la variable de interés, ésta variable puede ser de tipo numérico o categórico.
#' @param ediciones Vector de caracteres o números indicando cuáles son las ediciones que desea comparar.
#' @param x Vector de caracteres indicando el nombre de las variables de corte.
#' @param digit Especificar el número de dígitos decimales, por defecto son 3 dígitos decimales.
#' @param plot Las mismas opciones que las de la funci´on `longitudinal`.
#' @param descarga Indicar si desea descargar los resultados numéricos y la gráfica, debe especificar la ubicación. Al usar la opción `descarga = TRUE` se descargará un archivo excel con los resultados numéricos y pdf con gráficas.
#' @param archivo Indicar el nombre del archivo excel que se descarga. Las gráficas se descargarán con el mismo nombre y extensión pdf en la misma ubicación que el archivo excel.
#' @param ancho El ancho en pulgadas de la gráfica, debe ser un vectro del mismo tamaño que el número de variables de corte.
#' @param alto El alto en pulgadas de la gráfica, debe ser un vectro del mismo tamaño que el número de variables de corte.
#'
#' @return
#' `longitudinal.multiple`: devuelve una lista de objetos correspondientes a estimacions y resultados de hipótesis de comparación:
#' * `estimaciones`: La estimación, error estándar, intervalo de confianza de 95% e información de los NA para la media de la variable de interés para cada valor de la variable de corte `x` y para cada edición
#' * `contrastes_individuales_entre_ediciones`: Los resultados de prueba de hipótesis para la igualdad de medias entre ediciones de la variable de interés para cada valor de cada variable de corte `x`.
#' * `estimaciones.globales`: Las estimaciones, errores estándares, intervalos de confianza de 95% e información de los NA para la media de la variable de interés para cada edición a nivel global.
#' * `contraste.global`: Los resultados de prueba de hipótesis para la igualdad de medidas de la variable de interés en diferentes ediciones.
#'
#' Adicionalmente, también la función arrojará la gráfica que usuario haya elegido para cada variable de corte `x`
#'
#' @details
#'
#' Esta función arroja los mismos resultados de la función `longitudinal` permitiendo que el usuario ingrese más de una variable de corte.
#'
#' @importFrom ggplot2 ggsave
#' @export
#'
#' @examples
#' # ------------------------------------------------------------------------------------------
#' # Unificar categorías de las variables
#' # ------------------------------------------------------------------------------------------
#' # Unificar las categorías de ARISTAS 2018 según las categorías de 2022
#' ARISTAS18.matematicas2 <- ARISTAS18.matematicas |>
#'   dplyr::mutate(regiones = dplyr::case_when(
#'     regiones == 1 ~ "SUR",
#'     regiones == 2 ~ "OESTE",
#'     regiones == 3 ~ "NORTE",
#'     regiones == 4 ~ "ESTE",
#'     regiones == 5 ~ "CENTRO",
#'     TRUE ~ NA_character_  # Para asegurarse de que otros valores no definidos se traten como NA
#'     )
#'   )
#'
#' # Unificar el nombre de la variable theta_MAT_300_50
#' ARISTAS18.matematicas2 <- ARISTAS18.matematicas2 |>
#'   dplyr::rename(theta_MAT_300_50 = theta_MAT_E300,
#'                 AlumnoGenero = AlumnoGenero15)
#'
#' # -------------------------------------------------------------------------------------------------
#' # Comparación de puntaje de matemáticas
#' # -------------------------------------------------------------------------------------------------
#'
#'  # Análisis de la variable theta_MAT_E300 por región y género y edición
#'  # Unir las bases de las dos ediciones a una sola base de datos
#' ARISTAS18.22.matematicas <- unionbases.longitudinal(ediciones = c(2018, 2022), bases = c("ARISTAS18.matematicas2", "ARISTAS22.matematicas"), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"))
#' longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "density")
#' longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "density", descarga = TRUE)
#' longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "density", descarga = TRUE, archivo = "theta_MAT_300_50.xlsx")
#' longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "density", descarga = TRUE, archivo = "theta_MAT_300_50.xlsx", ancho = c(10,4), alto = c(6,9))
#'
#' # -------------------------------------------------------------------------------------------------
#' # Comparación de nivel de desempeño de matemáticas
#' # -------------------------------------------------------------------------------------------------
#'
#'  # Análisis de la variable theta_MAT_E300 por región y género y edición
#'  # Unir las bases de las dos ediciones a una sola base de datos
#' ARISTAS18.22.matematicas <- unionbases.longitudinal(ediciones = c(2018, 2022), bases = c("ARISTAS18.matematicas2", "ARISTAS22.matematicas"), y = "Niveles_MAT", x = c("regiones", "AlumnoGenero"))
#' longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "Niveles_MAT", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "bar")
#' longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "Niveles_MAT", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "bar", descarga = TRUE, archivo = "Niveles_MAT.xlsx")
#' longitudinal.multiple(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "Niveles_MAT", x = c("regiones", "AlumnoGenero"), digit = 2, plot = "bar", descarga = TRUE, archivo = "Niveles_MAT.xlsx", ancho = c(10,4), alto = c(6,9))
#'

longitudinal.multiple <- function(datos, ediciones, y, x, digit = 3, plot = "NULL", descarga = FALSE, archivo = NULL, ...) {
  args <- list(...)
  options(warn = -1) # Eliminar los warning que produce ggplot

  # Inicializar listas para guardar los resultados
  estimaciones_list <- list()
  contrastes_list <- list()
  graficas_list <- list()


  # Capturar mensajes de print
  sink(tempfile())
  on.exit(sink(), add = TRUE)

  # Iterar sobre cada variable en x
  for (i in seq_along(x)) {
    var <- x[i]
    resultado <- longitudinal(datos = datos, ediciones = ediciones, y = y, x = var, digit = digit, plot = plot)

    # Guardar los resultados
    estimaciones_list[[var]] <- resultado$estimaciones
    contrastes_list[[var]] <- resultado$contrastes.invididuales
    graficas_list[[var]] <- resultado$p1
    # Guardar la gráfica si descarga es TRUE
    if (descarga) {
      if(is.null(archivo)){
        archivo = paste0(y, ".xlsx")
      }
      nombre.plot <- paste0(sub("\\.xlsx$", "", archivo), "_", i, ".pdf")
      if (!is.null(args$ancho) && !is.null(args$alto)) {
        ggsave(filename = nombre.plot, plot = resultado$p1, device = "pdf", width = args$ancho[i], height = args$alto[i])
      } else {
        ggsave(filename = nombre.plot, plot = resultado$p1, device = "pdf")
      }
    }
  }

  # Obtener los resultados globales
  resultado_global <- longitudinal(datos = datos, ediciones = ediciones, y = y, x = NULL, digit = digit, plot = plot)

  resultados_finales <- list(
    estimaciones = ajustarDecimalesFormatoLista(estimaciones_list, digito = digit, decimal.mark = ",") ,
    contrastes_individuales_entre_ediciones = ajustarDecimalesFormatoLista(contrastes_list, digito = digit, decimal.mark = ","),
    estimaciones_globales = format(ajustarDecimales(resultado_global$estimaciones.globales, digito = digit), decimal.mark = ","),
    contraste_global = resultado_global$contraste.global)

  # Guardar resultados en Excel si se solicita
  if (descarga) {
    if(is.null(archivo)){
      archivo = paste0(y, ".xlsx")
    }
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Resultados")

    # Escribir las estimaciones
    row_counter <- 1
    for (var in names(resultados_finales$estimaciones)) {
      openxlsx::writeData(wb, sheet = "Resultados", x = resultados_finales$estimaciones[[var]], startRow = row_counter, startCol = 1)
      row_counter <- row_counter + nrow(resultados_finales$estimaciones[[var]]) + 2
    }

    # Escribir los contrastes individuales
    for (var in names(resultados_finales$contrastes_individuales_entre_ediciones)) {
      openxlsx::writeData(wb, sheet = "Resultados", x = resultados_finales$contrastes_individuales_entre_ediciones[[var]], startRow = row_counter, startCol = 1)
      row_counter <- row_counter + nrow(resultados_finales$contrastes_individuales_entre_ediciones[[var]]) + 2
    }

    # Escribir las estimaciones globales
    openxlsx::writeData(wb, sheet = "Resultados", x = resultados_finales$estimaciones_globales, startRow = row_counter, startCol = 1)
    row_counter <- row_counter + nrow(resultados_finales$estimaciones_globales) + 2

    # Escribir el contraste global
    openxlsx::writeData(wb, sheet = "Resultados", x = resultados_finales$contraste_global, startRow = row_counter, startCol = 1)

    # Guardar el archivo de Excel
    openxlsx::saveWorkbook(wb, file = archivo, overwrite = TRUE)
  }

  sink()

  cat("Los siguientes resultados consideran todos los estudiantes para los que se cuenta con ponderador \n",
                     "los resultados de estimación y prueba de hipótesis consideran pesos réplica y pesos de estudiantes \n",
                     "los conteos de datos y de NA no son ponderados \n")
  print(resultados_finales)
  sink()
  cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1 \n")

  return(invisible(resultados_finales))
}
