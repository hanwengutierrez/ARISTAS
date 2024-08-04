# Esta función conserva solo una cierta cantidad de cifras decimales
#'
#' @param data La base de datos que se quiere ajustar
#' @param digito La cantidad de cifras decimales que se quiere
#' @return
#' Esta función devuelve: la misma base de datos con la cantidad de cifras decimales ajustadas
#' @noRd
#'
ajustarDecimales <- function(data, digito = 3) {
  data_ajustada <- lapply(data, function(column) {
    if (is.numeric(column)) {
      return(round(column, digito))
    } else {
      return(column)
    }
  })
  # Convertir la lista resultante de nuevo en un dataframe
  data_ajustada <- as.data.frame(data_ajustada)
  colnames(data_ajustada) <- colnames(data)
  return(data_ajustada)
}
