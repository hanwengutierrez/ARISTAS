# Función para aplicar ajustarDecimales y format a una lista de data frames
ajustarDecimalesFormatoLista <- function(lista_dataframes, digito = 3, decimal.mark = ",") {
  lista_formateada <- lapply(lista_dataframes, function(data) {
    data_ajustada <- ajustarDecimales(data, digito)
    data_formateada <- data_ajustada |>
      dplyr::mutate(across(where(is.numeric), ~format(.x, decimal.mark = decimal.mark)))
    return(data_formateada)
  })
  return(lista_formateada)
}
