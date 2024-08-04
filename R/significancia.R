# Esta función devuelve las marcas según la magnitud de un valor p
#' Title
#'
#' @param p_values El valor p
#' @return
#' Esta función devuelve: *** si el valor p es menor a 0.001, ** si está entre 0.001 y 0.01, * si está entre 0.01 y 0.05, + si está entre 0.05 y 0.1
#' @noRd
significancia <- function(p_values) {
  significados <- character(length(p_values))
  significados[p_values < 0.001] <- "***"
  significados[p_values < 0.01 & p_values >= 0.001] <- "**"
  significados[p_values < 0.05 & p_values >= 0.01] <- "*"
  significados[p_values < 0.1 & p_values >= 0.05] <- "+"
  significados[p_values >= 0.1] <- ""
  return(significados)
}
