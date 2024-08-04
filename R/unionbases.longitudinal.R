#' @title
#' Esta función une bases de ARISTAS de diferentes ediciones para poder ser analizado con la fucnión `longitudinal`.
#'
#' @param ediciones Vector que indica para cuáles ediciones se quiere unir las bases de datos.
#' @param datos Vector con el nombre de las bases que se quiere unir, con el mismo orden del parámetro `ediciones`.
#' @param y Nombre de la variable de interés `y`.
#' @param x (Opcional) Nombre de la variable de corte `x`.
#'
#' @return
#' `unionbases.longitudinal`: devuelve una base de datos uniendo las bases de datos indicados por el usuario, con una variable de nombre `edicion` indicando el año.
#'
#' @details
#'
#' Las variables (variable de interés 'y', la variable de corte 'x') de las bases de las diferentes ediciones deben:
#' (1) tener el mismo nombre y (2) tener los mismos valores.
#' Los pesos de estudiantes deben llamarse 'peso_MEst',  los pesos réplicas deben llamarse 'EST_W_REP_1’, 'EST_W_REP_2’, etc.
#' @export
#'
#' @examples
#'
#' # Convertir las variables numéricas a categóricas
#' ARISTAS18.matematicas$regiones <- as.character(ARISTAS18.matematicas$regiones)
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
#' ARISTAS18.22.matematicas <- unionbases.longitudinal(ediciones = c(2018, 2022), bases = c("ARISTAS18.matematicas2", "ARISTAS22.matematicas"), y = "theta_MAT_300_50", x = "regiones")
#' ARISTAS18.22.matematicas <- unionbases.longitudinal(ediciones = c(2018, 2022), bases = c("ARISTAS18.matematicas2", "ARISTAS22.matematicas"), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"))
#'

unionbases.longitudinal <- function(ediciones, bases, y, x = NULL){
  if(length(ediciones) != length(bases)){
    stop("El número de bases no coincide con el número de ediciones.")
  }
  # Inicializar una lista para almacenar las bases modificadas
  bases_modificadas <- list()

  # Iterar sobre las ediciones y bases
  for (i in seq_along(ediciones)) {
    # Cargar la base de datos correspondiente
    base_actual <- get(bases[i])

    # Seleccionar las columnas necesarias
    if (is.null(x)) {
      base_sub <- base_actual |>
        dplyr::select(all_of(y), peso_MEst, starts_with("EST_W_REP_")) |>
        dplyr::mutate(edicion = ediciones[i])
    } else {
      base_sub <- base_actual |>
        dplyr::select(all_of(c(y, x)), peso_MEst, starts_with("EST_W_REP_")) |>
        dplyr::mutate(edicion = ediciones[i])
    }

    # Añadir la base modificada a la lista
    bases_modificadas[[i]] <- base_sub
  }

  # Unir todas las bases modificadas en un solo data frame
  base_unida <- dplyr::bind_rows(bases_modificadas)

  # Reubicar la columna 'edicion'
  base_unida <- base_unida |>
    dplyr::relocate(edicion, .before = matches("^(peso_|EST_W_REP)"))

  return(base_unida)
}
