reorder_factor_last <- function(factor_var, level_to_last) {
  # Convertir a caracteres
  char_var <- as.character(factor_var)

  # Reemplazar NA con "NA" si el nivel es "NA"
  if (is.na(level_to_last)) {
    char_var[is.na(char_var)] <- "NA"
    level_to_last <- "NA"
  }

  # Obtener los niveles únicos actuales manteniendo el orden original
  original_levels <- levels(factor_var)

  # Si el nivel "NA" no está en los niveles originales, añadirlo al final
  if (!level_to_last %in% original_levels) {
    original_levels <- c(original_levels, level_to_last)
  }

  # Crear un vector de niveles con el nivel deseado al final
  ordered_levels <- c(setdiff(original_levels, level_to_last), level_to_last)

  # Convertir de nuevo a factor con el nivel deseado al final
  factor_var <- factor(char_var, levels = ordered_levels)

  return(factor_var)
}
