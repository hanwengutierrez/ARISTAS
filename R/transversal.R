#' @title
#' Esta función calcula las estimaciones para una edición específica de ARISTAS
#'
#' @param data La base de datos que se quiere analizar, puede ser cualquiera de las bases de ARISTAS incluidas en el paquete o cualquiera otra base ARISTAS que no sea de uso público. Tenga en cuenta que las columnas que contengan los pesos replicados deben tener nombres: EST_W_REP_1, EST_W_REP_2, etc.
#' @param y El nombre de la variable de interés, ésta variable puede ser de tipo numérico o categórico.
#' @param x (Opcional) Nombre de la variable numérica o categórica que define los subgrupos para la comparación.
#' @param limits Vector de caracteres o vector numérico. En el caso de una variable numérica `x`, se debe proveer los valores límites para definir los subgrupos.
#' Por ejemplo, si `x` es la variable edad de los estudiantes, y se proveen los valores límites de 15 y 20, entonces se crearán tres subgrupos (intervalos): \eqn{(-\infty, 15]}, \eqn{(15, 20]} y \eqn{(20, \infty)}.
#' En el caso que `x` sea una variable categórica, los valores límites son opcionales.
#' Por ejemplo, si `x` es la variable región (con 5 valores para las 5 regiones de Uruguay), y se provee los valores 1 y 3, entonces la función creará tres subgrupos: `region==1`, `region==3` y la unión de las demás regiones.
#' Si no se provee valores límites para una variable `x` categórica, entonces la función comparará la variable de interés `y` en todas las categorías de `x`.
#' @param digit Especificar el número de dígitos decimales, por defecto son 3 dígitos decimales.
#' @param IC Indicar si se muestran las estimaciones e intervalos de confianza sobre la gráfica.
#' @param plot Puede elegir entre `histogram`, `density`, `violin`, `Lollipop`  para una variable numérica, la opción `violin` incluirá también la gráfica de cajas. Para una variable categórica, puede usar la opción `bar`.
#' @param descarga Indicar si desea descargar los resultados numéricos y la gráfica, debe especificar la ubicación. Al usar la opción `descarga = TRUE` se descargará un archivo excel con los resultados numéricos y un pdf con la gráfica.
#' @param archivo Indicar el nombre del archivo excel que se descarga. La gráfica, si hay, se descargará con el mismo nombre y extensión pdf en la misma ubicación que el archivo excel.
#' @param ancho El ancho en pulgadas de la gráfica.
#' @param alto El alto en pulgadas de la gráfica.
#'
#' @return
#' `transversal` devuelve una lista de objetos correspondientes a estimacions y resultados de hipótesis de comparación:
#' * `estimaciones`: La estimación, error estándar, intervalo de confianza de 95% e información de los NA para la media de la variable de interés para cada valor de la variable de corte `x` y también a nivel global (para todos los datos ingresados en el argumento `data`)
#' * `contraste.individual`: Resultados de la prueba de hipótesis para la igualdad de medidas para cada combinación de 2 categorías de la variable de corte (de las categorías especificados por el usuario en el argumento `limits`). Estos resultas solo se muestran cuando el usuario no especifica la variable de corte.
#' * `contraste.global`: Resultados de la prueba de hipótesis para la comparación múltiple de la media de `y` para todas las categorías especificados por el usuario en el argumento `limits`. Estos resultas solo se muestran cuando el usuario no especifica la variable de corte.
#'
#'  Adicionalmente, también la función arrojará la gráfica que usuario haya elegido.
#'
#' @details
#' Cuando la variable de interés `y` es una variable numérica (puntaje en pruebas), y el usuario ingresa la varible de corte `x` con o sin valores en el argumento `limits`,
#' la función realizará la prueba \eqn{t} para cada pareja de categorías de `x` con la función `survey::svyttest` con base en un diseño muestral de pesos réplica definido con `survey::svrepdesign`,
#' y estos resultados se encuentran en el resultado `contraste.individual`. La salida de la función incluye la estimación, error estándar, intervalo de confianza de 95% para la media de la variable `y` para cada categoría de `x` y a nivel global
#' También calcula el número de datos, número y porcentaje de NA para cada categoría de `x` y a nivel global
#' De la misma forma, la función llevará a cabo la prueba global \eqn{F} de igualdad de medias  para todas las categorías de la variable `x`.
#'
#' Cuando la variable de interés `y` es una variable numérica (puntaje en pruebas), y el usuario no ingresa la varible de corte `x`,
#' la función solo calculará la estimación, error estándar, intervalo de confianza de 95%, cantidad de datos, cantidad y porcentaje de NA para la media de la variable `y` para todos los datos ingresados en `data`.
#'
#' Cuando la variable de interés `y` es una variable categórica (nivel de desempeño), y el usuario ingresa la varible de corte `x` con o sin valores en el argumento `limits`,
#' la función realizará la prueba chi-cuadrado para cada pareja de categorías de `x` con la función `survey::svytable` para la independencia entre los diferentes niveles de desempeño y las dos categorías de la variable `x`,
#' lo anterior con base en un diseño muestral de pesos réplica definido con `survey::svrepdesign`,
#' y estos resultados se encuentran en `contraste.individual`.
#' También calcula el número de datos, número y porcentaje de NA para cada categoría de `x` y a nivel global
#' De la misma forma, la función llevará a cabo la prueba global de independencia de chi-cuadrado entre los diferentes niveles de desempeño y todas las categorías de la variable `x`.
#' Cuando el usuario no especifica la variable de corte `x`, el resultado `contraste.individual` no está en la salida de la función.
#'
#' Cuando la variable de interés `y` es una variable categórica (nivel de desempeño), y el usuario no ingresa la varible de corte `x`,
#' la función solo calculará la estimación, error estándar e intervalo de confianza de 95% para la variable `y` para todos los datos ingresados en `data`.
#'
#' La opción de `plot` para una variable de interés `y` numérica admite `histogram`  (el cual hace uso de la función `survey::svyhist`), `density`, `violin` y `Lollipop`, mientras que
#' para una variable de interés `y` categórica admite la opción de `bar`.
#'
#' Las estimaciones se obtuvieron utilizando la función `svymean` del paquete `survey` apoyado en un diseño muestral de pesos réplica creado con `svrepdesign`
#'
#' @import ggplot2
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats complete.cases
#' @importFrom stats confint
#' @importFrom stats gaussian
#' @importFrom stats lm
#' @importFrom stats na.omit
#' @importFrom stats setNames
#' @importFrom stats var
#' @importFrom utils combn
#' @importFrom utils tail
#' @export
#'
#' @examples
#'# ------------------------------------------------------------------------------------------------------
#'# Variable de interés numérica y con variable de corte corte x categórica (regiones y AlumnoGenero15)
#'# ------------------------------------------------------------------------------------------------------
#'
#' # Análisis de la variable theta_MAT_E300 con la gráfica de histograma
#' transversal(data = ARISTAS18.matematicas, y = "theta_MAT_E300", plot = "histogram")
#' transversal(data = ARISTAS18.matematicas, y = "theta_MAT_E300", x = "AlumnoGenero15", plot = "histogram")
#' transversal(data = ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", digit = 3, plot = "histogram")
#' transversal(data = ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", digit = 3, plot = NULL, descarga = TRUE)
#' transversal(data = ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", digit = 3, plot = NULL, descarga = TRUE, archivo = "theta_MAT_regiones.xlsx")
#' transversal(data = ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", digit = 4, plot = "histogram", descarga = TRUE, archivo = "theta_MAT_regiones.xlsx", ancho = 6, alto = 8)
#'
#' # Análisis de la variable theta_MAT_E300 con la gráfica de densidad
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", plot = "density")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", IC = FALSE, plot = "density")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "AlumnoGenero15", plot = "density")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "AlumnoGenero15", IC = FALSE, plot = "density")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", IC = TRUE, plot = NULL, descarga = TRUE, archivo = "theta_MAT_E300.xlsx")
#' # Al no especificar la dimensión de la gráfica, el pdf guardará la gráfica con las mismas dimensiones que aparece en el panel de Plots
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", IC = FALSE, plot = "density", descarga = TRUE, archivo = "theta_MAT_E300.xlsx")
#'
#' # Análisis de la variable theta_MAT_E300 con la gráfica de violin
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", plot = "violin")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", IC = FALSE, plot = "violin")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "AlumnoGenero15", IC = TRUE, plot = "violin")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "AlumnoGenero15", IC = FALSE, plot = "violin")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", IC = TRUE, plot = "violin")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", IC = FALSE, plot = "violin", descarga = TRUE, archivo = "theta_MAT_E300.xlsx")
#'
#' # Análisis de la variable theta_MAT_E300 con la gráfica de Lollipop
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", IC = TRUE, plot = "Lollipop")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", IC = FALSE, plot = "Lollipop")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "AlumnoGenero15", IC = TRUE, plot = "Lollipop", descarga = TRUE, archivo = "theta_MAT_E300.xlsx", ancho = 6, alto = 8)
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "AlumnoGenero15", IC = FALSE, plot = "Lollipop")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", IC = TRUE, plot = "Lollipop")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", IC = FALSE, plot = "Lollipop")
#'
#' # ---------------------------------------------------------------------------------------
#' # Variable de interés numérica y con variable de corte corte corte x numérica (EDAD)
#' # ---------------------------------------------------------------------------------------
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "EDAD", limits = c(15), IC = TRUE, plot = "histogram")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "EDAD", limits = c(15, 20, 22), IC = TRUE, plot = "density")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "EDAD", limits = c(15, 20, 22), IC = TRUE, plot = "violin")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "EDAD", limits = c(15, 20, 22), IC = TRUE, plot = "Lollipop")
#'
#' # --------------------------------------------------------------------------------------------------------------
#' # Variable de interés numérica y con varaible de corte x categórica (regiones) con categorías especificadas
#' # --------------------------------------------------------------------------------------------------------------
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", limits = c("2"), IC = TRUE, plot = "histogram")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", limits = c("2"), IC = TRUE, plot = "density")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", limits = c("2", "4"), IC = TRUE, plot = "violin")
#' transversal(data=ARISTAS18.matematicas, y = "theta_MAT_E300", x = "regiones", limits = c("2", "4"), IC = TRUE, plot = "Lollipop")
#'
#' # ----------------------------------------------------------------------
#' # Variable de interés y categórica con variable de corte x categórica
#' # ----------------------------------------------------------------------
#'
#' # Gráfica de barras
#' transversal(data=ARISTAS18.matematicas, y = "Niveles_MAT", x = "AlumnoGenero15", plot = "bar", descarga = TRUE, archivo = "Niveles_MAT.xlsx")
#' transversal(data=ARISTAS18.matematicas, y = "Niveles_MAT", x = "regiones", plot  = "bar")
#' transversal(data=ARISTAS18.matematicas, y = "Niveles_MAT", x = "regiones", limits =  c("1", "5"), plot  = "bar")
#' transversal(data=ARISTAS18.matematicas, y = "Niveles_MAT", plot  = "bar")


transversal <- function(data, y, x = NULL, limits = NULL, digit = 3, IC = TRUE, plot = NULL, descarga = FALSE, archivo = NULL, ...){
  args <- list(...)

  options(warn = -1) # Eliminar los warning que produce ggplot

  #---------------------------------------------------------------------------
  # Funciones auxiliares
  #---------------------------------------------------------------------------
  is_numerical <- function(variable) {
    is.numeric(variable) || is.integer(variable) || is.double(variable)
  }
  is_categorical <- function(variable) {
    is.factor(variable) || is.character(variable)
  }

  #---------------------------------------------------------------------------
  # Eliminar estudiantes con NA en los pesos muestrales de los estudiantes
  #---------------------------------------------------------------------------
  datos <- data |>
    dplyr::filter(!is.na(peso_MEst))
  datos$var.y <- datos[, which(names(datos) == y)]

    if(is_categorical(datos[[y]]) == TRUE){
    datos <- datos |>
      dplyr::mutate(var.y = dplyr::na_if(var.y, ""))
    }

  #---------------------------------------------------------------------------
  # Si el usuario ingresa variable de corte x, crear los grupos según los valores límite
  #---------------------------------------------------------------------------
  if(!is.null(x)){
    # Eliminar estudiantes con NA en la variable de corte x
    datos$var.x <- datos[, which(names(datos) == x)]
    datos <- datos |>
      dplyr::mutate(var.x = ifelse(var.x == "" | is.na(var.x), "NA", var.x))
    #---------------------------------------------------------------------------
    # Crear grupos cuando la variable de corte es numérica
    #---------------------------------------------------------------------------
    if(is_numerical(datos[[x]])){
      if(is.null(limits)){
        stop("La variable de corte que ingresaste es numérica, debes ingresar los valores límites en el argumento 'limits' para definir los grupos de comparación.")
      }
      if(!is.null(limits)){
        # Crear las categorías para la variable numérica de corte x
        # Crear las etiquetas para los intervalos
        etiquetas <- c(paste("(-Inf,", limits[1], "]"))
        if(length(limits) > 1){
          for (i in 2:length(limits)) {
            etiquetas <- c(etiquetas, paste("(", limits[i-1], ",", limits[i], "]"))
          }
        }
        etiquetas <- c(etiquetas, paste("(", tail(limits, 1), ", Inf)"))

        # Usar cut para clasificar los datos y mutate para crear una nueva columna
        datos <- datos |> dplyr::mutate(var.x1 = cut(datos[[x]],
                                               breaks = c(-Inf, limits, Inf),
                                               labels = etiquetas,
                                               right = FALSE),
                                        var.x1 = factor(var.x1, levels = etiquetas, ordered = TRUE))
        ind = which(datos$var.x == "NA")
        datos$var.x1[ind] = NA
        # datos$var.x1 <- as.character(datos$var.x1)
        # datos$var.x1[is.na(datos$var.x1)] <- "NA"
        # # Convertir la variable a factor nuevamente con "NA" como el último nivel
        datos$var.x1 <- reorder_factor_last(datos$var.x1, NA)
      }
    }
    #-----------------------------------------------------------------------------------
    # Crear grupos cuando la variable de corte es categórica y hay valores en limits
    #-----------------------------------------------------------------------------------
    if(is_categorical(datos[[x]]) == TRUE){
      if(!is.null(limits)){
        if(any(!limits %in% unique(datos[[x]]))){  # Si algunos de los valores de limits NO son valores de x
          stop("Algunos de los valores límites ingresados no corresponden a valores de la variable de corte x.")
        }
        if(all(limits %in% unique(datos[[x]]))){ # Si todos los valores de limits son valores de x
          datos <- datos |>
            dplyr::mutate(var.x1 = dplyr::case_when(
              var.x %in% limits ~ as.character(var.x),
              TRUE ~ "Demás")
            ) |>
            dplyr::mutate(var.x1 = factor(var.x1))
        }
      }
      if(is.null(limits)){
        datos <- datos |>
          dplyr::mutate(var.x1 = factor(var.x))
      }
    }
  }

  #---------------------------------------------------------------------------
  # Especificar el diseño muestral
  #---------------------------------------------------------------------------
  pesos <- grep("^EST_W_REP", names(datos), value = TRUE)
  dis = survey::svrepdesign(data=datos, type="Fay", weights=~peso_MEst,
                    repweights=paste0("EST_W_REP_[", "1-", length(pesos), "]"),
                    combined.weights=TRUE, rho = 1.5, mse = F)
  total <- survey::svymean(~var.y, dis, na.rm = T) # estimación nacional

  #---------------------------------------------------------------------------
  # Estimación de puntaje para diferentes subgrupos definidos por x
  #---------------------------------------------------------------------------
  # Si hay variable x
  if(!is.null(x)){
    grupo <- survey::svyby(~var.y, ~var.x1, dis, svymean, na.rm = T)

    # Si y es numérica
    if(is_numerical(datos[[y]]) == TRUE){
      grupo <- cbind(grupo, confint(grupo)) # estimación por grupo
      grupo <- grupo |> dplyr::mutate(var.x1 = as.character(var.x1))
      # Calcular el número y el porcentaje de NA por grupo y a nivel nacional
      grupo <- grupo |>
        dplyr::left_join(datos |>
                    dplyr::group_by(var.x1) |>
                    dplyr::summarise( n = dplyr::n(),
                                      n.NA = sum(is.na(var.y)),
                                      `%.NA` = 100*n.NA / dplyr::n()),
                  by = "var.x1")

      #---------------------------------------------------------------------------
      # Unir las estimaciones por grupos con la estimación nacional
      #---------------------------------------------------------------------------
      estimates = rbind(grupo, setNames(data.frame("Global", 0, 0, 0, 0, 0, 0,0),
                                        c("var.x1", "var.y", "se", "2.5 %", "97.5 %","n", "n.NA", "%.NA")))
      estimates[nrow(estimates),2] <- coef(total)
      estimates[nrow(estimates),3] <- survey::SE(total)
      estimates[nrow(estimates),4:5] <- confint(total)
      estimates[nrow(estimates),6] <- nrow(datos)
      estimates[nrow(estimates),7] <- sum(is.na(datos[[y]]))
      estimates[nrow(estimates),8] <- 100 * estimates$n.NA[nrow(estimates)] / estimates$n[nrow(estimates)]
      estimates$var.x1 <- as.factor(estimates$var.x1)
    }
    # Si y es categórica
    if(is_categorical(datos[[y]]) == TRUE){
      colnames(grupo)[2:length(unique(datos[[y]]))] <- gsub("var.y", "", colnames(grupo)[2:length(unique(datos[[y]]))])
      estimaciones1.aux <- grupo[1:(length(unique(na.omit(datos$var.y)))+1)]
      estimaciones <- estimaciones1.aux |> tidyr::pivot_longer(!var.x1, names_to = "y", values_to = "mean")
      estimaciones2.aux <- grupo[-(2:(length(unique(na.omit(datos$var.y)))+1))]
      colnames(estimaciones2.aux) <- colnames(estimaciones1.aux)
      se <- estimaciones2.aux |> tidyr::pivot_longer(!var.x1, names_to = "y", values_to = "se")
      ######
      a <- as.data.frame(confint(grupo))
      a$var <- rownames(a)
      columnas_separadas <- as.data.frame(do.call("rbind", strsplit(as.character(a$var), ":", fixed = TRUE)))
      names(columnas_separadas) <- c(x, "var.y")
      columnas_separadas[,2] <- gsub("var.y", "",columnas_separadas[,2])
      # c(x, paste0("var.y", sort(unique(datos$var.y))))
      a <- cbind(columnas_separadas, a)
      rownames(a) <- NULL
      a <- a |>
        dplyr::select(-var)
      resultados <- estimaciones |>
        dplyr::left_join(se, by = c("var.x1" = "var.x1", "y" = "y")) |>
        dplyr::left_join(a, by = c("var.x1" = x, "y" = "var.y"))
      # Pegar el número de n, NA y porcentaje de NA
      resultados <- resultados |> left_join(datos |>
                                      dplyr::group_by(var.x1) |>
                                      dplyr::summarise( n = dplyr::n(),
                                                        n.NA = sum(is.na(var.y)),
                                                        `%.NA` = 100*n.NA / dplyr::n()),
                                    by = "var.x1")


      #---------------------------------------------------------------------------
      # Unir las estimaciones por grupos con la estimación nacional
      #---------------------------------------------------------------------------

      resultados.nacional <-  setNames(data.frame("Global", row.names(as.data.frame(total)), coef(total), survey::SE(total),
                                                  confint(total)[,1], confint(total)[,2], nrow(datos), sum(is.na(datos$var.y)), 100*sum(is.na(datos$var.y))/nrow(datos)),
                                       names(resultados))

      rownames(resultados.nacional) <- NULL
      resultados.nacional$y <- gsub("var.y", "", resultados.nacional$y)
      estimates <- rbind(resultados, resultados.nacional)
    }
 }

  if(is.null(x)){
    total2 <- as.data.frame(total) # estimación nacional
    total2$y <- rownames(total2)
    estimates <- cbind(data.frame("Global"), total2, confint(total))
    estimates <- estimates |>
      dplyr::mutate(y = rownames(estimates),
             y = stringr::str_replace(y, "var.y", "")) |>
      dplyr::relocate(y) |>
      dplyr::rename(var.y = mean)

    estimates$n <- nrow(datos)
    estimates$n.NA <- sum(is.na(datos$var.y))
    estimates$`%.NA` <- 100 * estimates$n.NA[nrow(estimates)] / estimates$n[nrow(estimates)]

  }

  #---------------------------------------------------------------------------
  # Título y subtítulo de las gráficas (contiene la estimación global)
  #---------------------------------------------------------------------------
  titulo = paste0("Datos muestrales de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} )
  if(is_categorical(datos[[y]]) == TRUE){
      subtitulo = NULL
    }
  if(is_numerical(datos[[y]]) == TRUE){
     subtitulo = paste0("Estimación global: ", format(round(estimates$var.y[nrow(estimates)], 1), decimal.mark = ","), ", IC: (",
                        format(round(estimates$`2.5 %`[nrow(estimates)], 1), decimal.mark = ","), ", ",
                        format(round(estimates$`97.5 %`[nrow(estimates)], 1),decimal.mark = ","), ")")
     }

  #---------------------------------------------------------------------------
  # Gráficas
  #---------------------------------------------------------------------------
  if(!is.null(plot)){
    if(!is.null(x)){
      datos.sinNAx <- datos |> dplyr::filter(!is.na(var.x))
      datos.sinNAx$var.x1 <- droplevels(datos.sinNAx$var.x1)
      #---------------------------------------------------------------------------
      # Histograma para y numérica
      #---------------------------------------------------------------------------
      if(plot == "histogram" && is_numerical(datos[[y]]) == TRUE){
        df = NULL
        # Histograma para cada valor de la variable x
        for(k in 1:length(unique(datos.sinNAx$var.x1))){
          dis1 <- survey::svrepdesign(data=datos.sinNAx |> dplyr::filter(var.x1 == unique(datos.sinNAx$var.x1)[k]),
                              type="Fay", weights=~peso_MEst,
                              repweights=paste0("EST_W_REP_[", "1-", length(pesos), "]"),
                              combined.weights=TRUE, rho = 1.5, mse = F)

          histograma <- survey::svyhist(~var.y, dis1, plot = FALSE)
          mids <- histograma$mids
          density <- histograma$density
          df <- rbind(df, data.frame(mids, density, var.x = unique(datos.sinNAx$var.x1)[k]))
        }
        # Histograma para cada valor de la variable x a nivel nacional
        histograma <- survey::svyhist(~var.y, dis)
        mids <- histograma$mids
        density <- histograma$density
        df <- rbind(df, data.frame(mids, density, var.x = "global"))

        # Paso 2: Crear el histograma con ggplot2
        p1 <- ggplot2::ggplot(df, aes(x = mids, y = density)) +
          geom_col(width = 50, fill = "#619CFF") +  # Usar un ancho de barra estimado basado en los breaks
          facet_wrap(vars(var.x)) +
          labs(title = paste0("Distribución de ", y, if(!is.null(x)){paste0(" según ", x)} else{""}),
               x = y, y = "Densidad") +
          theme_minimal()
      }
      #---------------------------------------------------------------------------
      # Densidad para y numérica
      #---------------------------------------------------------------------------
      if(plot == "density"  && is_numerical(datos[[y]]) == TRUE){
        p1 <- ggplot2::ggplot(datos.sinNAx, aes(x = var.y, y = var.x1, fill = var.x1)) +
          ggridges::geom_density_ridges(alpha = 1/2) +
          geom_density(data = datos.sinNAx, aes(x = var.y, y = ..scaled..),
                       inherit.aes = FALSE, fill = "grey", alpha = 1/2, adjust = 1) +
          annotate("text", x = min(datos.sinNAx$var.y,na.rm = TRUE), y = 0, label = "Global", hjust = 1) +
          scale_y_discrete(expand = expansion(add = c(1.2, 0.2))) +
          scale_x_discrete(expand = expansion(mult = c(0.08, 0))) +
          ggridges::theme_ridges() +
          theme(legend.position = "none",
                axis.text.y = element_text(margin = margin(r = -30))) +
          labs(x = "",
               y = x,
               title = titulo,
               subtitle = subtitulo,
               caption = "En esta gráfica no se tienen en cuenta los pesos muestrales.")
        if(IC == TRUE){
          p1 <- p1 +
            geom_text(data = estimates[-nrow(estimates),], aes(x = 520, label = paste("Estimación: ", format(round(estimates[-nrow(estimates),2], 1), decimal.mark = ","))), nudge_y = 0.5) +
            geom_text(data = estimates[-nrow(estimates),], aes(x = 520, label = paste("IC: (", format(round(estimates[-nrow(estimates),4], 1), decimal.mark = ","), ", ", format(round(estimates[-nrow(estimates),5], 1), decimal.mark = ","), ")")), nudge_y = 0.3) +
            geom_text(data = estimates[nrow(estimates),], aes(x = 520, y = 0, label = paste("Estimación: ", format(round(estimates[nrow(estimates),2], 1), decimal.mark = ","))), nudge_y = 0.5) +
            geom_text(data = estimates[nrow(estimates),], aes(x = 520, y=0, label = paste("IC: (", format(round(estimates[nrow(estimates),4], 1), decimal.mark = ","), ", ", format(round(estimates[nrow(estimates),5], 1), decimal.mark = ","), ")")), nudge_y = 0.3)
        }
      }
      #---------------------------------------------------------------------------
      # Violin para y numérica
      #---------------------------------------------------------------------------
      if(plot == "violin"  && is_numerical(datos[[y]]) == TRUE){
        p1 <- ggplot2::ggplot(datos.sinNAx, aes(y = var.y, x = var.x1, fill = var.x1), alpha = 1/2) +
          geom_violin(alpha = 1/2) +
          geom_boxplot(width=0.1) +
          geom_violin(data = datos.sinNAx, aes(x = factor("Global"), y = var.y),
                      fill = "grey", alpha = 0.5, adjust = 1, width = 0.5) +
          geom_boxplot(data = datos.sinNAx, aes(x = factor("Global"), y = var.y),
                       width = 0.1, fill = NA) +  # Sin relleno para evitar duplicado
          theme_minimal() +
          theme(legend.position = "none") +
          labs(x = x) +
          labs(y = "",
               title = titulo,
               subtitle = subtitulo,
               caption = "En esta gráfica no se tienen en cuenta los pesos muestrales.")
        if(IC == TRUE){
          p1 <- p1 +
            labs(subtitle = NULL) +
            ylim(0, 650) +
            geom_text(data = estimates[-nrow(estimates),], aes(y = 630, label = paste("Estimación: ", format(round(estimates[-nrow(estimates),2], 1), decimal.mark = ",")))) +
            geom_text(data = estimates[-nrow(estimates),], aes(y = 600, label = paste("IC: (", format(round(estimates[-nrow(estimates),4], 1), decimal.mark = ","), ", ", format(round(estimates[-nrow(estimates),5], 1), decimal.mark = ","), ")")))+
            geom_text(data = estimates[nrow(estimates),], aes(x = factor("Global"), y = 630, label = paste("Estimación: ", format(round(estimates[nrow(estimates), 2], 1), decimal.mark = ",")))) +
            geom_text(data = estimates[nrow(estimates),], aes(x = factor("Global"), y = 600, label = paste("IC: (", format(round(estimates[nrow(estimates), 4], 1), decimal.mark = ","), ", ", format(round(estimates[nrow(estimates), 5], 1), decimal.mark = ","), ")")))
        }
      }
      #---------------------------------------------------------------------------
      # Barras apiladas para y categórica
      #---------------------------------------------------------------------------
      colors <- RColorBrewer::brewer.pal("Spectral", n = max(RColorBrewer::brewer.pal.info["Spectral", "maxcolors"]))
      color_pal <- grDevices::colorRampPalette(colors)
      n_categories <- length(unique(estimates$y))
      extended_colors <- color_pal(n_categories)
      rev_colors <- rev(extended_colors)
      if(plot == "bar" && is_categorical(datos[[y]]) == TRUE){
        ajuste.x <- rep(1:length(unique(estimates$y)), length(unique(estimates$var.x1)))
        estimates.bar <- estimates |>
          dplyr::mutate(mean = 100 * mean,
                 se = 100 * se,
                 `2.5 %` = 100 * `2.5 %`,
                 `97.5 %` = 100 * `97.5 %`,
                 y = forcats::fct_rev(as.factor(y)),
                 ajuste = dplyr::case_when(
                   mean < 3 & y == unique(y)[1] ~ -6,
                   mean < 3 & y == unique(y)[length(unique(y))] ~ 6,
                   TRUE ~ 0
                 )
          )
        estimates.bar$var.x1 <- as.factor(estimates.bar$var.x1)
        # Reordenar los niveles del factor para que "Global" sea el último nivel
        estimates.bar$var.x1 <- factor(estimates.bar$var.x1, levels = c(setdiff(levels(estimates.bar$var.x1), "Global"), "Global"))


        p1 <- ggplot2::ggplot(estimates.bar, aes(x = var.x1, y = mean, fill = y)) +
          geom_col() +
          geom_text(aes(x = var.x1, y = mean + ajuste , label = format(round(mean, 1), decimal.mark = ",")),
                    position = position_stack(vjust = 0.5)) +
          scale_fill_manual(values = rev_colors) +
          guides(fill = guide_legend(reverse = TRUE, nrow =1)) +
          theme_minimal() +
          theme(legend.position = "top", legend.title = element_blank()) +
          xlab("") +
          ylab("") +
          labs(title = paste0("Estimación de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} )) +
          coord_flip()
      }
      #---------------------------------------------------------------------------
      # Lollipop para y numérica
      #---------------------------------------------------------------------------
      if(plot == "Lollipop" && is_numerical(datos[[y]]) == TRUE){
        estimates.Lolli <- estimates |>
          dplyr::mutate(var.x1.numeric = c(1:nrow(estimates)))
        margen <- 0.5
        p1 <- ggplot2::ggplot(estimates.Lolli) +
          geom_segment(aes(x = var.x1.numeric - margen,
                           xend = var.x1.numeric + margen,
                           y = var.y, yend = var.y, col = var.x1)) +
          geom_text(aes(x = var.x1.numeric, y = var.y, label = paste(format(round(var.y, 1), decimal.mark = ","),
                                                                     sep="")),
                    nudge_y = 0.9) +
          labs(x = x,
               y = y,
               title = paste0("Estimación de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} ),
               shape = x, color = y) +
          theme_minimal() +
          theme(strip.text = element_text(face = "bold", size = 12),
                legend.position = "none",
                axis.text.x = element_blank()) +
          ylim(min(estimates$`2.5 %`)-2, max(estimates$`97.5 %`)) +
          geom_text(aes(x = var.x1.numeric, y = min(estimates$`2.5 %`)-2, label = var.x1))
        if(IC == TRUE){
          p1 <- p1  +
            geom_segment(aes(x = var.x1.numeric - margen,
                             xend = var.x1.numeric + margen,
                             y = var.y, yend = var.y)) +
            geom_rect(aes(xmin = var.x1.numeric - margen,
                          xmax = var.x1.numeric + margen,
                          ymin = `2.5 %`, ymax= `97.5 %`,
                          fill = as.character(var.x1.numeric)), alpha = 1/2)
        }
      }

      colnames(estimates)[1] <- x
      colnames(estimates)[2] <- y
    }
    if(is.null(x)){
      #---------------------------------------------------------------------------
      # Gráfica si no hay variable de x
      #---------------------------------------------------------------------------
      #---------------------------------------------------------------------------
      # Histograma
      #---------------------------------------------------------------------------
      if(plot == "histogram"  && is_numerical(datos[[y]]) == TRUE){
        histograma <- survey::svyhist(~var.y, dis)
        mids <- histograma$mids
        density <- histograma$density
        df <- data.frame(mids, density)

        p1 <- ggplot2::ggplot(df, aes(x = mids, y = density)) +
          geom_col(width = 50, fill = "#619CFF") +  # Usar un ancho de barra estimado basado en los breaks
          labs(title = paste0("Distribución de ", y, if(!is.null(x)){paste0(" según ", x)} else{""}),
               x = y, y = "Densidad") +
          theme_minimal()

        if(IC == TRUE){
          p1 <- p1 +
            labs(subtitle = subtitulo)
        }
      }
      #---------------------------------------------------------------------------
      # Densidad
      #---------------------------------------------------------------------------
      if(plot == "density"  && is_numerical(datos[[y]]) == TRUE){
        p1 <- ggplot2::ggplot(datos, aes(x = var.y)) +
          geom_density(alpha = 1/2, fill = "#619CFF") +
          ggridges::theme_ridges() +
          theme(legend.position = "none") +
          labs(y = "",
               x = y,
               title = titulo,
               caption = "En esta gráfica no se tienen en cuenta los pesos muestrales.")
        if(IC == TRUE){
          p1 <- p1 +
            labs(subtitle = subtitulo)
        }
      }
      #---------------------------------------------------------------------------
      # Violin
      #---------------------------------------------------------------------------
      if(plot == "violin"  && is_numerical(datos[[y]]) == TRUE){
        p1 <- ggplot2::ggplot(datos |> dplyr::mutate(x.aux = "") , aes_string(x = "x.aux", y = y)) +
          geom_violin(alpha = 1/2, trim = FALSE, fill = "#619CFF") +
          geom_boxplot(width=0.1, fill = "#619CFF") +
          theme_minimal() +
          theme(legend.position = "none") +
          labs(y = "",
               title = titulo,
               caption = "En esta gráfica no se tienen en cuenta los pesos muestrales.")
        if(IC == TRUE){
          p1 <- p1  +
            labs(subtitle = subtitulo)
        }
      }
      #---------------------------------------------------------------------------
      # Lollipop para variable y numérica
      #---------------------------------------------------------------------------
      if(plot == "Lollipop"  && is_numerical(datos[[y]]) == TRUE){
        estimates.Lolli <- estimates |>
          dplyr::mutate(var.x1.numeric = 1) |>
          dplyr::rename(mean = var.y)
        margen <- 0.5
        p1 <- ggplot2::ggplot(estimates.Lolli) +
          geom_segment(aes(x = var.x1.numeric - margen, xend = var.x1.numeric + margen,
                           y = mean, yend = mean)) +
          geom_text(aes(x = var.x1.numeric, y = mean, label = paste(format(round(mean, 1), decimal.mark = ","),
                                                                    sep="")),
                    nudge_y = 0.9) +
          labs(x = "Global",
               y = y,
               title = paste0("Estimación de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} ),
               shape = x, color = y) +
          theme_minimal() +
          theme(strip.text = element_text(face = "bold", size = 12),
                legend.position = "none",
                axis.text.x = element_blank()) +
          ylim(min(estimates.Lolli$mean) - 5, max(estimates.Lolli$mean) + 5)
        if(IC == TRUE){
          p1 <- p1  +
            geom_segment(aes(x = var.x1.numeric - margen, xend = var.x1.numeric + margen,
                             y = mean, yend = mean)) +
            geom_rect(aes(xmin = var.x1.numeric - margen, xmax = var.x1.numeric + margen,
                          ymin = `2.5 %`, ymax= `97.5 %`), alpha = 1/2)
        }
      }
      #---------------------------------------------------------------------------
      # Barras para variable y categórica
      #---------------------------------------------------------------------------
      if(plot == "bar" && is_categorical(datos[[y]]) == TRUE){
        estimates.bar <- estimates |>
          dplyr::rename(mean = var.y) |>
          dplyr::mutate(y = rownames(estimates),
                 y = stringr::str_replace(y, "var.y", ""),
                 mean = 100 * mean,
                 SE = 100 * SE,
                 `2.5 %` = 100 * `2.5 %`,
                 `97.5 %` = 100 * `97.5 %`,
                 y = forcats::fct_rev(as.factor(y)),
                 ajuste = dplyr::case_when(
                   mean < 3 & y == unique(y)[1] ~ -5,
                   mean < 3 & y == unique(y)[length(unique(y))] ~ 5,
                   TRUE ~ 0
                 )
          )
        colors <- RColorBrewer::brewer.pal("Spectral", n = max(RColorBrewer::brewer.pal.info["Spectral", "maxcolors"]))
        # Create a function to interpolate more colors if needed
        color_pal <- grDevices::colorRampPalette(colors)
        # Calculate the number of unique categories in your data
        n_categories <- length(unique(estimates.bar$y))
        # Generate a sufficient number of colors
        extended_colors <- color_pal(n_categories)
        # Reverse the colors
        rev_colors <- rev(extended_colors)

        p1 <- ggplot2::ggplot(estimates.bar, aes(x = X.Global., y = mean, fill = y)) +
          geom_col() +
          geom_text(aes(x = X.Global., y = mean + ajuste , label = format(round(mean, 1), decimal.mark = ",")),
                    position = position_stack(vjust = 0.5)) +
          scale_fill_manual(values = rev_colors) +
          guides(fill = guide_legend(reverse = TRUE, nrow =1)) +
          theme_minimal() +
          theme(legend.position = "top", legend.title = element_blank()) +
          xlab("") +
          ylab("") +
          labs(title = paste0("Estimación de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} )) +
          coord_flip()
      }
    }
  }

  #---------------------------------------------------------------------------
  # Prueba de hipótesis
  #---------------------------------------------------------------------------
  # Si hay variable x
   if(!is.null(x)){
    # Obtener las categorías únicas
    categorias_unicas <- unique(datos$var.x1)
    combinaciones <- combn(categorias_unicas, 2)
    # Inicializar un data.frame para almacenar los resultados
    resultados.prueba <- data.frame(Categoria1 = character(),
                                Categoria2 = character(),
                                Estadistica = numeric(),
                                ValorP = numeric(),
                                stringsAsFactors = FALSE)
    if(is_numerical(datos[[y]]) == TRUE){
      # Iterar sobre cada combinación de categorías
      if(ncol(combinaciones)>1){
        for(i in 1:ncol(combinaciones)) {
          par <- combinaciones[, i]
          # Realizar la prueba t
          dis1 = survey::svrepdesign(data=datos |> dplyr::filter(var.x1 %in% combinaciones[,i]),
                             type="Fay", weights=~peso_MEst,
                             repweights=paste0("EST_W_REP_[", "1-", length(pesos), "]"),
                             combined.weights=TRUE, rho = 1.5, mse = F)
          resultado_ttest <- survey::svyttest(var.y ~ as.character(var.x1), dis1)
          # Almacenar los resultados en el data.frame
          resultados.prueba <- rbind(resultados.prueba, data.frame(Categoria1 = par[1],
                                                                   Categoria2 = par[2],
                                                                   Estadistica = resultado_ttest$statistic,
                                                                   ValorP = resultado_ttest$p.value))
          row.names(resultados.prueba) <- NULL
        }
      }
      resultados.prueba$signif <- significancia(resultados.prueba$ValorP)
      # Pruebas globales
      model <- survey::svyglm(var.y ~ var.x1, design=dis, family=gaussian())
      prueba.Wald <- survey::regTermTest(model, ~ var.x1)
      resultados.global <- paste0("Estadística F con ", format(round(prueba.Wald$df, digit), decimal.mark = ","), " y ", format(round(prueba.Wald$ddf, digit), decimal.mark = ","), " grados de libertad: ", format(round(prueba.Wald$Ftest, digit), decimal.mark = ","), ", valor p: ", format(signif(prueba.Wald$p, digits = digit), decimal.mark = ","))
    }
    if(is_categorical(datos[[y]]) == TRUE){
      # Iterar sobre cada combinación de categorías
      if(ncol(combinaciones)>1){
        for(i in 1:ncol(combinaciones)) {
          par <- combinaciones[, i]
          # Realizar la prueba chi-cuadrado
          dis1 = survey::svrepdesign(data=datos |> dplyr::filter(var.x1 %in% combinaciones[,i]) |>
                                       dplyr:: mutate(var.x2 =as.character(var.x1)), type="Fay", weights=~peso_MEst,
                             repweights=paste0("EST_W_REP_[", "1-", length(pesos), "]"), combined.weights=TRUE, rho = 1.5, mse = F)
          tabla = survey::svytable(~var.y+var.x2, dis1)
          resultado_chisq <- summary(tabla, statistic="Chisq")
          # Almacenar los resultados en el data.frame
          resultados.prueba <- rbind(resultados.prueba, data.frame(Categoria1 = par[1],
                                                                  Categoria2 = par[2],
                                                                  Estadistica = resultado_chisq$statistic$statistic,
                                                                  ValorP = resultado_chisq$statistic$p.value))
          row.names(resultados.prueba) <- NULL
        }
      }
      resultados.prueba$signif <- significancia(resultados.prueba$ValorP)
      # Pruebas globales
      tabla.global <- survey::svytable(~var.y + var.x1, dis)
      model <- summary(tabla.global, statistic="Chisq")
      resultados.global <- paste0("Estadística Chi-cuadrado de Pearson: ",
                                  format(round(model$statistic$statistic, digit), decimal.mark = ","),
                                  " con df = ",
                                  model$statistic$parameter, " , ",
                                  "valor p: ",
                                  format(signif(model$statistic$p.value, digits = digit +1), decimal.mark = ",")
                                  )
    }
  }

  #---------------------------------------------------------------------------
  # Imprimir los resultados de prueba de hipótesis
  #---------------------------------------------------------------------------
  declaracion <- cat("Los siguientes resultados consideran todos los estudiantes para los que se cuenta con ponderador \n",
                     "los resultados de estimación y prueba de hipótesis consideran pesos réplica y pesos de estudiantes \n",
                     "los conteos de datos y de NA no son ponderados \n")

  if(!is.null(plot)){
    print(p1)
  }

  if(!is.null(x)){
    if(is_categorical(datos[[y]]) == TRUE){
      estimates <- estimates |>
        dplyr::rename(porcentaje = mean) |>
        dplyr::mutate(porcentaje = porcentaje * 100,
               se = se * 100,
               `2.5 %` = `2.5 %`*100,
               `97.5 %` = `97.5 %`*100)
    }

    if(ncol(combinaciones) > 1){
      resultados <- list(estimaciones = estimates,
                         contraste.individual = resultados.prueba,
                         contraste.global = resultados.global)
      if(!is.null(plot)){
        resultados$p1 = p1
      }

      declaracion
      print(list(estimaciones = format(ajustarDecimales(resultados$estimaciones, digito = digit), decimal.mark = ","),
                 contraste.individual = format(ajustarDecimales(resultados$contraste.individual, digito = digit), decimal.mark = ","),
                 contraste.global = resultados$contraste.global)
      )
      signif.code <- cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1 \n")
    }
     if(ncol(combinaciones) == 1){
      resultados <- list(estimaciones = estimates,
                         contraste.global = resultados.global)
      if(!is.null(plot)){
        resultados$p1 = p1
      }
      declaracion
      print(list(estimaciones = format(ajustarDecimales(estimates, digito = digit), decimal.mark = ","),
                 contraste.global = resultados.global))
    }
  }

  if(is.null(x)){
    if(is_categorical(datos[[y]]) == TRUE){
      estimates <- estimates |>
        dplyr::rename(porcentaje = var.y) |>
        dplyr::mutate(porcentaje = porcentaje * 100,
               SE = SE * 100,
               `2.5 %` = `2.5 %`*100,
               `97.5 %` = `97.5 %`*100)
    }

    resultados <- list(estimaciones = estimates)
    if(!is.null(plot)){
      resultados$p1 = p1
    }
    declaracion
    print(list(estimaciones = format(ajustarDecimales(estimates, digito = digit), decimal.mark = ",")))
  }
  if(descarga){
      # Guarlos resultados en Excel
      wb <- openxlsx::createWorkbook() # Crea un nuevo libro de Excel
      openxlsx::addWorksheet(wb, "Hoja 1") # Añade una hoja
      if(is.null(archivo)){
        archivo = paste0(y, ".xlsx")
      }
      if(!is.null(x)){
        if(ncol(combinaciones) > 1){
          openxlsx::writeData(wb, sheet = "Hoja 1", estimates, startRow = 1, startCol = 1)
          openxlsx::writeData(wb, sheet = "Hoja 1", resultados.prueba, startRow = nrow(estimates) + 3, startCol = 1)
          openxlsx::writeData(wb, "Hoja 1", resultados.global, startRow = nrow(estimates) + nrow(resultados.prueba) + 6, startCol = 1, colNames = FALSE)
          openxlsx::saveWorkbook(wb, file = archivo, overwrite = TRUE)
        }
        if(ncol(combinaciones) == 1){
          openxlsx::writeData(wb, sheet = "Hoja 1", estimates, startRow = 1, startCol = 1)
          openxlsx::writeData(wb, "Hoja 1", resultados.global, startRow = nrow(estimates) + 3, startCol = 1, colNames = FALSE)
          openxlsx::saveWorkbook(wb, file = archivo, overwrite = TRUE)
        }
      }

      if(is.null(x)){
        openxlsx::writeData(wb, sheet = "Hoja 1", estimates, startRow = 1, startCol = 1)
        openxlsx::saveWorkbook(wb, file = archivo, overwrite = TRUE)
      }
      if(!is.null(plot)){
        nombre.plot <- sub("\\.xlsx$", ".pdf", archivo)
        if (!is.null(args$ancho) && !is.null(args$alto)) {
          ggsave(filename = nombre.plot, plot = p1, device = "pdf", width = args$ancho, height = args$alto)
        } else {
          ggsave(filename = nombre.plot, plot = p1, device = "pdf")
        }
      }
      cat("Los resultados se han exportado a la ruta suministrada.")
    }

    return(invisible(resultados))
}

