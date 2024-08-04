#' @title
#' Esta función compara las diferentes ediciones de ARISTAS
#'
#' @param datos La base de datos que se quiere analizar, debe tener una columna llamada edicion indicando a cuál edición corresponde cada fila de la base.
#' @param y El nombre de la variable de interés, ésta variable puede ser de tipo numérico o categórico.
#' @param ediciones Vector de caracteres o números indicando cuáles son las ediciones que desea comparar.
#' @param x (Opcional) Nombre de la variable numérica o categórica que define los subgrupos para la comparación.
#' @param digit Especificar el número de dígitos decimales, por defecto son 3 dígitos decimales.
#' @param plot Puede elegir entre `density`, `violin`, `Lollipop`, `bar`. La opción `violin` incluirá también la gráfica de cajas.
#' @param descarga Indicar si desea descargar los resultados numéricos y la gráfica, debe especificar la ubicación. Al usar la opción `descarga = TRUE` se descargará un archivo excel con los resultados numéricos y un pdf con la gráfica.
#' @param archivo Indicar el nombre del archivo excel que se descarga. La gráfica, si hay, se descargará con el mismo nombre y extensión pdf en la misma ubicación que el archivo excel.
#' @param ancho El ancho en pulgadas de la gráfica.
#' @param alto El alto en pulgadas de la gráfica.
#'
#' @return
#' `longitudinal`: devuelve una lista de objetos correspondientes a estimacions y resultados de hipótesis de comparación:
#' * `estimaciones`: La estimación, error estándar, intervalo de confianza de 95% e información de los NA para la media de la variable de interés para cada valor de la variable de corte `x` y para cada edición
#' * `estimaciones.globales`: Las estimaciones, errores estándares, intervalos de confianza de 95% e información de los NA para la media de la variable de interés para cada edición a nivel global (para todos los datos ingresados en el argumento `data`). Cuando no se ingresa variable de corte `x`, este resultado coincide con `estimaciones`, por lo que se omite en la salida.
#' * `contraste.global`: Los resultados de prueba de hipótesis para la igualdad de medidas de la variable de interés en diferentes ediciones.
#' * `contrastes.invididuales.entre.ediciones`: Los resultados de prueba de hipótesis para la igualdad de medidas de la variable de interés en cada valor de la variable de corte `x` en diferentes ediciones .
#'
#' Adicionalmente, también la función arrojará la gráfica que usuario haya elegido.
#'
#' @details
#'
#' Cuando la variable de interés `y` es una variable numérica (puntaje en pruebas), y el usuario ingresa la varible de corte `x` con o sin valores en el argumento `limits`,
#' la función realizará la prueba \eqn{F} para la igualdad de medias de la variable de interés `y` en cada categoría de `x` entre las ediciones, la prueba se realiza con la función `survey::svyglm` y `survey::regTermTest` con base en un diseño muestral de pesos réplica definido con `survey::svrepdesign`,
#' y estos resultados se encuentran en el resultado `contraste.individual`. La salida de la función incluye la estimación, error estándar, intervalo de confianza de 95% para la media de la variable `y` para cada categoría de `x` y a nivel global
#' También calcula el número de datos, número y porcentaje de NA para cada categoría de `x` y a nivel global para cada edición.
#' De la misma forma, la función llevará a cabo la prueba global \eqn{F} de igualdad de medias  entre las diferentes ediciones.
#'
#' Cuando la variable de interés `y` es una variable numérica (puntaje en pruebas), y el usuario no ingresa la varible de corte `x`,
#' la función solo calculará la estimación, error estándar, intervalo de confianza de 95%, cantidad de datos, cantidad y porcentaje de NA para la media de la variable `y` para cada edición,
#' y también la prueba global \eqn{F} de igualdad de medias  entre las diferentes ediciones.
#'
#' Cuando la variable de interés `y` es una variable categórica (nivel de desempeño), y el usuario ingresa la varible de corte `x` con o sin valores en el argumento `limits`,
#' la función realizará la prueba chi-cuadrado para la independencia entre los diferentes valores de `y` y las ediciones para cada categoría de la variable `x` haciendo uso de la función `survey::svytable`,
#' lo anterior con base en un diseño muestral de pesos réplica definido con `survey::svrepdesign`,
#' y estos resultados se encuentran en `contrastes.invididuales.entre.ediciones`.
#' También calcula el número de datos, número y porcentaje de NA para cada categoría de `x` y a nivel global para cada edición.
#' De la misma forma, la función llevará a cabo la prueba global de independencia de chi-cuadrado entre los diferentes valores de `y` y las ediciones.
#'
#' Cuando la variable de interés `y` es una variable categórica (nivel de desempeño), y el usuario no ingresa la varible de corte `x`,
#' la función solo calculará la estimación, error estándar e intervalo de confianza de 95%, cantidad de datos, cantidad y porcentaje de NA para la variable `y` en cada edición.
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
#' @importFrom survey svymean
#' @importFrom survey svychisq
#' @importFrom dplyr left_join
#'
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
#' # -------------------------------------------------------------------------------------------------
#' # Comparación de puntaje de matemáticas
#' # -------------------------------------------------------------------------------------------------
#'  # Unificar el nombre de la variable theta_MAT_300_50
#' ARISTAS18.matematicas2 <- ARISTAS18.matematicas2 |>
#'   dplyr::rename(theta_MAT_300_50 = theta_MAT_E300,
#'                 AlumnoGenero = AlumnoGenero15)
#'
#'
#'  # Unir las bases de las dos ediciones a una sola base de datos
#' ARISTAS18.22.matematicas <- unionbases.longitudinal(ediciones = c(2018, 2022), bases = c("ARISTAS18.matematicas2", "ARISTAS22.matematicas"), y = "theta_MAT_300_50", x = "regiones")
#'
#'# Análisis de la variable theta_MAT_E300 por región a nivel nacional
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", digit = 3, plot = "violin")
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", digit = 3, plot = "violin", descarga = TRUE)
#'
#'# Análisis de la variable theta_MAT_E300 por región y edición
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = "regiones", digit = 1, plot = "density")
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = "regiones", digit = 2, plot = "Lollipop")
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = "regiones", digit = 3, plot = "violin", descarga = TRUE, archivo = "theta_MAT_regiones.xlsx")
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = "regiones", digit = 4, plot = "density", descarga = TRUE, archivo = "theta_MAT_regiones.xlsx", ancho = 10, alto = 8)
#'
#'# Análisis de la variable theta_MAT_E300 por región y edición
#' ARISTAS18.22.matematicas <- unionbases.longitudinal(ediciones = c(2018, 2022), bases = c("ARISTAS18.matematicas2", "ARISTAS22.matematicas"), y = "theta_MAT_300_50", x = c("regiones", "AlumnoGenero"))
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = "AlumnoGenero", digit = 1, plot = "density")
#'
#' # Análisis de la variable theta_MAT_E300 por edición para los estudiantes de la región SUR
#' longitudinal(datos = ARISTAS18.22.matematicas |> dplyr::filter(regiones == "SUR"), ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = NULL, digit = 3, plot = "density")
#'
#' # Análisis de la variable theta_MAT_E300 por edición para los estudiantes de la región SUR y ESTE
#' longitudinal(datos = ARISTAS18.22.matematicas |> dplyr::filter(regiones %in% c("SUR", "ESTE")), ediciones = c(2018, 2022), y = "theta_MAT_300_50", x = "regiones", digit = 3, plot = "violin")
#'
#' # -------------------------------------------------------------------------------------------------
#' # Comparación de nivel de desempeño de matemáticas
#' # -------------------------------------------------------------------------------------------------
#'
#' # Unir las bases de las dos ediciones a una sola base de datos para anlizar Niveles_MAT por regiones
#' ARISTAS18.22.matematicas <- unionbases.longitudinal(ediciones = c(2018, 2022), bases = c("ARISTAS18.matematicas2", "ARISTAS22.matematicas"), y = "Niveles_MAT", x = "regiones")
#'
#' # Análisis de la variable Niveles_MAT por edición a nivel nacional
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "Niveles_MAT", digit = 2, plot = "bar")
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "Niveles_MAT", digit = 3, plot = "bar", descarga = TRUE, archivo = "Niveles_MAT_ediciones.xlsx", ancho = 6, alto = 8)
#'
#' # Análisis de la variable Niveles_MAT por regiones y edición
#' longitudinal(datos = ARISTAS18.22.matematicas, ediciones = c(2018, 2022), y = "Niveles_MAT", x = "regiones", digit = 3, plot = "bar")
#'
#' # Análisis de la variable Niveles_MAT por edición para la región ESTE
#' longitudinal(datos = ARISTAS18.22.matematicas |> dplyr::filter(regiones == "ESTE"), ediciones = c(2018, 2022), y = "Niveles_MAT",  digit = 3, plot = "bar")
#'
#' # Análisis de la variable Niveles_MAT por edición para la región ESTE y CENTRO
#' longitudinal(datos = ARISTAS18.22.matematicas |> dplyr::filter(regiones %in% c("ESTE", "CENTRO")), ediciones = c(2018, 2022), y = "Niveles_MAT", x = "regiones", digit = 3, plot = "bar")

longitudinal <- function(datos, ediciones, y, x = NULL, digit = 3, plot = "density", descarga = FALSE, archivo = NULL, ...){
  args <- list(...)

  options(warn = -1) # Eliminar los warning que produce ggplot
  if(is.null(ediciones) | length(ediciones) == 1){
    stop("Debes especificar al menos 2 ediciones de ARISTAS para poder realizar la comparación.")
  }

  #---------------------------------------------------------------------------
  # Funciones auxiliares
  #---------------------------------------------------------------------------
  is_numerical <- function(variable) {
    is.numeric(variable) || is.integer(variable) || is.double(variable)
  }
  is_categorical <- function(variable) {
    is.factor(variable) || is.character(variable)
  }
  # Eliminar los NA en la variable y
  datos <- datos |>
    dplyr::filter(!is.na(peso_MEst))
  datos$var.y <- datos[, which(names(datos) == y)]

  if(!is.null(x)){
    datos$var.x <- datos[, which(names(datos) == x)]
    datos <- datos |>
      dplyr::filter(!is.na(var.x) & var.x != "" & var.x != "NA")
  }

  if(is_categorical(datos[[y]])){
    datos <- datos |>
      dplyr::mutate(var.y = dplyr::na_if(var.y, ""))
  }

  datos$edicion <- as.factor(datos$edicion)

  # Especificar el diseño muestral
  pesos <- grep("^EST_W_REP", names(datos), value = TRUE)
  datos <- datos[complete.cases(datos[, pesos]), ] # Eliminar observaciones con missing en los pesos
  dis = survey::svrepdesign(data=datos, type="Fay", weights=~peso_MEst,
                    repweights=paste0("EST_W_REP_[", "1-", length(pesos), "]"),
                    combined.weights=TRUE, rho = 1.5, mse = F)

  #---------------------------------------------------------------------------
  # Comparación de la variable y para diferentes ediciones
  #---------------------------------------------------------------------------
  if(length(ediciones) >= 2){
    if(is_numerical(datos[[y]])){
      estimates.nacional <- survey::svyby(as.formula(paste("~", y)), ~edicion, dis, survey::svymean, na.rm = T)
      estimates.nacional <- cbind(estimates.nacional, confint(estimates.nacional))
      estimates.nacional <- estimates.nacional |>
        dplyr::left_join(datos |>
                    dplyr::group_by(edicion) |>
                      dplyr::summarise(n = dplyr::n(),
                              n.NA = sum(is.na(var.y)),
                              `%.NA` = 100 * n.NA / dplyr::n()),
                  by = "edicion")

      prueba.f <- survey::svyglm(var.y ~ edicion, design=dis, family=gaussian())
      prueba.Wald <- survey::regTermTest(prueba.f, ~ edicion)
      resultados.global <- paste0("Estadística F para comparación de ", y, " entre las ", length(unique(datos$edicion)), " ediciones: ",
                                  format(round(prueba.Wald$Ftest, digit), decimal.mark = ","), " con ",
                                  format(round(prueba.Wald$df, digit), decimal.mark = ","), " y ",
                                  format(round(prueba.Wald$ddf, digit), decimal.mark = ","), " grados de libertad, ",
                                  "valor p: ",
                                  format(round(prueba.Wald$p, digit), decimal.mark = ","))
      if(!is.null(x)){
        if(is.numeric(x)){
          print("La variable de corte ingresada es numérica, considere categorizar para comparación entre ediciones")
        }
        if(is_categorical(x)){
          resultados.individuales <- data.frame(x = character(),
                                   Estadistica_F = numeric(),
                                   ValorP = numeric(),
                                   signif = character(),
                                   stringsAsFactors = FALSE)
          categorias_x <- unique(datos$var.x)
          for (cat in categorias_x) {
            # Filtrar los datos por la categoría actual de x
            df_filtrado <- datos |> dplyr::filter(var.x == cat)
            if(min(table(df_filtrado$edicion)) == 0){
              next
            }
            # Especificar el diseño muestral
            pesos <- grep("^EST_W_REP", names(df_filtrado), value = TRUE)
            df_filtrado <- df_filtrado[complete.cases(df_filtrado[, pesos]), ] # Eliminar observaciones con missing en los pesos
            dis.individual = survey::svrepdesign(data=df_filtrado, type="Fay", weights=~peso_MEst,
                                      repweights=paste0("EST_W_REP_[", "1-", length(pesos), "]"),
                                      combined.weights=TRUE, rho = 1.5, mse = F)

            # Realizar la regresión y la prueba Wald
            prueba.f <- survey::svyglm(var.y ~ edicion, design = dis.individual, family = gaussian())
            prueba.Wald <- survey::regTermTest(prueba.f, ~ edicion)

            # Agregar los resultados a la tabla
            resultados.individuales <- rbind(resultados.individuales,
                                             data.frame(x = cat,
                                                       Estadistica_F =  format(round(prueba.Wald$Ftest, digit), decimal.mark = ","),
                                                       ValorP =  format(round(prueba.Wald$p, digit), decimal.mark = ","),
                                                       signif = significancia(prueba.Wald$p)))
          }
        }
      }
    }
    if(is_categorical(datos[[y]])){
      estimates.global <-  survey::svyby(~var.y, ~edicion, dis, survey::svymean, na.rm = T)
      estimates.global[,-1] <- 100 * estimates.global[,-1]
      estimates1.aux <- estimates.global |> dplyr::select(edicion, starts_with("var.y"))
      estimates2.aux <- estimates.global |> dplyr::select(edicion, starts_with("se"))
      estimates1 <- estimates1.aux |>
        tidyr::pivot_longer(
          cols = -edicion,
          names_to = y,
          values_to = "mean"
        )
      estimates1[[y]] <- gsub("var.y", "", estimates1[[y]])
      estimates2 <- estimates2.aux |>
        tidyr::pivot_longer(
          cols = -edicion,
          names_to = y,
          values_to = "se"
        )
      estimates2[[y]] <- estimates1[[y]]
      estimates.nacional <- estimates1 |> dplyr::left_join(estimates2, by = c("edicion", y))
      ic <- as.data.frame(confint(estimates.global))
      split_ic <- strsplit(rownames(ic), ":")
      edicion <- sapply(split_ic, `[`, 1)
      niveles <- sapply(split_ic, `[`, 2)
      rownames(ic) <- NULL
      ic <- cbind(edicion, niveles, ic)
      ic <- ic |>
        dplyr::mutate(niveles = gsub("var.y", "", ic$niveles))
      colnames(ic)[2] <- y
      estimates.nacional <- estimates.nacional |> left_join(ic, by = c("edicion", y))
      estimates.nacional <- estimates.nacional |>
        dplyr::rename (porcentaje = mean) |>
        dplyr::left_join(datos |>
                    dplyr::group_by(edicion) |>
                    dplyr::summarise(n = dplyr::n(),
                                     n.NA = sum(is.na(var.y)),
                                     `%.NA` = 100*n.NA / dplyr::n()),
                  by = "edicion")

      # Prueba chi cuadrado
      tabla.global <- survey::svytable(as.formula(paste0("~", y, "+edicion")), dis)
      model <- summary(tabla.global, statistic="Chisq")
      tabla <- model$table
      resultados.global <- paste0("Estadística Chi-cuadrado de Pearson: ",
                                  format(round(model$statistic$statistic, digit), decimal.mark = ","),
                                  " con ",
                                  model$statistic$parameter, " grados de libertad, ",
                                  "valor p: ",
                                  format(signif(model$statistic$p.value, digits = digit), decimal.mark = ","))
      if(!is.null(x)){
        if(is.numeric(x)){
          print("La variable de corte ingresada es numérica, considere categorizar para comparación entre ediciones")
        }
        if(is_categorical(x)){
          resultados.individuales <- data.frame(x = character(),
                                                Estadistica_Chi2 = numeric(),
                                                ValorP = numeric(),
                                                signif = character(),
                                                stringsAsFactors = FALSE)
          categorias_x <- unique(datos$var.x)
          for (cat in categorias_x) {

            # Filtrar los datos por la categoría actual de x
            df_filtrado <- datos |> dplyr::filter(var.x == cat)
            if(min(table(df_filtrado$edicion)) == 0){
              next
            }
            # Especificar el diseño muestral
            pesos <- grep("^EST_W_REP", names(df_filtrado), value = TRUE)
            df_filtrado <- df_filtrado[complete.cases(df_filtrado[, pesos]), ] # Eliminar observaciones con missing en los pesos
            dis.individual = survey::svrepdesign(data=df_filtrado, type="Fay", weights=~peso_MEst,
                                                 repweights=paste0("EST_W_REP_[", "1-", length(pesos), "]"),
                                                 combined.weights=TRUE, rho = 1.5, mse = F)

            # Realizar la prueba Chi cuadrado
            tabla.global <- survey::svytable(as.formula(paste0("~", y, "+edicion")), dis.individual)
            model <- summary(tabla.global, statistic="Chisq")

            # Agregar los resultados a la tabla
            resultados.individuales <- rbind(resultados.individuales,
                                             data.frame(x = cat,
                                                        Estadistica_Chi2 =  format(round(model$statistic$statistic, digit), decimal.mark = ","),
                                                        ValorP =  format(round(model$statistic$p.value, digit), decimal.mark = ","),
                                                        signif = significancia(model$statistic$p.value)))
            rownames(resultados.individuales) = NULL
          }
        }
      }
    }
  }

  #---------------------------------------------------------------------------
  # Título y subtítulo de las gráficas (contiene la estimación nacional)
  #---------------------------------------------------------------------------
  titulo = paste0("Datos muestrales de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} )

  #---------------------------------------------------------------------------
  # Si el usuario ingresa variable de corte x
  #---------------------------------------------------------------------------
  if(!is.null(x)){
    # Crear una base donde se repiten los datos, pero en la variable x
    datos.plot.repeat <- datos
    datos.plot.repeat[[x]] <- "Global"
    datos.plot <- dplyr::bind_rows(datos, datos.plot.repeat)
    datos.plot[[x]] <- as.factor(datos.plot[[x]])
    datos.plot[[x]] <- factor(datos.plot[[x]], levels = c(setdiff(levels(datos.plot[[x]]), "Global"), "Global"))
    #---------------------------------------------------------------------------
    # Cálculo de las estimaciones
    #---------------------------------------------------------------------------
    estimates.x <- survey::svyby(as.formula(paste("~ var.y")), as.formula(paste("~edicion+var.x")), dis, survey::svymean, na.rm = T)


    if(is_numerical(datos[[y]])){

      estimates <- cbind(estimates.x, confint(estimates.x))

      estimates <- estimates |>
        dplyr::arrange(estimates[[2]], edicion)
      n.nacionales <- datos |>
        dplyr::group_by(var.x, edicion) |>
        dplyr::summarise( n = dplyr::n(),
                          n.NA = sum(is.na(var.y)),
                          `%.NA` = 100*n.NA / dplyr::n(),
                          .groups = "drop") |>
        dplyr::arrange(var.x, edicion)
      estimates <- cbind(estimates, n.nacionales[,-c(1,2)])
      }
    if(is_categorical(datos[[y]])){
      names(estimates.x) <- gsub("var.y", "", names(estimates.x))
      p <- length(table(datos$var.y)) # cantidad de catogorias en la variable Y
      estimates1.aux <- estimates.x[,1:(3+p-1)]
      estimates2.aux <- estimates.x |> dplyr::select(edicion, var.x, starts_with("se"))
      estimates1 <- estimates1.aux |>
        tidyr::pivot_longer(
          cols = -c("edicion", "var.x"),
          names_to = y,
          values_to = "mean"
        )
      estimates1[[y]] <- gsub("var.y", "", estimates1[[y]])
      estimates2 <- estimates2.aux |>
        tidyr::pivot_longer(
          cols = -c("edicion", "var.x"),
          names_to = y,
          values_to = "se"
        )
      estimates2[[y]] <- estimates1[[y]]
      estimates <- estimates1 |> dplyr::left_join(estimates2, by = c("edicion", "var.x", y))

      ic <- as.data.frame(confint(estimates.x))
      rownames(ic) <- gsub("var.y", "", rownames(ic))
      split_periods <- strsplit(rownames(ic), "\\.")
      split_colons <- lapply(split_periods, function(x) strsplit(x[2], ":"))
      edicion <- sapply(split_periods, function(x) x[1])
      var.x <- sapply(split_colons, function(x) x[[1]][1])
      var.y <- sapply(split_colons, function(x) x[[1]][2])
      rownames(ic) <- NULL
      ic <- cbind(edicion, var.x, var.y, ic)
      ic <- ic |>
        dplyr::mutate(var.y = gsub(y, "", ic$var.y))
      colnames(ic)[3] <- y

      estimates <- estimates |>
        dplyr::left_join(ic, by = c("edicion", "var.x", y)) |>
        dplyr::left_join(datos |>
                    dplyr::group_by(edicion, var.x) |>
                    dplyr::summarise( n = dplyr::n(),
                                      n.NA = sum(is.na(var.y)),
                                      `%.NA` = 100*n.NA / dplyr::n(),
                                      .groups = "drop"),
                  )
    }

    datos <- datos |>
      dplyr::filter(!is.na(datos[[x]]), !is.na(datos[[y]]), datos[[y]] != "")

    if(!is.null(plot)){
      if(plot == "Lollipop" && is_numerical(datos[[y]]) == TRUE){
        estimates.nacional.Lollipop <- estimates.nacional |>
          dplyr::mutate(x = "Global")

        k <- ncol(estimates.nacional.Lollipop)
        # Reorganizar las columnas para mover la última columna a la segunda posición
        estimates.nacional.Lollipop <- estimates.nacional.Lollipop[, c(1, k, 2:(k-1))]
        names(estimates.nacional.Lollipop) <- names(estimates)

        estimates.Lollipop <- rbind(estimates, estimates.nacional.Lollipop)
        estimates.Lollipop$regiones <- factor(estimates.Lollipop$var.x,
                                              levels = c("CENTRO", "ESTE", "NORTE", "OESTE", "SUR", "Global"))

        p1 <- ggplot(estimates.Lollipop, aes(x = regiones)) +
          geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`, colour = edicion), width = 0.4, position = "dodge") +
          geom_errorbar(aes(ymin = estimates.Lollipop$var.y , ymax = estimates.Lollipop$var.y, colour = edicion), width = 0.4, position = "dodge") +
          labs(x = x,
               y = y,
               title = paste0("Estimación de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} ),
               colour = "Edición") +
          theme_minimal()
      }
      if(plot == "density" && is_numerical(datos[[y]]) == TRUE){
        p1 <- ggplot(datos.plot, aes(datos.plot[[y]])) +
          geom_density(aes(colour = edicion)) +
          facet_wrap(vars(datos.plot[[x]])) +
          labs(x = y,
               title = paste0("Densidad de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} ),
               colour = "Edición",
               caption = "En esta gráfica no se tienen en cuenta los pesos muestrales.") +
          theme_minimal() +
          theme(strip.text = element_text(face = "bold", size = 12))

      }
      if(plot == "violin" && is_numerical(datos[[y]]) == TRUE){
      p1 <- ggplot(datos.plot, aes(y = datos.plot[[y]], x = "",
                                fill = edicion),
                     alpha = 1/2) +
          geom_violin(alpha = 1/2) +
          geom_boxplot(width = 0.9) +
          facet_wrap(vars(datos.plot[[x]])) +
          theme_minimal() +
          # theme(legend.position = "none") +
          labs(fill = "edición",
               x = "",
               y = "",
               title = titulo,
               caption = "En esta gráfica no se tienen en cuenta los pesos muestrales.")
      }
      if(plot == "bar" && is_categorical(datos[[y]]) == TRUE){
        estimates.bar <- estimates |>
          dplyr::select(-se, -`2.5 %`, - `97.5 %`)
        estimates.bar <- rbind(estimates.bar,
              estimates.nacional |>
                dplyr::mutate(var.x = "Global",
                       porcentaje = porcentaje / 100) |>
                dplyr::rename(mean = porcentaje) |>
                dplyr::select(edicion, var.x, y, mean, n, n.NA, `%.NA`))

        estimates.bar <- estimates.bar |>
          dplyr::mutate(mean = 100 * mean,
                        var.x = factor(var.x, levels = c(setdiff(unique(var.x), "Global"), "Global")),
                 ajuste = dplyr::case_when(
                   mean < 3 & estimates[[y]] == unique(estimates[[y]])[1] ~ 14,
                   mean < 3 & estimates[[y]] == unique(estimates[[y]])[length(unique(estimates[[y]]))] ~ -14,
                   TRUE ~ 0)
                 )

        colors <- RColorBrewer::brewer.pal("Spectral", n = max(RColorBrewer::brewer.pal.info["Spectral", "maxcolors"]))
        # Create a function to interpolate more colors if needed
        color_pal <- grDevices::colorRampPalette(colors)
        # Calculate the number of unique categories in your data
        n_categories <- length(unique(estimates.bar[[y]]))
        # Generate a sufficient number of colors
        extended_colors <- color_pal(n_categories)
        # Reverse the colors
        rev_colors <- rev(extended_colors)
        # Calcular el número de niveles
        num_levels <- length(unique(estimates.bar$var.x))

        # Calcular el número de filas y columnas para hacer la gráfica más larga que ancha
        # Aquí estamos buscando una disposición de facetas que sea más alta que ancha
        num_cols <- ceiling(sqrt(num_levels))
        num_rows <- ceiling(num_levels / num_cols)

        # Ajustar para que sea más larga que ancha
        if (num_rows < num_cols) {
          temp <- num_rows
          num_rows <- num_cols
          num_cols <- temp
        }
        p1 <- ggplot(estimates.bar,
                     aes(x = edicion, y = mean, fill = estimates.bar[[y]])) +
          geom_col() +
          geom_text(aes(x = edicion, y = mean + ajuste ,
                        label = format(round(mean, max(1, digit-2)), decimal.mark = ",")),
                    position = position_stack(vjust = 0.5),
                    size = 4) +
          scale_fill_manual(values = rev_colors) +
          facet_wrap(vars(estimates.bar$var.x), ncol = num_cols) +
          guides(fill = guide_legend(reverse = TRUE, nrow =1)) +
          theme_minimal() +
          theme(legend.position = "top", legend.title = element_blank()) +
          xlab("") +
          ylab("") +
          ylim(-15, 115) +
          labs(title = paste0("Estimación de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} )) +
          coord_flip()
      }
    }
  }

  #---------------------------------------------------------------------------
  # Si el usuario no ingresa variable de corte x
  #---------------------------------------------------------------------------
  if(is.null(x)){
    #---------------------------------------------------------------------------
    # Graficas
    #---------------------------------------------------------------------------
    if(!is.null(plot)){
      if(plot == "Lollipop" && is_numerical(datos[[y]]) == TRUE){
        p1 <- ggplot(estimates.nacional, aes(x = "")) +
          geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`, colour = edicion), width = 0.4, position = "dodge") +
          geom_errorbar(aes(ymin = estimates.nacional[[y]] , ymax = estimates.nacional[[y]], colour = edicion), width = 0.4, position = "dodge") +
          labs(x = "",
               y = y,
               title = paste0("Estimación de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} ),
               colour = "Edición") +
          theme_minimal()
      }
      if(plot == "density" && is_numerical(datos[[y]]) == TRUE){
        p1 <- ggplot(datos, aes(datos[[y]])) +
          geom_density(aes(colour = as.character(edicion))) +
          labs(x = y,
               title = paste0("Densidad de ", y, if(!is.null(x)){paste0(" según ", x)} else{NULL} ),
               colour = "Edición",
               caption = "En esta gráfica no se tienen en cuenta los pesos muestrales.") +
          theme_minimal()
      }
      if(plot == "violin" && is_numerical(datos[[y]]) == TRUE){
        p1 <- ggplot(datos, aes(y = datos[[y]], x = "",
                                fill = as.character(edicion)),
                     alpha = 1/2) +
          geom_violin(alpha = 1/2) +
          geom_boxplot(width = 0.9) +
          theme_minimal() +
          # theme(legend.position = "none") +
          labs(fill = "edición",
               x = "",
               y = "",
               title = titulo,
               caption = "En esta gráfica no se tienen en cuenta los pesos muestrales.")
      p1
      }
      if(plot == "bar" && is_categorical(datos[[y]]) == TRUE){
        estimates.bar <- estimates.nacional |>
          dplyr::select(-se, -`2.5 %`, -`97.5 %`)  |>
          dplyr::mutate(mean = porcentaje,
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
        n_categories <- length(unique(estimates.bar[[y]]))
        # Generate a sufficient number of colors
        extended_colors <- color_pal(n_categories)
        # Reverse the colors
        rev_colors <- rev(extended_colors)

        p1 <- ggplot(estimates.bar, aes(x = edicion, y = mean, fill = estimates.bar[[y]])) +
          geom_col() +
          geom_text(aes(x = edicion, y = mean + ajuste , label = format(round(mean, max(1, digit - 2)), decimal.mark = ",")),
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
  # Imprimir los resultados
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

      resultados <- list(estimaciones = estimates,
                         estimaciones.globales = estimates.nacional,
                         contraste.global = resultados.global,
                         contrastes.invididuales = resultados.individuales)
      if(!is.null(plot)){
        resultados$p1 = p1
      }
      declaracion
       print(list(estimaciones = format(ajustarDecimales(resultados$estimaciones, digito = digit), decimal.mark = ","),
                 estimaciones.globales = format(ajustarDecimales(resultados$estimaciones.globales, digito = digit), decimal.mark = ","),
                 contraste.global = resultados$contraste.global,
                 contrastes.invididuales.entre.ediciones = resultados$contrastes.invididuales))
       signif.code <- cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1 \n")
    }
    if(is.null(x)){
      resultados <- list(estimaciones.globales = estimates.nacional,
                         contraste.global = resultados.global)
      if(!is.null(plot)){
        resultados$p1 = p1
      }
      declaracion
      print(list(estimaciones = format(ajustarDecimales(resultados$estimaciones.globales, digito = digit), decimal.mark = ","),
                 contraste.global = resultados$contraste.global))
    }

    if(descarga){
      if(is.null(archivo)){
        archivo = paste0(y, ".xlsx")
      }
      # Guarlos resultados en Excel
      wb <- openxlsx::createWorkbook() # Crea un nuevo libro de Excel
      openxlsx::addWorksheet(wb, "Hoja 1") # Añade una hoja
      if(!is.null(x)){
        openxlsx::writeData(wb, sheet = "Hoja 1", estimates, startRow = 1, startCol = 1)
        openxlsx::writeData(wb, sheet = "Hoja 1", estimates.nacional, startRow = nrow(estimates) + 3, startCol = 1)
        openxlsx::writeData(wb, sheet = "Hoja 1", resultados.global, startRow = nrow(estimates) + nrow(estimates.nacional) + 6, startCol = 1, colNames = FALSE)
        openxlsx::writeData(wb, sheet = "Hoja 1", resultados.individuales, startRow = nrow(estimates) + nrow(estimates.nacional) + 9, startCol = 1, colNames = FALSE)
        openxlsx::saveWorkbook(wb, file = archivo, overwrite = TRUE)

      }
      if(is.null(x)){
        openxlsx::writeData(wb, sheet = "Hoja 1", estimates.nacional, startRow = 1, startCol = 1)
        openxlsx::writeData(wb, "Hoja 1", resultados.global, startRow = nrow(estimates.nacional) + 3, startCol = 1, colNames = FALSE)
        openxlsx::saveWorkbook(wb,  file = archivo, overwrite = TRUE)
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

