#' @title
#' Esta función ajusta modelos lineales y mixtos para datos de ARISTAS
#' @param base La base de datos que se quiere analizar, puede ser cualquiera de las bases de ARISTAS incluidas en el paquete o cualquiera otra base ARISTAS que no sea de uso público. Tenga en cuenta que las columnas que contengan los pesos replicados deben tener nombres: EST_W_REP_1, EST_W_REP_2, etc.
#' @param formula La fórmula para especificar (i) el modelo para un modelo de regresión o un modelo con errores clusterizados, (ii) el modelo de los efectos fijos en un modelo mixto.
#' @param tipo Especificar el tipo de modelo que se estimará, `lm` para un modelo de regresión, `lm_error_cluster` para un modelo de regresión con errores clusterizados,
#' `linear_mixed` para un modelo mixto de dos niveles donde solo se permite pesos en el primer nivel y `linear_mixed_weight_2` para un modelo mixto de dos niveles permitiendo pesos en ambos niveles.
#' @param pesos Especificar el tipo de modelo que se estimará, `lm` para un modelo de regresión, `lm_error_cluster` para un modelo de regresión con errores clusterizados, `linear_mixed` parea un modelo multinivel con pesos en el nivel de estudiantes, `linear_mixed_weight_2` para un modelo multinivel con pesos en el nivel de estudiantes y a nivel de centros, y `logit` para un modelo de regresión logística con pesos en el nivel de estudiantes.
#' @param pesos.rep Pesos réplica para el nivel de estudiantes.
#' @param pesos.centro Un vector de pesos o ponderadores para el nivel de centros.
#' @param cluster_var La variable que indica los clusters para todos los modelos que consideren dos niveles
#'
#' @return
#' La función `modelo` devuele un objeto de clase `lm` o `lmer`
#' @importFrom WeMix mix
#' @importFrom lme4 lmer
#' @importFrom MuMIn r.squaredGLMM
#' @import dplyr
#'
#' @details
#' Esta función ajusta 5 tipos de modelos:
#' * (1) Modelo de regresión con o sin pesos a nivel de estudiantes, se permite el uso de pesos réplica.
#' * (2) Modelo de regresión con errores clusterizados con o sin pesos a nivel de estudiantes, los clusteres son a nivel de centros, se permite el uso de pesos réplica.
#' * (3) Modelo lineal mixto con dos niveles: nivel de estudiantes y nivel de centros usando la función 'lmer' del paquete 'lme4'.
#' Solo se permite ingresar ponderadores para el primer nivel (el nivel de estudiantes), que puede ser un vector de ponderadores con o sin los pesos réplica con el método de BRR.
#' *  (4) Modelo lineal mixto con dos niveles: nivel de estudiantes y nivel de centros usando la función 'mix' del paquete 'WeMix'.
#' No se permiten pesos réplica en ningún nivel.
#' *  (5) Modelo mixto logística con dos niveles: nivel de estudiantes y nivel de centro usando la función 'glmer' del paquete 'lme4' con o sin pesos a nivel de estudiantes.
#' Para los modelos (2) y (3) cuando se ingresan los pesos réplica se debe también ingresar el vector de pesos. Y la metodología para calcular los errores estándares de las estimaciones consistenten en:
#' *  (i) Para el vector de pesos, ajustar el modelo correspondiente y se denota el parámetro de interés (coeficiente de regresión) con \eqn{\hat{\beta}},
#' *  (ii) Para cada conjunto de pesos de los pesos réplica, ajustar el modelo correspondiente y el parámetro de interés correspondiente se denota por \eqn{\hat{\beta}_k} para \eqn{k=1,\cdots,K}, donde \eqn{K} denota el vector de pesos réplica.
#' *  (iii) El error estándar de la estimación se calcula como \deqn{\sqrt{\dfrac{\sum_{i=1}^K(\hat{\beta}_k-\hat{\beta})^2}{K}}}
#' @export
#'
#' @examples
#' ARISTAS22.matematicas.modelo <- ARISTAS22.matematicas |>
#'          dplyr::select(AlumnoCodigoDes, CentroCodigoDes, #Seleccionar código de estudiantes y centros,
#'          peso_CENTRO, peso_MEst, #Seleccionar los ponderadores de estudiantes, de centro,
#'          theta_MAT_300_50, # Seleccionar la variabla dependiente numérica
#'          Niveles_MAT, # Seleccionar la variabla dependiente categórica
#'          starts_with("EST_W_REP_")) # Pesos réplica
#'
#' # Procesamiento previo de información de contexto
#' # Variable EF1m: género. Convertir el 99 a NA, y convertir la variable a factor
#' ARISTAS22.contexto$EF1m[ARISTAS22.contexto$EF1m == 99] <- NA_integer_
#' ARISTAS22.contexto$EF1m <- factor(ARISTAS22.contexto$EF1m)
#' # Variable EF2d: ascendencia. Convertir el 99 a NA, y convertir la variable a factor
#' ARISTAS22.contexto$EF2d[ARISTAS22.contexto$EF2d == 99] <- NA_integer_
#' ARISTAS22.contexto$EF2d <- factor(ARISTAS22.contexto$EF2d)
#' # Variable EF2d: ascendencia. Unir las categorías de ascendencia
#' ARISTAS22.contexto$EF2d <- as.double(ARISTAS22.contexto$EF2d == "1") # 1 es ascendencia blanca
#' # Variable EF7_1: repetición en primaria. Convertir el 99 a NA, y convertir la variable a factor
#' ARISTAS22.contexto$EF7_1[ARISTAS22.contexto$EF7_1 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF7_1 <- factor(ARISTAS22.contexto$EF7_1)
#' # Variable EF7_2: repetición en liceo o UTU. Convertir el 99 a NA, y convertir la variable a factor
#' ARISTAS22.contexto$EF7_2[ARISTAS22.contexto$EF7_2 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF7_2 <- factor(ARISTAS22.contexto$EF7_2)
#' # Variable EF8_2: inasistencia. Convertir el 99 a NA, y convertir la variable a factor
#' ARISTAS22.contexto$EF8_2[ARISTAS22.contexto$EF8_2 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF8_2 <- factor(ARISTAS22.contexto$EF8_2)
#' # Variable EF9_1: Expectativas de los estudiantes.
#' # Convertir el 99 a NA, y convertir la variable a factor
#' ARISTAS22.contexto$EF9_1[ARISTAS22.contexto$EF9_1 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF9_2[ARISTAS22.contexto$EF9_2 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF9_3[ARISTAS22.contexto$EF9_3 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF9_4[ARISTAS22.contexto$EF9_4 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF9_5[ARISTAS22.contexto$EF9_5 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF9_6[ARISTAS22.contexto$EF9_6 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF9_7[ARISTAS22.contexto$EF9_7 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF9_8[ARISTAS22.contexto$EF9_8 == 99] <- NA_integer_
#' ARISTAS22.contexto$EF9_17 <- ARISTAS22.contexto$EF9_1 * ARISTAS22.contexto$EF9_7
#'
#' ARISTAS22.contexto.modelo <- ARISTAS22.contexto |>
#'   dplyr::select(AlumnoCodigoDes, EDAD,
#'          EF1m, EF2d, EF7_1, EF7_2, EF8_2, EF9_17, EF9_2, EF9_3,
#'          EF9_4, EF9_5, EF9_6, EF9_8)
#'
#' # Unir las informaciones
#' datos1 <- ARISTAS22.matematicas.modelo |>
#'   dplyr::left_join(ARISTAS22.contexto.modelo,
#'             by = "AlumnoCodigoDes")
#'
#' # Definir la formula del modelo de los efectos fijos
#' formula.fija <- theta_MAT_300_50 ~ EDAD + EF1m + EF2d + EF7_1 + EF7_2 + EF8_2 + EF9_2 + EF9_3 + EF9_4 + EF9_5 + EF9_6 + EF9_8
#'
#' # Modelo 1: Ajustar un modelo de regresión lineal con y sin pesos de estudiantes
#' modelo(base = datos1, formula.fija, tipo = "lm")
#' modelo(base = datos1, formula.fija, tipo = "lm", pesos = "peso_MEst")
#' modelo(base = datos1, formula.fija, tipo = "lm", pesos = "peso_MEst", pesos.rep = paste0("EST_W_REP_", 1:160))
#'
#' # Modelo 2: Ajustar un modelo de regresión con error clusterizado con y sin pesos de estudiantes
#' modelo(base = datos1, formula.fija, tipo = "lm_error_cluster", cluster_var = "CentroCodigoDes")
#' modelo(base = datos1, formula.fija, tipo = "lm_error_cluster", cluster_var = "CentroCodigoDes", pesos = "peso_MEst")
#' modelo(base = datos1, formula.fija, tipo = "lm_error_cluster", cluster_var = "CentroCodigoDes", pesos = "peso_MEst", pesos.rep = paste0("EST_W_REP_", 1:160))
#'
#' # Modelo 3: Ajustar un modelo mixto sin pesos
#' modelo(base = datos1, formula.fija, tipo = "linear_mixed", cluster_var = "CentroCodigoDes")
#' # Ajustar un modelo mixto con pesos de los estudiantes
#' modelo(base = datos1, formula.fija, tipo = "linear_mixed", cluster_var = "CentroCodigoDes", pesos = "peso_MEst")
#' # Ajustar un modelo mixto con pesos y pesos réplica de los estudiantes
#' modelo(base = datos1, formula.fija, tipo = "linear_mixed", cluster_var = "CentroCodigoDes", pesos = "peso_MEst", pesos.rep = paste0("EST_W_REP_", 1:160))
#'
#' # Modelo 4: Ajustar un modelo mixto con pesos de estudiantes y pesos de centros
#' modelo(base = datos1, formula.fija, tipo = "linear_mixed_weight_2", pesos = "peso_MEst", pesos.centro = "peso_CENTRO", cluster_var = "CentroCodigoDes")
#'
#' # Modelo 5: Ajustar un modelo mixto logit con variable dependiente nivel de desempeño igual o superior a dos, vs. nivel de desempeño menor a dos
#' datos1 <- datos1 |> dplyr::mutate(nivel_dos= Niveles_MAT == "B1" | Niveles_MAT == "N1")
#' formula.fija.logit <- nivel_dos ~ EDAD + EF1m
#' modelo(base = datos1, formula = formula.fija.logit, tipo = "logit", pesos = "peso_MEst", cluster_var = "CentroCodigoDes")
#' # Para extraer el pseudo R2 puede ejecutar MuMIn::r.squaredGLMM para el modelo anterior
#' modelo(base = datos1, formula = formula.fija.logit, tipo = "logit", pesos = "peso_MEst", cluster_var = "CentroCodigoDes", pesos.rep = paste0("EST_W_REP_", 1:160))

modelo <- function(base, formula, tipo, pesos = NULL, pesos.rep = NULL, pesos.centro = NULL, cluster_var = NULL) {

  #----------------------------------------------------------------------------------
  # Modelo 1: modelo lineal de un solo nivel
  #----------------------------------------------------------------------------------
  if (tipo == "lm") {
    if(is.null(pesos) & is.null(pesos.rep)) { # no hay pesos ni peso réplica
      model <- lm(formula, data = base)
    }
    if(!is.null(pesos) & is.null(pesos.rep)) { # sí hay pesos, no hay peso peso réplica
      base[["pesos.lm"]] <-  base[[pesos]]
      model <- lm(formula, data = base, weights = pesos.lm)
    }
    if(!is.null(pesos.rep) & !is.null(pesos)){ # sí hay ambos tipos de pesos
      base[["pesos.lm"]] <-  base[[pesos]]
      dis = survey::svrepdesign(data=base, type="Fay", weights=~pesos.lm,
                                repweights=paste0("EST_W_REP_[", 1, "-", length(pesos.rep), "]"),
                                combined.weights=TRUE, rho = 1.5, mse = F)
      model <- survey::svyglm(formula, design=dis, family=gaussian())
    }
    if(is.null(pesos) & !is.null(pesos.rep)) { # si no hay pesos pero sí hay pesos réplica, pedir al usuario ingresar los pesos
      stop("Falta ingresar los pesos de estudiantes en el argumento pesos.")
    }

  }

  #----------------------------------------------------------------------------------
  # Modelo 2: regresión lineal de un solo nivel con errores clusterizados por centro
  #----------------------------------------------------------------------------------
  if (tipo == "lm_error_cluster") {
    if(is.null(cluster_var)) {
      stop("Se debe proveer una variable que indique los clusters.")
    }
    if(is.null(pesos) & is.null(pesos.rep)) {
      model <- estimatr::lm_robust(formula, data = base, clusters = base[[cluster_var]])
    }
    if(!is.null(pesos) & is.null(pesos.rep)) {
      model <- estimatr::lm_robust(formula, data = base, clusters = base[[cluster_var]], weights = base[[pesos]])
    }
    if(is.null(pesos) & !is.null(pesos.rep)) {
      stop("Falta ingresar los pesos de estudiantes en el argumento pesos.")
    }

    if(!is.null(pesos) & !is.null(pesos.rep)) {
      base[["pesos.lm.robust"]] <-  base[[pesos]]
      model <- estimatr::lm_robust(formula, data = base, clusters = base[[cluster_var]], weights = pesos.lm.robust)
      theta <- summary(model)$coefficients[,1] # El vector de coeficientes estimados con los pesos de estudiantes
        p <- length(theta)  # Número de parámetros
        n.rep <- length(pesos.rep)
        theta.est.result <- matrix(NA, p, n.rep)
        R2.rep <- c()
        for(k in 1:n.rep){
          base[["pesos.lm.robust.rep"]] <-  base[[pesos.rep[k]]]
          model.k <- estimatr::lm_robust(formula, data = base, clusters = base[[cluster_var]], weights = pesos.lm.robust.rep)
          theta.est.result[,k] <- summary(model.k)$coefficients[,1]
          R2.rep[k] <- model.k$r.squared
        }
        sd.brr <- sqrt(rowMeans((theta.est.result - kronecker(matrix(theta), matrix(1, ncol=n.rep)))^2))
        valor.t <- theta/sd.brr
        coeficientes <- data.frame(estimacion = theta,
                                   s.e. = sd.brr,
                                   valor.t = valor.t)
        print(coeficientes)
        R2 = cat("\n","El R2 para el modelo con error clusterizado es ", mean(R2.rep), "\n\n")
        model = NULL
    }
    # model$coefficients.complete <- coef(summary(lm(formula, data = base), vcov = vcovCL(model, cluster = base[[cluster_var]])))
  }

  #----------------------------------------------------------------------------------
  # Modelo 3: modelo multinivel con pesos réplica en el primer nivel
  #----------------------------------------------------------------------------------
  if (tipo == "linear_mixed") {
    formula.lmer <- paste(deparse(formula, width.cutoff = 500), paste(" + (1|", cluster_var, ")", collapse = ""), sep = " ")
    if(is.null(pesos)){
      if(is.null(pesos.rep)) {
        model <- lme4::lmer(formula.lmer, data = base) }
      if(!is.null(pesos.rep)) {
        stop("Falta ingresar los pesos en el argumento pesos.") }
      }

    if(!is.null(pesos)){
      base[["pesos.lm"]] <-  base[[pesos]]
      model <- lme4::lmer(formula.lmer, data = base, weights = pesos.lm)
      theta <- summary(model)$coefficients[,1] # El vector de coeficientes estimados con los pesos de estudiantes
      p <- length(theta)  # Número de parámetros
      if(!is.null(pesos.rep)){
        n.rep <- length(pesos.rep)
        theta.est.result <- matrix(NA, p, n.rep)
        R2.rep <- c()
        for(k in 1:n.rep){
          base[["pesos.lm.rep"]] <-  base[[pesos.rep[k]]]
          model.k <- lme4::lmer(formula.lmer, data = base, weights = pesos.lm.rep)
          theta.est.result[,k] <- summary(model.k)$coefficients[,1]
          R2.rep[k] <- MuMIn::r.squaredGLMM(model.k)[2]
        }
      sd.brr <- sqrt(rowMeans((theta.est.result - kronecker(matrix(theta), matrix(1, ncol=n.rep)))^2))
      valor.t <- theta/sd.brr
      coeficientes <- data.frame(estimacion = theta,
                                 s.e. = sd.brr,
                                 valor.t = valor.t)

      print(coeficientes)
      R2 = cat("El R2 condicional para el modelo mixto es ", mean(R2.rep), "\n")
      model = NULL
      }
    }
  }

  #------------------------------------------------------------------------------------------
  # Modelo 4: modelo multinivel con pesos en ambos niveles
  #------------------------------------------------------------------------------------------
  if (tipo == "linear_mixed_weight_2") {
    formula.lmer <- as.formula(paste(deparse(formula, width.cutoff = 500), paste(" + (1|", cluster_var, ")", collapse = ""), sep = " "))
    if(is.null(pesos.centro)){
      stop("Falta ingresar los pesos de los centros en el argumento pesos.centro")
    }
    if(is.null(pesos)){
      stop("Falta ingresar los pesos de los estudiantes en el argumento pesos")
    }
    if(!is.null(pesos) & !is.null(pesos.centro)){
      base[["pesos.estudiantes"]] <-  base[[pesos]]
      base[["pesos.centro"]] <-  base[[pesos.centro]]
      model <- WeMix::mix(formula.lmer, data=base, weights=c("pesos.estudiantes", "pesos.centro"))
    }
  }
  #------------------------------------------------------------------------------------------
  # Modelo 5: modelo regresión logística con pesos en el primer nivel
  #------------------------------------------------------------------------------------------
  if(tipo == "logit"){
    formula.lmer <- as.formula(paste(deparse(formula, width.cutoff = 500), paste(" + (1|", cluster_var, ")", collapse = ""), sep = " "))
    if(is.null(pesos)){
      model <- lme4::glmer(formula.lmer, data = base, family = binomial)
    }
    if(!is.null(pesos) & is.null(pesos.rep)){
      base[["pesos.estudiantes"]] <-  base[[pesos]]
      model <- lme4::glmer(formula.lmer, data = base, weights = base[["pesos.estudiantes"]], family = binomial)
    }

    if(!is.null(pesos) & !is.null(pesos.rep)) {
      base[["pesos.estudiantes"]] <-  base[[pesos]]
      model <- lme4::glmer(formula.lmer, data = base, weights = base[["pesos.estudiantes"]], family = binomial)
      theta <- summary(model)$coefficients[,1] # El vector de coeficientes estimados con los pesos de estudiantes
      p <- length(theta)  # Número de parámetros
      n.rep <- length(pesos.rep)
      theta.est.result <- matrix(NA, p, n.rep)
      R2.rep <- c()
      for(k in 1:n.rep){
        base[["pesos.logit.rep"]] <-  base[[pesos.rep[k]]]
        model.k <- lme4::glmer(formula.lmer, data = base, weights = pesos.logit.rep, family = binomial)
        theta.est.result[,k] <- summary(model.k)$coefficients[,1]
        R2.rep[k] <- MuMIn::r.squaredGLMM(model.k)[1, 2]
      }
      sd.brr <- sqrt(rowMeans((theta.est.result - kronecker(matrix(theta), matrix(1, ncol=n.rep)))^2))
      valor.t <- theta/sd.brr
      coeficientes <- data.frame(estimacion = theta,
                                 s.e. = sd.brr,
                                 valor.t = valor.t)
      print(coeficientes)
      R2 = cat("\n", "El R2 condicional para el modelo logit es ", mean(R2.rep), "\n\n")
      model = NULL
    }

  }
  return(model)
}
