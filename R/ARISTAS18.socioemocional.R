#' Datos ARISTAS socioemocional edición 2018
#'
#' Aristas, la Evaluación Nacional de Logros Educativos, es realizada en Uruguay por el Instituto Nacional de Evaluación Educativa (INEEd).
#' Tiene como objetivo fundamental dar cuenta de los logros del sistema educativo en tercero y sexto de primaria y en tercero de educación media a nivel nacional en múltiples dimensiones. En el año 2018 se aplicó Aristas en tercero de media.
#' En esta base 'ARISTAS18.socioemocional' se incluye los resultados de desempeño de los estudiantes en lectura.
#'
#'
#' @format ## `ARISTAS18.socioemocional`
#' Una base de datos con 9060 filas (estudiantes) y 279 columnas (variables)
#' \describe{
#'   \item{IdCentroDes}{Identificador del centro educativo}
#'   \item{IdAlumnoDes}{Identificador del estudiante}
#'   \item{GrupoCodigo}{Identificador del grupo}
#'   \item{Grado}{Grado escolar (9: "Tercer año de educación media")}
#'   \item{regiones}{Región (1: "Sur", 2: "Litoral oeste", 3: "Norte", 4: "Literal sureste", 5: "Centro",  NA: "sin dato")}
#'   \item{categoria}{Categoría de Centro (1: "Liceos públicos", 2: "Liceos privados", 3: "Centros técnicos con cursos de ciclo básico tecnológico", 4: "Centros técnicos con formación profesional básica", 5: "Liceos gratuitos de gestión privada")}
#'   \item{peso_CENTRO}{Ponderador del centro}
#'   \item{peso_MGrup}{Ponderador del grupo}
#'   \item{peso_MEst}{Ponderador del estudiante}
#'   \item{AlumnoGenero15}{Sexo del estudiante (F: "Femenino", M: "Masculino")}
#'   \item{EDAD}{Edad del estudiante}
#'   \item{SituacionCentro}{Criterio de selección (1: "Muestra",  2: "Liceos gratuitos de gestión privada de la muestra nacional", 3: "Liceos gratuitos de gestión privada del censo")}
#'   \item{ESCS_IMP}{Índice de estatus socioeconómico y cultural del estudiante}
#'   \item{ESCS_Alumno_cat_IMP}{Índice de estatus socioeconómico y cultural del estudiante en 5 categorías (Q1: "Muy desfavorable", Q2: "Desfavorable", Q3: "Medio", Q4: "Favorable", Q5: "Muy favorable")}
#'   \item{ESCS_Centro_IMP}{Contexto socioeconómico y cultural del centro}
#'   \item{ESCS_Centro_cat}{Contexto socioeconómico y cultural centro en 5 categorías (Q1: "Muy desfavorable", Q2: "Desfavorable", Q3: "Medio", Q4: "Favorable", Q5: "Muy favorable")}
#'   \item{ESCS_Grupo_IMP}{Contexto socioeconómico y cultural del grupo}
#'   \item{ESCS_Grupo_cat}{Contexto socioeconómico y cultural grupo en 5 categorías (Q1: "Muy desfavorable", Q2: "Desfavorable", Q3: "Medio", Q4: "Favorable", Q5: "Muy favorable")}
#'   \item{theta_MAT_E300}{Habilidad obtenida en la prueba de matemática, escalada con media 300 y desvío 50}
#'   \item{theta_LEN_E300}{Habilidad obtenida en la prueba de lectura, escalada con media 300 y desvío 50}
#'   \item{Niveles_MAT}{Nivel de desempeño en matemática (B1 "Nivel bajo 1", N1 "Nivel 1", N2 "Nivel 2", N3 "Nivel 3", N4 "Nivel 4", N5 "Nivel 5", NA "Sin dato")}
#'   \item{Niveles_LEN}{Nivel de desempeño en lectura (B1 "Nivel bajo 1", N1 "Nivel 1", N2 "Nivel 2", N3 "Nivel 3", N4 "Nivel 4", N5 "Nivel 5", N6 "Nivel 6", NA "Sin dato")}
#'   \item{EST_W_REP_1 : EST_W_REP_160}{Pesos repetidos mediante método BRR (Balanced Repeated Replication)}
#'   \item{ES1_1, ..., ES15_5}{Resultados a ítems de la prueba.}
#'   \item{IND_Estu_SE_3}{1: "El alumno respondió el cuestionario de habilidades socioemocionales", 0: "El alumno no respondió el cuestionario de habilidades socioemocionales"}
#'   \item{EXTERNALIZ_ESC50}{Conductas externalizantes.}
#'   \item{INTERNALIZ_ESC50}{Conductas internalizantes.}
#'   \item{EMPATIA_ESC50}{Empatía.}
#'   \item{HABINTER_ESC50}{Habilidades interpersonales.}
#'   \item{HABRELAC_ESC50}{Habilidades de relacionamiento.}
#'   \item{RELREDES_ESC50}{Relacionamiento en redes sociales.}
#'   \item{AUTOCON_ESC50}{Autocontrol.}
#'   \item{HABINTRA_ESC50}{Habilidades intrapersonales.}
#'   \item{REGEMO_ESC50}{Regulación emocional.}
#'   \item{AUTOEIDE_ESC50}{Autoeficacia académica en Idioma Español.}
#'   \item{AUTOEMAT_ESC50}{Autoeficacia académica en Matemática.}
#'   \item{AUTOMCOG_ESC50}{Autorregulación metacognitiva.}
#'   \item{MOTAUTREGA_ESC50}{Motivación y autorregulación del aprendizaje.}
#'   \item{MOTINT_ESC50}{Motivación intrínseca.}
#'   \item{PERSAC_ESC50}{Perseverancia académica.}
#'   \item{VALTIDE_ESC50}{Valoración de la tarea en Idioma Español.}
#'   \item{VALTMAT_ESC50}{Valoración de la tarea en Matemática.}
#' }
#' @source <https://www.ineed.edu.uy/aristas-2018-tercero-de-educacion-media/>
"ARISTAS18.socioemocional"
