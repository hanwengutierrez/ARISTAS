#' Datos ARISTAS socioemocional edición 2022
#'
#' Aristas, la Evaluación Nacional de Logros Educativos, es realizada en Uruguay por el Instituto Nacional de Evaluación Educativa (INEEd).
#' Tiene como objetivo fundamental dar cuenta de los logros del sistema educativo en tercero y sexto de primaria y en tercero de educación media a nivel nacional en múltiples dimensiones. En el año 2022 se aplicó Aristas en tercero de media.
#' En esta base 'ARISTAS22.socioemocional' se incluye la información de habilidades socioemocionales.
#'
#'
#' @format ## `ARISTAS22.socioemocional`
#' Una base de datos con 11546 filas (estudiantes) y 324 columnas (variables)
#' \describe{
#'   \item{AlumnoCodigoDes}{Identificador del estudiante}
#'   \item{GrupoCodigoDes}{Identificador del grupo}
#'   \item{CentroCodigoDes}{Identificador del centro educativo}
#'   \item{MdeoInt}{Región ("Montevideo", "Interior")}
#'   \item{regiones}{Región ("SUR", "OESTE", "NORTE", "ESTE", "CENTRO")}
#'   \item{categoria_centro}{Tipo de Centro ("Secundaria", "pública", "Secundaria privada", "UTU (escuela técnica)")}
#'   \item{ESCS_Centro}{Índice de contexto socioeconómico y cultural del centro}
#'   \item{ESCS_Centro_cat}{Índice de contexto socioeconómico y cultural del centro en quintiles (Q1: "Muy desfavorable", Q2: "Desfavorable", Q3: "Medio", Q4: "Favorable", Q5: "Muy favorable")}
#'   \item{ESCS_Centro_cat_comparable2018}{Índice de contexto socioeconómico y cultural del centro en quintiles comparables con Aristas 2018 (Q1: "Muy desfavorable", Q2: "Desfavorable", Q3: "Medio", Q4: "Favorable", Q5: "Muy favorable")}
#'   \item{categoria_grupo}{Tipo de curso ("Secundaria pública", "Secundaria privada", "UTU-CBT (escuelas técnicas con ciclo básico tecnológico)", "UTU-FPB (escuelas técnicas con formación profesional básica)")}
#'   \item{ESCS_Grupo}{Índice de contexto socioeconómico y cultural del grupo}
#'   \item{ESCS_Grupo_cat}{Índice de contexto socioeconómico y cultural del grupo en quintiles (Q1: "Muy desfavorable", Q2: "Desfavorable", Q3: "Medio", Q4: "Favorable", Q5: "Muy favorable")}
#'   \item{EDAD}{Edad}
#'   \item{AlumnoGenero}{Sexo (dato administrativo) (F: "Femenino", M: "Masculino")}
#'   \item{Concurre_cod}{Asistencia a clase reportada por el referente del grupo (1 "Asiste habitualmente", 2 "Asiste poco, todas las semanas falta algunos días",3 "Asiste muy poco, aproximadamente 1 vez cada 15 días , 4 "No asiste se cambió de centro", 5 "No asiste se cambió de grupo", 6 "No asiste, dejó de venir hace dos meses o más")}
#'   \item{INSE}{Índice de nivel socioeconómico }
#'   \item{ESCS_Alumno}{Estatus socioeconómico y cultural del estudiante}
#'   \item{ESCS_Alumno_cat_comparable2018}{Estatus socioeconómico y cultural del estudiante en quintiles comparable con Aristas 2018 (Q1: "Muy desfavorable", Q2: "Desfavorable", Q3: "Medio", Q4: "Favorable", Q5: "Muy favorable")}
#'   \item{ESCS_Alumno_cat}{Estatus socioeconómico y cultural del estudiante en quintiles}
#'   \item{ESCS_Centro_comparable2018}{Índice de contexto socioeconómico y cultural del centro comparable con Aristas 2018}
#'   \item{peso_CENTRO}{Ponderador de centro}
#'   \item{peso_MGrup}{Ponderador de grupo}
#'   \item{NEE}{Estudiantes identificados con necesidades educativas específicas cuyos resultados se excluyeron de las pruebas de Aristas (1 "estudiante con NEE excluido del puntaje de las pruebas de Aristas", 0 "estudiante no excluido del puntaje de las pruebas de Aristas")}
#'   \item{ESCS_Alumno_comparable2018}{Estatus socioeconómico y cultural del estudiante comparable con 2018}
#'   \item{theta_LEN_300_50}{Habilidad obtenida en la prueba de lectura}
#'   \item{Niveles_LEN}{Nivel de desempeño en lectura (N1 "Nivel 1", N2 "Nivel 2", N3 "Nivel 3", N4 "Nivel 4", N5 "Nivel 5", N6 "Nivel 6")}
#'   \item{theta_MAT_300_50}{Habilidad obtenida en la prueba de matemática}
#'   \item{Niveles_MAT}{Nivel de desempeño en matemática (N1 "Nivel 1", N2 "Nivel 2", N3 "Nivel 3", N4 "Nivel 4", N5 "Nivel 5")}
#'   \item{peso_MEst}{Ponderador del estudiante}
#'   \item{EST_W_REP_1 : EST_W_REP_160}{Pesos repetidos mediante método BRR (Balanced Repeated Replication)}
#'   \item{ES1_1, ..., ECD2_3}{Resultados a ítems de la prueba.}
#'   \item{IND_Socioemocional}{Respuesta al cuestionario (1: "El alumno respondió el cuestionario de contexto", vacía: "El alumno no respondió el cuestionario de contexto")}
#'   \item{AUTOCON_E_ESC50}{Índice de Autocontrol}
#'   \item{AUTOEIDE_E_ESC50}{Índice de Autoeficacia académica en Idioma Español}
#'   \item{AUTOEMAT_E_ESC50}{Índice de Autoeficacia académica en Matemática}
#'   \item{AUTOMETA_E_ESC50}{Índice de Autorregulación metacognitiva}
#'   \item{EMPATIA_E_ESC50}{Índice de Empatía}
#'   \item{EXTERNALIZ_E_ESC50}{Índice de Conductas externalizantes}
#'   \item{HABINTER_E_ESC50}{Índice de Habilidades interpersonales}
#'   \item{HABINTRA_E_ESC50}{Índice de Habilidades intrapersonales}
#'   \item{HABRELAC_E_ESC50}{Índice de Habilidades de relacionamiento}
#'   \item{INTERNALIZ_E_ESC50}{Índice de Conductas internalizantes}
#'   \item{MOTAUTREGA_E_ESC50}{Índice de Motivación y autorregulación del aprendizaje}
#'   \item{MOTINT_E_ESC50}{Índice de Motivación intrínseca}
#'   \item{PERSAC_E_ESC50}{Índice de Perseverancia académica}
#'   \item{REGEMO_E_ESC50}{Índice de Regulación emocional}
#'   \item{VALTIDE_E_ESC50}{Índice de Valoración de la tarea en Idioma Español}
#'   \item{VALTMAT_E_ESC50}{Índice de Valoración de la tarea en Matemática}
#' }
#' @source <https://www.ineed.edu.uy/base-de-datos-aristas-media-2022/>
"ARISTAS22.socioemocional"
