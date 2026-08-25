# Define una columna `meets_condition` indicando si se cumple la condición de
# branching logic para cada caso. Si la expresión falla (p.ej. alguna variable
# del branching no existe en el formulario actual), asume que la condición se
# cumple para todos los casos. Devuelve NA para los casos en que las variables
# del branching están ausentes, de modo que si el branching no puede resolverse
# el chequeo de completitud se aplica igualmente.
safe_condition_definition <- function(.data, condition_expr) {
  tryCatch(
    # Expresión que queremos intentar ejecutar
    dplyr::mutate(.data, meets_condition = !!condition_expr),

    # Manejador para la condición de 'error'
    error = function(e) {
      # Si se captura un error, simplemente devolvemos el data.frame original
      # asumiendo que el completeness aplica a todos los casos como si no
      # hubiese branching ()
      # La 'e' contiene el objeto del error, que podríamos inspeccionar si quisiéramos.
      dplyr::mutate(.data, meets_condition = TRUE)
    }
  )
}

# Auto update del paquetwe via add-in
update_argosrc <- function() {
  rlang::check_installed("pak")
  rlang::check_installed("rstudioapi")

  current_version <- as.character(packageVersion("argosrc"))

  sure <- rstudioapi::showQuestion(
    "Update argosrc from GitHub",
    stringr::str_c(
      "Do you want to update argosrc? (current version ",
      current_version,
      ")"
    )
  )

  if (!sure) {
    stop("Update aborted")
  }

  if ("argosrc" %in% (.packages())) {
    detach("package:argosrc", unload = TRUE)
  }

  master_branch <- rstudioapi::showQuestion(
    "Update argosrc from GitHub",
    "From which branch do you want to update?",
    ok = "Master",
    cancel = "Dev"
  )

  if (master_branch) {
    pak::pkg_install("VHIO-Odyssey/argosrc")
  } else {
    pak::pkg_install("VHIO-Odyssey/argosrc@dev")
  }

  require("argosrc")
}
