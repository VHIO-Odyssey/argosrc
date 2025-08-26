


# If the filter fails, it rerturns the full data frame. Used in
safe_filter <- function(.data, ...) {
  tryCatch(
    # Expresión que queremos intentar ejecutar
    dplyr::filter(.data, ...),

    # Manejador para la condición de 'error'
    error = function(e) {
      # Si se captura un error, simplemente devolvemos el data.frame original.
      # La 'e' contiene el objeto del error, que podríamos inspeccionar si quisiéramos.
      .data
    }
  )
}
