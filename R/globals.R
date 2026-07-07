#' @keywords internal
#' @importFrom rlang .data .env :=
#' @importFrom stats na.omit
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      # This is an internal dataset.
      "argos_verifications_master"
    )
  )
}
