#' @noRd
debugme_on <- \() Sys.setenv(DEBUGME = "providertwo")

#' @noRd
debugme_off <- \() Sys.unsetenv("DEBUGME")

#' @noRd
is_debuggingme <- \() identical(Sys.getenv("DEBUGME"), "providertwo")

