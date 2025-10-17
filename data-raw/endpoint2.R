# endpoint2("dial_facility")
# endpoint2("man_mltss")
# endpoint2("ab_reg_comp")
# endpoint2("asc_facility")
# endpoint2("qppe")
#' @autoglobal
#' @noRd
endpoint2 <- function(alias, call = rlang::caller_env()) {

  check_required(alias, call = call)
  check_endpoint_alias(alias, call = call)

  x <- alias_lookup(alias, call = call)

  x$point <- switch(
    x$catalog,
    care = glue::glue("care_{x$point}"),
    glue::glue("class_{x$point}"))

  .pnt <- rlang::as_function(
    x$point,
    env = rlang::pkg_env("providertwo"))

  x$catalog <- glue::glue("class_{x$catalog}")

  .cls <- rlang::as_function(
    x$catalog,
    env = rlang::pkg_env("providertwo"))

  x$fields     <- class_fields(x$fields)
  x$dimensions <- class_dimensions(x$limit, x$total)
  x$limit      <- NULL
  x$total      <- NULL

  x <- x[names(.pnt@properties)]

  arg_nms <- names(x)

  list2(!!!x)

  .cls(
    .pnt(!!!rlang::dots_list(x))
  )
}
