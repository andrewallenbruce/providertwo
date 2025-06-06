#' @autoglobal
#' @noRd
catnew <- function() {

  check_catalog_exists()

  list(
    endpoint = cheapr::row_c(.catalog$care$main,
                             .catalog$pro$end,
                             .catalog$open$main,
                             .catalog$caid$main,
                             .catalog$hgov$main),
    temporal = cheapr::row_c(.catalog$care$temp,
                             .catalog$open$temp,
                             .catalog$caid$temp,
                             .catalog$hgov$temp)
  )
}

rlang::on_load(.catnew <<- catnew())

#' @autoglobal
#' @noRd
check_catnew_exists <- function() {
  if (!exists(".catnew")) .catnew <<- catnew()
}

#' @autoglobal
#' @noRd
aka <- function() {
  list(
    endpoint = list_combine(
      care_list$endpoint,
      pro_list$endpoint,
      open_list$endpoint,
      caid_list$endpoint,
      hgov_list$endpoint
    ),
    temporal = list_combine(
      care_list$temporal,
      open_list$temporal,
      caid_list$temporal,
      hgov_list$temporal
    )
  )
}

rlang::on_load(.aka <<- aka())

#' @autoglobal
#' @noRd
select_endpoint <- function(x, call = caller_env()) {

  check_catnew_exists()

  res <- select_alias(.catnew$endpoint, x)

  if (is_empty(res))  cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  if (nrow(res) > 1L) cli::cli_abort(c("x" = "> 1 match found for {.val {x}}."), call = call)

  c(res)
}

#' @autoglobal
#' @noRd
select_temporal <- function(x, call = caller_env()) {

  check_catnew_exists()

  res <- select_alias(.catnew$temporal, x)

  if (is_empty(res)) cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)

  list_tidy(
    !!!c(slt(res, -data)),
    endpoints   = get_elem(res, "data") |> _[[1]],
    identifier  = endpoints$identifier[1]
  )
}
