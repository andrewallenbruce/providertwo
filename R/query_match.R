#' @autoglobal
#' @noRd
params <- S7::new_generic("params", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(params, class_query) <- function(obj) {
  S7::prop(obj, "params")
}

#' @autoglobal
#' @noRd
input <- S7::new_generic("input", "obj", function(obj) {
  S7::S7_dispatch()
})

S7::method(input, class_query) <- function(obj) {
  S7::prop(obj, "input")
}

#' @autoglobal
#' @noRd
remove_year <- function(x) {
  params(x)[names2(params(x)) %!=% "year"]
}

#' @autoglobal
#' @noRd
remove_year2 <- function(x) {
  input(x)[names2(input(x)) %!=% "year"]
}

#' @autoglobal
#' @noRd
match_query <- function(obj, qry) {

  param  <- remove_year(qry)
  pname  <- names2(param)
  field  <- keys(obj)
  clean  <- clean_names(field)

  set_names(
    param[qmatch(clean, pname)],
    field[sort(qmatch(pname, clean))])
}

#' @autoglobal
#' @noRd
select_years <- function(obj, qry) {
  x <- list(
    idx   = seq_along(obj@year),
    year  = obj@year,
    id    = obj@identifier,
    field = keys(obj))

  if ("year" %!in_% names2(params(qry))) {
    return(x)
  }

  idx <- which_(obj@year %in_% params(qry)$year)

  if (is_empty(idx)) {
    return(x)
  }

  list(idx  = idx,
       year = obj@year[idx],
       id   = obj@identifier[idx],
       field = keys(obj)[idx])
}

#' @autoglobal
#' @noRd
match_query2 <- function(obj, qry) {

  x     <- select_years(obj, qry)
  param <- remove_year2(qry)

  df <- join_on(

    purrr::imap(x$field, function(x, i) {
      cheapr::new_df(year = i, field = x)
    }) |>
      purrr::list_rbind() |>
      collapse::mtt(clean = clean_names(field)) |>
      collapse::sbt(clean %in_% names2(param)),

    cheapr::new_df(
      clean = names2(param),
      param = unname(param)),

    on = "clean"
  ) |>
    collapse::slt(-clean)

  list(
    idx   = x$idx,
    year  = x$year,
    id    = x$id,
    field = df)
}

#' @autoglobal
#' @noRd
map_parse_eval <- function(x) {
  purrr::map(x, function(x) {
    rlang::parse_expr(x) |>
      rlang::eval_bare()
  })
}

#' @autoglobal
#' @noRd
as_glue_list <- function(x) {
  purrr::map(x, function(x) {
    glue::as_glue("list(") +
      glue::glue_collapse(x, ", ") +
      glue::as_glue(")")
  })
}

#' @autoglobal
#' @noRd
finalize_match2 <- function(x) {
  combo_cd <- paste0(
    "{glue::backtick(field)} = ",
    "{ifelse(ismod, unlist(param, use.names = FALSE), ",
    "glue::double_quote(unlist(param, use.names = FALSE)))}"
  )

  x <- collapse::mtt(x,
    ismod = purrr::map_lgl(param, is_mod),
    combo = glue::glue(combo_cd),
    ismod = NULL,
    field = NULL,
    param = NULL) |>
    collapse::rsplit(~ year) |>
    cheapr::cheapr_rev() |> # rsplit reverses order of years
    as_glue_list()

  map_parse_eval(x)
}
