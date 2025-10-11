# x <- list(
#   alias = "example",
#   regex = "^example$",
#   tbl = fastplyr::new_tbl())
# check_alias_results(x$alias, x$tbl)
#' @autoglobal
#' @noRd
check_alias_results <- function(alias, df, call = rlang::caller_env()) {
  msg <- c("x" = "{.field {alias}} had {.strong {nrow(df)}} matches.")

  if (nrow(df) == 0L) {
    cli::cli_abort(msg, call = call)
  }

  if (nrow(df) > 1L) {
    msg <- c(msg, cli::col_yellow(cli::format_bullets_raw(df$title)))
    cli::cli_abort(msg, call = call)
  }

}

#' @autoglobal
#' @noRd
check_endpoint_alias <- function(alias, call = rlang::caller_env()) {
  if (length(alias) > 1L) {
    msg <- c("x" = "Only one {.cls endpoint} can be loaded at a time.")
    cli::cli_abort(msg, call = call)
  }

  if (!is_alias(alias)) {
    msg <- c("x" = "{.val {alias}} is not an {.cls endpoint} alias.")
    cli::cli_abort(msg, call = call)
  }
}


#' @autoglobal
#' @noRd
check_collection_alias <- function(alias, call = rlang::caller_env()) {
  if (length(alias) > 1L) {
    cli::cli_abort(
      c("x" = "Only one {.cls collection} can be loaded at a time."),
      call = call)
  }

  if (!alias_is_collection(alias)) {
    cli::cli_abort(
      c("x" = "{.val {alias}} is not a {.cls collection} alias."),
      call = call)
  }
}

#' @autoglobal
#' @noRd
check_group_alias <- function(alias, call = rlang::caller_env()) {

  avec  <- unlist(alias, use.names = FALSE)
  msg   <- c("x" = "A {.cls group} must contain more than one {.cls endpoint}.")

  if (rlang::is_empty(alias) || !any(nzchar(alias))) {
    cli::cli_abort(msg, call = call)
  }

  if (any_collection(avec)) {
    avec <- avec[alias_is_collection(avec)][[1]]
    cli::cli_abort(
      c("x" = "A {.cls group} cannot contain a {.cls collection}.",
        ">" = "Run e.g., {.field collection({.val {avec}})}."),
      call = call
    )
  }

  if (rlang::has_length(avec, 1L)) {
    cli::cli_abort(
      c(msg, ">" = "Run {.field endpoint({.val {avec}})} to load this endpoint."),
      call = call)
  }
}

#' @autoglobal
#' @noRd
print_list <- function(ls, prefix = "") {
  if (length(ls) == 0) cat("<empty>\n")

  if (length(names(ls)) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(names(ls)), ls), sep = "\n")

  invisible(ls)
}

# test_aliases(the$aka$open)
# test_aliases(the$aka$care)
# test_aliases(the$aka$caid)
# test_aliases(the$aka$prov)
# test_aliases(the$aka$hgov)
#' Test Aliases
#' @param name description
#' @autoglobal
#' @noRd
test_aliases <- function(x) {

  all_alias <- purrr::list_flatten(x, name_spec = "{inner}")

  all_names <- rlang::names2(all_alias) |> sort()

  errors <- purrr::map_dfr(
    all_names, function(x) {
      cheapr::new_df(
        alias = x,
        title = tryCatch(
          alias_lookup(x)$title,
          error = function(e)
            NA_character_
        ),
        identifier = tryCatch(
          alias_lookup(x)$identifier,
          error = function(e)
            NA_character_
        )
      )
    }
  ) |>
    fastplyr::as_tbl() |>
    collapse::sbt(is.na(identifier)) |>
    _$alias

  print_list(
    all_alias[errors],
    prefix = paste0(cli::symbol$record, " ")
  )
}

# alias_column(the$clog$care$temporal, the$aka$care$temporal)
#' @autoglobal
#' @noRd
alias_column <- function(df, aka, default = "NA") {

  default <- match.arg(default, c("NA", "alias"))

  to_col <- function(aka) {
    code_def  <- glue::glue(",\n .default = {default})")

    string <- paste0(
      "gdetect(title, ",
      "{glue::single_quote(unname(x))}) ~ ",
      "{glue::single_quote(names(x))}"
    )

    glue::as_glue("cheapr::case(\n") +
      glue::glue(string, x = aka) |>
      glue::glue_collapse(sep = ",\n") +
      code_def
  }

  collapse::mtt(
    df,
    alias = to_col(aka) |>
      rlang::parse_expr() |>
      rlang::eval_bare()
  ) |>
    collapse::colorder(alias)
}
