#' @autoglobal
#' @noRd
convert_epoch <- function(x) {
  as.Date(as.POSIXct.numeric(as.numeric(x) / 1000L, origin = "1970-01-01"))
}

#' @noRd
yank <- function(x) x[[1]]

#' @noRd
yank_index_name <- function(x, nm, i = 1L) get_elem(x[[i]], elem = rlang::ensym(nm))


#' @noRd
#' @autoglobal
`:=` <- function(left, right) {
  name <- substitute(left)
  if (!is.symbol(name))
    stop("left hand side must be a symbol")

  right <- substitute(right)
  if (!is.call(right))
    stop("right hand side must be a call")

  if (is.symbol(cl <- right[[1L]]) &&
      as.character(cl) %in% c("function", "new.env")) {
    # attach "name" attr for usage like:
    # foo := function(){}
    # foo := new.env()
    right <- eval(right, parent.frame())
    attr(right, "name") <- as.character(name)
  } else {
    # for all other usage,
    # inject name as a named arg, so that
    #   foo := new_class(...)
    # becomes
    #   foo <- new_class(..., name = "foo")

    right <- as.call(c(as.list(right), list(name = as.character(name))))

    ## skip check; if duplicate 'name' arg is an issue the call itself will signal an error.
    # if (hasName(right, "name")) stop("duplicate `name` argument.")

    ## alternative code path that injects `name` as positional arg instead
    # right <- as.list(right)
    # right <- as.call(c(right[[1L]], as.character(name), right[-1L]))
  }
  eval(call("<-", name, right), parent.frame())
}


# quick_care_temp("quality_payment")
#' @autoglobal
#' @noRd
quick_care_temp <- function(x, offset = 0L, limit = 5000L) {
  careTemp(x) |>
    prop("endpoints") |>
    _[["identifier"]][1] |>
    request() |>
    req_url_query(
      offset = thresh(offset, total_rows(careTemp(x))),
      size   = thresh(limit, 5000L)
    ) |>
    perform_simple() |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

# quick_open("profile_covered")
#' @autoglobal
#' @noRd
quick_open <- function(x, offset = 0L, limit = 500L) {
  openMain(x) |>
    prop("identifier") |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = thresh(offset, total_rows(openMain(x))),
      limit   = thresh(limit, 500L)
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}

# quick_open_temp("ownership")
#' @autoglobal
#' @noRd
quick_open_temp <- function(x, offset = 0L, limit = 500L) {
  openTemp(x) |>
    prop("endpoints") |>
    _[["identifier"]][1] |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = thresh(offset, total_rows(openTemp(x))),
      limit   = thresh(limit, 500L)
    ) |>
    perform_simple() |>
    _[["results"]] |>
    map_na_if() |>
    rnm(clean_names) |>
    as_tbl()
}
