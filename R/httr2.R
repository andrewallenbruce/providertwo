# x <- build(endpoint("dial_facility"), query(
#   state = "GA",
#   city = "Atlanta",
#   provcity = "Atlanta",
#   provider_name = starts_with("C"),
#   provname = starts_with("C")))
#
# query_string_as_list(x@string, x@year)
#' @autoglobal
#' @noRd
query_string_as_list <- function(url, n = NULL) {
  stringi::stri_extract(
    url,
    regex = str_look(pattern = "[?]", look = "ahead"),
    opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
  ) |>
    stringi::stri_split(fixed = "&") |>
    rlang::set_names(n)
}

#' @autoglobal
#' @noRd
parse_string <- function(resp, query = NULL) {
  f <- function(x, qry = NULL) {
    RcppSimdJson::fparse(httr2::resp_body_string(x), query = qry)
  }

  if (!is.null(query)) {
    switch(
      query,
      count      = return(f(resp) |> _[["count"]]),
      found_rows = return(f(resp) |> _[["found_rows"]]),
      names      = return(f(resp) |> rlang::names2()),
      results    = return(f(resp) |> _[["results"]]),
      total_rows = return(f(resp) |> _[["total_rows"]]),
      return(f(resp, qry = query))
    )
  }
  f(resp)
}

#' @autoglobal
#' @noRd
map_perform_parallel <- function(x, query = NULL) {
  purrr::map(x, httr2::request) |>
    httr2::req_perform_parallel(on_error = "continue") |>
    httr2::resps_successes() |>
    purrr::map(function(x) parse_string(x, query = query))
}

#' @autoglobal
#' @noRd
api_limit <- function(api, call = rlang::caller_env()) {
  switch(
    api,
    caid = 8000L,
    care = 5000L,
    prov = 1500L,
    hgov = 500L,
    open = 500L,
    cli::cli_abort(c("x" = "API {.val {api}} not recognized."), call = call)
  )
}

# base_url
# care_current: base_url + stats + ? + offset + limit
# care_temporal: base_url + data + offset
# default: base_url + offset + size + count + results
#' @autoglobal
#' @noRd
append_url <- function(url, api = "default") {
  paste0(url, switch(
    api,
    stats = "/stats?offset=0&size=1",
    care = "?offset=0&size=1",
    default = "?count=true&results=true&offset=0&limit=1",
  ))
}

#' @autoglobal
#' @noRd
perform_simple <- function(req, ...) {
  req |>
    httr2::req_perform() |>
    httr2::resp_body_json(
      simplifyVector = TRUE,
      check_type = FALSE,
      ...)
}

#' Generate API Request "Offset" Sequence
#'
#' @param nres  `<int>` Number of results in an API request
#' @param limit `<int>` API rate limit
#' @param type  `<chr>` Return type, either `"seq"` or `"size"` (default)
#' @returns Depending on `type` input and conditions listed below, returns:
#'    * `n == 0`: returns `0`
#'    * `n <= limit`: returns `n`
#'    * `n > limit`:
#'       * `type = "seq"`: integer sequence beginning at `0`, incremented by `limit`, of length equal to `ceiling(n / limit)`.
#'       * `type = "size"`: single integer equal to length of sequence returned by `type = "seq"`.
#'
#' @examplesIf interactive()
#' offset(100, 10)
#' offset(100, 10, "seq")
#'
#' offset(10, 100)
#' offset(10, 100, "seq")
#'
#' offset(47984, 5000)
#' offset(47984, 5000, "seq")
#'
#' offset(147984, 2000)
#' offset(147984, 2000, "seq")
#' @autoglobal
#' @noRd
offset <- function(nres, limit, type = "size") {

  check_number_whole(nres, min = 0)
  check_number_whole(limit, min = 1)

  if (nres == 0L) {
    return(0L)
  }

  switch (
    match.arg(type, c("size", "seq")),
    size = seq_size(from = 0L, to = nres, by = limit),
    seq  = seq_(    from = 0L, to = nres, by = limit)
  )
}

#' @autoglobal
#' @noRd
page_count <- function(total, limit) {
  purrr::map_int(total, \(x) offset(nres = x, limit = limit))
}
