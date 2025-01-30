perform_request <- \(request, query, limit) {

  req <- req_url_query(
    request,
    !!!format_query(query),
    size = limit)

  n <- req_url_path_append(req, "stats") |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    getelem("found_rows")

  if (n == 0) {
    abort(
      message = c("!" = glue("{n} results found.")),
      use_cli_format = TRUE,
      call = caller_env(),
      class = "abort_no_results"
    )
  }

  off_len <- length(offset_sequence(n = n, limit = limit))

  if (off_len == 1) {
    return(
      qTBL(
        fparse(
          resp_body_string(
            req_perform(req))) |>
          _[["data"]]) |>
        set_names(names(query)) |>
        map_na_if()
    )
  }

  if (off_len > 1) {
    return(
      req_perform_iterative(
        req,
        next_req        = iterate_with_offset(
          param_name    = "offset",
          start         = 0,
          offset        = limit,
          resp_complete = is_complete_with_limit(limit))) |>
        map(\(x) qTBL(fparse(resp_body_string(x)) |> _[["data"]])) |>
        rowbind() |>
        set_names(names(query)) |>
        map_na_if()
    )
  }
}
