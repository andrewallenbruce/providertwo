#' Browse Links
#' @param x `<backend>` or `<group>` object
#' @param active `<logical>` whether to open links in browser (default: `TRUE` if interactive)
#' @param call `<env>` environment to use for error reporting
#' @returns `<list>` of catalogs
#' @examplesIf rlang::is_interactive()
#' care_endpoint("care_enrollees") |> browse_link(active = FALSE)
#' try(caid_endpoint("managed_longterm") |> browse_link(active = FALSE))
#' @autoglobal
#' @noRd
browse_link <- function(x, active = is_interactive(), call = caller_env()) {

  x <- metadata_(x) |> get_elem(c("dictionary", "site", "references"))

  if (is_empty(x)) cli_abort(c("x" = "{.val {x}} has no browsable links."), call = call)

  x <- x[path_ext(delist(x)) %in_% c("", "pdf")]

  if (active) {
    cli_alert_info("Opening {.href [{toupper(names(x))}]({delist(x)})} Links")
    walk(x, browseURL)
    invisible(x)
  } else {
    cli_alert_info("{.emph {toupper(names(x))}} Link{?s}: {.url {delist(x)}}")
    invisible(x)
  }
}

#' @autoglobal
#' @noRd
tidy_resources <- function(x) {
  x |>
    fcompute(
      year     = extract_year(name),
      file     = rp_dbl_space(greplace(name, " [0-9]{4}|[0-9]{4} ", "")),
      size     = as_fs_bytes(fileSize),
      ext      = tolower(path_ext(downloadURL)),
      download = downloadURL
    ) |>
    f_fill(year) |>
    roworder(-year, ext, -size) |>
    as_tbl()
}

#' List resources
#' @param x `class_endpoint` or `class_temporal` object
#' @returns A list of available API resources, either for download or to browse online.
#' @examples
#' care_endpoint("care_enrollees") |> list_resources()
#' care_temporal("quality_payment") |> list_resources()
#' care_group("care_hha") |> list_resources()
#' care_group("care_inpatient") |> list_resources()
#' @autoglobal
#' @export
list_resources <- new_generic("list_resources", "x", function(x) {
  S7_dispatch()
})

method(list_resources, class_endpoint) <- function(x) {
  if (clog_(x) != "care") return(invisible(NULL))
  resources_(x) |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(function(resp)
      parse_string(resp, query = "/data") |>
        tidy_resources()) |>
    pluck(1L)
}

method(list_resources, class_temporal) <- function(x) {
  if (clog_(x) != "care") return(invisible(NULL))
  resources_(x) |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(function(resp)
      parse_string(resp, query = "/data")) |>
    tidy_resources()
}

method(list_resources, class_group) <- function(x) {
  members_(x) |>
    map(list_resources, .progress = TRUE) |>
    name_members_(x)
}
