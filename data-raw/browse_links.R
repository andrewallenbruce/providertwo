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
