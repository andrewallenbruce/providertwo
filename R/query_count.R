#' Return the number of results for a given query
#'
#' @param obj An object of class `<class_care>`, `<class_group>`, or `<class_collection>`.
#'
#' @param ... Additional arguments passed to the query.
#'
#' @returns An `<int>` representing the number of results found.
#'
#' @examples
#' end_dial <- endpoint("care_dial_end")
#' qr_state <- query(state = in_(c("CA", "GA", "NY"), care = TRUE),
#'                   .type = "medicare")
#'
#' ncount(end_dial, qr_state)
#'
#' end_rhc <- collection("care_rhc")
#' qr_own  <- query(STATE = in_(c("CA", "GA", "NY"), care = TRUE),
#'                 `STATE - OWNER` = in_(c("CA", "GA", "NY"), care = TRUE),
#'                 .type = "medicare")
#'
#' ncount(end_rhc, qr_own)
#'
#' @autoglobal
#' @export
ncount <- new_generic("ncount", "obj")

method(ncount, class_group) <- function(obj, query = NULL) {
  prop(obj, "members") |>
    walk(\(x) ncount(x, query = query))
}

method(ncount, class_care) <- function(obj, query = NULL) {

  q <- S7_data(prop(obj, "identifier")) |> url_modify(query = query@string)
  n <- req_url_path_append(request(q), "stats") |> perform_simple() |> _$data

  cli::cli_text(
    cli::col_black(cli::style_bold("Results ")),
    cli::col_silver(paste0(strrep(cli::symbol$stop, 8)))
    )

  cli::cli_text(
    cli::col_silver(fmt_int(n$found_rows)),
    " (",
    roundup(n$found_rows / n$total_rows),
    " %)"
    )

  cli::cli_text(
    cli::col_silver(paste0(strrep(cli::symbol$double_line, 8))),
    cli::style_italic(" Query"))

  cli::cli_bullets(
    paste(
      cli::style_bold(cli::col_yellow(names(query@input))),
      cli::col_silver(cli::symbol$double_line),
      cli::col_yellow(query@input),
      sep = " "
      ))

  cli::cli_text(
    cli::col_silver(paste0(strrep(cli::symbol$double_line, 8))),
    cli::style_italic(" Endpoint"))

  cli::cli_text(cli::col_cyan(obj@metadata$title))

  invisible(q)
}
