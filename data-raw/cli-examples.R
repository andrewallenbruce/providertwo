collapse_new_lines <- function(x) {
  gsub(x = x,
       pattern = "(\\n\\s*){2,}",
       replacement = "\n\n",)
}

#' Various utilities for formatting ANSI output
#' @keywords internal
#' @name ansi
NULL

#' @describeIn ansi
#' Erase the current line
#'
#' @param n The number of characters to erase, defaults to an empty string,
#'   which will erase the entire line.
ansi_line_erase <- function(n = "") {
  paste0("\033[", n, "K")
}

#' @describeIn ansi
#' Offset the cursor by a relative number of lines
#'
#' @param n The number of lines to move. Positive is up, negative is down.
ansi_move_line_rel <- function(n) {
  paste0("\033[", abs(n), if (n > 0L)
    "F"
    else
      "E")
}

#' Create a 'cli' Spinner With Suppressed Output
#'
#' 'cli' will implicitly push spinner output to various output streams,
#' affecting the terminal cursor position. To allow for a terminal interface
#' that has spinners above the last line, this function suppresses the output
#' and simply returns its frame contents.
#'
#' @param ... passed to [cli::make_spinner]
#' @param stream passed to [cli::make_spinner], defaults to a null file device
#' @return A interface similar to a 'cli' spinner, but with suppressed output
#'
#' @importFrom cli make_spinner
#' @keywords internal
silent_spinner <- function(..., stream = devnull()) {
  spinner <- cli::make_spinner(..., stream = stream)

  spin <- function(...) {
    spinner$spin(...)
    with(environment(spinner$spin), c_spinner$frames[[c_state]])
  }

  list(spin = spin, final = spinner$final)
}

#' Reuse or Create A Null File Connection
#' @keywords internal
devnull <- function() {
  cons <- showConnections(all = TRUE)[, "description"]
  if (length(w <- which(nullfile() == cons))) {
    getConnection(names(cons)[[w[[1]]]])
  } else {
    file(nullfile())
  }
}

format_time <- function(x) {
  n <- unclass(x)
  sprintf("%.01f%s", n, substring(attr(n, "units"), 1, 1))
}

str_pad <- function(x, n) {
  x <- format(x)
  paste0(strrep(" ", n - nchar(x)), x)
}

#' Internal Utilities for Command-line Output
#'
#' Various helper functions for consistent cli output, including theming and
#' formatting.
#'
#' @param status,ok,notes,warnings,errors `character[1L]` A value to include
#'   in the respective columns of the table. Will be coerced to `character`
#'   if another type is provided.
#' @param msg `character[1L]` A message to include to the right of the  table
#'   row entry.
#' @param ...,.envir Additional arguments passed to [`cli::cli_div()`]
#'
#' @name cli
#' @keywords internal
NULL

#' @name cli
cli_table_row <- function(status,
                          ok = "OK",
                          notes = "N",
                          warnings = "W",
                          errors = "E",
                          msg = "",
                          title = FALSE) {
  cli_theme()
  status <- trimws(as.character(status))
  status <- switch(
    status,
    "1" = ,
    "2" = ,
    "OK" = ,
    "3" = ,
    "NONE" = cli::format_inline("{.success \u2713}"),
    "4" = ,
    "NOTE" = cli::format_inline("{.note !}"),
    "5" = ,
    "WARNING" = cli::format_inline("{.warn ?}"),
    "6" = ,
    "ERROR" = cli::format_inline("{.err \u2a2f}"),
    if (title)
      cli::col_none(cli::style_bold(status))
    else
      status
  )

  ok <- str_pad(ok, n = 2)
  notes <- str_pad(notes, n = 2)
  warnings <- str_pad(warnings, n = 2)
  errors <- str_pad(errors, n = 2)

  if (title) {
    ok <- cli::col_none(cli::style_bold(ok))
    notes <- cli::col_none(cli::style_bold(notes))
    warnings <- cli::col_none(cli::style_bold(warnings))
    errors <- cli::col_none(cli::style_bold(errors))
  } else {
    ok <- cli::format_inline("{.ok {ok}}")
    notes <- cli::format_inline("{.note {notes}}")
    warnings <- cli::format_inline("{.warn {warnings}}")
    errors <- cli::format_inline("{.err {errors}}")
  }

  fmt <- "\u2502 {status} \u2502 {ok} {notes} {warnings} {errors} \u2502 {msg}"
  cli::format_inline(fmt)
}

#' @name cli
cli_theme <- function(..., .envir = parent.frame()) {
  cli_div(
    ...,
    .envir = .envir,
    theme = list(
      span.ok = list(),
      span.time_active = list(color = "cyan"),
      span.time_taken = list(color = "grey"),
      span.success = list(color = "green"),
      span.err = list(color = "yellow"),
      span.warn = list(color = "magenta"),
      span.note = list(color = "blue")
    )
  )
}

cli_wrap_lines <- function(text, w = cli::console_width()) {
  n <- cli::ansi_nchar(text)
  cli::ansi_substring(text, seq_len(ceiling(n / w)) * w - w + 1, seq_len(ceiling(n / w)) * w)
}
