#' Mount [pins][pins::pins-package] board
#' @param source `<chr>` `"local"` or `"remote"`
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#' @autoglobal
#' @noRd
mount_board <- function(source = c("local", "remote")) {

  gh_raw  <- function(x) paste0("https://raw.githubusercontent.com/", x)
  gh_path <- gh_raw(paste0("andrewallenbruce/", utils::packageName(), "/master/inst/extdata/pins/"))

  switch(
    match.arg(source),
    local  = pins::board_folder(
      fs::path_package("extdata/pins",
                       package = utils::packageName())),
    remote = pins::board_url(gh_path))
}

#' Get pinned dataset
#' @param pin `<chr>` string name of pinned dataset
#' @param ... additional arguments passed to mount_board()
#' @returns `<tibble>` or `<data.frame>`
#' @autoglobal
#' @noRd
get_pin <- function(pin, ...) {

  board <- mount_board(...)

  pin <- rlang::arg_match0(pin, list_pins())

  pins::pin_read(board, pin)

}

#' List pins
#' @param ... arguments to pass to mount_board()
#' @returns `<chr>` vector of named pins
#' @autoglobal
#' @noRd
list_pins <- function(...) {

  board <- mount_board(...)

  pins::pin_list(board)

}
