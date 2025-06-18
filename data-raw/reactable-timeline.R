library(tidyverse)
library(lubridate) # needed for time_length()
library(htmltools) # needed for div()
library(reactable)

sales <- readr::read_csv(
  "https://raw.githubusercontent.com/sccmckenzie/reactable-timeline/master/sales.csv"
  )

convert_timestamps <- function(t1, t2) {
  # extract timestamp range
  t01 <- min(t1)
  t02 <- max(t2)

  # normalize event timestamps within total range
  left  <- time_length(t1 - t01) / time_length(t02 - t01)
  width <- time_length(t2 - t1) / time_length(t02 - t01)

  # splice values into list
  out <- list()

  for (i in 1:length(left)) {
    out[[i]] <- list(left[i], width[i])
  }
  out
}

sales |>
  mutate(timeline = convert_timestamps(enter, exit))


create_timeline_bar <- function(left  = 0,
                                width = "100%",
                                fill  = "#00bfc4") {

  left  <- scales::percent(left)
  width <- scales::percent(width)

  bar <- htmltools::div(
    style      = list(
    position   = "absolute",
    left       = left,
    background = fill,
    width      = width,
    height     = "140%"
    )
  )

  chart <- htmltools::div(
    style = list(
    flexGrow   = 1,
    position   = "relative",
    display    = "flex",
    alignItems = "center",
    height     = "100%"),
  bar)

  htmltools::div(
    style  = list(
    height = "100%"),
    chart)
}

sales |>
  mutate(timeline = convert_timestamps(enter, exit)) |>
  reactable(
    fullWidth  = FALSE,
    compact    = TRUE,
    bordered   = TRUE,
    columns    = list(
      c_id     = colDef(width = 55),
      revenue  = colDef(
        width  = 90,
        show   = FALSE,
        format = colFormat(digits = 1)
        ),
      enter       = colDef(width = 80),
      exit        = colDef(width = 80),
      timeline    = colDef(
        width     = 90,
        cell      = function(value) {
          create_timeline_bar(
            left  = value[[1]],
            width = value[[2]]
        )
      }
        )
      )
  )
