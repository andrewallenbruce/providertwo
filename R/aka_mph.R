#' @include aka_end.R
#' @include aka_col.R
NULL

# ---- end_nms ----
#' @autoglobal
#' @noRd
end_nms <- mph_init(
  names(c(
    end_prov$current,
    end_hgov$current,
    end_caid$current,
    end_care$current,
    end_open$current,
    end_hgov$temporal,
    end_caid$temporal,
    end_care$temporal,
    end_open$temporal
  )))

# ---- end_rex ----
#' @autoglobal
#' @noRd
end_rex <- unlist(
  c(
    end_prov$current,
    end_hgov$current,
    end_caid$current,
    end_care$current,
    end_open$current,
    end_hgov$temporal,
    end_caid$temporal,
    end_care$temporal,
    end_open$temporal
  ),
  use.names = FALSE
)


#' @autoglobal
#' @noRd
alias_rex <- function(x) {
  end_rex[mph_match(x, end_nms)]
}

#' @autoglobal
#' @noRd
point <- list(
  current = mph_init(names(
    c(
      end_prov$current,
      end_hgov$current,
      end_caid$current,
      end_care$current,
      end_open$current
    ))),
  temporal = mph_init(names(
    c(
      end_hgov$temporal,
      end_caid$temporal,
      end_care$temporal,
      end_open$temporal
    ))))

#' @autoglobal
#' @noRd
clog <- list(
  care = mph_init(names(c(
    end_care$current,
    end_care$temporal
  ))),
  prov = mph_init(names(
    end_prov$current
    )),
  open = mph_init(names(c(
    end_open$current,
    end_open$temporal
  ))),
  caid = mph_init(names(c(
    end_caid$current,
    end_caid$temporal
  ))),
  hgov = mph_init(names(c(
    end_hgov$current,
    end_hgov$temporal
  )))
)

#' @autoglobal
#' @noRd
is_care <- function(x) {
  !is.na(mph_match(x, clog$care))
}

#' @autoglobal
#' @noRd
is_caid <- function(x) {
  !is.na(mph_match(x, clog$caid))
}

#' @autoglobal
#' @noRd
is_prov <- function(x) {
  !is.na(mph_match(x, clog$prov))
}

#' @autoglobal
#' @noRd
is_open <- function(x) {
  !is.na(mph_match(x, clog$open))
}

#' @autoglobal
#' @noRd
is_hgov <- function(x) {
  !is.na(mph_match(x, clog$hgov))
}

#' @autoglobal
#' @noRd
is_current <- function(x) {
  !is.na(mph_match(x, point$current))
}

#' @autoglobal
#' @noRd
is_temporal <- function(x) {
  !is.na(mph_match(x, point$temporal))
}

#' @autoglobal
#' @noRd
clog_type <- function(x, call = caller_env()) {
  res <- nif(is_care(x), "care",
             is_caid(x), "caid",
             is_prov(x), "prov",
             is_open(x), "open",
             is_hgov(x), "hgov")

  res %|% cli_abort(c("x" = "{.val {x}} does not belong to a catalog."), call = call)
}

#' @autoglobal
#' @noRd
point_type <- function(x, call = caller_env()) {
  res <- nif(is_current(x), "current", is_temporal(x), "temporal")
  res %|% cli_abort(c("x" = "{.val {x}} is not a valid endpoint type."), call = call)
}


# ---- col_nms ----
#' @autoglobal
#' @noRd
col_nms <- mph_init(
  names(c(
    col_prov,
    col_caid,
    col_care,
    col_open
  )))

# ---- col_rex ----
#' @autoglobal
#' @noRd
col_rex <- c(
  col_prov,
  col_caid,
  col_care,
  col_open
)

#' @autoglobal
#' @noRd
is_collection <- function(x) {
  !is.na(mph_match(x, col_nms))
}

#' @autoglobal
#' @noRd
collect_rex <- function(x, call = caller_env()) {
  if (!is_collection(x))
    cli_abort(c("x" = "{.val {x}} is not a valid collection."), call = call)

  col_rex[mph_match(x, col_nms)] |> unname() |> yank()
}

