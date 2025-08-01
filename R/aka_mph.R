#' @include aka_end.R
#' @include aka_col.R
NULL

# ---- end_nms ----
#' @autoglobal
#' @noRd
rlang::on_load({
  endpoint_regex <- c(
    end_prov$current,
    end_hgov$current,
    end_caid$current,
    end_care$current,
    end_open$current,
    end_hgov$temporal,
    end_caid$temporal,
    end_care$temporal,
    end_open$temporal)
  endpoint_names <- oomph::mph_init(names(endpoint_regex))
})


#' @autoglobal
#' @noRd
rex_endpoint <- function(x) {
  endpoint_regex[oomph::mph_match(x, endpoint_names)]
}

#' @autoglobal
#' @noRd
rlang::on_load({
  point <- list(
    current = oomph::mph_init(
      names(
    c(
      end_prov$current,
      end_hgov$current,
      end_caid$current,
      end_care$current,
      end_open$current
    )
  )), temporal = oomph::mph_init(
    names(
    c(
      end_hgov$temporal,
      end_caid$temporal,
      end_care$temporal,
      end_open$temporal
    )
  )))
})

#' @autoglobal
#' @noRd
rlang::on_load({
  clog <- list(
    care = oomph::mph_init(names(c(end_care$current, end_care$temporal))),
    prov = oomph::mph_init(names(end_prov$current)),
    open = oomph::mph_init(names(c(end_open$current, end_open$temporal))),
    caid = oomph::mph_init(names(c(end_caid$current, end_caid$temporal))),
    hgov = oomph::mph_init(names(c(end_hgov$current, end_hgov$temporal)))
  )
})

#' @autoglobal
#' @noRd
is_care <- function(x) {
  !is.na(oomph::mph_match(x, clog$care))
}

#' @autoglobal
#' @noRd
is_caid <- function(x) {
  !is.na(oomph::mph_match(x, clog$caid))
}

#' @autoglobal
#' @noRd
is_prov <- function(x) {
  !is.na(oomph::mph_match(x, clog$prov))
}

#' @autoglobal
#' @noRd
is_open <- function(x) {
  !is.na(oomph::mph_match(x, clog$open))
}

#' @autoglobal
#' @noRd
is_hgov <- function(x) {
  !is.na(oomph::mph_match(x, clog$hgov))
}

#' @autoglobal
#' @noRd
is_current <- function(x) {
  !is.na(oomph::mph_match(x, point$current))
}

#' @autoglobal
#' @noRd
is_temporal <- function(x) {
  !is.na(oomph::mph_match(x, point$temporal))
}

#' @autoglobal
#' @noRd
catalog_type <- function(x, call = caller_env()) {
  res <- nif(
    is_care(x),
    "care",
    is_caid(x),
    "caid",
    is_prov(x),
    "prov",
    is_open(x),
    "open",
    is_hgov(x),
    "hgov"
  )

  res %|% cli::cli_abort(c("x" = "{.val {x}} does not belong to a catalog."), call = call)
}

#' @autoglobal
#' @noRd
point_type <- function(x, call = caller_env()) {
  res <- nif(is_current(x), "current", is_temporal(x), "temporal")
  res %|% cli::cli_abort(c("x" = "{.val {x}} is not a valid endpoint type."), call = call)
}

