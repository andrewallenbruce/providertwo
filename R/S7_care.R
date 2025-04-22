#' @autoglobal
#' @noRd
Care <- new_class(name = "Care", package = NULL)

#' @noRd
#' @autoglobal
careMeta <- new_class(
  name = "careMeta",
  package = NULL,
  properties = list(
    description = class_character,
    temporal    = class_character,
    periodicity = class_character,
    modified    = new_union(class_character, class_Date),
    dictionary  = class_character,
    site        = class_character,
    references  = class_character,
    download    = class_character
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      temporal    = x$temporal,
      modified    = x$modified,
      periodicity = x$periodicity,
      dictionary  = x$dictionary,
      site        = x$site,
      references  = x$references,
      download    = x$download
    )
  }
)

#' @noRd
#' @autoglobal
careDim <- new_class(
  name = "careDim",
  package = NULL,
  properties = list(
    rows = new_property(
      class_integer,
      default = 0L
      ),
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, 5000L)
      ),
    fields = class_character
    ),
  constructor = function(x) {

    x <- x$identifier |>
      request() |>
      req_url_query(offset = 0L, size = 1L) |>
      perform_simple() |>
      _[["meta"]]

    new_object(
      S7_object(),
      rows   = x$total_rows,
      fields = x$headers)
  }
)

#' Medicare Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<careMain>` object.
#' @examples
#' careMain("contact")
#' careMain("crosswalk")
#' careMain("dialysis")
#' careMain("enrollees")
#' careMain("facilities")
#' careMain("hospice_acute")
#' careMain("IQIES")
#' careMain("laboratories")
#' careMain("long_term")
#' careMain("opt_out")
#' careMain("order_refer")
#' careMain("RBCS")
#' careMain("transparency")
#' @autoglobal
#' @rdname careMain
#' @export
careMain <- new_class(
  parent     = Care,
  name       = "careMain",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = careMeta,
    dimensions  = careDim,
    identifier  = class_character,
    resources   = class_character
  ),
  constructor = function(alias) {

    x <- care_main(alias)

    new_object(
      Care(),
      title       = x$title,
      metadata    = careMeta(x),
      dimensions  = careDim(x),
      identifier  = x$identifier,
      resources   = x$resources
    )
  }
)

#' @noRd
#' @autoglobal
careTempMeta <- new_class(
  name = "careMeta",
  package = NULL,
  properties = list(
    description = class_character,
    periodicity = class_character,
    dictionary  = class_character,
    site        = class_character
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      periodicity = x$periodicity,
      dictionary  = x$dictionary,
      site        = x$site
    )
  }
)

#' @noRd
#' @autoglobal
careTempDim <- new_class(
  name = "careTempDim",
  package = NULL,
  properties = list(
    rows = new_property(
      class_integer,
      default = 0L
    ),
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, 5000L)
    ),
    fields = class_character
  ),
  constructor = function(x) {

    x <- x$identifier |>
      request() |>
      req_url_query(offset = 0L, size = 1L)

    new_object(
      S7_object(),
      rows = x |> req_url_path_append("stats") |> perform_simple() |>_[["total_rows"]],
      fields = x |> perform_simple() |>names()
    )
  }
)

#' Medicare Temporal Endpoint
#' @param alias `<chr>` title alias
#' @returns An S7 `<careTemp>` object.
#' @examples
#' careTemp("quality_payment")
#' @autoglobal
#' @rdname careTemp
#' @export
careTemp <- new_class(
  parent     = Care,
  name       = "careTemp",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = careTempMeta,
    dimensions  = careTempDim,
    endpoints   = class_list
  ),
  constructor = function(alias) {

    x <- care_temp(alias)

    new_object(
      Care(),
      title       = x$title,
      metadata    = careTempMeta(x),
      dimensions  = careTempDim(x),
      endpoints   = slt(x$endpoints, -temporal, -filetype)
    )
  }
)

#' Medicare Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<careGroup>` object.
#' @examples
#' careGroup("HHA")
#' careGroup("hospice")
#' careGroup("hospital")
#' careGroup("RHC")
#' careGroup("FQHC")
#' careGroup("pending")
#' careGroup("reassignment")
#' careGroup("SNF")
#' @autoglobal
#' @rdname careGroup
#' @export
careGroup <- new_class(
  parent     = Care,
  name       = "careGroup",
  package    = NULL,
  properties = list(
    group = class_character,
    members = class_list),
  constructor = function(alias) {

    x <- care_group(alias)

    new_object(
      Care(),
      group  = x$group,
      members = map(x$alias, careMain) |>
        set_names(x$alias)
    )
  }
)

#' Group of Medicare Temporal Endpoints
#' @param alias `<chr>` title alias
#' @returns An S7 `<careTempGroup>` object.
#' @examples
#' careTempGroup("inpatient")
#' careTempGroup("outpatient")
#' careTempGroup("utilization")
#' careTempGroup("suppliers")
#' careTempGroup("prescribers")
#' careTempGroup("staffing")
#' @autoglobal
#' @rdname careTempGroup
#' @export
careTempGroup <- new_class(
  parent     = Care,
  name       = "careTempGroup",
  package    = NULL,
  properties = list(
    group = class_character,
    # years = class_integer,
    members = class_list
  ),
  constructor = function(alias) {

    x <- care_temp_group(alias)

    new_object(
      Care(),
      group  = x$group,
      members = map(x$alias, careTemp) |>
        set_names(x$alias)
    )
  }
)
