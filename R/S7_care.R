#' @autoglobal
#' @noRd
Care <- new_class(name = "Care", package = NULL)

#' @noRd
#' @autoglobal
care_dimensions <- new_class(
  name = "care_dimensions",
  package = NULL,
  properties = list(
    limit = class_integer,
    rows = class_integer,
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows,
                    self@limit)
    ),
    fields = new_property(
      class_character,
      getter = function(self)
        as.list(self@fields) |>
        set_names(self@fields)
    )
  ),
  constructor = function(x) {

    if (grepl("data-viewer$", x$identifier, perl = TRUE)) {

      x <- request(x$identifier) |>
        req_url_query(offset = 0L, size = 1L) |>
        perform_simple() |>
        _[["meta"]]

      new_object(
        S7_object(),
        limit  = 5000L,
        rows   = x$total_rows,
        fields = x$headers
      )

    } else {
      x <- request(x$identifier) |>
        req_url_query(offset = 0L, size = 1L)

      new_object(
        S7_object(),
        limit  = 5000L,
        rows   = req_url_path_append(x, "stats") |>
          perform_simple() |>
          _[["total_rows"]],
        fields = perform_simple(x) |>
          names()
      )
    }
  }
)

#' @noRd
#' @autoglobal
care_metadata <- new_class(
  name = "care_metadata",
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

#' Medicare Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<careMain>` object.
#' @examples
#' careMain("contact")
#' careMain("crosswalk")
#' careMain("CARE_dialysis")
#' careMain("enrollees")
#' careMain("facilities")
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
    identifier  = class_character,
    dimensions  = care_dimensions,
    metadata    = care_metadata,
    resources   = class_character
  ),
  constructor = function(alias) {

    x <- care_main(alias)

    new_object(
      Care(),
      title       = x$title,
      metadata    = care_metadata(x),
      dimensions  = care_dimensions(x),
      identifier  = x$identifier,
      resources   = x$resources
    )
  }
)

#' @noRd
#' @autoglobal
care_temp_metadata <- new_class(
  name = "care_temp_metadata",
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
    metadata    = care_temp_metadata,
    dimensions  = care_dimensions,
    endpoints   = class_list
  ),
  constructor = function(alias) {

    x <- care_temp(alias)

    new_object(
      Care(),
      title       = x$title,
      metadata    = care_temp_metadata(x),
      dimensions  = care_dimensions(x),
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
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members, careMain) |>
        set_names(self@members)
    )
  ),
  constructor = function(alias) {

    x <- care_group(alias)

    new_object(
      Care(),
      group  = x$group,
      members = x$alias
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
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members, careTemp) |>
        set_names(self@members)
    )
  ),
  constructor = function(alias) {

    x <- care_temp_group(alias)

    new_object(
      Care(),
      group   = x$group,
      members = x$alias
    )
  }
)
