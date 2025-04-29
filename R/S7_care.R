#' @noRd
#' @autoglobal
care_dimensions <- new_class(
  name       = "care_dimensions",
  package    = NULL,
  properties = list(
    limit    = class_integer,
    rows     = class_integer,
    pages    = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, self@limit)
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

#' @noRd
#' @autoglobal
care_metatemp <- new_class(
  name = "care_metatemp",
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

#' Medicare Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<care_endpoint>` object.
#' @examples
#' care_endpoint("contact")
#' care_endpoint("crosswalk")
#' care_endpoint("CARE_dialysis")
#' care_endpoint("enrollees")
#' care_endpoint("facilities")
#' care_endpoint("IQIES")
#' care_endpoint("laboratories")
#' care_endpoint("long_term")
#' care_endpoint("opt_out")
#' care_endpoint("order_refer")
#' care_endpoint("RBCS")
#' care_endpoint("transparency")
#' @autoglobal
#' @rdname care_endpoint
#' @export
care_endpoint <- new_class(
  name       = "care_endpoint",
  package    = NULL,
  properties = list(
    title       = class_character,
    identifier  = class_character,
    dimensions  = care_dimensions,
    metadata    = care_metadata,
    resources   = class_character
  ),
  constructor = function(alias) {

    x <- select_care(alias)

    new_object(
      S7_object(),
      title       = x$title,
      metadata    = care_metadata(x),
      dimensions  = care_dimensions(x),
      identifier  = x$identifier,
      resources   = x$resources
    )
  }
)

#' Medicare Temporal Endpoint
#' @param alias `<chr>` title alias
#' @returns An S7 `<care_temporal>` object.
#' @examples
#' care_temporal("quality_payment")
#' @autoglobal
#' @rdname care_temporal
#' @export
care_temporal <- new_class(
  name       = "care_temporal",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = care_metatemp,
    dimensions  = care_dimensions,
    endpoints   = class_list
  ),
  constructor = function(alias) {

    x <- select_care_temp(alias)

    new_object(
      S7_object(),
      title       = x$title,
      metadata    = care_metatemp(x),
      dimensions  = care_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)

#' Medicare Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<care_group>` object.
#' @examples
#' care_group("HHA")
#' care_group("hospice")
#' care_group("hospital")
#' care_group("RHC")
#' care_group("FQHC")
#' care_group("pending")
#' care_group("reassignment")
#' care_group("SNF")
#' @autoglobal
#' @rdname care_group
#' @export
care_group <- new_class(
  name       = "care_group",
  package    = NULL,
  properties = list(
    group = class_character,
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members, care_endpoint) |>
        set_names(self@members)
    )
  ),
  constructor = function(alias) {

    x <- select_care_group(alias)

    new_object(
      S7_object(),
      group   = x$group,
      members = x$alias
    )
  }
)

#' Group of Medicare Temporal Endpoints
#' @param alias `<chr>` title alias
#' @returns An S7 `<care_troup>` object.
#' @examples
#' care_troup("inpatient")
#' care_troup("outpatient")
#' care_troup("utilization")
#' care_troup("suppliers")
#' care_troup("prescribers")
#' care_troup("staffing")
#' @autoglobal
#' @rdname care_troup
#' @export
care_troup <- new_class(
  name       = "care_troup",
  package    = NULL,
  properties = list(
    group = class_character,
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members, care_temporal) |>
        set_names(self@members)
    )
  ),
  constructor = function(alias) {

    x <- select_care_troup(alias)

    new_object(
      S7_object(),
      group   = x$group,
      members = x$alias
    )
  }
)
