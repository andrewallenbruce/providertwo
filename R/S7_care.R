#' @include S7_classes.R
NULL

#' @noRd
#' @autoglobal
care_dimensions <- function(x) {
  req <- req_url_query(request(x$identifier), offset = 0L, size = 1L)

  if (grepl("data-viewer$", x$identifier, perl = TRUE)) {
    x <- perform_simple(req)$meta

    class_dimensions(
      limit  = 5000L,
      rows   = x$total_rows,
      fields = x$headers
    )

  } else {
    class_dimensions(
      limit  = 5000L,
      rows   = perform_simple(req_url_path_append(req, "stats"))$total_rows,
      fields = names(perform_simple(req))
    )
  }
}

#' @noRd
#' @autoglobal
care_metadata <- new_class(
  name = "care_metadata",
  parent = class_metadata,
  package = NULL,
  properties = list(
    temporal    = class_character,
    periodicity = class_character,
    download    = class_character,
    resources   = class_character,
    dictionary  = class_character,
    site        = class_character,
    references  = class_character
  ),
  constructor = function(x) {
    new_object(
      class_metadata(),
      title       = x$title,
      description = x$description,
      modified    = x$modified %||% NA_character_,
      temporal    = x$temporal,
      periodicity = x$periodicity,
      download    = x$download,
      resources   = x$resources,
      dictionary  = x$dictionary,
      site        = x$site,
      references  = x$references
    )
  }
)

#' @noRd
#' @autoglobal
temp_metadata <- new_class(
  name = "temp_metadata",
  parent = class_metadata,
  package = NULL,
  properties = list(
    periodicity = class_character,
    dictionary  = class_character,
    site        = class_character
  ),
  constructor = function(x) {
    new_object(
      class_metadata(),
      title       = x$title,
      description = x$description,
      modified    = x$modified %||% NA_character_,
      periodicity = x$periodicity,
      dictionary  = x$dictionary,
      site        = x$site
    )
  }
)

#' Medicare API Endpoint Classes
#' @name medicare
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<care_endpoint>`, `<care_group>`, `<care_temporal>`, or `<care_troup>` object
#' @examples
#' care_endpoint("CARE_dialysis")
#' care_endpoint("long_term")
#' care_endpoint("transparency")
#' care_temporal("quality_payment")
#' care_group("HHA")
#' care_group("hospice")
#' care_group("hospital")
#' care_group("RHC")
#' care_group("FQHC")
#' care_group("pending")
#' care_group("reassignment")
#' care_group("SNF")
#' care_troup("inpatient")
#' care_troup("outpatient")
#' care_troup("utilization")
#' care_troup("suppliers")
#' care_troup("prescribers")
#' care_troup("staffing")
NULL

#' @autoglobal
#' @rdname medicare
#' @export
care_endpoint <- new_class(
  name        = "care_endpoint",
  parent      = class_endpoint,
  package     = NULL,
  constructor = function(alias) {

    x <- select_care(alias)

    new_object(
      class_endpoint(),
      identifier  = x$identifier,
      metadata    = care_metadata(x),
      dimensions  = care_dimensions(x)
    )
  }
)

#' @autoglobal
#' @rdname medicare
#' @export
care_group <- new_class(
  name        = "care_group",
  parent      = class_group,
  package     = NULL,
  constructor = function(alias) {

    x <- select_care_group(alias)

    new_object(
      class_group(),
      group   = x$group,
      members = set_names(map(x$alias, care_endpoint), x$alias)
    )
  }
)

#' @autoglobal
#' @rdname medicare
#' @export
care_temporal <- new_class(
  name       = "care_temporal",
  parent      = class_temporal,
  package    = NULL,
  constructor = function(alias) {

    x <- select_care_temp(alias)

    new_object(
      class_temporal(),
      title       = x$title,
      metadata    = temp_metadata(x),
      dimensions  = care_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)

#' @autoglobal
#' @rdname medicare
#' @export
care_troup <- new_class(
  name = "care_troup",
  parent = class_group,
  package = NULL,
  constructor = function(alias) {

    x <- select_care_troup(alias)

    new_object(
      class_group(),
      group   = x$group,
      members = set_names(map(x$alias, care_temporal), x$alias)
    )
  }
)
