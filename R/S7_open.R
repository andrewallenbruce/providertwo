#' @noRd
#' @autoglobal
open_dimensions <- new_class(
  name       = "open_dimensions",
  package    = NULL,
  properties = list(
    limit    = class_integer,
    rows     = class_integer,
    pages    = new_property(
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

    x <- x$identifier |>
      request() |>
      req_url_query(
        schema  = "false",
        keys    = "false",
        results = "false",
        count   = "true",
        format  = "json",
        rowIds  = "false",
        offset  = 0L,
        limit   = 1L
      ) |>
      perform_simple()

    new_object(
      S7_object(),
      limit  = 500L,
      rows   = x$count,
      fields = x$query$properties)
  }
)

#' @noRd
#' @autoglobal
open_metadata <- new_class(
  name = "open_metadata",
  package = NULL,
  properties = list(
    description = class_character,
    modified    = new_union(class_character, class_Date),
    download    = class_character
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      modified    = x$modified,
      download    = x$download
    )
  }
)

#' @noRd
#' @autoglobal
open_metatemp <- new_class(
  name          = "open_metatemp",
  package       = NULL,
  properties    = list(
    description = class_character,
    modified    = new_union(class_character, class_Date)
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      modified    = x$modified
    )
  }
)

#' Open Payments Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<open_endpoint>` object.
#' @examples
#' open_endpoint("PROF_covered")
#' open_endpoint("PROF_physician")
#' open_endpoint("PROF_information")
#' open_endpoint("PROF_mapping")
#' open_endpoint("PROF_entity")
#' open_endpoint("PROF_teaching")
#' open_endpoint("SUMM_dashboard")
#' open_endpoint("SUMM_state_all")
#' open_endpoint("SUMM_state_group")
#' open_endpoint("SUMM_nation_all")
#' open_endpoint("SUMM_nation_group")
#' @autoglobal
#' @rdname openpayments
#' @export
open_endpoint <- new_class(
  name       = "open_endpoint",
  package    = NULL,
  properties = list(
    title       = class_character,
    identifier  = class_character,
    metadata    = open_metadata,
    dimensions  = open_dimensions
  ),
  constructor = function(alias) {

    x <- select_open(alias)

    new_object(
      S7_object(),
      title       = x$title,
      identifier  = x$identifier,
      metadata    = open_metadata(x),
      dimensions  = open_dimensions(x)
    )
  }
)

#' Open Payments Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<open_group>` object.
#' @examples
#' open_group("profile")
#  open_group("summary")
#' @autoglobal
#' @rdname openpayments
#' @export
open_group <- new_class(
  name       = "open_group",
  package    = NULL,
  properties = list(
    group = class_character,
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members,
            open_endpoint) |>
        set_names(self@members)
      )
    ),
  constructor = function(alias) {

    x <- select_open_group(alias)

    new_object(
      S7_object(),
      group   = x$group,
      members = x$alias
    )
  }
)

#' Open Payments Temporal Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<open_temporal>` object.
#' @examples
#' open_temporal("DATA_general")
#' open_temporal("DATA_ownership")
#' open_temporal("DATA_research")
#' open_temporal("GROUP_recip_nature")
#' open_temporal("GROUP_recip_entity")
#' open_temporal("GROUP_entity_nature")
#' open_temporal("GROUP_all")
#' @autoglobal
#' @rdname openpayments
#' @export
open_temporal <- new_class(
  name       = "open_temporal",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = open_metatemp,
    dimensions  = open_dimensions,
    endpoints   = class_list
    ),
  constructor = function(alias) {

    x <- select_open_temp(alias)

    new_object(
      S7_object(),
      title       = x$title,
      metadata    = open_metatemp(x),
      dimensions  = open_dimensions(x),
      endpoints   = x$endpoints
    )
  }
)

#' Open Payments Temporal Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<open_troup>` object.
#' @examples
#' open_troup("grouped_payment")
#' open_troup("detailed_payment")
#' @autoglobal
#' @rdname openpayments
#' @export
open_troup <- new_class(
  name       = "open_troup",
  package    = NULL,
  properties = list(
    group    = class_character,
    members  = new_property(
      class_list,
      getter = function(self)
        map(self@members,
            open_temporal) |>
        set_names(self@members)
    )
  ),
  constructor = function(alias) {

    x <- select_open_troup(alias)

    new_object(
      S7_object(),
      group   = x$group,
      members = x$alias
    )
  }
)
