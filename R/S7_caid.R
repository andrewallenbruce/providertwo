#' @noRd
#' @autoglobal
caid_dimensions <- new_class(
  name = "caid_dimensions",
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
      limit  = 8000L,
      rows   = x$count,
      fields = x$query$properties)
  }
)

#' @noRd
#' @autoglobal
caid_metadata <- new_class(
  name = "caid_metadata",
  package = NULL,
  properties = list(
    description = class_character,
    modified    = new_union(class_character, class_Date),
    dictionary  = class_character,
    download    = class_character
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      modified    = x$modified,
      dictionary  = x$dictionary,
      download    = x$download
    )
  }
)

#' Medicaid Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<caid_endpoint>` object.
#' @examples
#' caid_endpoint("MLR")
#' caid_endpoint("enterprise")
#' @autoglobal
#' @rdname caid
#' @export
caid_endpoint <- new_class(
  name       = "caid_endpoint",
  package    = NULL,
  properties = list(
    title       = class_character,
    identifier  = class_character,
    metadata    = caid_metadata,
    dimensions  = caid_dimensions
  ),
  constructor = function(alias) {

    x <- select_caid_main(alias)

    new_object(
      S7_object(),
      title       = x$title,
      identifier  = x$identifier,
      metadata    = caid_metadata(x),
      dimensions  = caid_dimensions(x)
    )
  }
)

#' Medicaid Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<caid_group>` object.
#' @examples
#' caid_group("demographics")
#' @autoglobal
#' @rdname caid
#' @export
caid_group <- new_class(
  name       = "caid_group",
  package    = NULL,
  properties = list(
    group    = class_character,
    members  = new_property(
      class_list,
      getter = function(self)
        map(self@members, caid_endpoint) |>
        set_names(self@members)
      )
    ),
  constructor = function(alias) {

    x <- select_caid_group(alias)

    new_object(
      S7_object(),
      group   = x$group,
      members = x$alias
    )
  }
)
