#' @noRd
#' @autoglobal
pro_dimensions <- new_class(
  name = "pro_dimensions",
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
      limit  = 2000L,
      rows   = x$count,
      fields = x$query$properties)
  }
)

#' @noRd
#' @autoglobal
pro_metadata <- new_class(
  name = "pro_metadata",
  package = NULL,
  properties = list(
    description = class_character,
    issued      = new_union(class_character, class_Date),
    modified    = new_union(class_character, class_Date),
    released    = new_union(class_character, class_Date),
    site        = class_character,
    dictionary  = class_character,
    download    = class_character
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      issued      = x$issued,
      modified    = x$modified,
      released    = x$released,
      site        = x$site,
      dictionary  = x$dictionary,
      download    = x$download
    )
  }
)

#' Provider Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<pro_endpoint>` object.
#' @examples
#' pro_endpoint("PDC_affiliations")
#' pro_endpoint("PDC_clinicians")
#' pro_endpoint("PDC_utilization")
#' @autoglobal
#' @rdname provider
#' @export
pro_endpoint <- new_class(
  name       = "pro_endpoint",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = pro_metadata,
    dimensions  = pro_dimensions,
    identifier  = class_character
  ),
  constructor = function(alias) {

    x <- select_pro_main(alias)

    new_object(
      S7_object(),
      title       = x$title,
      identifier  = x$identifier,
      metadata    = pro_metadata(x),
      dimensions  = pro_dimensions(x)
    )
  }
)

#' Provider Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<pro_group>` object.
#' @examples
#' pro_group("PDC")
#' pro_group("MIPS")
#' @autoglobal
#' @rdname provider
#' @export
pro_group <- new_class(
  name       = "pro_group",
  package    = NULL,
  properties = list(
    group = class_character,
    members = new_property(
      class_list,
      getter = function(self)
        map(self@members, pro_endpoint) |>
          set_names(self@members)
      )
    ),
  constructor = function(alias) {

    x <- select_pro_group(alias)

    new_object(
      S7_object(),
      group   = x$group,
      members = x$alias
    )
  }
)
