#' @autoglobal
#' @noRd
Caid <- new_class(name = "Caid", package = NULL)

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
      # list_c() |>
      # get_elem(
      # c("^count$", "properties$"),
      # regex = TRUE)

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
    temporal    = class_character,
    periodicity = class_character,
    issued      = new_union(class_character, class_Date),
    modified    = new_union(class_character, class_Date),
    dictionary  = class_character,
    download    = class_character
  ),
  constructor = function(x) {
    new_object(
      S7_object(),
      description = x$description,
      temporal    = x$temporal,
      periodicity = x$periodicity,
      issued      = x$issued,
      modified    = x$modified,
      dictionary  = x$dictionary,
      download    = x$download
    )
  }
)

#' Medicaid Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<caidMain>` object.
#' @examples
#' caidMain("MLR")
#' caidMain("enterprise")
#' @autoglobal
#' @rdname caid
#' @export
caidMain <- new_class(
  parent     = Caid,
  name       = "caidMain",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = caid_metadata,
    identifier  = class_character,
    dimensions  = caid_dimensions
  ),
  constructor = function(alias) {

    x <- caid_main(alias)

    new_object(
      Caid(),
      title       = x$title,
      metadata    = caid_metadata(x),
      identifier  = x$identifier,
      dimensions  = caid_dimensions(x)
    )
  }
)

#' Medicaid Endpoint Group
#' @param alias `<chr>` title alias
#' @returns An S7 `<caidGroup>` object.
#' @examples
#' caidGroup("demographics")
#' @autoglobal
#' @rdname caid
#' @export
caidGroup <- new_class(
  parent     = Caid,
  name       = "caidGroup",
  package    = NULL,
  properties = list(
    group = class_character,
    members = class_list),
  constructor = function(alias) {

    x <- caid_group(alias)

    new_object(
      Caid(),
      group  = x$group,
      members = map(x$alias, caidMain) |> set_names(x$alias)
    )
  }
)
