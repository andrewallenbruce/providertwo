#' @autoglobal
#' @noRd
Caid <- new_class(name = "Caid", package = NULL)

#' @noRd
#' @autoglobal
caidDim <- new_class(
  name = "caidDim",
  package = NULL,
  properties = list(
    rows = new_property(
      class_integer,
      default = 0L
    ),
    pages = new_property(
      class_integer,
      getter = function(self)
        offset_size(self@rows, 8000L)
    ),
    fields = class_character
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
      rows   = x$count,
      fields = x$query$properties)
  }
)

#' @noRd
#' @autoglobal
caidMeta <- new_class(
  name = "caidMeta",
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

#' Provider Endpoint
#' @param alias `<chr>` endpoint alias
#' @returns An S7 `<caidMain>` object.
#' @examplesIf interactive()
#' caidMain("mlr")
#' caidMain("mesd")
#' caidMain("wcv")
#' caidMain("mhsud")
#' caidMain("disability")
#' caidMain("pi")
#' caidMain("livebirth")
#' caidMain("blood")
#' caidMain("lang")
#' caidMain("race")
#' caidMain("rural")
#' caidMain("waive")
#' @autoglobal
#' @rdname caid
#' @export
caidMain <- new_class(
  parent     = Caid,
  name       = "caidMain",
  package    = NULL,
  properties = list(
    title       = class_character,
    metadata    = caidMeta,
    dimensions  = caidDim,
    identifier  = class_character
  ),
  constructor = function(alias) {

    x <- caid_main(alias)

    new_object(
      Caid(),
      title       = x$title,
      metadata    = caidMeta(x),
      dimensions  = caidDim(x),
      identifier  = x$identifier
    )
  }
)
