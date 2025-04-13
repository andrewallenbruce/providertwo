#' Program Code
#' Code for primary program related to a data asset, from the
#' [Federal Program Inventory](https://resources.data.gov/schemas/dcat-us/v1.1/FederalProgramInventory_FY13_MachineReadable_091613.csv).
#' @source [DCAT Schema: Program Code](https://resources.data.gov/resources/dcat-us/#programCode)
#' @param x `<chr>`  The program code to search for, e.g., `"009:000"`;
#'        if `NULL` (the default), returns all program codes
#' @returns `<tibble>` of search results
#' @examplesIf rlang::is_interactive()
#' program_code("009:000")
#' head(program_code())
#' @autoglobal
#' @keywords internal
#' @export
program_code <- function(x = NULL) {
  search_in(get_pin("programCodes"), "programCodePODfmt", x)
}

#' Bureau Code
#' Combined Agency and Bureau Code, from the
#' [OMB Circular A-11, Appendix C (PDF)](https://obamawhitehouse.archives.gov/sites/default/files/omb/assets/a11_current_year/app_c.pdf)
#' @source [DCAT Schema: Bureau Code](https://resources.data.gov/resources/dcat-us/#bureauCode)
#' @param x `<chr>` The bureau code to search for, e.g., `"38"`;
#'          if `NULL` (the default), returns all bureau codes
#' @returns `<tibble>` of search results
#' @examplesIf rlang::is_interactive()
#' bureau_code("38")
#' head(bureau_code())
#' @autoglobal
#' @keywords internal
#' @export
bureau_code <- function(x = NULL) {
  search_in(get_pin("bureauCodes"), "bureauCode", x)
}

#' Open Payments Dictionary
#' @examplesIf rlang::is_interactive()
#' open_dictionary()
#' @returns `<tibble>` of search results
#' @autoglobal
#' @keywords internal
#' @export
open_dictionary <- function() {
  x <- map(get_elem(
    get_elem(as_tbl(fload(
      paste0(
        "https://openpaymentsdata.cms.gov/",
        "api/1/metastore/schemas/dataset/",
        "items?show-reference-ids"
      )
    )), "data", DF.as.list = TRUE),
    "title|describedBy",
    regex = TRUE
  ), function(x)
    x[not_null(names(x))])


  x <- new_df(name = delist(get_elem(x, "title")),
              dictionary = delist(get_elem(x, "describedBy"))) |>
    mtt(
      year = as_int(stri_extract_all_regex(name, "[0-9]{4}")),
      name = cheapr_if_else(
        na(year),
        name,
        stri_extract_all_regex(name, "^.*(?=\\s.\\sDetailed Dataset [0-9]{4} Reporting Year)")
      ),
      year = cheapr_if_else(na(year), fmax(year), year)
    ) |>
    sbt(year == fmax(year), -year)


  x <- x[["dictionary"]] |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    resps_data(\(resp) resp_body_string(resp)) |>
    fparse(query = "/data")


  funique(
    new_tbl(
      field = delist(map(get_elem(x, "fields"), function(x)
        get_elem(x, "name"))),
      description = delist(map(get_elem(x, "fields"), function(x)
        get_elem(x, "description"))) |> replace_fixed(c("\n", '"'), c(" ", ""))
    ),
    cols = "field",
    sort = TRUE
  )
}

#' @noRd
#' @autoglobal
st_extract <- function(string, pattern, perl = TRUE, ...) {
  regmatches(
    x = string,
    m = gregexec(
      pattern = pattern,
      text = string,
      perl = perl,
      ...)
  ) |>
    unlist(use.names = FALSE)
}
