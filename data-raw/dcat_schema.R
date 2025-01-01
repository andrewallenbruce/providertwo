library(tidyverse)
library(here)
source(here::here("data-raw", "pins_internal.R"))

programCodes <- read_csv(
  here(
    "data-raw/files/FederalProgramInventory_FY13_MachineReadable_091613.csv"
  ),
  show_col_types                      = FALSE,
  col_types                           = cols(
    `Agency Name`                       = col_character(),
    `Program Name`                      = col_character(),
    `Additional Information (optional)` = col_character(),
    agencyCode                          = col_character(),
    ProgramCode                         = col_character(),
    ProgramCodePODFormat                = col_character()
  )) |>
  setNames(c("agencyName",
             "programName",
             "additionalInfo",
             "agencyCode",
             "programCode",
             "programCodePODfmt"))

pin_update(
  programCodes,
  name        = "programCodes",
  title       = "Data Asset Primary Program",
  description = "Primary program related to data asset, from the Federal Program Inventory"
)

bureauCodes <- read_csv(
  here("data-raw/files/omb_bureau_codes.csv"),
  show_col_types  = FALSE,
  col_types       = cols(
    `Agency Name`   = col_character(),
    `Bureau Name`   = col_character(),
    `Agency Code`   = col_character(),
    `Bureau Code`   = col_character(),
    `Treasury Code` = col_character(),
    `CGAC Code`     = col_character())) |>
  setNames(c("agencyName",
             "bureauName",
             "agencyCode",
             "bureauCode",
             "treasuryCode",
             "cgacCode")) |>
  mutate(
    agencyCode   = str_pad(agencyCode, width = 3, pad = "0"),
    bureauCode   = str_pad(bureauCode, width = 2, pad = "0"),
    treasuryCode = str_pad(treasuryCode, width = 2, pad = "0"),
    cgacCode     = str_pad(cgacCode, width = 3, pad = "0"))

bureauCodes

pin_update(
  bureauCodes,
  name        = "bureauCodes",
  title       = "Combined Agency and Bureau Code",
  description = "Combined Agency and Bureau Code from OMB Circular A-11, Appendix C"
)
