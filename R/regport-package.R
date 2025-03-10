#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table data.table
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stats formula
#' @importFrom R6 R6Class
#' @importFrom parameters model_parameters
#' @importFrom survival coxph
#' @importFrom broom.helpers model_get_model_frame
#' @importFrom ggnewscale new_scale
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    "estimate", "level", "level_no",
    "reference", "term", "term_label",
    "variable", "."
  )
)
