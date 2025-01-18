#' Prepare Data to Create a `REGObject`
#' @param data A `data.table` storing input data.
#' @param vars_x Focal variables.
#' @param vars_y Predicted variables or formulas.
#' @param vars_c Covariables.
#' @export
#' @examples
#' x <- prepare(
#'   data = mtcars,
#'   vars_y = "mpg",
#'   vars_x = c("factor(cyl)", colnames(mtcars)[3:5]),
#'   vars_c = c(colnames(mtcars)[8:9], "factor(gear)")
#' )
#' x
#' @testexamples
#' is(x, "REGObject")
prepare <- REGObject
