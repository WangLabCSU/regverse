#' Prepare data to create a `REGObject`
#'
#' It is recommended that the input `data` has been cleaned for modeling purposes. While setting "factor(cyl)" is acceptable in the `vars_*` options, ensuring data cleanliness will improve the overall readability.
#'
#' @param data A `data.table` storing input data.
#' @param vars_x Column names storing the **focal variables**.
#' @param vars_y Column names storing the **predicted variables**.
#' @param vars_c Column names storing the **Covariables**.
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
prepare <- function(
    data, vars_y, vars_x, vars_c) {
  stopifnot(is.data.frame(data))

  data <- as.data.table(data, keep.rownames = TRUE)

  vars_x <- unique(vars_x)
  vars_y <- unique(vars_y)
  vars_c <- unique(vars_c)
  #orig_vars = purrr::reduce(list(vars_y, vars_x, vars_c), union)
  all_vars <- merge_vars(vars_y, vars_x, vars_c)

  if (!all(all_vars %in% colnames(data))) {
    cli::cli_abort("column(s) not available in {.field data}: {.val {all_vars[!all_vars %in% colnames(data)]}}")
  }
  mdata <- data[, all_vars, with = FALSE]

  # TODO: handle labelled var variables?

  its <- intersect(vars_y, vars_x)
  if (length(its) > 0) {
    cli::cli_warn("common variable(s) {.val {its}} found in input {.field vars_x} and {.field vars_y}, remove from {.field vars_x}")
    vars_x <- setdiff(vars_x, vars_y)
  }

  new("REGObject",
      data = data, mdata = mdata,
      vars_x = vars_x, vars_y = vars_y, vars_c = vars_c
  )
}
