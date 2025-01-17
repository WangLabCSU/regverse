# Roadmap: https://github.com/WangLabCSU/regverse/issues/1
# Reference for impl: https://adv-r.hadley.nz/s4.html
# Reference for class:
# https://github.com/ShixiangWang/regport/blob/main/R/REGModelList.R
# https://github.com/ShixiangWang/regport/blob/main/R/REGModel.R

# TODO: 先把基本的 class 的分析过程先重新实现一遍
# 然后构建 tidy-first workflow
# reg下划线函数 prepare run plot tbl 等


# Class -------------------------------------------------------------------

# TODO: vars_y is multiple variables?
# TODO: mdata 的必要性？也涉及 mdata 生成时的列选择

#' REGObject: class representing a list of regression model
#'
#' Contains fields storing data and methods to build, process and visualize
#' a list of regression model.
#' Currently, this class is designed for CoxPH and GLM regression models.
#'
#' @slot data A `data.table` storing input data.
#' @slot mdata A `data.table` storing cleaned data for modeling.
#' @slot vars_x Focal variables.
#' @slot vars_y Predicted variables or formulas.
#' @slot vars_c Covariables.
#' @slot args Other arguments used for building regression models.
#' @slot models Models built.
#' @slot results Model results, a object of `parameters_model`. Can be converted into
#' data.frame with [as.data.frame()] or [data.table::as.data.table()].
#' @slot .forest_data More detailed data used for plotting forest.
#' @rdname REGObject
#' @export
setClass(
  Class = "REGObject",
  slots = c(
    data = "data.table",
    mdata = "data.table",
    vars_x = "character",
    vars_y = "character",
    vars_c = "character",
    args = "list",
    models = "list",
    results = "data.table",
    .forest_data = "data.table"
  ),
  prototype = list(
    data = data.table(),
    mdata = data.table(),
    vars_x = NA_character_,
    vars_y = NA_character_,
    vars_c = NA_character_,
    args = list(),
    models = list(),
    results = data.table(),
    .forest_data = data.table()
  )
)


# New ---------------------------------------------------------------------
#' Create a `REGObject`
#' @param data A `data.table` storing input data.
#' @param vars_x Focal variables.
#' @param vars_y Predicted variables or formulas.
#' @param vars_c Covariables.
#' @rdname REGObject
#' @export
#' @examples
#' x <- REGObject(
#'   data = mtcars,
#'   vars_y = "mpg",
#'   vars_x = c("factor(cyl)", colnames(mtcars)[3:5]),
#'   vars_c = c(colnames(mtcars)[8:9], "factor(gear)")
#' )
#' x
#' @testexamples
#' is(x, "REGObject")
REGObject <- function(
    data, vars_y, vars_x, vars_c) {
  stopifnot(is.data.frame(data))

  data <- as.data.table(data, keep.rownames = TRUE)

  vars_x <- unique(vars_x)
  vars_y <- unique(vars_y)
  vars_c <- unique(vars_c)
  all_vars <- merge_vars(vars_x, vars_y, vars_c)

  if (!all(all_vars %in% colnames(data))) {
    rlang::abort(glue("column(s) not available: {all_vars[!all_vars %in% colnames(data)]}"))
  }
  mdata <- data[, all_vars, with = FALSE]
  # TODO 特殊列名的处理

  its <- intersect(vars_y, vars_x)
  if (length(its) > 0) {
    rlang::warn(glue("common variable(s) {paste(its, collapse = ', ')} found in input `x` and `y`, remove from x"))
    vars_x <- setdiff(vars_x, vars_y)
  }

  new("REGObject",
    data = data, mdata = mdata,
    vars_x = vars_x, vars_y = vars_y, vars_c = vars_c
  )
}

# Validator ---------------------------------------------------------------
# explicitly check
# validObject(alex)

# setValidity("Person", function(object) {
#   if (length(object@name) != length(object@age)) {
#     "@name and @age must be same length"
#   } else {
#     TRUE
#   }
# })


# Generics and Methods ---------------------------------------------------------

# setGeneric("myGeneric", function(x) standardGeneric("myGeneric"))
#
# It is occasionally useful to remove arguments from dispatch. This allows you to require that methods provide arguments like verbose = TRUE or quiet = FALSE, but they don’t take part in dispatch.
# setGeneric("myGeneric",
#            function(x, ..., verbose = TRUE) standardGeneric("myGeneric"),
#            signature = "x"
# )

# To list all the methods that belong to a generic, or that are associated with a class, use methods("generic") or methods(class = "class"); to find the implementation of a specific method, use selectMethod("generic", "class").

# To define a method for an existing generic, you must first determine the arguments. You can get those from the documentation or by looking at the args() of the generic:
# args(getGeneric("show"))

# setMethod("myGeneric", "Person", function(x) {
#   # method implementation
# })

setMethod(
  f = "show",
  signature = "REGObject",
  definition = function(object) {
    cat(paste("An object of class", class(object), "\n"))

    cat(glue("\t variable y: {y}", y = paste(object@vars_y, collapse = ",")), "\n")
    cat(glue("\t variable x: {x}", x = paste(object@vars_x, collapse = ",")), "\n")
    cat(glue("\tcovariables: {c}", c = paste(object@vars_c, collapse = ",")), "\n")

    cat("\nmodel data head:\n")
    print(head(object@mdata))
  }
)
