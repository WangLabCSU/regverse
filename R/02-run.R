#' Run Regression Model Building
#' @param obj a `REGObject` R6 object.
#' @param f a length-1 string specifying modeling function or family of [glm()], default is 'coxph'.
#' Other options are members of GLM family, see [stats::family()].
#' 'binomial' is logistic, and 'gaussian' is linear.
#' @param ... other parameters passing to corresponding regression model function.
#' @param exp logical, indicating whether or not to exponentiate the the coefficients.
#' @param ci confidence Interval (CI) level. Default to 0.95 (95%).
#' e.g. [survival::coxph()].
#' @param parallel if `TRUE`, use N-1 cores to run the task.
#' @return a `REGObject` R6 object.
#' @export
#' @examples
#' x <- REGObject(
#'   data = mtcars,
#'   vars_y = "mpg",
#'   vars_x = c("factor(cyl)", colnames(mtcars)[3:5]),
#'   vars_c = c(colnames(mtcars)[8:9], "factor(gear)")
#' )
#' x
#' x |> run(f = "gaussian")
run <- function(
    obj,
    f = c(
      "coxph", "binomial", "gaussian",
      "Gamma", "inverse.gaussian",
      "poisson", "quasi", "quasibinomial",
      "quasipoisson"
    ),
    exp = NULL, ci = 0.95,
    parallel = FALSE,
    ...) {
  f <- f[1]
  stopifnot(
    is.character(f),
    is.null(exp) || is.logical(exp)
  )

  obj@args <- list(..., f = f, exp = exp, ci = ci, parallel = parallel)
  ml <- list()

  build_one <- function(i, ...) {
    data <- obj@mdata
    x <- c(obj@vars_x[i], obj@vars_c) |> unique()
    y <- obj@vars_y

    m <- tryCatch(
      {
        # recipe to formula
        recipe <- if (f == "coxph") {
          if (length(y) < 2) {
            rlang::abort("<2 y variables for coxph model, 'time' and 'status' are required")
          }
          glue("survival::Surv({paste(y, collapse = ', ')}) ~ {paste(unique(x), collapse = ' + ')}")
        } else {
          glue("{paste(y, collapse = ' + ')} ~ {paste(unique(x), collapse = ' + ')}")
        }
        recipe <- stats::formula(recipe)

        # build
        m <- if (f == "coxph") {
          survival::coxph(recipe, data = data, ...)
        } else {
          is_call <- length(all.vars(parse(text = f))) == 0L
          if (is_call) {
            # e.g., quasi(variance = "mu", link = "log")
            f <- eval(parse(text = f))
          }
          stats::glm(recipe, data = data, family = f, ...)
        }
        m$result <- parameters::model_parameters(m,
          exponentiate = exp, ci = ci
        )
        m
      },
      error = function(e) {
        message("failed for ", x[1], " due to following error")
        message(e$message)
        NULL
      }
    )
    m
  }

  if (.Platform$OS.type == "windows") {
    message("parallel computation from parallel package is not supported in Windows, disable it.")
    parallel <- FALSE
  }

  # TODO: 设置为 plyr？
  fcall <- if (parallel) parallel::mclapply else lapply
  args <- if (!parallel) {
    list(seq_along(obj@vars_x), FUN = build_one, ...)
  } else {
    list(seq_along(obj@vars_x),
      FUN = build_one,
      mc.cores = max(parallel::detectCores() - 1L, 1L),
      ...
    )
  }
  ml <- do.call("fcall", args = args)

  ml_status <- sapply(ml, is.null)
  if (all(ml_status)) {
    message("no model built, return NULL")
    return(invisible(NULL))
  }
  xs <- obj@vars_x[!ml_status]
  ml <- ml[!ml_status]
  obj@models <- ml

  obj@results <- data.table::rbindlist(
    lapply(
      seq_along(ml),
      function(x) cbind(focal_term = xs[x], ml[[x]]$result)
    )
  )
  colnames(obj@results)[2:3] <- c("variable", "estimate")
  # private$model_data <- broom.helpers::model_get_model_frame(self$model)
  # self$forest_data <- make_forest_terms(
  #   self$model,
  #   as.data.frame(self$result),
  #   private$model_data,
  #   separate_factor, global_p
  # )
  obj
}
