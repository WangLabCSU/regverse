#' R6 class representing a list of regression model
#'
#' @description
#' Contains fields storing data and methods to build, process and visualize
#' a list of regression model ([REGModel]).
#'
#' @export
#' @examples
#' # GLM regression
#' ml <- REGModelList$new(
#'   data = mtcars,
#'   y = "mpg",
#'   x = c("factor(cyl)", colnames(mtcars)[3:5]),
#'   covars = c(colnames(mtcars)[8:9], "factor(gear)")
#' )
#' ml
#' ml$print()
#' ml$plot_forest()
#'
#' ml$build(f = "gaussian")
#' \dontrun{
#' ml$build(f = "gaussian", parallel = TRUE)
#' }
#' ml$print()
#' ml$result
#' ml$forest_data
#' ml$plot_forest()
#'
#' lung <- survival::lung
#' # Cox-PH regression
#' ml2 <- REGModelList$new(
#'   data = lung,
#'   y = c("time", "status"),
#'   x = c("age", "ph.ecog", "ph.karno"),
#'   covars = c("factor(sex)")
#' )
#' ml2$build()
#' ml2$plot_forest()
#'
#' # Group Cox analysis
#' lung$ph.ecog <- factor(lung$ph.ecog)
#' ml3 <- REGModelList$new(
#'   data = lung,
#'   y = c("time", "status"),
#'   x = c("ph.ecog"),
#'   covars = "age", group = "sex"
#' )
#' ml3$build()
#'
#' @testexamples
#' expect_s3_class(ml, "REGModelList")
REGModelList <- R6::R6Class(
  "REGModelList",
  inherit = NULL,
  public = list(
    #' @field data A `data.table` storing modeling data.
    #' @field x Focal variables (terms).
    #' @field y Predicted variables or expression.
    #' @field covars Covariables.
    #' @field group A split variable.
    #' @field mlist A list of `REGModel`.
    #' @field args Other arguments used for building model.
    #' @field type Model type (class).
    #' @field result Model result, a object of `parameters_model`.
    #' Can be converted into
    #' data.frame with [as.data.frame()] or [data.table::as.data.table()].
    #' @field forest_data More detailed data used for plotting forest.
    data = NULL,
    x = NULL,
    y = NULL,
    covars = NULL,
    group = NULL,
    args = NULL,
    mlist = NULL,
    type = NULL,
    result = NULL,
    forest_data = NULL,
    #' @description Create a `REGModelList` object.
    #' @param data A `data.table` storing modeling data.
    #' @param x Focal variables (terms).
    #' @param y Predicted variables or expression.
    #' @param covars Covariables.
    #' @param group A split variable.
    #' @return A `REGModelList` R6 object.
    initialize = function(data, y, x, covars = NULL, group = NULL) {
      stopifnot(is.data.frame(data))

      all_vars <- merge_vars(x, y, covars, group)
      data <- data.table::as.data.table(data)
      if (!all(all_vars %in% colnames(data))) {
        rlang::abort(glue("Column not available: {all_vars[!all_vars %in% colnames(data)]}"))
      }
      data <- data[, all_vars, with = FALSE]
      self$data <- data
      self$x <- setdiff(x, y)
      self$y <- y
      self$covars <- covars
      self$group <- group

      invisible(self)
    },
    #' @description Build `REGModelList` object.
    #' @param f A length-1 string specifying modeling function or family of [glm()], default is 'coxph'.
    #' Other options are members of GLM family, see [stats::family()].
    #' 'binomial' is logistic, and 'gaussian' is linear.
    #' @param ... Other parameters passing to corresponding regression model function.
    #' @param exp Logical, indicating whether or not to exponentiate the the coefficients.
    #' @param ci Confidence Interval (CI) level. Default to 0.95 (95%).
    #' e.g. [survival::coxph()].
    #' @param parallel If `TRUE`, use N-1 cores to run the task.
    #' @return A `REGModel` R6 object.
    build = function(f = c(
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

      data <- self$data
      x <- self$x
      y <- self$y
      covars <- self$covars
      grp_var <- self$group
      self$args <- list(...)
      ml <- list()

      if (!is.null(grp_var)) {
        cli::cli_inform("group model data by {.val {grp_var}}")

        if (length(data[[grp_var]]) == length(table(data[[grp_var]]))) {
          rlang::abort("Cannot set group by a variable that cannot be groupped!")
        }

        # only support one focal term
        build_one <- function(data, ...) {
          m <- tryCatch(
            {
              m <- REGModel$new(
                data,
                recipe = list(
                  x = unique(c(x, covars)),
                  y = y
                ),
                f = f, exp = exp, ci = ci, ...
              )
              m$get_forest_data()
              m
            },
            error = function(e) {
              message("failed for ", x, " due to following error")
              message(e$message)
              NULL
            }
          )
          m
        }

        ml <- data |>
          dplyr::group_split(.data[[grp_var]]) |>
          lapply(build_one, ...)
      } else {
        build_one <- function(i, ...) {
          m <- tryCatch(
            {
              m <- REGModel$new(
                data,
                recipe = list(
                  x = unique(c(x[i], covars)),
                  y = y
                ),
                f = f, exp = exp, ci = ci, ...
              )
              m$get_forest_data()
              m
            },
            error = function(e) {
              message("failed for ", x[i], " due to following error")
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


        fcall <- if (parallel) parallel::mclapply else lapply
        args <- if (!parallel) {
          list(seq_along(self$x), FUN = build_one, ...)
        } else {
          list(seq_along(self$x),
            FUN = build_one,
            mc.cores = max(parallel::detectCores() - 1L, 1L),
            ...
          )
        }
        ml <- do.call("fcall", args = args)
      }

      ml_status <- sapply(ml, is.null)
      if (all(ml_status)) {
        message("no model built, return NULL")
        return(invisible(NULL))
      }

      if (!is.null(grp_var)) {
        xs <- unique(data[[grp_var]])
      } else {
        xs <- x[!ml_status]
      }

      self$mlist <- ml[!ml_status]
      ml <- ml[!ml_status]
      self$type <- ml[[1]]$type
      self$result <- data.table::rbindlist(
        lapply(
          seq_along(ml),
          function(x) cbind(focal_term = xs[x], ml[[x]]$result)
        )
      )
      colnames(self$result)[2:3] <- c("variable", "estimate")
      self$forest_data <- data.table::rbindlist(
        lapply(
          seq_along(ml),
          function(x) {
            d <- ml[[x]]$forest_data
            if (is.null(grp_var)) {
              cbind(focal_term = xs[x], d)
            } else {
              cbind(focal_term = c(paste0(grp_var, ":", xs[x]), rep(NA, nrow(d) - 1)), d)
            }
          }
        )
      )
      # TODO: Only keep focal term? Use filter controls
      if (is.null(grp_var)) {
        self$forest_data <- self$forest_data[focal_term == term_label]
      }

      invisible(self)
    },
    #' @description Plot forest.
    #' @param ref_line Reference line, default is `1` for HR.
    #' @param xlim Limits of x axis.
    #' @param vars Selected variables to show.
    #' @param p Selected variables with level' p value lower than p.
    #' @param ... Other plot options passing to [forestploter::forest()].
    #' Also check <https://github.com/adayim/forestploter> to see more complex adjustment of the result plot.
    plot_forest = function(ref_line = NULL, xlim = NULL, vars = NULL, p = NULL, ...) {
      data <- self$forest_data
      if (is.null(data)) {
        message("Please run $build() before $plot_forest()")
        return(NULL)
      }
      if (!is.null(vars)) data <- data[data$focal_term %in% vars]
      if (!is.null(p)) {
        minps <- sapply(split(data, data$focal_term), function(x) min(x$p, na.rm = TRUE))
        vars2 <- names(minps[minps < p])
        data <- data[data$focal_term %in% vars2]
      }
      if (!is.null(self$group)) attr(data, "group") <- TRUE
      plot_forest(data, ref_line, xlim, ...)
    },
    #' @description Plot connected risk network
    #' Append `scale_size()` operation (i.e.,`scale_size(range = c(0.1, 4))`)
    #' to reset the range of line width
    plot_connected_risk = function() {
      if (self$type != "coxph") {
        rlang::abort("This function is designed for coxph model analysis")
      }
      rlang::inform("please note only continuous focal terms analyzed and visualized")

      # 1. Obtain regression results
      data_reg <- self$result |> dplyr::filter(focal_term == variable)
      data_reg$role <- data.table::fcase(
        data_reg$p > 0.05, "non-signf",
        data_reg$estimate < 1, "protector",
        data_reg$estimate > 1, "risker"
      )
      data_reg$`-log10(p)` <- -log10(data_reg$p)

      # 2. Correlation analysis
      vars_comb <- combn(self$x |> get_vars(), 2, simplify = FALSE)
      data <- self$data
      cor_value <- sapply(vars_comb, function(x) {
        cor(data[[x[1]]], data[[x[2]]], use = "pairwise")
      })

      data_cor <- cbind(as.data.frame(t(sapply(vars_comb, function(x) x))), cor_value)
      colnames(data_cor) <- c("var1", "var2", "correlation")
      data_cor$size <- abs(data_cor$correlation)
      data_cor$way <- ifelse(data_cor$correlation > 0, "positive", "negative")
      data_cor

      # 3. Visualization
      p <- polar_init(data_reg,
        x = focal_term,
        aes(color = role, size = `-log10(p)`)
      ) + ggplot2::scale_color_manual(values = c("grey", "blue", "red")) +
        labs(size = "-log10(p)", color = "risk type") +
        ggnewscale::new_scale("color") +
        ggnewscale::new_scale("size") +
        polar_connect(data_cor, x1 = var1, x2 = var2, size = size, color = way, alpha = 0.5) +
        ggplot2::scale_color_manual(values = c("cyan", "orange")) +
        labs(color = "correlation type", size = "correlation size") +
        theme(
          legend.position = "bottom",
          legend.direction = "vertical",
          legend.box = "horizontal"
        )
      p
    },
    #' @description Print the `REGModelList` object
    #' @param ... Unused.
    print = function(...) {
      cat(glue("<{cli::col_br_magenta('REGModelList')}>    =========="), "\n\n")
      cat(glue("{cli::col_green('X')}(s): {paste(self$x, collapse = ', ')}"), "\n")
      cat(glue("{cli::col_green('Y')}(s): {paste(self$y, collapse = ', ')}"), "\n")
      cat(glue("covars: {paste(self$covars, collapse = ', ')}"), "\n")
      if (is.null(self$result)) {
        cat("\nNot build yet, run $build() method", "\n")
      } else {
        cat("----\n", glue("{cli::col_green('Result')}:"), "\n")
        print(self$result)
      }
      cat(glue("[{cli::col_br_green(paste(self$type, collapse = '/'))}] model =========="))
    }
  ),
  private = list(),
  active = list()
)
