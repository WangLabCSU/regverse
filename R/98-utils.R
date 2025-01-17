`%||%` <- function(a, b) if (!is.null(a)) a else b


notnull_or_na <- function(x) {
  if (is.null(x)) NA_character_ else x
}

attr_notnull_or_na <- function(x, at = "label") {
  notnull_or_na(attr(x, at, exact = TRUE))
}

get_vars <- function(text) {
  if (!is.null(text)) {
    all.vars(parse(text = text))
  } else {
    NULL
  }
}

merge_vars <- function(...) {
  vars_list <- list(...)
  rv <- NULL
  for (i in vars_list) {
    v <- unique(sapply(i, get_vars))
    if (length(v) > 0) rv <- union(rv, v)
  }
  rv
}

remove_backticks <- function(x) {
  gsub("^`|`$|\\\\(?=`)|`(?=:)|(?<=:)`", "", x, perl = TRUE)
}


# # Adapted from forestmodel package
# make_forest_terms <- function(model, tidy_model, data,
#                               separate_factor = FALSE,
#                               global_p = FALSE) {
#   # tidy_model <- broom::tidy(model, conf.int = TRUE)
#   colnames(tidy_model)[1:2] <- c("term", "estimate")
#
#   forest_terms <- merge(
#     # TODO: 这个对纯交互项处理也存在问题
#     data.table::data.table(
#       term_label = attr(model$terms, "term.labels")
#     )[, variable := remove_backticks(term_label)],
#     data.table::data.table(
#       variable = names(attr(model$terms, "dataClasses"))[-1],
#       class = attr(model$terms, "dataClasses")[-1]
#     ),
#     by = "variable", all.x = FALSE, all.y = FALSE
#   )
#
#   # TODO:交互项的处理
#   create_term_data <- function(term_row) {
#     if (!is.na(term_row$class)) {
#       var <- term_row$variable
#       if (term_row$class %in% c("factor", "character")) {
#         tab <- table(data[, var])
#         if (!any(paste0(term_row$term_label, names(tab)) %in% tidy_model$term)) {
#           # Filter out terms not in final model summary (e.g. strata)
#           out <- data.frame(variable = NA, stringsAsFactors = FALSE)
#         } else {
#           out <- data.frame(
#             term_row,
#             level = names(tab),
#             level_no = 1:length(tab),
#             n = as.integer(tab),
#             stringsAsFactors = FALSE
#           )
#           if (separate_factor) {
#             out <- dplyr::bind_rows(as.data.frame(term_row, stringsAsFactors = FALSE), out)
#           }
#         }
#       } else {
#         out <- data.frame(term_row,
#           level = NA, level_no = NA,
#           n = sum(!is.na(data[, var])),
#           stringsAsFactors = FALSE
#         )
#         if (term_row$class == "logical") {
#           out$term_label <- paste0(term_row$term_label, "TRUE")
#         }
#       }
#     } else {
#       out <- data.frame(
#         term_row,
#         level = NA, level_no = NA, n = NA,
#         stringsAsFactors = FALSE
#       )
#     }
#     out
#   }
#
#   forest_terms <- forest_terms %>%
#     dplyr::rowwise() %>%
#     dplyr::do(create_term_data(.)) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(!is.na(variable)) %>%
#     dplyr::mutate(term = paste0(term_label, replace(level, is.na(level), ""))) %>%
#     dplyr::left_join(tidy_model, by = "term") %>%
#     dplyr::mutate(
#       reference = ifelse(is.na(level_no), FALSE, level_no == 1),
#       estimate = ifelse(reference, 0, estimate),
#       variable = ifelse(is.na(variable), remove_backticks(term), variable)
#     ) %>%
#     dplyr::mutate(
#       variable = ifelse(is.na(level_no) | (level_no == 1 & !separate_factor), variable, NA)
#     ) %>%
#     dplyr::select(c("variable", "term"), dplyr::everything())
#
#   if (global_p) {
#     if (inherits(model, "coxph")) {
#       p_val <- as.numeric(summary(model)$sctest[3])
#       label <- paste("Global p ", format.pval(p_val, digits = 2, eps = 1e-3))
#       forest_terms <- forest_terms %>%
#         dplyr::add_row(term_label = "Global p", variable = label)
#     } else {
#       message("No global p value availabe for non-Cox model")
#     }
#   }
#
#   data.table::as.data.table(forest_terms)
# }
