#' Evaluate a metANN Model
#'
#' Evaluates a fitted metANN model on new data.
#'
#' @param object A fitted object of class `"metann"`.
#' @param newdata New data used for evaluation. For formula-based models, this
#' should be a data frame containing the response variable. For x-y models,
#' this should be a numeric matrix or numeric data frame.
#' @param y_true Optional true response values. Required for x-y models. For
#' formula-based models, if `NULL`, the response is extracted from `newdata`.
#' @param metrics Optional performance metrics. If `NULL`, the metrics stored
#' in the fitted model are used.
#' @param threshold Classification threshold for binary classification.
#' @param ... Additional arguments passed to `predict()`.
#'
#' @return An object of class `"metann_evaluation"`.
#' @export
#'
#' @examples
#' fit <- met_mlp(
#'   formula = Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length,
#'   data = iris,
#'   hidden_layers = c(5),
#'   optimizer = optimizer_pso(pop_size = 10, max_iter = 10),
#'   seed = 123,
#'   verbose = FALSE
#' )
#'
#' evaluate(fit, newdata = iris)
evaluate <- function(object,
                     newdata,
                     y_true = NULL,
                     metrics = NULL,
                     threshold = 0.5,
                     ...) {
  if (!inherits(object, "metann")) {
    stop("'object' must be a fitted metANN model.", call. = FALSE)
  }

  if (missing(newdata) || is.null(newdata)) {
    stop("'newdata' must be supplied.", call. = FALSE)
  }

  if (!is.numeric(threshold) ||
      length(threshold) != 1L ||
      threshold <= 0 ||
      threshold >= 1) {
    stop("'threshold' must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  if (isTRUE(object$formula_mode)) {
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata)
    }

    response_name <- as.character(stats::formula(object$formula)[[2L]])

    if (is.null(y_true)) {
      if (!response_name %in% names(newdata)) {
        stop(
          "The response variable '",
          response_name,
          "' was not found in 'newdata'. Please supply 'y_true'.",
          call. = FALSE
        )
      }

      y_true <- newdata[[response_name]]
    }
  } else {
    if (is.null(y_true)) {
      stop(
        "'y_true' must be supplied when evaluating a model fitted with the x-y interface.",
        call. = FALSE
      )
    }
  }

  if (object$task == "regression") {
    y_pred <- stats::predict(
      object,
      newdata = newdata,
      type = "response",
      ...
    )

    if (!is.numeric(y_true)) {
      stop(
        "'y_true' must be numeric for regression evaluation.",
        call. = FALSE
      )
    }

    y_true <- as.numeric(y_true)

    if (length(y_true) != length(y_pred)) {
      stop(
        "'y_true' and predictions must have the same length.",
        call. = FALSE
      )
    }

    if (is.null(metrics)) {
      metrics <- object$metrics
    } else {
      metrics <- as_metrics(metrics)
    }

    values <- compute_metric_values(
      metrics = metrics,
      y_true = y_true,
      y_pred = y_pred
    )
  } else if (object$task == "classification") {
    y_pred <- stats::predict(
      object,
      newdata = newdata,
      type = "class",
      threshold = threshold,
      ...
    )

    y_true <- factor(y_true, levels = object$class_levels)

    if (any(is.na(y_true))) {
      stop(
        "'y_true' contains class labels that were not seen during training.",
        call. = FALSE
      )
    }

    if (length(y_true) != length(y_pred)) {
      stop(
        "'y_true' and predictions must have the same length.",
        call. = FALSE
      )
    }

    values <- classification_metric_values(
      y_true = y_true,
      y_pred = y_pred
    )

    if (!is.null(metrics)) {
      metric_names <- if (is.character(metrics)) {
        metrics
      } else {
        names(as_metrics(metrics))
      }

      values <- values[names(values) %in% metric_names]
    }
  } else {
    stop(
      "Unknown model task.",
      call. = FALSE
    )
  }

  class(values) <- "metann_evaluation"

  values
}


#' Print metANN Evaluation Results
#'
#' @param x An object of class `"metann_evaluation"`.
#' @param ... Additional arguments.
#'
#' @return The input object invisibly.
#' @export
print.metann_evaluation <- function(x, ...) {
  cat("metANN evaluation\n")

  if (length(x) == 0L) {
    cat("  No metrics were computed.\n")
    return(invisible(x))
  }

  for (nm in names(x)) {
    cat("  ", nm, ": ", x[[nm]], "\n", sep = "")
  }

  invisible(x)
}
