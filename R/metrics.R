#' Create a Metric Function Object
#'
#' Internal helper for constructing metric function objects.
#'
#' @param name A character string specifying the metric name.
#' @param fn A function that computes the metric from observed and predicted values.
#' @param task A character string specifying the supported task.
#' @param parameters A list of metric-specific parameters.
#'
#' @return An object of class `"met_metric"`.
#' @keywords internal
new_metric <- function(name, fn, task = "both", parameters = list()) {
  if (!is.character(name) || length(name) != 1L) {
    stop("'name' must be a single character string.", call. = FALSE)
  }

  if (!is.function(fn)) {
    stop("'fn' must be a function.", call. = FALSE)
  }

  if (!is.character(task) || length(task) != 1L) {
    stop("'task' must be a single character string.", call. = FALSE)
  }

  if (!task %in% c("regression", "classification", "both")) {
    stop(
      "'task' must be one of 'regression', 'classification', or 'both'.",
      call. = FALSE
    )
  }

  if (!is.list(parameters)) {
    stop("'parameters' must be a list.", call. = FALSE)
  }

  structure(
    list(
      name = name,
      fn = fn,
      task = task,
      parameters = parameters
    ),
    class = "met_metric"
  )
}


#' Mean Squared Error Metric
#'
#' Creates a mean squared error metric object.
#'
#' @return An object of class `"met_metric"`.
#' @export
#'
#' @examples
#' metric <- metric_mse()
#' metric$fn(c(1, 2, 3), c(1.1, 1.9, 3.2))
metric_mse <- function() {
  new_metric(
    name = "mse",
    task = "regression",
    fn = function(y_true, y_pred) {
      mean((y_true - y_pred)^2)
    }
  )
}


#' Root Mean Squared Error Metric
#'
#' Creates a root mean squared error metric object.
#'
#' @return An object of class `"met_metric"`.
#' @export
#'
#' @examples
#' metric <- metric_rmse()
#' metric$fn(c(1, 2, 3), c(1.1, 1.9, 3.2))
metric_rmse <- function() {
  new_metric(
    name = "rmse",
    task = "regression",
    fn = function(y_true, y_pred) {
      sqrt(mean((y_true - y_pred)^2))
    }
  )
}


#' Mean Absolute Error Metric
#'
#' Creates a mean absolute error metric object.
#'
#' @return An object of class `"met_metric"`.
#' @export
#'
#' @examples
#' metric <- metric_mae()
#' metric$fn(c(1, 2, 3), c(1.1, 1.9, 3.2))
metric_mae <- function() {
  new_metric(
    name = "mae",
    task = "regression",
    fn = function(y_true, y_pred) {
      mean(abs(y_true - y_pred))
    }
  )
}


#' Coefficient of Determination Metric
#'
#' Creates an R-squared metric object.
#'
#' @return An object of class `"met_metric"`.
#' @export
#'
#' @examples
#' metric <- metric_r2()
#' metric$fn(c(1, 2, 3), c(1.1, 1.9, 3.2))
metric_r2 <- function() {
  new_metric(
    name = "r2",
    task = "regression",
    fn = function(y_true, y_pred) {
      ss_res <- sum((y_true - y_pred)^2)
      ss_tot <- sum((y_true - mean(y_true))^2)

      if (ss_tot == 0) {
        return(NA_real_)
      }

      1 - ss_res / ss_tot
    }
  )
}


#' Accuracy Metric
#'
#' Creates an accuracy metric object for classification tasks.
#'
#' @return An object of class `"met_metric"`.
#' @export
#'
#' @examples
#' metric <- metric_accuracy()
#' metric$fn(c(0, 1, 1), c(0, 1, 0))
metric_accuracy <- function() {
  new_metric(
    name = "accuracy",
    task = "classification",
    fn = function(y_true, y_pred) {
      mean(y_true == y_pred)
    }
  )
}


#' Precision Metric
#'
#' Creates a precision metric object for classification tasks.
#'
#' @param positive_class The class label treated as the positive class.
#' Defaults to `1`.
#'
#' @return An object of class `"met_metric"`.
#' @export
#'
#' @examples
#' metric <- metric_precision()
#' metric$fn(c(0, 1, 1, 0), c(0, 1, 0, 0))
metric_precision <- function(positive_class = 1) {
  new_metric(
    name = "precision",
    task = "classification",
    fn = function(y_true, y_pred) {
      tp <- sum(y_true == positive_class & y_pred == positive_class)
      fp <- sum(y_true != positive_class & y_pred == positive_class)

      if ((tp + fp) == 0) {
        return(NA_real_)
      }

      tp / (tp + fp)
    },
    parameters = list(positive_class = positive_class)
  )
}


#' Recall Metric
#'
#' Creates a recall metric object for classification tasks.
#'
#' @param positive_class The class label treated as the positive class.
#' Defaults to `1`.
#'
#' @return An object of class `"met_metric"`.
#' @export
#'
#' @examples
#' metric <- metric_recall()
#' metric$fn(c(0, 1, 1, 0), c(0, 1, 0, 0))
metric_recall <- function(positive_class = 1) {
  new_metric(
    name = "recall",
    task = "classification",
    fn = function(y_true, y_pred) {
      tp <- sum(y_true == positive_class & y_pred == positive_class)
      fn <- sum(y_true == positive_class & y_pred != positive_class)

      if ((tp + fn) == 0) {
        return(NA_real_)
      }

      tp / (tp + fn)
    },
    parameters = list(positive_class = positive_class)
  )
}


#' F1 Score Metric
#'
#' Creates an F1 score metric object for classification tasks.
#'
#' @param positive_class The class label treated as the positive class.
#' Defaults to `1`.
#'
#' @return An object of class `"met_metric"`.
#' @export
#'
#' @examples
#' metric <- metric_f1()
#' metric$fn(c(0, 1, 1, 0), c(0, 1, 0, 0))
metric_f1 <- function(positive_class = 1) {
  new_metric(
    name = "f1",
    task = "classification",
    fn = function(y_true, y_pred) {
      tp <- sum(y_true == positive_class & y_pred == positive_class)
      fp <- sum(y_true != positive_class & y_pred == positive_class)
      fn <- sum(y_true == positive_class & y_pred != positive_class)

      precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
      recall <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)

      if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
        return(NA_real_)
      }

      2 * precision * recall / (precision + recall)
    },
    parameters = list(positive_class = positive_class)
  )
}


#' Check Whether an Object is a metANN Metric
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` is a metANN metric object; otherwise `FALSE`.
#' @export
#'
#' @examples
#' is_metric(metric_rmse())
is_metric <- function(x) {
  inherits(x, "met_metric")
}


#' Convert Character Input to a Metric Object
#'
#' Converts a character string such as `"rmse"` into the corresponding metric
#' function object.
#'
#' @param metric A character string or an object of class `"met_metric"`.
#'
#' @return An object of class `"met_metric"`.
#' @export
#'
#' @examples
#' as_metric("rmse")
#' as_metric(metric_accuracy())
as_metric <- function(metric) {
  if (is_metric(metric)) {
    return(metric)
  }

  if (!is.character(metric) || length(metric) != 1L) {
    stop(
      "'metric' must be a single character string or a met_metric object.",
      call. = FALSE
    )
  }

  metric <- tolower(metric)

  switch(
    metric,
    mse = metric_mse(),
    rmse = metric_rmse(),
    mae = metric_mae(),
    r2 = metric_r2(),
    accuracy = metric_accuracy(),
    precision = metric_precision(),
    recall = metric_recall(),
    f1 = metric_f1(),
    stop("Unknown metric: '", metric, "'.", call. = FALSE)
  )
}


#' Convert Multiple Inputs to Metric Objects
#'
#' Converts a character vector or a list of metric objects into a list of
#' metric objects.
#'
#' @param metrics A character vector, a single metric object, or a list of
#' metric objects.
#'
#' @return A list of objects of class `"met_metric"`.
#' @export
#'
#' @examples
#' as_metrics(c("rmse", "mae", "r2"))
#' as_metrics(list(metric_accuracy(), metric_f1()))
as_metrics <- function(metrics) {
  if (is.null(metrics)) {
    return(list())
  }

  if (is_metric(metrics)) {
    return(list(metrics))
  }

  if (is.character(metrics)) {
    return(lapply(metrics, as_metric))
  }

  if (is.list(metrics)) {
    converted <- lapply(metrics, as_metric)
    return(converted)
  }

  stop(
    "'metrics' must be a character vector, a met_metric object, or a list of met_metric objects.",
    call. = FALSE
  )
}
