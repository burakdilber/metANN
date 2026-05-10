#' Detect Task Type
#'
#' Internal helper for detecting whether the response corresponds to a
#' regression or classification task.
#'
#' @param y Response vector.
#' @param task Character value. One of `"auto"`, `"regression"`, or
#' `"classification"`.
#'
#' @return A character value: `"regression"` or `"classification"`.
#' @keywords internal
detect_task <- function(y, task = "auto") {
  task <- match.arg(task, choices = c("auto", "regression", "classification"))

  if (task != "auto") {
    return(task)
  }

  if (is.factor(y) || is.character(y) || is.logical(y)) {
    return("classification")
  }

  "regression"
}


#' Encode Classification Response
#'
#' Internal helper for encoding binary and multi-class responses.
#'
#' @param y Response vector.
#'
#' @return A list containing encoded response, class levels, and classification
#' type.
#' @keywords internal
encode_classification_response <- function(y) {
  if (is.logical(y)) {
    y <- factor(y, levels = c(FALSE, TRUE))
  } else {
    y <- factor(y)
  }

  class_levels <- levels(y)
  n_classes <- length(class_levels)

  if (n_classes < 2L) {
    stop(
      "Classification requires at least two classes.",
      call. = FALSE
    )
  }

  if (n_classes == 2L) {
    encoded <- as.numeric(y == class_levels[2L])

    return(
      list(
        y_encoded = encoded,
        class_levels = class_levels,
        n_classes = n_classes,
        classification_type = "binary"
      )
    )
  }

  encoded <- matrix(0, nrow = length(y), ncol = n_classes)
  encoded[cbind(seq_along(y), as.integer(y))] <- 1
  colnames(encoded) <- class_levels

  list(
    y_encoded = encoded,
    class_levels = class_levels,
    n_classes = n_classes,
    classification_type = "multiclass"
  )
}


#' Decode Classification Predictions
#'
#' Internal helper for converting probabilities to class labels.
#'
#' @param probabilities Numeric vector or matrix of predicted probabilities.
#' @param class_levels Class labels.
#' @param classification_type Either `"binary"` or `"multiclass"`.
#' @param threshold Classification threshold for binary classification.
#'
#' @return A factor of predicted class labels.
#' @keywords internal
decode_classification_prediction <- function(probabilities,
                                             class_levels,
                                             classification_type,
                                             threshold = 0.5) {
  if (classification_type == "binary") {
    probabilities <- as.numeric(probabilities)

    predicted_index <- ifelse(probabilities >= threshold, 2L, 1L)

    return(
      factor(
        class_levels[predicted_index],
        levels = class_levels
      )
    )
  }

  if (classification_type == "multiclass") {
    if (!is.matrix(probabilities)) {
      probabilities <- as.matrix(probabilities)
    }

    predicted_index <- max.col(probabilities, ties.method = "first")

    return(
      factor(
        class_levels[predicted_index],
        levels = class_levels
      )
    )
  }

  stop(
    "Unknown classification type.",
    call. = FALSE
  )
}


#' Classification Loss Value
#'
#' Internal helper for computing binary or multi-class classification loss.
#'
#' @param y_true Encoded true response.
#' @param y_pred Predicted probabilities.
#' @param classification_type Either `"binary"` or `"multiclass"`.
#' @param epsilon Small value used for numerical stability.
#'
#' @return A single numeric loss value.
#' @keywords internal
classification_loss_value <- function(y_true,
                                      y_pred,
                                      classification_type,
                                      epsilon = 1e-15) {
  if (classification_type == "binary") {
    p <- pmin(pmax(as.numeric(y_pred), epsilon), 1 - epsilon)
    y_true <- as.numeric(y_true)

    return(
      -mean(y_true * log(p) + (1 - y_true) * log(1 - p))
    )
  }

  if (classification_type == "multiclass") {
    p <- pmin(pmax(y_pred, epsilon), 1 - epsilon)

    return(
      -mean(rowSums(y_true * log(p)))
    )
  }

  stop(
    "Unknown classification type.",
    call. = FALSE
  )
}


#' Classification Metric Values
#'
#' Internal helper for computing basic classification metrics.
#'
#' @param y_true True class labels.
#' @param y_pred Predicted class labels.
#'
#' @return A named numeric vector.
#' @keywords internal
classification_metric_values <- function(y_true, y_pred) {
  y_true <- factor(y_true)
  y_pred <- factor(y_pred, levels = levels(y_true))

  accuracy <- mean(y_true == y_pred)

  classes <- levels(y_true)

  precision_values <- numeric(length(classes))
  recall_values <- numeric(length(classes))
  f1_values <- numeric(length(classes))

  for (i in seq_along(classes)) {
    cls <- classes[i]

    tp <- sum(y_true == cls & y_pred == cls)
    fp <- sum(y_true != cls & y_pred == cls)
    fn <- sum(y_true == cls & y_pred != cls)

    precision_values[i] <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
    recall_values[i] <- if ((tp + fn) == 0) 0 else tp / (tp + fn)

    f1_values[i] <- if ((precision_values[i] + recall_values[i]) == 0) {
      0
    } else {
      2 * precision_values[i] * recall_values[i] /
        (precision_values[i] + recall_values[i])
    }
  }

  c(
    accuracy = accuracy,
    precision = mean(precision_values),
    recall = mean(recall_values),
    f1 = mean(f1_values)
  )
}
