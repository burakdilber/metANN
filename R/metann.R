#' Train an Artificial Neural Network with metANN
#'
#' Trains a feed-forward multilayer perceptron using metaheuristic or
#' gradient-based optimization algorithms. The function supports regression
#' and classification tasks through either an x-y interface or a formula-data
#' interface.
#'
#' @param formula Optional formula specifying the model.
#' @param data Optional data frame containing the variables in `formula`.
#' @param x Optional numeric matrix or data frame of input features.
#' @param y Optional response vector or one-column matrix.
#' @param architecture Optional MLP architecture created by `mlp_architecture()`.
#' @param hidden_layers Optional integer vector specifying hidden layer sizes.
#' Used when `architecture` is not supplied.
#' @param activation Activation function used for hidden layers when
#' `hidden_layers` is supplied. It can be a single value or a vector with the
#' same length as `hidden_layers`.
#' @param output_activation Optional activation function used for the output
#' layer when `hidden_layers` is supplied. If `NULL`, it is selected
#' automatically based on the task.
#' @param task One of `"auto"`, `"regression"`, or `"classification"`. If
#' `"auto"`, the task is detected from the response variable.
#' @param optimizer A character string or a metANN optimizer object.
#' @param loss Optional character string or metANN loss object. If `NULL`, it is
#' selected automatically based on the task.
#' @param metrics Optional character vector, metric object, or list of metric
#' objects. If `NULL`, default metrics are selected automatically based on the
#' task.
#' @param seed Optional random seed.
#' @param verbose Logical. If `TRUE`, optimization or training progress is
#' printed.
#'
#' @return An object of class `"metann"`.
#' @references
#' Montana, D. J., and Davis, L. (1989). Training Feedforward Neural Networks
#' Using Genetic Algorithms. Proceedings of the 11th International Joint
#' Conference on Artificial Intelligence, 762--767.
#'
#' Ilonen, J., Kamarainen, J.-K., and Lampinen, J. (2003). Differential
#' Evolution Training Algorithm for Feed-Forward Neural Networks.
#' Neural Processing Letters, 17, 93--105.
#' doi:10.1023/A:1022995128597
#'
#' Karaboga, D., and Ozturk, C. (2009). Neural Networks Training by
#' Artificial Bee Colony Algorithm on Pattern Classification.
#' Neural Network World, 19(3), 279--292.
#'
#' Mirjalili, S. (2015). How Effective is the Grey Wolf Optimizer in Training
#' Multi-Layer Perceptrons. Applied Intelligence, 43, 150--161.
#' doi:10.1007/s10489-014-0645-7
#'
#' Dilber, B., and Ozdemir, A. F. (2026). A novel approach to training
#' feed-forward multi-layer perceptrons with recently proposed secretary bird
#' optimization algorithm. Neural Computing and Applications, 38(5).
#' doi:10.1007/s00521-026-11874-x
#' @export
#'
#' @examples
#' fit <- metann(
#'   formula = Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length,
#'   data = iris,
#'   hidden_layers = c(5),
#'   optimizer = optimizer_pso(pop_size = 10, max_iter = 20),
#'   loss = "mse",
#'   metrics = c("rmse", "mae", "r2"),
#'   seed = 123,
#'   verbose = FALSE
#' )
#' fit
#'
#' iris_bin <- iris
#' iris_bin$IsSetosa <- factor(
#'   ifelse(iris_bin$Species == "setosa", "setosa", "other")
#' )
#'
#' fit_class <- metann(
#'   formula = IsSetosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'   data = iris_bin,
#'   hidden_layers = c(5),
#'   optimizer = optimizer_pso(pop_size = 10, max_iter = 20),
#'   seed = 123,
#'   verbose = FALSE
#' )
#' fit_class
metann <- function(formula = NULL,
                   data = NULL,
                   x = NULL,
                   y = NULL,
                   architecture = NULL,
                   hidden_layers = NULL,
                   activation = "relu",
                   output_activation = NULL,
                   task = c("auto", "regression", "classification"),
                   optimizer = optimizer_pso(),
                   loss = NULL,
                   metrics = NULL,
                   seed = NULL,
                   verbose = TRUE) {
  call <- match.call()
  task <- match.arg(task)

  formula_mode <- !is.null(formula) || !is.null(data)

  if (formula_mode) {
    if (is.null(formula) || is.null(data)) {
      stop(
        "Both 'formula' and 'data' must be supplied when using the formula interface.",
        call. = FALSE
      )
    }

    if (!inherits(formula, "formula")) {
      stop("'formula' must be a formula object.", call. = FALSE)
    }

    if (!is.data.frame(data)) {
      stop("'data' must be a data frame.", call. = FALSE)
    }

    if (!is.null(x) || !is.null(y)) {
      stop(
        "When using 'formula' and 'data', do not also supply 'x' or 'y'.",
        call. = FALSE
      )
    }

    model_frame <- stats::model.frame(formula, data = data)
    y <- stats::model.response(model_frame)

    x <- stats::model.matrix(formula, data = model_frame)

    intercept_col <- match("(Intercept)", colnames(x), nomatch = 0L)
    if (intercept_col > 0L) {
      x <- x[, -intercept_col, drop = FALSE]
    }
  }

  if (is.null(x) || is.null(y)) {
    stop(
      "Please provide either 'formula' and 'data', or both 'x' and 'y'.",
      call. = FALSE
    )
  }

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (!is.matrix(x) || !is.numeric(x)) {
    stop("'x' must be a numeric matrix or a numeric data frame.", call. = FALSE)
  }

  if (is.matrix(y)) {
    if (ncol(y) != 1L) {
      stop("'y' must be a vector or a single-column matrix.", call. = FALSE)
    }

    y <- y[, 1L]
  }

  if (length(y) != nrow(x)) {
    stop(
      "'y' must have length equal to nrow(x).",
      call. = FALSE
    )
  }

  task <- detect_task(y = y, task = task)

  classification_info <- NULL
  y_original <- y

  if (task == "classification") {
    classification_info <- encode_classification_response(y)

    y_model <- classification_info$y_encoded

    if (is.null(output_activation)) {
      output_activation <- if (classification_info$classification_type == "binary") {
        "sigmoid"
      } else {
        "softmax"
      }
    }

    if (is.null(loss)) {
      loss <- if (classification_info$classification_type == "binary") {
        "binary_crossentropy"
      } else {
        "crossentropy"
      }
    }

    if (is.null(metrics)) {
      metrics <- c("accuracy", "precision", "recall", "f1")
    }
  } else {
    if (!is.numeric(y)) {
      stop(
        "'y' must be numeric for regression. For classification, use a factor, character, or logical response.",
        call. = FALSE
      )
    }

    y <- as.numeric(y)
    y_model <- y
    y_original <- y

    if (is.null(output_activation)) {
      output_activation <- "linear"
    }

    if (is.null(loss)) {
      loss <- "mse"
    }

    if (is.null(metrics)) {
      metrics <- c("rmse", "mae", "r2")
    }
  }

  if (is.null(architecture)) {
    if (is.null(hidden_layers)) {
      stop(
        "Please provide either 'architecture' or 'hidden_layers'.",
        call. = FALSE
      )
    }

    if (!is.numeric(hidden_layers) ||
        length(hidden_layers) == 0L ||
        any(hidden_layers <= 0) ||
        any(hidden_layers != as.integer(hidden_layers))) {
      stop(
        "'hidden_layers' must be a vector of positive integers.",
        call. = FALSE
      )
    }

    hidden_layers <- as.integer(hidden_layers)

    if (length(activation) == 1L) {
      hidden_activations <- rep(activation, length(hidden_layers))
    } else if (length(activation) == length(hidden_layers)) {
      hidden_activations <- activation
    } else {
      stop(
        "'activation' must have length 1 or the same length as 'hidden_layers'.",
        call. = FALSE
      )
    }

    output_units <- if (task == "classification" &&
                        classification_info$classification_type == "multiclass") {
      classification_info$n_classes
    } else {
      1L
    }

    layers <- vector("list", length(hidden_layers) + 1L)

    for (i in seq_along(hidden_layers)) {
      layers[[i]] <- dense_layer(
        units = hidden_layers[i],
        activation = hidden_activations[[i]]
      )
    }

    layers[[length(layers)]] <- dense_layer(
      units = output_units,
      activation = output_activation
    )

    architecture <- mlp_architecture(
      input_dim = ncol(x),
      layers = layers
    )
  } else {
    if (!is.null(hidden_layers)) {
      stop(
        "Please provide either 'architecture' or 'hidden_layers', not both.",
        call. = FALSE
      )
    }
  }

  if (!is_mlp_architecture(architecture)) {
    stop("'architecture' must be an MLP architecture object.", call. = FALSE)
  }

  if (is.null(architecture$input_dim)) {
    architecture$input_dim <- ncol(x)
  }

  if (architecture$input_dim != ncol(x)) {
    stop(
      "The architecture input_dim is ", architecture$input_dim,
      ", but 'x' has ", ncol(x), " columns.",
      call. = FALSE
    )
  }

  last_layer <- architecture$layers[[length(architecture$layers)]]

  expected_output_units <- if (task == "classification" &&
                               classification_info$classification_type == "multiclass") {
    classification_info$n_classes
  } else {
    1L
  }

  if (last_layer$units != expected_output_units) {
    stop(
      "The output layer must have ",
      expected_output_units,
      " unit(s) for this task.",
      call. = FALSE
    )
  }

  optimizer <- as_optimizer(optimizer)
  loss <- as_loss(loss)
  metrics <- as_metrics(metrics)

  n_parameters <- count_parameters(architecture)

  if (optimizer$type == "metaheuristic") {
    objective <- function(weights) {
      pred <- forward_pass(
        x = x,
        weights = weights,
        architecture = architecture
      )

      if (task == "regression") {
        pred <- as.numeric(pred[, 1L])

        return(
          loss$fn(y_model, pred)
        )
      }

      if (classification_info$classification_type == "binary") {
        pred <- as.numeric(pred[, 1L])

        return(
          classification_loss_value(
            y_true = y_model,
            y_pred = pred,
            classification_type = "binary"
          )
        )
      }

      classification_loss_value(
        y_true = y_model,
        y_pred = pred,
        classification_type = "multiclass"
      )
    }

    opt_result <- met_optimize(
      fn = objective,
      optimizer = optimizer,
      lower = rep(-1, n_parameters),
      upper = rep(1, n_parameters),
      seed = seed,
      verbose = verbose
    )
  } else if (optimizer$type == "gradient") {
    opt_result <- train_mlp_gradient(
      x = x,
      y = y_model,
      architecture = architecture,
      optimizer = optimizer,
      loss = loss,
      seed = seed,
      verbose = verbose
    )
  } else {
    stop(
      "Hybrid training is not implemented yet. Please use a metaheuristic or gradient-based optimizer.",
      call. = FALSE
    )
  }

  final_weights <- stats::coef(opt_result)

  fitted_raw <- forward_pass(
    x = x,
    weights = final_weights,
    architecture = architecture
  )

  if (task == "regression") {
    fitted_values <- as.numeric(fitted_raw[, 1L])
    predicted_probabilities <- NULL
    predicted_class <- NULL

    metric_values <- compute_metric_values(
      metrics = metrics,
      y_true = y_model,
      y_pred = fitted_values
    )
  } else {
    if (classification_info$classification_type == "binary") {
      predicted_probabilities <- as.numeric(fitted_raw[, 1L])
    } else {
      predicted_probabilities <- fitted_raw
      colnames(predicted_probabilities) <- classification_info$class_levels
    }

    predicted_class <- decode_classification_prediction(
      probabilities = predicted_probabilities,
      class_levels = classification_info$class_levels,
      classification_type = classification_info$classification_type
    )

    fitted_values <- predicted_class

    metric_values <- classification_metric_values(
      y_true = y_original,
      y_pred = predicted_class
    )
  }

  result <- list(
    task = task,
    classification_type = if (!is.null(classification_info)) {
      classification_info$classification_type
    } else {
      NULL
    },
    class_levels = if (!is.null(classification_info)) {
      classification_info$class_levels
    } else {
      NULL
    },
    n_classes = if (!is.null(classification_info)) {
      classification_info$n_classes
    } else {
      NULL
    },
    formula = if (formula_mode) formula else NULL,
    formula_mode = formula_mode,
    terms = if (formula_mode) {
      stats::terms(formula, data = data)
    } else {
      NULL
    },
    xlevels = if (formula_mode) {
      stats::.getXlevels(stats::terms(formula, data = data), data)
    } else {
      NULL
    },
    architecture = architecture,
    optimizer = optimizer,
    loss = loss,
    metrics = metrics,
    metric_values = metric_values,
    weights = final_weights,
    optimization = opt_result,
    fitted_values = fitted_values,
    predicted_probabilities = predicted_probabilities,
    predicted_class = predicted_class,
    residuals = if (task == "regression") {
      y_model - fitted_values
    } else {
      NULL
    },
    y = y_original,
    y_model = y_model,
    x_colnames = colnames(x),
    input_dim = ncol(x),
    n_obs = nrow(x),
    call = call
  )

  class(result) <- "metann"

  result
}


#' Compute Metric Values
#'
#' Internal helper for evaluating a list of metric objects.
#'
#' @param metrics A list of metric objects.
#' @param y_true Observed values.
#' @param y_pred Predicted values.
#'
#' @return A named numeric vector of metric values.
#' @keywords internal
compute_metric_values <- function(metrics, y_true, y_pred) {
  if (length(metrics) == 0L) {
    return(numeric())
  }

  values <- vapply(
    metrics,
    function(metric) {
      metric$fn(y_true, y_pred)
    },
    numeric(1L)
  )

  names(values) <- vapply(
    metrics,
    function(metric) metric$name,
    character(1L)
  )

  values
}

#' Predict with a metANN Model
#'
#' Generates predictions from a fitted metANN model.
#'
#' @param object A fitted object of class `"metann"`.
#' @param newdata New data used for prediction. For formula-based models, this
#' should be a data frame. For x-y models, this should be a numeric matrix or
#' numeric data frame.
#' @param type Prediction type. For regression models, `"response"` returns
#' numeric predictions. For classification models, `"class"` returns predicted
#' class labels, `"prob"` returns predicted probabilities, and `"response"`
#' returns the default response, which is class labels.
#' @param threshold Classification threshold for binary classification.
#' @param ... Additional arguments.
#'
#' @return A numeric vector, matrix, or factor depending on the task and
#' prediction type.
#' @export
predict.metann <- function(object,
                           newdata,
                           type = c("response", "prob", "class"),
                           threshold = 0.5,
                           ...) {
  type <- match.arg(type)

  if (!inherits(object, "metann")) {
    stop("'object' must be a fitted metANN model.", call. = FALSE)
  }

  if (!is.numeric(threshold) ||
      length(threshold) != 1L ||
      threshold <= 0 ||
      threshold >= 1) {
    stop("'threshold' must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  if (missing(newdata) || is.null(newdata)) {
    stop("'newdata' must be supplied.", call. = FALSE)
  }

  if (isTRUE(object$formula_mode)) {
    if (!is.data.frame(newdata)) {
      newdata <- as.data.frame(newdata)
    }

    terms_no_response <- stats::delete.response(object$terms)

    model_frame <- stats::model.frame(
      terms_no_response,
      data = newdata,
      xlev = object$xlevels
    )

    x_new <- stats::model.matrix(
      terms_no_response,
      data = model_frame
    )

    intercept_col <- match("(Intercept)", colnames(x_new), nomatch = 0L)
    if (intercept_col > 0L) {
      x_new <- x_new[, -intercept_col, drop = FALSE]
    }

    missing_cols <- setdiff(object$x_colnames, colnames(x_new))
    if (length(missing_cols) > 0L) {
      stop(
        "The following required columns are missing from 'newdata': ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }

    x_new <- x_new[, object$x_colnames, drop = FALSE]
  } else {
    if (is.data.frame(newdata)) {
      x_new <- as.matrix(newdata)
    } else {
      x_new <- newdata
    }

    if (!is.matrix(x_new) || !is.numeric(x_new)) {
      stop(
        "'newdata' must be a numeric matrix or numeric data frame.",
        call. = FALSE
      )
    }

    if (ncol(x_new) != object$input_dim) {
      stop(
        "'newdata' must have ",
        object$input_dim,
        " columns.",
        call. = FALSE
      )
    }

    if (!is.null(object$x_colnames) && !is.null(colnames(x_new))) {
      missing_cols <- setdiff(object$x_colnames, colnames(x_new))

      if (length(missing_cols) == 0L) {
        x_new <- x_new[, object$x_colnames, drop = FALSE]
      }
    }
  }

  raw_pred <- forward_pass(
    x = x_new,
    weights = object$weights,
    architecture = object$architecture
  )

  if (object$task == "regression") {
    if (type %in% c("prob", "class")) {
      stop(
        "'type = \"prob\"' and 'type = \"class\"' are available only for classification models.",
        call. = FALSE
      )
    }

    return(as.numeric(raw_pred[, 1L]))
  }

  if (object$classification_type == "binary") {
    probabilities <- as.numeric(raw_pred[, 1L])

    if (type == "prob") {
      out <- cbind(
        1 - probabilities,
        probabilities
      )

      colnames(out) <- object$class_levels

      return(out)
    }

    predicted_class <- decode_classification_prediction(
      probabilities = probabilities,
      class_levels = object$class_levels,
      classification_type = object$classification_type,
      threshold = threshold
    )

    return(predicted_class)
  }

  if (object$classification_type == "multiclass") {
    probabilities <- raw_pred
    colnames(probabilities) <- object$class_levels

    if (type == "prob") {
      return(probabilities)
    }

    predicted_class <- decode_classification_prediction(
      probabilities = probabilities,
      class_levels = object$class_levels,
      classification_type = object$classification_type,
      threshold = threshold
    )

    return(predicted_class)
  }

  stop(
    "Unknown model task or classification type.",
    call. = FALSE
  )
}

#' Print a metANN Model
#'
#' @param x A metANN model object.
#' @param ... Additional arguments, currently unused.
#'
#' @return The input object invisibly.
#' @export
print.metann <- function(x, ...) {
  cat("metANN model\n")
  cat("  Task         :", x$task, "\n")
  cat("  Optimizer    :", x$optimizer$name, "\n")
  cat("  Loss         :", x$loss$name, "\n")
  cat("  Observations :", x$n_obs, "\n")
  cat("  Input dim    :", x$input_dim, "\n")
  cat("  Parameters   :", length(x$weights), "\n")
  cat("  Best loss    :", x$optimization$best_value, "\n")

  if (length(x$metric_values) > 0L) {
    cat("\nMetrics:\n")
    print(x$metric_values)
  }

  invisible(x)
}


#' Summarize a metANN Model
#'
#' @param object A metANN model object.
#' @param ... Additional arguments, currently unused.
#'
#' @return A list containing model summary information.
#' @export
summary.metann <- function(object, ...) {
  cat("metANN model summary\n")
  cat("  Task        :", object$task, "\n")
  cat("  Optimizer   :", object$optimizer$name, "\n")
  cat("  Loss        :", object$loss$name, "\n")
  cat("  Observations:", object$n_obs, "\n")
  cat("  Input dim   :", object$input_dim, "\n")
  cat("  Parameters  :", length(object$weights), "\n")
  cat("  Best loss   :", object$optimization$best_value, "\n\n")

  cat("Architecture:\n")
  print(object$architecture)

  if (length(object$metric_values) > 0L) {
    cat("\nMetrics:\n")
    print(object$metric_values)
  }

  invisible(
    list(
      task = object$task,
      optimizer = object$optimizer$name,
      loss = object$loss$name,
      metric_values = object$metric_values,
      best_loss = object$optimization$best_value,
      architecture = object$architecture,
      weights = object$weights
    )
  )
}


#' Extract Weights from a metANN Model
#'
#' @param object A fitted metANN model.
#' @param ... Additional arguments, currently unused.
#'
#' @return A numeric vector of fitted network weights.
#' @export
coef.metann <- function(object, ...) {
  object$weights
}


#' Plot a metANN Model
#'
#' @param x A fitted metANN model.
#' @param ... Additional arguments passed to `plot()`.
#'
#' @return The input object invisibly.
#' @export
plot.metann <- function(x, ...) {
  graphics::plot(
    x$optimization$convergence,
    type = "l",
    xlab = "Iteration",
    ylab = "Best loss",
    main = paste("Training convergence -", toupper(x$optimizer$name)),
    ...
  )

  invisible(x)
}
