#' List Available Optimizers
#'
#' Returns the names of optimization algorithms currently available in
#' the metANN package.
#'
#' @return A character vector of optimizer names.
#' @export
#'
#' @examples
#' available_optimizers()
available_optimizers <- function() {
  c(
    available_metaheuristics(),
    available_gradient_optimizers()
  )
}


#' List Available Metaheuristic Optimizers
#'
#' Returns the names of metaheuristic optimization algorithms currently
#' available in the metANN package.
#'
#' @return A character vector of metaheuristic optimizer names.
#' @export
#'
#' @examples
#' available_metaheuristics()
available_metaheuristics <- function() {
  c(
    "pso",
    "de",
    "ga",
    "abc",
    "gwo",
    "woa",
    "tlbo",
    "sboa"
  )
}


#' List Available Gradient-Based Optimizers
#'
#' Returns the names of gradient-based optimizer objects currently available
#' in the metANN package.
#'
#' @return A character vector of gradient-based optimizer names.
#' @export
#'
#' @examples
#' available_gradient_optimizers()
available_gradient_optimizers <- function() {
  c(
    "sgd",
    "adam"
  )
}


#' List Available Activation Functions
#'
#' Returns the names of activation functions currently available in the
#' metANN package.
#'
#' @return A character vector of activation function names.
#' @export
#'
#' @examples
#' available_activations()
available_activations <- function() {
  c(
    "linear",
    "sigmoid",
    "tanh",
    "relu",
    "leaky_relu",
    "softmax"
  )
}


#' List Available Loss Functions
#'
#' Returns the names of loss functions currently available in the metANN
#' package.
#'
#' @return A character vector of loss function names.
#' @export
#'
#' @examples
#' available_losses()
available_losses <- function() {
  c(
    "mse",
    "mae",
    "huber",
    "log_cosh",
    "binary_crossentropy",
    "crossentropy"
  )
}


#' List Available Performance Metrics
#'
#' Returns the names of performance metrics currently available in the
#' metANN package.
#'
#' @return A character vector of metric names.
#' @export
#'
#' @examples
#' available_metrics()
available_metrics <- function() {
  c(
    "mse",
    "rmse",
    "mae",
    "r2",
    "accuracy",
    "precision",
    "recall",
    "f1"
  )
}
