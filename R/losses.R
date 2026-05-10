#' Create a Loss Function Object
#'
#' Internal helper for constructing loss function objects.
#'
#' @param name A character string specifying the loss name.
#' @param fn A function that computes the loss from observed and predicted values.
#' @param parameters A list of loss-specific parameters.
#'
#' @return An object of class `"met_loss"`.
#' @keywords internal
new_loss <- function(name, fn, parameters = list()) {
  if (!is.character(name) || length(name) != 1L) {
    stop("'name' must be a single character string.", call. = FALSE)
  }

  if (!is.function(fn)) {
    stop("'fn' must be a function.", call. = FALSE)
  }

  if (!is.list(parameters)) {
    stop("'parameters' must be a list.", call. = FALSE)
  }

  structure(
    list(
      name = name,
      fn = fn,
      parameters = parameters
    ),
    class = "met_loss"
  )
}


#' Mean Squared Error Loss
#'
#' Creates a mean squared error loss function object.
#'
#' @return An object of class `"met_loss"`.
#' @export
#'
#' @examples
#' loss <- loss_mse()
#' loss$fn(c(1, 2, 3), c(1.1, 1.9, 3.2))
loss_mse <- function() {
  new_loss(
    name = "mse",
    fn = function(y_true, y_pred) {
      mean((y_true - y_pred)^2)
    }
  )
}


#' Mean Absolute Error Loss
#'
#' Creates a mean absolute error loss function object.
#'
#' @return An object of class `"met_loss"`.
#' @export
#'
#' @examples
#' loss <- loss_mae()
#' loss$fn(c(1, 2, 3), c(1.1, 1.9, 3.2))
loss_mae <- function() {
  new_loss(
    name = "mae",
    fn = function(y_true, y_pred) {
      mean(abs(y_true - y_pred))
    }
  )
}


#' Huber Loss
#'
#' Creates a Huber loss function object.
#'
#' @param delta A positive numeric value controlling the transition point
#' between squared and absolute loss behavior.
#'
#' @return An object of class `"met_loss"`.
#' @references
#' Huber, P. J. (1964). Robust Estimation of a Location Parameter.
#' The Annals of Mathematical Statistics, 35(1), 73--101.
#' doi:10.1214/aoms/1177703732
#' @export
#'
#' @examples
#' loss <- loss_huber(delta = 1)
#' loss$fn(c(1, 2, 3), c(1.1, 1.9, 3.2))
loss_huber <- function(delta = 1) {
  if (!is.numeric(delta) || length(delta) != 1L || delta <= 0) {
    stop("'delta' must be a single positive numeric value.", call. = FALSE)
  }

  new_loss(
    name = "huber",
    fn = function(y_true, y_pred) {
      error <- y_true - y_pred
      abs_error <- abs(error)

      loss_values <- ifelse(
        abs_error <= delta,
        0.5 * error^2,
        delta * (abs_error - 0.5 * delta)
      )

      mean(loss_values)
    },
    parameters = list(delta = delta)
  )
}


#' Log-Cosh Loss
#'
#' Creates a log-cosh loss function object.
#'
#' @return An object of class `"met_loss"`.
#' @export
#'
#' @examples
#' loss <- loss_log_cosh()
#' loss$fn(c(1, 2, 3), c(1.1, 1.9, 3.2))
loss_log_cosh <- function() {
  new_loss(
    name = "log_cosh",
    fn = function(y_true, y_pred) {
      error <- y_pred - y_true
      mean(log(cosh(error)))
    }
  )
}


#' Binary Cross-Entropy Loss
#'
#' Creates a binary cross-entropy loss function object.
#'
#' @param epsilon A small positive numeric value used for numerical stability.
#'
#' @return An object of class `"met_loss"`.
#' @references
#' Bridle, J. S. (1990). Probabilistic Interpretation of Feedforward
#' Classification Network Outputs, with Relationships to Statistical Pattern
#' Recognition. In Neurocomputing: Algorithms, Architectures and Applications,
#' 227--236. Springer.
#' @export
#'
#' @examples
#' loss <- loss_binary_crossentropy()
#' loss$fn(c(0, 1, 1), c(0.1, 0.8, 0.9))
loss_binary_crossentropy <- function(epsilon = 1e-15) {
  if (!is.numeric(epsilon) || length(epsilon) != 1L || epsilon <= 0) {
    stop("'epsilon' must be a single positive numeric value.", call. = FALSE)
  }

  new_loss(
    name = "binary_crossentropy",
    fn = function(y_true, y_pred) {
      y_pred <- pmin(pmax(y_pred, epsilon), 1 - epsilon)
      -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
    },
    parameters = list(epsilon = epsilon)
  )
}


#' Categorical Cross-Entropy Loss
#'
#' Creates a categorical cross-entropy loss function object.
#'
#' @param epsilon A small positive numeric value used for numerical stability.
#'
#' @return An object of class `"met_loss"`.
#' @references
#' Bridle, J. S. (1990). Probabilistic Interpretation of Feedforward
#' Classification Network Outputs, with Relationships to Statistical Pattern
#' Recognition. In Neurocomputing: Algorithms, Architectures and Applications,
#' 227--236. Springer.
#' @export
#'
#' @examples
#' loss <- loss_crossentropy()
#' y_true <- matrix(c(1, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE)
#' y_pred <- matrix(c(0.8, 0.1, 0.1, 0.2, 0.7, 0.1), nrow = 2, byrow = TRUE)
#' loss$fn(y_true, y_pred)
loss_crossentropy <- function(epsilon = 1e-15) {
  if (!is.numeric(epsilon) || length(epsilon) != 1L || epsilon <= 0) {
    stop("'epsilon' must be a single positive numeric value.", call. = FALSE)
  }

  new_loss(
    name = "crossentropy",
    fn = function(y_true, y_pred) {
      y_pred <- pmin(pmax(y_pred, epsilon), 1 - epsilon)

      if (is.vector(y_true) && is.vector(y_pred)) {
        return(-mean(y_true * log(y_pred)))
      }

      if (is.matrix(y_true) && is.matrix(y_pred)) {
        return(-mean(rowSums(y_true * log(y_pred))))
      }

      stop(
        "'y_true' and 'y_pred' must both be vectors or both be matrices.",
        call. = FALSE
      )
    },
    parameters = list(epsilon = epsilon)
  )
}


#' Check Whether an Object is a metANN Loss
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` is a metANN loss object; otherwise `FALSE`.
#' @export
#'
#' @examples
#' is_loss(loss_mse())
is_loss <- function(x) {
  inherits(x, "met_loss")
}


#' Convert Character Input to a Loss Object
#'
#' Converts a character string such as `"mse"` into the corresponding loss
#' function object.
#'
#' @param loss A character string or an object of class `"met_loss"`.
#'
#' @return An object of class `"met_loss"`.
#' @export
#'
#' @examples
#' as_loss("mse")
#' as_loss(loss_huber(delta = 1.5))
as_loss <- function(loss) {
  if (is_loss(loss)) {
    return(loss)
  }

  if (!is.character(loss) || length(loss) != 1L) {
    stop(
      "'loss' must be a single character string or a met_loss object.",
      call. = FALSE
    )
  }

  loss <- tolower(loss)

  switch(
    loss,
    mse = loss_mse(),
    mae = loss_mae(),
    huber = loss_huber(),
    log_cosh = loss_log_cosh(),
    binary_crossentropy = loss_binary_crossentropy(),
    crossentropy = loss_crossentropy(),
    stop("Unknown loss function: '", loss, "'.", call. = FALSE)
  )
}
