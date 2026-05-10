#' Create an Activation Function Object
#'
#' Internal helper for constructing activation function objects.
#'
#' @param name A character string specifying the activation name.
#' @param fn A function that applies the activation to numeric input.
#' @param parameters A list of activation-specific parameters.
#'
#' @return An object of class `"met_activation"`.
#' @keywords internal
new_activation <- function(name, fn, parameters = list()) {
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
    class = "met_activation"
  )
}


#' Linear Activation Function
#'
#' Creates a linear activation function object.
#'
#' @return An object of class `"met_activation"`.
#' @export
#'
#' @examples
#' act <- activation_linear()
#' act$fn(c(-1, 0, 1))
activation_linear <- function() {
  new_activation(
    name = "linear",
    fn = function(x) x
  )
}


#' Sigmoid Activation Function
#'
#' Creates a sigmoid activation function object.
#'
#' @return An object of class `"met_activation"`.
#' @export
#'
#' @examples
#' act <- activation_sigmoid()
#' act$fn(c(-1, 0, 1))
activation_sigmoid <- function() {
  new_activation(
    name = "sigmoid",
    fn = function(x) {
      1 / (1 + exp(-x))
    }
  )
}


#' Hyperbolic Tangent Activation Function
#'
#' Creates a hyperbolic tangent activation function object.
#'
#' @return An object of class `"met_activation"`.
#' @export
#'
#' @examples
#' act <- activation_tanh()
#' act$fn(c(-1, 0, 1))
activation_tanh <- function() {
  new_activation(
    name = "tanh",
    fn = tanh
  )
}


#' Rectified Linear Unit Activation Function
#'
#' Creates a rectified linear unit activation function object.
#'
#' @return An object of class `"met_activation"`.
#' @references
#' Nair, V., and Hinton, G. E. (2010). Rectified Linear Units Improve
#' Restricted Boltzmann Machines. Proceedings of the 27th International
#' Conference on Machine Learning, 807--814.
#' @export
#'
#' @examples
#' act <- activation_relu()
#' act$fn(c(-1, 0, 1))
activation_relu <- function() {
  new_activation(
    name = "relu",
    fn = function(x) {
      out <- x
      out[out < 0] <- 0
      out
    }
  )
}


#' Leaky Rectified Linear Unit Activation Function
#'
#' Creates a leaky rectified linear unit activation function object.
#'
#' @param alpha A non-negative numeric value controlling the slope for
#' negative inputs.
#'
#' @return An object of class `"met_activation"`.
#' @export
#'
#' @examples
#' act <- activation_leaky_relu(alpha = 0.01)
#' act$fn(c(-1, 0, 1))
activation_leaky_relu <- function(alpha = 0.01) {
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha < 0) {
    stop("'alpha' must be a single non-negative numeric value.", call. = FALSE)
  }

  new_activation(
    name = "leaky_relu",
    fn = function(x) {
      out <- x
      out[out < 0] <- alpha * out[out < 0]
      out
    },
    parameters = list(alpha = alpha)
  )
}


#' Softmax Activation Function
#'
#' Creates a softmax activation function object.
#'
#' @return An object of class `"met_activation"`.
#' @references
#' Bridle, J. S. (1990). Probabilistic Interpretation of Feedforward
#' Classification Network Outputs, with Relationships to Statistical Pattern
#' Recognition. In Neurocomputing: Algorithms, Architectures and Applications,
#' 227--236. Springer.
#' @export
#'
#' @examples
#' act <- activation_softmax()
#' act$fn(c(1, 2, 3))
activation_softmax <- function() {
  new_activation(
    name = "softmax",
    fn = function(x) {
      if (is.vector(x)) {
        z <- x - max(x)
        exp_z <- exp(z)
        return(exp_z / sum(exp_z))
      }

      if (is.matrix(x)) {
        z <- sweep(x, 1L, apply(x, 1L, max), FUN = "-")
        exp_z <- exp(z)
        return(sweep(exp_z, 1L, rowSums(exp_z), FUN = "/"))
      }

      stop("'x' must be a numeric vector or matrix.", call. = FALSE)
    }
  )
}


#' Check Whether an Object is a metANN Activation
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` is a metANN activation object; otherwise `FALSE`.
#' @export
#'
#' @examples
#' is_activation(activation_relu())
is_activation <- function(x) {
  inherits(x, "met_activation")
}


#' Convert Character Input to an Activation Object
#'
#' Converts a character string such as `"relu"` into the corresponding
#' activation function object.
#'
#' @param activation A character string or an object of class
#' `"met_activation"`.
#'
#' @return An object of class `"met_activation"`.
#' @export
#'
#' @examples
#' as_activation("relu")
#' as_activation(activation_leaky_relu(alpha = 0.05))
as_activation <- function(activation) {
  if (is_activation(activation)) {
    return(activation)
  }

  if (!is.character(activation) || length(activation) != 1L) {
    stop(
      "'activation' must be a single character string or a met_activation object.",
      call. = FALSE
    )
  }

  activation <- tolower(activation)

  switch(
    activation,
    linear = activation_linear(),
    sigmoid = activation_sigmoid(),
    tanh = activation_tanh(),
    relu = activation_relu(),
    leaky_relu = activation_leaky_relu(),
    softmax = activation_softmax(),
    stop("Unknown activation function: '", activation, "'.", call. = FALSE)
  )
}
