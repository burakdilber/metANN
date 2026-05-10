#' Count the Number of Trainable Parameters in an MLP Architecture
#'
#' Computes the total number of weights and bias terms required by a
#' multilayer perceptron architecture.
#'
#' @param architecture An object created by `mlp_architecture()`.
#' @param input_dim Optional positive integer specifying the number of input
#' features. If `NULL`, `architecture$input_dim` is used.
#'
#' @return A positive integer giving the total number of parameters.
#' @export
#'
#' @examples
#' arch <- mlp_architecture(
#'   input_dim = 4,
#'   layers = list(
#'     dense_layer(5, activation = "relu"),
#'     dense_layer(1, activation = "linear")
#'   )
#' )
#' count_parameters(arch)
count_parameters <- function(architecture, input_dim = NULL) {
  if (!is_mlp_architecture(architecture)) {
    stop("'architecture' must be an MLP architecture object.", call. = FALSE)
  }

  if (is.null(input_dim)) {
    input_dim <- architecture$input_dim
  }

  if (is.null(input_dim)) {
    stop(
      "'input_dim' must be supplied either directly or inside the architecture.",
      call. = FALSE
    )
  }

  if (!is.numeric(input_dim) || length(input_dim) != 1L || input_dim <= 0 || input_dim != as.integer(input_dim)) {
    stop("'input_dim' must be a single positive integer.", call. = FALSE)
  }

  previous_units <- as.integer(input_dim)
  total_parameters <- 0L

  for (layer in architecture$layers) {
    weight_count <- previous_units * layer$units
    bias_count <- if (isTRUE(layer$use_bias)) layer$units else 0L

    total_parameters <- total_parameters + weight_count + bias_count
    previous_units <- layer$units
  }

  as.integer(total_parameters)
}


#' Initialize MLP Weights
#'
#' Creates a numeric vector of randomly initialized weights and bias terms for
#' an MLP architecture.
#'
#' @param architecture An object created by `mlp_architecture()`.
#' @param input_dim Optional positive integer specifying the number of input
#' features. If `NULL`, `architecture$input_dim` is used.
#' @param method Initialization method. Currently `"uniform"` and `"normal"`
#' are supported.
#' @param lower Lower bound for uniform initialization.
#' @param upper Upper bound for uniform initialization.
#' @param mean Mean for normal initialization.
#' @param sd Standard deviation for normal initialization.
#' @param seed Optional random seed.
#'
#' @return A numeric vector containing initialized parameters.
#' @export
#'
#' @examples
#' arch <- mlp_architecture(
#'   input_dim = 3,
#'   layers = list(dense_layer(2), dense_layer(1, activation = "linear"))
#' )
#' initialize_weights(arch, seed = 123)
initialize_weights <- function(architecture,
                               input_dim = NULL,
                               method = c("uniform", "normal"),
                               lower = -0.5,
                               upper = 0.5,
                               mean = 0,
                               sd = 0.1,
                               seed = NULL) {
  method <- match.arg(method)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_parameters <- count_parameters(architecture, input_dim = input_dim)

  if (method == "uniform") {
    return(stats::runif(n_parameters, min = lower, max = upper))
  }

  if (method == "normal") {
    return(stats::rnorm(n_parameters, mean = mean, sd = sd))
  }
}


#' Decode an MLP Weight Vector
#'
#' Converts a numeric parameter vector into layer-wise weight matrices and
#' bias vectors.
#'
#' @param weights A numeric vector of MLP parameters.
#' @param architecture An object created by `mlp_architecture()`.
#' @param input_dim Optional positive integer specifying the number of input
#' features. If `NULL`, `architecture$input_dim` is used.
#'
#' @return A list containing layer-wise weight matrices and bias vectors.
#' @export
#'
#' @examples
#' arch <- mlp_architecture(
#'   input_dim = 2,
#'   layers = list(dense_layer(3), dense_layer(1, activation = "linear"))
#' )
#' w <- initialize_weights(arch, seed = 123)
#' decoded <- decode_weights(w, arch)
decode_weights <- function(weights, architecture, input_dim = NULL) {
  if (!is_mlp_architecture(architecture)) {
    stop("'architecture' must be an MLP architecture object.", call. = FALSE)
  }

  if (!is.numeric(weights)) {
    stop("'weights' must be a numeric vector.", call. = FALSE)
  }

  if (is.null(input_dim)) {
    input_dim <- architecture$input_dim
  }

  if (is.null(input_dim)) {
    stop(
      "'input_dim' must be supplied either directly or inside the architecture.",
      call. = FALSE
    )
  }

  expected_length <- count_parameters(architecture, input_dim = input_dim)

  if (length(weights) != expected_length) {
    stop(
      "'weights' has length ", length(weights),
      ", but the architecture requires ", expected_length, " parameters.",
      call. = FALSE
    )
  }

  previous_units <- as.integer(input_dim)
  position <- 1L
  decoded <- vector("list", length(architecture$layers))

  for (i in seq_along(architecture$layers)) {
    layer <- architecture$layers[[i]]

    weight_count <- previous_units * layer$units
    weight_values <- weights[position:(position + weight_count - 1L)]

    weight_matrix <- matrix(
      weight_values,
      nrow = previous_units,
      ncol = layer$units
    )

    position <- position + weight_count

    if (isTRUE(layer$use_bias)) {
      bias_values <- weights[position:(position + layer$units - 1L)]
      position <- position + layer$units
    } else {
      bias_values <- rep(0, layer$units)
    }

    decoded[[i]] <- list(
      W = weight_matrix,
      b = bias_values,
      layer = layer
    )

    previous_units <- layer$units
  }

  decoded
}


#' Forward Pass for an MLP
#'
#' Computes predictions from input data, an MLP architecture, and a parameter
#' vector.
#'
#' @param x A numeric matrix or data frame of input features.
#' @param weights A numeric vector of MLP parameters.
#' @param architecture An object created by `mlp_architecture()`.
#'
#' @return A numeric matrix containing network outputs.
#' @export
#'
#' @examples
#' x <- matrix(rnorm(10), nrow = 5, ncol = 2)
#' arch <- mlp_architecture(
#'   input_dim = 2,
#'   layers = list(
#'     dense_layer(3, activation = "relu"),
#'     dense_layer(1, activation = "linear")
#'   )
#' )
#' w <- initialize_weights(arch, seed = 123)
#' forward_pass(x, w, arch)
forward_pass <- function(x, weights, architecture) {
  if (!is_mlp_architecture(architecture)) {
    stop("'architecture' must be an MLP architecture object.", call. = FALSE)
  }

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (!is.matrix(x) || !is.numeric(x)) {
    stop("'x' must be a numeric matrix or a numeric data frame.", call. = FALSE)
  }

  input_dim <- ncol(x)

  decoded <- decode_weights(
    weights = weights,
    architecture = architecture,
    input_dim = input_dim
  )

  output <- x

  for (layer_params in decoded) {
    z <- output %*% layer_params$W

    if (length(layer_params$b) > 0L) {
      z <- sweep(z, 2L, layer_params$b, FUN = "+")
    }

    output <- layer_params$layer$activation$fn(z)
  }

  output
}
