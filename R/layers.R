#' Create a Dense Layer
#'
#' Creates a fully connected dense layer object for use in metANN
#' architectures.
#'
#' @param units A positive integer specifying the number of neurons in the
#' layer.
#' @param activation A character string or a `"met_activation"` object.
#' @param use_bias Logical. Whether to include a bias term in the layer.
#' @param trainable Logical. Whether the layer parameters should be trainable.
#' @param name An optional character string specifying the layer name.
#'
#' @return An object of class `"met_dense_layer"`.
#' @export
#'
#' @examples
#' dense_layer(10, activation = "relu")
#' dense_layer(1, activation = activation_linear())
dense_layer <- function(units,
                        activation = "relu",
                        use_bias = TRUE,
                        trainable = TRUE,
                        name = NULL) {
  if (!is.numeric(units) || length(units) != 1L || units <= 0 || units != as.integer(units)) {
    stop("'units' must be a single positive integer.", call. = FALSE)
  }

  if (!is.logical(use_bias) || length(use_bias) != 1L) {
    stop("'use_bias' must be a single logical value.", call. = FALSE)
  }

  if (!is.logical(trainable) || length(trainable) != 1L) {
    stop("'trainable' must be a single logical value.", call. = FALSE)
  }

  if (!is.null(name) && (!is.character(name) || length(name) != 1L)) {
    stop("'name' must be NULL or a single character string.", call. = FALSE)
  }

  activation <- as_activation(activation)

  structure(
    list(
      type = "dense",
      units = as.integer(units),
      activation = activation,
      use_bias = use_bias,
      trainable = trainable,
      name = name
    ),
    class = c("met_dense_layer", "met_layer")
  )
}


#' Check Whether an Object is a metANN Layer
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` is a metANN layer object; otherwise `FALSE`.
#' @export
#'
#' @examples
#' is_layer(dense_layer(5))
is_layer <- function(x) {
  inherits(x, "met_layer")
}


#' Check Whether an Object is a Dense Layer
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` is a dense layer object; otherwise `FALSE`.
#' @export
#'
#' @examples
#' is_dense_layer(dense_layer(5))
is_dense_layer <- function(x) {
  inherits(x, "met_dense_layer")
}


#' Create an MLP Architecture
#'
#' Creates a multilayer perceptron architecture object from a list of dense
#' layers.
#'
#' @param layers A list of dense layer objects created by `dense_layer()`.
#' @param input_dim Optional positive integer specifying the number of input
#' features. This can be left as `NULL` and inferred later from data.
#' @param name Optional character string specifying the architecture name.
#'
#' @return An object of class `"met_mlp_architecture"`.
#' @export
#'
#' @examples
#' architecture <- mlp_architecture(
#'   layers = list(
#'     dense_layer(10, activation = "relu"),
#'     dense_layer(1, activation = "linear")
#'   )
#' )
#' architecture
mlp_architecture <- function(layers,
                             input_dim = NULL,
                             name = "mlp") {
  if (!is.list(layers) || length(layers) == 0L) {
    stop("'layers' must be a non-empty list of dense layer objects.", call. = FALSE)
  }

  layer_check <- vapply(layers, is_dense_layer, logical(1L))

  if (!all(layer_check)) {
    stop("All elements of 'layers' must be dense layer objects created by dense_layer().", call. = FALSE)
  }

  if (!is.null(input_dim)) {
    if (!is.numeric(input_dim) || length(input_dim) != 1L || input_dim <= 0 || input_dim != as.integer(input_dim)) {
      stop("'input_dim' must be NULL or a single positive integer.", call. = FALSE)
    }
    input_dim <- as.integer(input_dim)
  }

  if (!is.null(name) && (!is.character(name) || length(name) != 1L)) {
    stop("'name' must be NULL or a single character string.", call. = FALSE)
  }

  structure(
    list(
      type = "mlp",
      input_dim = input_dim,
      layers = layers,
      name = name
    ),
    class = c("met_mlp_architecture", "met_architecture")
  )
}


#' Check Whether an Object is a metANN Architecture
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` is a metANN architecture object; otherwise `FALSE`.
#' @export
#'
#' @examples
#' arch <- mlp_architecture(list(dense_layer(1)))
#' is_architecture(arch)
is_architecture <- function(x) {
  inherits(x, "met_architecture")
}


#' Check Whether an Object is an MLP Architecture
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` is an MLP architecture object; otherwise `FALSE`.
#' @export
#'
#' @examples
#' arch <- mlp_architecture(list(dense_layer(1)))
#' is_mlp_architecture(arch)
is_mlp_architecture <- function(x) {
  inherits(x, "met_mlp_architecture")
}


#' Print a Dense Layer
#'
#' @param x A dense layer object.
#' @param ... Additional arguments, currently unused.
#'
#' @return The input object invisibly.
#' @export
print.met_dense_layer <- function(x, ...) {
  cat("Dense layer\n")
  cat("  Units      :", x$units, "\n")
  cat("  Activation :", x$activation$name, "\n")
  cat("  Use bias   :", x$use_bias, "\n")
  cat("  Trainable  :", x$trainable, "\n")

  if (!is.null(x$name)) {
    cat("  Name       :", x$name, "\n")
  }

  invisible(x)
}


#' Print an MLP Architecture
#'
#' @param x An MLP architecture object.
#' @param ... Additional arguments, currently unused.
#'
#' @return The input object invisibly.
#' @export
print.met_mlp_architecture <- function(x, ...) {
  cat("MLP architecture\n")
  cat("  Name      :", x$name, "\n")
  cat("  Input dim :", ifelse(is.null(x$input_dim), "NULL", x$input_dim), "\n")
  cat("  Layers    :", length(x$layers), "\n\n")

  for (i in seq_along(x$layers)) {
    layer <- x$layers[[i]]
    cat("  Layer", i, "\n")
    cat("    Type       :", layer$type, "\n")
    cat("    Units      :", layer$units, "\n")
    cat("    Activation :", layer$activation$name, "\n")
    cat("    Use bias   :", layer$use_bias, "\n")
    cat("    Trainable  :", layer$trainable, "\n")
  }

  invisible(x)
}
