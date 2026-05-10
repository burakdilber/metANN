#' Derivative of Activation Functions
#'
#' Internal helper for computing activation derivatives during backpropagation.
#'
#' @param activation A metANN activation object.
#' @param z Pre-activation values.
#' @param a Activation values.
#'
#' @return A numeric matrix with the same dimensions as `z`.
#' @keywords internal
activation_derivative <- function(activation, z, a) {
  name <- activation$name

  if (name == "linear") {
    out <- z
    out[] <- 1
    return(out)
  }

  if (name == "sigmoid") {
    return(a * (1 - a))
  }

  if (name == "tanh") {
    return(1 - a^2)
  }

  if (name == "relu") {
    out <- z
    out[] <- 0
    out[z > 0] <- 1
    return(out)
  }

  if (name == "leaky_relu") {
    alpha <- activation$parameters$alpha
    out <- z
    out[] <- 1
    out[z < 0] <- alpha
    return(out)
  }

  stop(
    "Activation '", name,
    "' is not currently supported by the gradient-based training engine.",
    call. = FALSE
  )
}


#' Forward Pass with Cache
#'
#' Internal helper that stores intermediate values needed for backpropagation.
#'
#' @param x Numeric input matrix.
#' @param weights Numeric parameter vector.
#' @param architecture MLP architecture object.
#'
#' @return A list containing activations, pre-activations, and decoded weights.
#' @keywords internal
mlp_forward_cache <- function(x, weights, architecture) {
  decoded <- decode_weights(
    weights = weights,
    architecture = architecture,
    input_dim = ncol(x)
  )

  activations <- vector("list", length(decoded) + 1L)
  pre_activations <- vector("list", length(decoded))

  activations[[1L]] <- x

  output <- x

  for (i in seq_along(decoded)) {
    z <- output %*% decoded[[i]]$W

    if (length(decoded[[i]]$b) > 0L) {
      z <- sweep(z, 2L, decoded[[i]]$b, FUN = "+")
    }

    output <- decoded[[i]]$layer$activation$fn(z)

    pre_activations[[i]] <- z
    activations[[i + 1L]] <- output
  }

  list(
    output = output,
    activations = activations,
    pre_activations = pre_activations,
    decoded = decoded
  )
}


#' Compute MSE Gradient for an MLP
#'
#' Internal helper for computing the gradient of MSE loss with respect to the
#' MLP parameter vector.
#'
#' @param x Numeric input matrix.
#' @param y Numeric response vector.
#' @param weights Numeric parameter vector.
#' @param architecture MLP architecture object.
#'
#' @return A list containing loss, predictions, and gradient vector.
#' @keywords internal
mlp_mse_gradient <- function(x, y, weights, architecture) {
  cache <- mlp_forward_cache(
    x = x,
    weights = weights,
    architecture = architecture
  )

  pred_matrix <- cache$output

  if (ncol(pred_matrix) != 1L) {
    stop(
      "Gradient-based regression currently requires a one-unit output layer.",
      call. = FALSE
    )
  }

  y_pred <- as.numeric(pred_matrix[, 1L])
  n <- length(y)

  loss_value <- mean((y - y_pred)^2)

  delta <- matrix(
    2 * (y_pred - y) / n,
    nrow = n,
    ncol = 1L
  )

  n_layers <- length(cache$decoded)
  grad_layers <- vector("list", n_layers)

  for (layer_index in seq(from = n_layers, to = 1L, by = -1L)) {
    layer <- cache$decoded[[layer_index]]$layer

    derivative <- activation_derivative(
      activation = layer$activation,
      z = cache$pre_activations[[layer_index]],
      a = cache$activations[[layer_index + 1L]]
    )

    delta_z <- delta * derivative

    a_prev <- cache$activations[[layer_index]]

    dW <- t(a_prev) %*% delta_z
    db <- colSums(delta_z)

    grad_layers[[layer_index]] <- list(
      dW = dW,
      db = db
    )

    if (layer_index > 1L) {
      delta <- delta_z %*% t(cache$decoded[[layer_index]]$W)
    }
  }

  grad_vector <- numeric(0L)

  for (layer_index in seq_len(n_layers)) {
    grad_vector <- c(
      grad_vector,
      as.vector(grad_layers[[layer_index]]$dW)
    )

    if (isTRUE(cache$decoded[[layer_index]]$layer$use_bias)) {
      grad_vector <- c(
        grad_vector,
        as.vector(grad_layers[[layer_index]]$db)
      )
    }
  }

  list(
    loss = loss_value,
    prediction = y_pred,
    gradient = grad_vector
  )
}

#' Compute Binary Cross-Entropy Gradient for an MLP
#'
#' Internal helper for computing the gradient of binary cross-entropy loss
#' with respect to the MLP parameter vector.
#'
#' @param x Numeric input matrix.
#' @param y Numeric binary response vector coded as 0/1.
#' @param weights Numeric parameter vector.
#' @param architecture MLP architecture object.
#' @param epsilon Small value used for numerical stability.
#'
#' @return A list containing loss, predictions, and gradient vector.
#' @keywords internal
mlp_binary_crossentropy_gradient <- function(x,
                                             y,
                                             weights,
                                             architecture,
                                             epsilon = 1e-15) {
  cache <- mlp_forward_cache(
    x = x,
    weights = weights,
    architecture = architecture
  )

  pred_matrix <- cache$output

  if (ncol(pred_matrix) != 1L) {
    stop(
      "Binary classification requires a one-unit output layer.",
      call. = FALSE
    )
  }

  y_pred <- as.numeric(pred_matrix[, 1L])
  y_pred_clipped <- pmin(pmax(y_pred, epsilon), 1 - epsilon)

  y <- as.numeric(y)
  n <- length(y)

  loss_value <- -mean(
    y * log(y_pred_clipped) + (1 - y) * log(1 - y_pred_clipped)
  )

  # For sigmoid output + binary cross-entropy:
  # dL/dz = (y_pred - y) / n
  delta <- matrix(
    (y_pred - y) / n,
    nrow = n,
    ncol = 1L
  )

  n_layers <- length(cache$decoded)
  grad_layers <- vector("list", n_layers)

  for (layer_index in seq(from = n_layers, to = 1L, by = -1L)) {
    a_prev <- cache$activations[[layer_index]]

    dW <- t(a_prev) %*% delta
    db <- colSums(delta)

    grad_layers[[layer_index]] <- list(
      dW = dW,
      db = db
    )

    if (layer_index > 1L) {
      delta <- delta %*% t(cache$decoded[[layer_index]]$W)

      previous_layer <- cache$decoded[[layer_index - 1L]]$layer

      derivative <- activation_derivative(
        activation = previous_layer$activation,
        z = cache$pre_activations[[layer_index - 1L]],
        a = cache$activations[[layer_index]]
      )

      delta <- delta * derivative
    }
  }

  grad_vector <- numeric(0L)

  for (layer_index in seq_len(n_layers)) {
    grad_vector <- c(
      grad_vector,
      as.vector(grad_layers[[layer_index]]$dW)
    )

    if (isTRUE(cache$decoded[[layer_index]]$layer$use_bias)) {
      grad_vector <- c(
        grad_vector,
        as.vector(grad_layers[[layer_index]]$db)
      )
    }
  }

  list(
    loss = loss_value,
    prediction = y_pred,
    gradient = grad_vector
  )
}

#' Compute Multi-Class Cross-Entropy Gradient for an MLP
#'
#' Internal helper for computing the gradient of softmax cross-entropy loss
#' with respect to the MLP parameter vector.
#'
#' @param x Numeric input matrix.
#' @param y Numeric one-hot encoded response matrix.
#' @param weights Numeric parameter vector.
#' @param architecture MLP architecture object.
#' @param epsilon Small value used for numerical stability.
#'
#' @return A list containing loss, predictions, and gradient vector.
#' @keywords internal
mlp_crossentropy_gradient <- function(x,
                                      y,
                                      weights,
                                      architecture,
                                      epsilon = 1e-15) {
  cache <- mlp_forward_cache(
    x = x,
    weights = weights,
    architecture = architecture
  )

  pred_matrix <- cache$output

  if (!is.matrix(y)) {
    stop(
      "Multi-class classification requires a one-hot encoded response matrix.",
      call. = FALSE
    )
  }

  if (ncol(pred_matrix) != ncol(y)) {
    stop(
      "The output layer size must match the number of classes.",
      call. = FALSE
    )
  }

  n <- nrow(y)

  pred_clipped <- pmin(pmax(pred_matrix, epsilon), 1 - epsilon)

  loss_value <- -mean(rowSums(y * log(pred_clipped)))

  # For softmax output + cross-entropy:
  # dL/dz = (y_pred - y) / n
  delta <- (pred_matrix - y) / n

  n_layers <- length(cache$decoded)
  grad_layers <- vector("list", n_layers)

  for (layer_index in seq(from = n_layers, to = 1L, by = -1L)) {
    a_prev <- cache$activations[[layer_index]]

    dW <- t(a_prev) %*% delta
    db <- colSums(delta)

    grad_layers[[layer_index]] <- list(
      dW = dW,
      db = db
    )

    if (layer_index > 1L) {
      delta <- delta %*% t(cache$decoded[[layer_index]]$W)

      previous_layer <- cache$decoded[[layer_index - 1L]]$layer

      derivative <- activation_derivative(
        activation = previous_layer$activation,
        z = cache$pre_activations[[layer_index - 1L]],
        a = cache$activations[[layer_index]]
      )

      delta <- delta * derivative
    }
  }

  grad_vector <- numeric(0L)

  for (layer_index in seq_len(n_layers)) {
    grad_vector <- c(
      grad_vector,
      as.vector(grad_layers[[layer_index]]$dW)
    )

    if (isTRUE(cache$decoded[[layer_index]]$layer$use_bias)) {
      grad_vector <- c(
        grad_vector,
        as.vector(grad_layers[[layer_index]]$db)
      )
    }
  }

  list(
    loss = loss_value,
    prediction = pred_matrix,
    gradient = grad_vector
  )
}


#' Train an MLP with a Gradient-Based Optimizer
#'
#' Internal gradient-based training engine for MLP regression and
#' classification.
#'
#' @param x Numeric input matrix.
#' @param y Response vector or matrix used for training.
#' @param architecture MLP architecture object.
#' @param optimizer Gradient-based optimizer object.
#' @param loss Loss object.
#' @param seed Optional random seed.
#' @param verbose Logical. If `TRUE`, training progress is printed.
#'
#' @return An object compatible with `"met_optimize_result"`.
#' @keywords internal
train_mlp_gradient <- function(x,
                               y,
                               architecture,
                               optimizer,
                               loss,
                               seed = NULL,
                               verbose = TRUE) {
  if (!optimizer$name %in% c("sgd", "adam")) {
    stop(
      "Gradient-based training currently supports optimizer_sgd() and optimizer_adam().",
      call. = FALSE
    )
  }

  supported_losses <- c("mse", "binary_crossentropy", "crossentropy")

  if (!loss$name %in% supported_losses) {
    stop(
      "Gradient-based training currently supports loss = 'mse', ",
      "'binary_crossentropy', and 'crossentropy'.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_parameters <- count_parameters(architecture)

  weights <- initialize_weights(
    architecture = architecture,
    method = "normal",
    mean = 0,
    sd = 0.1,
    seed = seed
  )

  pars <- optimizer$parameters
  epochs <- pars$epochs
  learning_rate <- pars$learning_rate
  batch_size <- pars$batch_size

  n <- nrow(x)

  if (is.null(batch_size)) {
    batch_size <- n
  }

  batch_size <- min(batch_size, n)

  convergence <- numeric(epochs)

  best_weights <- weights
  best_value <- Inf

  if (optimizer$name == "adam") {
    m <- numeric(length(weights))
    v <- numeric(length(weights))
    t_step <- 0L
  }

  gradient_function <- switch(
    loss$name,
    mse = mlp_mse_gradient,
    binary_crossentropy = mlp_binary_crossentropy_gradient,
    crossentropy = mlp_crossentropy_gradient,
    stop(
      "Unsupported loss for gradient-based training.",
      call. = FALSE
    )
  )

  if (isTRUE(verbose)) {
    cat(toupper(optimizer$name), "training started\n")
  }

  for (epoch in seq_len(epochs)) {
    indices <- sample(seq_len(n))

    batch_starts <- seq(1L, n, by = batch_size)

    for (start in batch_starts) {
      end <- min(start + batch_size - 1L, n)
      batch_index <- indices[start:end]

      x_batch <- x[batch_index, , drop = FALSE]

      if (is.matrix(y)) {
        y_batch <- y[batch_index, , drop = FALSE]
      } else {
        y_batch <- y[batch_index]
      }

      grad_info <- gradient_function(
        x = x_batch,
        y = y_batch,
        weights = weights,
        architecture = architecture
      )

      grad <- grad_info$gradient

      if (optimizer$name == "sgd") {
        weights <- weights - learning_rate * grad
      }

      if (optimizer$name == "adam") {
        t_step <- t_step + 1L

        beta1 <- pars$beta1
        beta2 <- pars$beta2
        epsilon <- pars$epsilon

        m <- beta1 * m + (1 - beta1) * grad
        v <- beta2 * v + (1 - beta2) * (grad^2)

        m_hat <- m / (1 - beta1^t_step)
        v_hat <- v / (1 - beta2^t_step)

        weights <- weights - learning_rate * m_hat / (sqrt(v_hat) + epsilon)
      }
    }

    full_info <- gradient_function(
      x = x,
      y = y,
      weights = weights,
      architecture = architecture
    )

    current_loss <- full_info$loss
    convergence[epoch] <- current_loss

    if (current_loss < best_value) {
      best_value <- current_loss
      best_weights <- weights
    }

    if (isTRUE(verbose) && (epoch %% 10L == 0L || epoch == epochs)) {
      cat("  Epoch", epoch, "- loss:", current_loss, "\n")
    }
  }

  result <- list(
    best_par = best_weights,
    best_value = best_value,
    convergence = convergence,
    optimizer = optimizer,
    lower = rep(NA_real_, n_parameters),
    upper = rep(NA_real_, n_parameters),
    objective = NULL,
    n_iter = epochs
  )

  class(result) <- c("met_gradient_result", "met_optimize_result")

  result
}
