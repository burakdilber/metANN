#' Plot Neural Network Architecture
#'
#' Plots the architecture of a feed-forward multilayer perceptron, showing
#' input, hidden, and output layers in a visually enhanced layout.
#'
#' @param object A fitted `"metann"` object or an MLP architecture object.
#' @param max_neurons Maximum number of neurons to display per layer. If a layer
#' has more neurons than this value, only a subset is displayed and the layer is
#' annotated.
#' @param show_connections Logical. If `TRUE`, connections between adjacent
#' layers are drawn.
#' @param neuron_cex Size of neuron circles.
#' @param label_cex Size of text labels.
#' @param main Main title of the plot.
#' @param ... Additional graphical arguments.
#'
#' @return The input object invisibly.
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
#' plot_network(fit)
plot_network <- function(object,
                         max_neurons = 20,
                         show_connections = TRUE,
                         neuron_cex = 2.2,
                         label_cex = 0.9,
                         main = "Neural Network Architecture",
                         ...) {
  if (!is.numeric(max_neurons) ||
      length(max_neurons) != 1L ||
      max_neurons <= 0 ||
      max_neurons != as.integer(max_neurons)) {
    stop("'max_neurons' must be a single positive integer.", call. = FALSE)
  }

  if (!is.logical(show_connections) || length(show_connections) != 1L) {
    stop("'show_connections' must be a single logical value.", call. = FALSE)
  }

  max_neurons <- as.integer(max_neurons)

  if (inherits(object, "metann")) {
    architecture <- object$architecture
    input_dim <- object$input_dim
    task <- object$task
    class_info <- if (!is.null(object$classification_type)) {
      paste0(" | ", object$classification_type, " classification")
    } else {
      ""
    }
  } else if (is_mlp_architecture(object)) {
    architecture <- object
    input_dim <- architecture$input_dim
    task <- NULL
    class_info <- ""
  } else {
    stop(
      "'object' must be a fitted metANN model or an MLP architecture object.",
      call. = FALSE
    )
  }

  if (is.null(input_dim)) {
    stop(
      "The architecture must have a non-null input_dim to plot the network.",
      call. = FALSE
    )
  }

  layer_sizes <- c(
    input_dim,
    vapply(architecture$layers, function(layer) layer$units, numeric(1L))
  )

  n_layers <- length(layer_sizes)

  layer_names <- character(n_layers)
  layer_names[1L] <- "Input"

  if (n_layers > 2L) {
    layer_names[2L:(n_layers - 1L)] <- paste0("Hidden ", seq_len(n_layers - 2L))
  }

  layer_names[n_layers] <- "Output"

  activation_names <- c(
    "",
    vapply(
      architecture$layers,
      function(layer) layer$activation$name,
      character(1L)
    )
  )

  # Colors
  input_fill <- "#5B8FF9"
  hidden_fill <- "#61DDAA"
  output_fill <- "#F6BD16"
  border_col <- "#2F2F2F"
  connection_col <- grDevices::adjustcolor("#7A7A7A", alpha.f = 0.35)
  subtitle_col <- "#555555"

  x_positions <- seq(0.1, 0.9, length.out = n_layers)
  displayed_sizes <- pmin(layer_sizes, max_neurons)
  y_positions <- vector("list", n_layers)

  for (i in seq_len(n_layers)) {
    if (displayed_sizes[i] == 1L) {
      y_positions[[i]] <- 0.5
    } else {
      y_positions[[i]] <- seq(0.18, 0.82, length.out = displayed_sizes[i])
    }
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(
    mar = c(3.8, 2, 4.2, 2),
    xpd = NA
  )

  graphics::plot(
    NA,
    xlim = c(0, 1),
    ylim = c(0, 1),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main,
    ...
  )

  # Subtitle
  if (!is.null(task)) {
    graphics::mtext(
      side = 3,
      line = 0.3,
      text = paste0("Task: ", task, class_info),
      cex = label_cex * 0.95,
      col = subtitle_col
    )
  }

  # Layer guide bands (very light)
  for (i in seq_len(n_layers)) {
    fill_band <- if (i == 1L) {
      grDevices::adjustcolor(input_fill, alpha.f = 0.08)
    } else if (i == n_layers) {
      grDevices::adjustcolor(output_fill, alpha.f = 0.08)
    } else {
      grDevices::adjustcolor(hidden_fill, alpha.f = 0.08)
    }

    graphics::rect(
      xleft = x_positions[i] - 0.06,
      ybottom = 0.10,
      xright = x_positions[i] + 0.06,
      ytop = 0.90,
      border = NA,
      col = fill_band
    )
  }

  # Connections
  if (isTRUE(show_connections)) {
    for (i in seq_len(n_layers - 1L)) {
      x1 <- x_positions[i]
      x2 <- x_positions[i + 1L]

      y1_values <- y_positions[[i]]
      y2_values <- y_positions[[i + 1L]]

      for (y1 in y1_values) {
        for (y2 in y2_values) {
          graphics::segments(
            x0 = x1,
            y0 = y1,
            x1 = x2,
            y1 = y2,
            col = connection_col,
            lwd = 1
          )
        }
      }
    }
  }

  # Draw neurons and labels
  for (i in seq_len(n_layers)) {
    x <- rep(x_positions[i], displayed_sizes[i])
    y <- y_positions[[i]]

    fill_col <- if (i == 1L) {
      input_fill
    } else if (i == n_layers) {
      output_fill
    } else {
      hidden_fill
    }

    # neuron shadow
    graphics::points(
      x + 0.004,
      y - 0.004,
      pch = 21,
      bg = grDevices::adjustcolor("black", alpha.f = 0.10),
      col = NA,
      cex = neuron_cex
    )

    # main neuron
    graphics::points(
      x,
      y,
      pch = 21,
      bg = fill_col,
      col = border_col,
      lwd = 1.2,
      cex = neuron_cex
    )

    # layer title
    graphics::text(
      x = x_positions[i],
      y = 0.93,
      labels = layer_names[i],
      cex = label_cex * 1.05,
      font = 2,
      col = border_col
    )

    # neuron count
    neuron_label <- paste0(layer_sizes[i], if (layer_sizes[i] == 1L) " neuron" else " neurons")
    graphics::text(
      x = x_positions[i],
      y = 0.065,
      labels = neuron_label,
      cex = label_cex,
      col = border_col
    )

    # activation label
    if (i > 1L) {
      graphics::text(
        x = x_positions[i],
        y = 0.025,
        labels = paste0("Activation: ", activation_names[i]),
        cex = label_cex * 0.85,
        col = subtitle_col
      )
    }

    # if truncated
    if (layer_sizes[i] > max_neurons) {
      graphics::text(
        x = x_positions[i],
        y = 0.50,
        labels = "...",
        cex = label_cex * 1.7,
        font = 2,
        col = border_col
      )

      graphics::text(
        x = x_positions[i],
        y = -0.005,
        labels = paste0("showing first ", max_neurons),
        cex = label_cex * 0.8,
        col = subtitle_col
      )
    }
  }

  invisible(object)
}
