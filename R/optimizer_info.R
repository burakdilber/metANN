#' Get Optimizer Information
#'
#' Returns basic information about an optimizer available in the metANN package.
#'
#' @param optimizer Character name of an optimizer or an optimizer object created
#' by functions such as `optimizer_pso()`, `optimizer_sboa()`,
#' `optimizer_sgd()`, or `optimizer_adam()`.
#'
#' @return An object of class `"met_optimizer_info"`.
#' @export
#'
#' @examples
#' optimizer_info("pso")
#' optimizer_info("sboa")
#' optimizer_info(optimizer_adam())
optimizer_info <- function(optimizer) {
  if (is.character(optimizer)) {
    if (length(optimizer) != 1L) {
      stop("'optimizer' must be a single character value.", call. = FALSE)
    }

    optimizer <- as_optimizer(optimizer)
  }

  if (!is_optimizer(optimizer)) {
    stop(
      "'optimizer' must be a valid optimizer name or a metANN optimizer object.",
      call. = FALSE
    )
  }

  info <- switch(
    optimizer$name,

    pso = list(
      name = "pso",
      full_name = "Particle Swarm Optimization",
      type = "metaheuristic",
      requires_gradient = FALSE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Particle Swarm Optimization is a population-based metaheuristic",
        "inspired by the collective behavior of swarms. It updates candidate",
        "solutions using personal and global best positions."
      ),
      main_parameters = c("pop_size", "max_iter", "w", "c1", "c2", "velocity_clamp")
    ),

    de = list(
      name = "de",
      full_name = "Differential Evolution",
      type = "metaheuristic",
      requires_gradient = FALSE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Differential Evolution is a population-based optimizer that generates",
        "new candidate solutions using mutation, crossover, and greedy selection."
      ),
      main_parameters = c("pop_size", "max_iter", "F", "CR", "strategy")
    ),

    ga = list(
      name = "ga",
      full_name = "Genetic Algorithm",
      type = "metaheuristic",
      requires_gradient = FALSE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Genetic Algorithm is an evolutionary optimizer based on selection,",
        "crossover, mutation, and elitism mechanisms."
      ),
      main_parameters = c(
        "pop_size", "max_iter", "crossover_rate", "mutation_rate",
        "mutation_sd", "elitism", "selection", "tournament_size"
      )
    ),

    abc = list(
      name = "abc",
      full_name = "Artificial Bee Colony",
      type = "metaheuristic",
      requires_gradient = FALSE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Artificial Bee Colony is a swarm-based optimizer inspired by the",
        "foraging behavior of honey bees, including employed, onlooker, and",
        "scout bee phases."
      ),
      main_parameters = c("colony_size", "max_iter", "limit")
    ),

    gwo = list(
      name = "gwo",
      full_name = "Grey Wolf Optimizer",
      type = "metaheuristic",
      requires_gradient = FALSE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Grey Wolf Optimizer is a population-based metaheuristic inspired by",
        "the leadership hierarchy and hunting mechanism of grey wolves."
      ),
      main_parameters = c("pop_size", "max_iter", "a_start", "a_end")
    ),

    woa = list(
      name = "woa",
      full_name = "Whale Optimization Algorithm",
      type = "metaheuristic",
      requires_gradient = FALSE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Whale Optimization Algorithm is inspired by the bubble-net hunting",
        "behavior of humpback whales and uses encircling, exploration, and",
        "spiral updating mechanisms."
      ),
      main_parameters = c("pop_size", "max_iter", "a_start", "a_end", "b")
    ),

    tlbo = list(
      name = "tlbo",
      full_name = "Teaching-Learning-Based Optimization",
      type = "metaheuristic",
      requires_gradient = FALSE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Teaching-Learning-Based Optimization is a population-based optimizer",
        "that models learning through teacher and learner phases."
      ),
      main_parameters = c("pop_size", "max_iter")
    ),

    sboa = list(
      name = "sboa",
      full_name = "Secretary Bird Optimization Algorithm",
      type = "metaheuristic",
      requires_gradient = FALSE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Secretary Bird Optimization Algorithm is a metaheuristic inspired by",
        "secretary bird predation and escape behaviors. It includes search,",
        "approach, attack, and escape strategies."
      ),
      main_parameters = c("pop_size", "max_iter")
    ),

    sgd = list(
      name = "sgd",
      full_name = "Stochastic Gradient Descent",
      type = "gradient",
      requires_gradient = TRUE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Stochastic Gradient Descent is a gradient-based optimizer that updates",
        "parameters in the negative direction of the gradient."
      ),
      main_parameters = c("learning_rate", "epochs", "batch_size")
    ),

    adam = list(
      name = "adam",
      full_name = "Adaptive Moment Estimation",
      type = "gradient",
      requires_gradient = TRUE,
      supported_in = c("met_optimize", "met_mlp"),
      description = paste(
        "Adam is a gradient-based optimizer that combines adaptive learning",
        "rates with first- and second-moment estimates of the gradient."
      ),
      main_parameters = c(
        "learning_rate", "beta1", "beta2", "epsilon",
        "epochs", "batch_size"
      )
    ),

    stop(
      "No information is currently available for optimizer '",
      optimizer$name,
      "'.",
      call. = FALSE
    )
  )

  info$current_parameters <- optimizer$parameters

  class(info) <- "met_optimizer_info"

  info
}


#' Print Optimizer Information
#'
#' @param x An object of class `"met_optimizer_info"`.
#' @param ... Additional arguments.
#'
#' @return The input object invisibly.
#' @export
print.met_optimizer_info <- function(x, ...) {
  cat("metANN optimizer information\n")
  cat("  Name: ", x$name, "\n", sep = "")
  cat("  Full name: ", x$full_name, "\n", sep = "")
  cat("  Type: ", x$type, "\n", sep = "")
  cat("  Requires gradient: ", x$requires_gradient, "\n", sep = "")
  cat("  Supported in: ", paste(x$supported_in, collapse = ", "), "\n", sep = "")
  cat("\n")
  cat("Description:\n")
  cat("  ", x$description, "\n", sep = "")
  cat("\n")
  cat("Main parameters:\n")
  cat("  ", paste(x$main_parameters, collapse = ", "), "\n", sep = "")

  if (length(x$current_parameters) > 0L) {
    cat("\n")
    cat("Current parameter values:\n")
    for (nm in names(x$current_parameters)) {
      value <- x$current_parameters[[nm]]

      if (is.null(value)) {
        value <- "NULL"
      } else if (length(value) > 1L) {
        value <- paste(value, collapse = ", ")
      }

      cat("  ", nm, ": ", value, "\n", sep = "")
    }
  }

  invisible(x)
}
