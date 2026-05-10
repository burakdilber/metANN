#' General-Purpose Optimization
#'
#' Performs continuous optimization using metaheuristic or gradient-based
#' optimization algorithms.
#'
#' @param fn Objective function to be minimized. It must accept a numeric
#' vector as its first argument and return a single numeric value.
#' @param optimizer Optimizer object created by functions such as
#' `optimizer_pso()`, `optimizer_sboa()`, `optimizer_sgd()`, or
#' `optimizer_adam()`.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param gr Optional gradient function. Required for gradient-based optimizers
#' such as `optimizer_sgd()` and `optimizer_adam()`. It must accept a numeric
#' vector as its first argument and return a numeric vector of the same length.
#' @param initial Optional numeric vector of initial parameter values. If `NULL`,
#' a random initial point is generated within the given bounds for
#' gradient-based optimizers.
#' @param seed Optional random seed.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn` and, when applicable, `gr`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @export
#'
#' @examples
#' sphere <- function(x) sum(x^2)
#'
#' result <- met_optimize(
#'   fn = sphere,
#'   optimizer = optimizer_pso(pop_size = 10, max_iter = 20),
#'   lower = rep(-5, 2),
#'   upper = rep(5, 2),
#'   seed = 123,
#'   verbose = FALSE
#' )
#'
#' result
#'
#' grad_sphere <- function(x) 2 * x
#'
#' result_adam <- met_optimize(
#'   fn = sphere,
#'   gr = grad_sphere,
#'   optimizer = optimizer_adam(learning_rate = 0.1, epochs = 20),
#'   lower = rep(-5, 2),
#'   upper = rep(5, 2),
#'   initial = rep(4, 2),
#'   seed = 123,
#'   verbose = FALSE
#' )
#'
#' result_adam
met_optimize <- function(fn,
                         optimizer = optimizer_pso(),
                         lower,
                         upper,
                         gr = NULL,
                         initial = NULL,
                         seed = NULL,
                         verbose = TRUE,
                         ...) {
  if (!is.function(fn)) {
    stop("'fn' must be a function.", call. = FALSE)
  }

  optimizer <- as_optimizer(optimizer)

  if (!is.numeric(lower) || !is.numeric(upper)) {
    stop("'lower' and 'upper' must be numeric vectors.", call. = FALSE)
  }

  if (length(lower) != length(upper)) {
    stop("'lower' and 'upper' must have the same length.", call. = FALSE)
  }

  if (length(lower) == 0L) {
    stop("'lower' and 'upper' must not be empty.", call. = FALSE)
  }

  if (any(lower >= upper)) {
    stop("Each lower bound must be smaller than the corresponding upper bound.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (!is.logical(verbose) || length(verbose) != 1L) {
    stop("'verbose' must be a single logical value.", call. = FALSE)
  }

  if (optimizer$type == "gradient") {
    return(
      gradient_optimize(
        fn = fn,
        gr = gr,
        lower = lower,
        upper = upper,
        initial = initial,
        optimizer = optimizer,
        verbose = verbose,
        ...
      )
    )
  }

  if (optimizer$name == "pso") {
    return(
      pso_optimize(
        fn = fn,
        lower = lower,
        upper = upper,
        optimizer = optimizer,
        verbose = verbose,
        ...
      )
    )
  }

  if (optimizer$name == "de") {
    return(
      de_optimize(
        fn = fn,
        lower = lower,
        upper = upper,
        optimizer = optimizer,
        verbose = verbose,
        ...
      )
    )
  }

  if (optimizer$name == "ga") {
    return(
      ga_optimize(
        fn = fn,
        lower = lower,
        upper = upper,
        optimizer = optimizer,
        verbose = verbose,
        ...
      )
    )
  }

  if (optimizer$name == "abc") {
    return(
      abc_optimize(
        fn = fn,
        lower = lower,
        upper = upper,
        optimizer = optimizer,
        verbose = verbose,
        ...
      )
    )
  }

  if (optimizer$name == "gwo") {
    return(
      gwo_optimize(
        fn = fn,
        lower = lower,
        upper = upper,
        optimizer = optimizer,
        verbose = verbose,
        ...
      )
    )
  }

  if (optimizer$name == "woa") {
    return(
      woa_optimize(
        fn = fn,
        lower = lower,
        upper = upper,
        optimizer = optimizer,
        verbose = verbose,
        ...
      )
    )
  }

  if (optimizer$name == "tlbo") {
    return(
      tlbo_optimize(
        fn = fn,
        lower = lower,
        upper = upper,
        optimizer = optimizer,
        verbose = verbose,
        ...
      )
    )
  }

  if (optimizer$name == "sboa") {
    return(
      sboa_optimize(
        fn = fn,
        lower = lower,
        upper = upper,
        optimizer = optimizer,
        verbose = verbose,
        ...
      )
    )
  }

  stop(
    "Optimizer '", optimizer$name, "' is not implemented yet in met_optimize().",
    call. = FALSE
  )
}


#' Particle Swarm Optimization Engine
#'
#' Internal implementation of Particle Swarm Optimization.
#'
#' @param fn Objective function.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param optimizer A PSO optimizer object.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @keywords internal
pso_optimize <- function(fn,
                         lower,
                         upper,
                         optimizer,
                         verbose = TRUE,
                         ...) {
  pars <- optimizer$parameters

  pop_size <- pars$pop_size
  max_iter <- pars$max_iter
  w <- pars$w
  c1 <- pars$c1
  c2 <- pars$c2
  velocity_clamp <- pars$velocity_clamp

  dim <- length(lower)

  positions <- matrix(NA_real_, nrow = pop_size, ncol = dim)
  for (j in seq_len(dim)) {
    positions[, j] <- stats::runif(pop_size, min = lower[j], max = upper[j])
  }

  range <- upper - lower
  velocities <- matrix(
    stats::runif(pop_size * dim, min = -abs(range), max = abs(range)),
    nrow = pop_size,
    ncol = dim
  )

  fitness <- apply(positions, 1L, function(x) fn(x, ...))

  if (any(!is.finite(fitness))) {
    stop("The objective function returned non-finite values for the initial population.", call. = FALSE)
  }

  personal_best_positions <- positions
  personal_best_values <- fitness

  best_index <- which.min(fitness)
  global_best_position <- positions[best_index, ]
  global_best_value <- fitness[best_index]

  convergence <- numeric(max_iter)
  convergence[1L] <- global_best_value

  if (isTRUE(verbose)) {
    cat("PSO optimization started\n")
    cat("  Initial best value:", global_best_value, "\n")
  }

  for (iter in seq_len(max_iter)) {
    r1 <- matrix(stats::runif(pop_size * dim), nrow = pop_size, ncol = dim)
    r2 <- matrix(stats::runif(pop_size * dim), nrow = pop_size, ncol = dim)

    cognitive <- c1 * r1 * (personal_best_positions - positions)
    social <- c2 * r2 * matrix(
      rep(global_best_position, each = pop_size),
      nrow = pop_size,
      ncol = dim
    )

    velocities <- w * velocities + cognitive + social

    if (!is.null(velocity_clamp)) {
      velocities[velocities > velocity_clamp] <- velocity_clamp
      velocities[velocities < -velocity_clamp] <- -velocity_clamp
    }

    positions <- positions + velocities

    for (j in seq_len(dim)) {
      positions[, j] <- pmin(pmax(positions[, j], lower[j]), upper[j])
    }

    fitness <- apply(positions, 1L, function(x) fn(x, ...))

    if (any(!is.finite(fitness))) {
      stop("The objective function returned non-finite values during optimization.", call. = FALSE)
    }

    improved <- fitness < personal_best_values

    if (any(improved)) {
      personal_best_positions[improved, ] <- positions[improved, , drop = FALSE]
      personal_best_values[improved] <- fitness[improved]
    }

    best_index <- which.min(personal_best_values)

    if (personal_best_values[best_index] < global_best_value) {
      global_best_value <- personal_best_values[best_index]
      global_best_position <- personal_best_positions[best_index, ]
    }

    convergence[iter] <- global_best_value

    if (isTRUE(verbose) && (iter %% 10L == 0L || iter == max_iter)) {
      cat("  Iteration", iter, "- best value:", global_best_value, "\n")
    }
  }

  result <- list(
    best_par = global_best_position,
    best_value = global_best_value,
    convergence = convergence,
    optimizer = optimizer,
    lower = lower,
    upper = upper,
    objective = fn,
    n_iter = max_iter
  )

  class(result) <- "met_optimize_result"

  result
}

#' Differential Evolution Engine
#'
#' Internal implementation of Differential Evolution using the rand/1/bin
#' strategy.
#'
#' @param fn Objective function.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param optimizer A DE optimizer object.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @keywords internal
de_optimize <- function(fn,
                        lower,
                        upper,
                        optimizer,
                        verbose = TRUE,
                        ...) {
  pars <- optimizer$parameters

  pop_size <- pars$pop_size
  max_iter <- pars$max_iter
  F <- pars$F
  CR <- pars$CR

  dim <- length(lower)

  population <- matrix(NA_real_, nrow = pop_size, ncol = dim)

  for (j in seq_len(dim)) {
    population[, j] <- stats::runif(pop_size, min = lower[j], max = upper[j])
  }

  fitness <- apply(population, 1L, function(x) fn(x, ...))

  if (any(!is.finite(fitness))) {
    stop("The objective function returned non-finite values for the initial population.", call. = FALSE)
  }

  best_index <- which.min(fitness)
  best_position <- population[best_index, ]
  best_value <- fitness[best_index]

  convergence <- numeric(max_iter)
  convergence[1L] <- best_value

  if (isTRUE(verbose)) {
    cat("DE optimization started\n")
    cat("  Initial best value:", best_value, "\n")
  }

  for (iter in seq_len(max_iter)) {
    for (i in seq_len(pop_size)) {
      candidates <- setdiff(seq_len(pop_size), i)
      selected <- sample(candidates, size = 3L, replace = FALSE)

      r1 <- selected[1L]
      r2 <- selected[2L]
      r3 <- selected[3L]

      mutant <- population[r1, ] + F * (population[r2, ] - population[r3, ])

      mutant <- pmin(pmax(mutant, lower), upper)

      trial <- population[i, ]

      j_rand <- sample(seq_len(dim), size = 1L)

      for (j in seq_len(dim)) {
        if (stats::runif(1L) <= CR || j == j_rand) {
          trial[j] <- mutant[j]
        }
      }

      trial <- pmin(pmax(trial, lower), upper)

      trial_fitness <- fn(trial, ...)

      if (!is.finite(trial_fitness)) {
        stop("The objective function returned a non-finite value during optimization.", call. = FALSE)
      }

      if (trial_fitness <= fitness[i]) {
        population[i, ] <- trial
        fitness[i] <- trial_fitness

        if (trial_fitness < best_value) {
          best_value <- trial_fitness
          best_position <- trial
        }
      }
    }

    convergence[iter] <- best_value

    if (isTRUE(verbose) && (iter %% 10L == 0L || iter == max_iter)) {
      cat("  Iteration", iter, "- best value:", best_value, "\n")
    }
  }

  result <- list(
    best_par = best_position,
    best_value = best_value,
    convergence = convergence,
    optimizer = optimizer,
    lower = lower,
    upper = upper,
    objective = fn,
    n_iter = max_iter
  )

  class(result) <- "met_optimize_result"

  result
}

#' Genetic Algorithm Engine
#'
#' Internal implementation of a real-coded Genetic Algorithm.
#'
#' @param fn Objective function.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param optimizer A GA optimizer object.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @keywords internal
ga_optimize <- function(fn,
                        lower,
                        upper,
                        optimizer,
                        verbose = TRUE,
                        ...) {
  pars <- optimizer$parameters

  pop_size <- pars$pop_size
  max_iter <- pars$max_iter
  crossover_rate <- pars$crossover_rate
  mutation_rate <- pars$mutation_rate
  mutation_sd <- pars$mutation_sd
  elitism <- pars$elitism
  tournament_size <- pars$tournament_size

  dim <- length(lower)
  range <- upper - lower

  population <- matrix(NA_real_, nrow = pop_size, ncol = dim)

  for (j in seq_len(dim)) {
    population[, j] <- stats::runif(pop_size, min = lower[j], max = upper[j])
  }

  fitness <- apply(population, 1L, function(x) fn(x, ...))

  if (any(!is.finite(fitness))) {
    stop("The objective function returned non-finite values for the initial population.", call. = FALSE)
  }

  best_index <- which.min(fitness)
  best_position <- population[best_index, ]
  best_value <- fitness[best_index]

  convergence <- numeric(max_iter)
  convergence[1L] <- best_value

  if (isTRUE(verbose)) {
    cat("GA optimization started\n")
    cat("  Initial best value:", best_value, "\n")
  }

  tournament_select <- function(fitness_values) {
    candidates <- sample(seq_along(fitness_values), size = tournament_size, replace = FALSE)
    candidates[which.min(fitness_values[candidates])]
  }

  for (iter in seq_len(max_iter)) {
    new_population <- matrix(NA_real_, nrow = pop_size, ncol = dim)

    start_index <- 1L

    if (isTRUE(elitism)) {
      new_population[1L, ] <- best_position
      start_index <- 2L
    }

    i <- start_index

    while (i <= pop_size) {
      parent1 <- population[tournament_select(fitness), ]
      parent2 <- population[tournament_select(fitness), ]

      child1 <- parent1
      child2 <- parent2

      if (stats::runif(1L) <= crossover_rate) {
        alpha <- stats::runif(dim)
        child1 <- alpha * parent1 + (1 - alpha) * parent2
        child2 <- alpha * parent2 + (1 - alpha) * parent1
      }

      mutation_mask1 <- stats::runif(dim) <= mutation_rate
      mutation_mask2 <- stats::runif(dim) <= mutation_rate

      if (any(mutation_mask1)) {
        child1[mutation_mask1] <- child1[mutation_mask1] +
          stats::rnorm(sum(mutation_mask1), mean = 0, sd = mutation_sd * range[mutation_mask1])
      }

      if (any(mutation_mask2)) {
        child2[mutation_mask2] <- child2[mutation_mask2] +
          stats::rnorm(sum(mutation_mask2), mean = 0, sd = mutation_sd * range[mutation_mask2])
      }

      child1 <- pmin(pmax(child1, lower), upper)
      child2 <- pmin(pmax(child2, lower), upper)

      new_population[i, ] <- child1

      if ((i + 1L) <= pop_size) {
        new_population[i + 1L, ] <- child2
      }

      i <- i + 2L
    }

    population <- new_population

    fitness <- apply(population, 1L, function(x) fn(x, ...))

    if (any(!is.finite(fitness))) {
      stop("The objective function returned non-finite values during optimization.", call. = FALSE)
    }

    current_best_index <- which.min(fitness)
    current_best_value <- fitness[current_best_index]

    if (current_best_value < best_value) {
      best_value <- current_best_value
      best_position <- population[current_best_index, ]
    }

    convergence[iter] <- best_value

    if (isTRUE(verbose) && (iter %% 10L == 0L || iter == max_iter)) {
      cat("  Iteration", iter, "- best value:", best_value, "\n")
    }
  }

  result <- list(
    best_par = best_position,
    best_value = best_value,
    convergence = convergence,
    optimizer = optimizer,
    lower = lower,
    upper = upper,
    objective = fn,
    n_iter = max_iter
  )

  class(result) <- "met_optimize_result"

  result
}

#' Artificial Bee Colony Engine
#'
#' Internal implementation of the Artificial Bee Colony algorithm for
#' continuous optimization.
#'
#' @param fn Objective function.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param optimizer An ABC optimizer object.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @keywords internal
abc_optimize <- function(fn,
                         lower,
                         upper,
                         optimizer,
                         verbose = TRUE,
                         ...) {
  pars <- optimizer$parameters

  colony_size <- pars$colony_size
  max_iter <- pars$max_iter
  limit <- pars$limit

  food_number <- colony_size / 2L
  dim <- length(lower)

  foods <- matrix(NA_real_, nrow = food_number, ncol = dim)

  for (j in seq_len(dim)) {
    foods[, j] <- stats::runif(food_number, min = lower[j], max = upper[j])
  }

  fitness_values <- apply(foods, 1L, function(x) fn(x, ...))

  if (any(!is.finite(fitness_values))) {
    stop("The objective function returned non-finite values for the initial population.", call. = FALSE)
  }

  trials <- integer(food_number)

  best_index <- which.min(fitness_values)
  best_position <- foods[best_index, ]
  best_value <- fitness_values[best_index]

  convergence <- numeric(max_iter)
  convergence[1L] <- best_value

  if (isTRUE(verbose)) {
    cat("ABC optimization started\n")
    cat("  Initial best value:", best_value, "\n")
  }

  calculate_selection_probabilities <- function(values) {
    quality <- 1 / (1 + values - min(values))

    if (any(!is.finite(quality)) || sum(quality) <= 0) {
      return(rep(1 / length(values), length(values)))
    }

    quality / sum(quality)
  }

  create_neighbor <- function(index, foods_matrix) {
    partner_candidates <- setdiff(seq_len(food_number), index)
    partner_index <- sample(partner_candidates, size = 1L)

    dimension_index <- sample(seq_len(dim), size = 1L)
    phi <- stats::runif(1L, min = -1, max = 1)

    candidate <- foods_matrix[index, ]
    candidate[dimension_index] <- foods_matrix[index, dimension_index] +
      phi * (foods_matrix[index, dimension_index] - foods_matrix[partner_index, dimension_index])

    candidate <- pmin(pmax(candidate, lower), upper)

    candidate
  }

  greedy_update <- function(index, candidate) {
    candidate_value <- fn(candidate, ...)

    if (!is.finite(candidate_value)) {
      stop("The objective function returned a non-finite value during optimization.", call. = FALSE)
    }

    if (candidate_value <= fitness_values[index]) {
      foods[index, ] <<- candidate
      fitness_values[index] <<- candidate_value
      trials[index] <<- 0L
    } else {
      trials[index] <<- trials[index] + 1L
    }

    invisible(NULL)
  }

  for (iter in seq_len(max_iter)) {
    for (i in seq_len(food_number)) {
      candidate <- create_neighbor(i, foods)
      greedy_update(i, candidate)
    }

    probabilities <- calculate_selection_probabilities(fitness_values)

    onlooker_count <- 0L

    while (onlooker_count < food_number) {
      selected_index <- sample(
        seq_len(food_number),
        size = 1L,
        prob = probabilities
      )

      candidate <- create_neighbor(selected_index, foods)
      greedy_update(selected_index, candidate)

      onlooker_count <- onlooker_count + 1L
    }

    abandoned <- which(trials >= limit)

    if (length(abandoned) > 0L) {
      for (idx in abandoned) {
        new_food <- numeric(dim)

        for (j in seq_len(dim)) {
          new_food[j] <- stats::runif(1L, min = lower[j], max = upper[j])
        }

        new_value <- fn(new_food, ...)

        if (!is.finite(new_value)) {
          stop("The objective function returned a non-finite value during scout phase.", call. = FALSE)
        }

        foods[idx, ] <- new_food
        fitness_values[idx] <- new_value
        trials[idx] <- 0L
      }
    }

    current_best_index <- which.min(fitness_values)
    current_best_value <- fitness_values[current_best_index]

    if (current_best_value < best_value) {
      best_value <- current_best_value
      best_position <- foods[current_best_index, ]
    }

    convergence[iter] <- best_value

    if (isTRUE(verbose) && (iter %% 10L == 0L || iter == max_iter)) {
      cat("  Iteration", iter, "- best value:", best_value, "\n")
    }
  }

  result <- list(
    best_par = best_position,
    best_value = best_value,
    convergence = convergence,
    optimizer = optimizer,
    lower = lower,
    upper = upper,
    objective = fn,
    n_iter = max_iter
  )

  class(result) <- "met_optimize_result"

  result
}

#' Grey Wolf Optimizer Engine
#'
#' Internal implementation of the Grey Wolf Optimizer for continuous
#' optimization.
#'
#' @param fn Objective function.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param optimizer A GWO optimizer object.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @keywords internal
gwo_optimize <- function(fn,
                         lower,
                         upper,
                         optimizer,
                         verbose = TRUE,
                         ...) {
  pars <- optimizer$parameters

  pop_size <- pars$pop_size
  max_iter <- pars$max_iter
  a_start <- pars$a_start
  a_end <- pars$a_end

  dim <- length(lower)

  positions <- matrix(NA_real_, nrow = pop_size, ncol = dim)

  for (j in seq_len(dim)) {
    positions[, j] <- stats::runif(pop_size, min = lower[j], max = upper[j])
  }

  fitness <- apply(positions, 1L, function(x) fn(x, ...))

  if (any(!is.finite(fitness))) {
    stop("The objective function returned non-finite values for the initial population.", call. = FALSE)
  }

  order_index <- order(fitness)

  alpha_pos <- positions[order_index[1L], ]
  alpha_score <- fitness[order_index[1L]]

  beta_pos <- positions[order_index[2L], ]
  beta_score <- fitness[order_index[2L]]

  delta_pos <- positions[order_index[3L], ]
  delta_score <- fitness[order_index[3L]]

  convergence <- numeric(max_iter)
  convergence[1L] <- alpha_score

  if (isTRUE(verbose)) {
    cat("GWO optimization started\n")
    cat("  Initial best value:", alpha_score, "\n")
  }

  for (iter in seq_len(max_iter)) {
    if (max_iter == 1L) {
      a <- a_end
    } else {
      a <- a_start - (a_start - a_end) * ((iter - 1L) / (max_iter - 1L))
    }

    for (i in seq_len(pop_size)) {
      r1 <- stats::runif(dim)
      r2 <- stats::runif(dim)
      A1 <- 2 * a * r1 - a
      C1 <- 2 * r2
      D_alpha <- abs(C1 * alpha_pos - positions[i, ])
      X1 <- alpha_pos - A1 * D_alpha

      r1 <- stats::runif(dim)
      r2 <- stats::runif(dim)
      A2 <- 2 * a * r1 - a
      C2 <- 2 * r2
      D_beta <- abs(C2 * beta_pos - positions[i, ])
      X2 <- beta_pos - A2 * D_beta

      r1 <- stats::runif(dim)
      r2 <- stats::runif(dim)
      A3 <- 2 * a * r1 - a
      C3 <- 2 * r2
      D_delta <- abs(C3 * delta_pos - positions[i, ])
      X3 <- delta_pos - A3 * D_delta

      new_position <- (X1 + X2 + X3) / 3

      new_position <- pmin(pmax(new_position, lower), upper)

      positions[i, ] <- new_position
    }

    fitness <- apply(positions, 1L, function(x) fn(x, ...))

    if (any(!is.finite(fitness))) {
      stop("The objective function returned non-finite values during optimization.", call. = FALSE)
    }

    order_index <- order(fitness)

    if (fitness[order_index[1L]] < alpha_score) {
      alpha_pos <- positions[order_index[1L], ]
      alpha_score <- fitness[order_index[1L]]
    }

    if (fitness[order_index[2L]] < beta_score) {
      beta_pos <- positions[order_index[2L], ]
      beta_score <- fitness[order_index[2L]]
    }

    if (fitness[order_index[3L]] < delta_score) {
      delta_pos <- positions[order_index[3L], ]
      delta_score <- fitness[order_index[3L]]
    }

    convergence[iter] <- alpha_score

    if (isTRUE(verbose) && (iter %% 10L == 0L || iter == max_iter)) {
      cat("  Iteration", iter, "- best value:", alpha_score, "\n")
    }
  }

  result <- list(
    best_par = alpha_pos,
    best_value = alpha_score,
    convergence = convergence,
    optimizer = optimizer,
    lower = lower,
    upper = upper,
    objective = fn,
    n_iter = max_iter
  )

  class(result) <- "met_optimize_result"

  result
}

#' Whale Optimization Algorithm Engine
#'
#' Internal implementation of the Whale Optimization Algorithm for continuous
#' optimization.
#'
#' @param fn Objective function.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param optimizer A WOA optimizer object.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @keywords internal
woa_optimize <- function(fn,
                         lower,
                         upper,
                         optimizer,
                         verbose = TRUE,
                         ...) {
  pars <- optimizer$parameters

  pop_size <- pars$pop_size
  max_iter <- pars$max_iter
  a_start <- pars$a_start
  a_end <- pars$a_end
  b <- pars$b

  dim <- length(lower)

  positions <- matrix(NA_real_, nrow = pop_size, ncol = dim)

  for (j in seq_len(dim)) {
    positions[, j] <- stats::runif(pop_size, min = lower[j], max = upper[j])
  }

  fitness <- apply(positions, 1L, function(x) fn(x, ...))

  if (any(!is.finite(fitness))) {
    stop("The objective function returned non-finite values for the initial population.", call. = FALSE)
  }

  best_index <- which.min(fitness)
  best_position <- positions[best_index, ]
  best_value <- fitness[best_index]

  convergence <- numeric(max_iter)
  convergence[1L] <- best_value

  if (isTRUE(verbose)) {
    cat("WOA optimization started\n")
    cat("  Initial best value:", best_value, "\n")
  }

  for (iter in seq_len(max_iter)) {
    if (max_iter == 1L) {
      a <- a_end
      a2 <- -2
    } else {
      progress <- (iter - 1L) / (max_iter - 1L)
      a <- a_start - (a_start - a_end) * progress
      a2 <- -1 - progress
    }

    for (i in seq_len(pop_size)) {
      r1 <- stats::runif(1L)
      r2 <- stats::runif(1L)

      A <- 2 * a * r1 - a
      C <- 2 * r2

      p <- stats::runif(1L)
      l <- (a2 - 1) * stats::runif(1L) + 1

      current_position <- positions[i, ]

      if (p < 0.5) {
        if (abs(A) < 1) {
          D_leader <- abs(C * best_position - current_position)
          new_position <- best_position - A * D_leader
        } else {
          rand_index <- sample(seq_len(pop_size), size = 1L)
          rand_position <- positions[rand_index, ]
          D_rand <- abs(C * rand_position - current_position)
          new_position <- rand_position - A * D_rand
        }
      } else {
        D_leader <- abs(best_position - current_position)
        new_position <- D_leader * exp(b * l) * cos(2 * pi * l) + best_position
      }

      new_position <- pmin(pmax(new_position, lower), upper)

      positions[i, ] <- new_position
    }

    fitness <- apply(positions, 1L, function(x) fn(x, ...))

    if (any(!is.finite(fitness))) {
      stop("The objective function returned non-finite values during optimization.", call. = FALSE)
    }

    current_best_index <- which.min(fitness)
    current_best_value <- fitness[current_best_index]

    if (current_best_value < best_value) {
      best_value <- current_best_value
      best_position <- positions[current_best_index, ]
    }

    convergence[iter] <- best_value

    if (isTRUE(verbose) && (iter %% 10L == 0L || iter == max_iter)) {
      cat("  Iteration", iter, "- best value:", best_value, "\n")
    }
  }

  result <- list(
    best_par = best_position,
    best_value = best_value,
    convergence = convergence,
    optimizer = optimizer,
    lower = lower,
    upper = upper,
    objective = fn,
    n_iter = max_iter
  )

  class(result) <- "met_optimize_result"

  result
}

#' Teaching-Learning-Based Optimization Engine
#'
#' Internal implementation of Teaching-Learning-Based Optimization for
#' continuous optimization.
#'
#' @param fn Objective function.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param optimizer A TLBO optimizer object.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @keywords internal
tlbo_optimize <- function(fn,
                          lower,
                          upper,
                          optimizer,
                          verbose = TRUE,
                          ...) {
  pars <- optimizer$parameters

  pop_size <- pars$pop_size
  max_iter <- pars$max_iter
  dim <- length(lower)

  population <- matrix(NA_real_, nrow = pop_size, ncol = dim)

  for (j in seq_len(dim)) {
    population[, j] <- stats::runif(pop_size, min = lower[j], max = upper[j])
  }

  fitness <- apply(population, 1L, function(x) fn(x, ...))

  if (any(!is.finite(fitness))) {
    stop(
      "The objective function returned non-finite values for the initial population.",
      call. = FALSE
    )
  }

  best_index <- which.min(fitness)
  best_position <- population[best_index, ]
  best_value <- fitness[best_index]

  convergence <- numeric(max_iter)
  convergence[1L] <- best_value

  if (isTRUE(verbose)) {
    cat("TLBO optimization started\n")
    cat("  Initial best value:", best_value, "\n")
  }

  for (iter in seq_len(max_iter)) {
    mean_position <- colMeans(population)

    teacher_index <- which.min(fitness)
    teacher_position <- population[teacher_index, ]

    # Teacher phase
    for (i in seq_len(pop_size)) {
      teaching_factor <- sample(c(1L, 2L), size = 1L)

      candidate <- population[i, ] +
        stats::runif(dim) * (teacher_position - teaching_factor * mean_position)

      candidate <- pmin(pmax(candidate, lower), upper)

      candidate_value <- fn(candidate, ...)

      if (!is.finite(candidate_value)) {
        stop(
          "The objective function returned a non-finite value during the teacher phase.",
          call. = FALSE
        )
      }

      if (candidate_value < fitness[i]) {
        population[i, ] <- candidate
        fitness[i] <- candidate_value

        if (candidate_value < best_value) {
          best_value <- candidate_value
          best_position <- candidate
        }
      }
    }

    # Learner phase
    for (i in seq_len(pop_size)) {
      candidates <- setdiff(seq_len(pop_size), i)
      j <- sample(candidates, size = 1L)

      step <- population[i, ] - population[j, ]

      if (fitness[j] < fitness[i]) {
        step <- -step
      }

      candidate <- population[i, ] + stats::runif(dim) * step

      candidate <- pmin(pmax(candidate, lower), upper)

      candidate_value <- fn(candidate, ...)

      if (!is.finite(candidate_value)) {
        stop(
          "The objective function returned a non-finite value during the learner phase.",
          call. = FALSE
        )
      }

      if (candidate_value < fitness[i]) {
        population[i, ] <- candidate
        fitness[i] <- candidate_value

        if (candidate_value < best_value) {
          best_value <- candidate_value
          best_position <- candidate
        }
      }
    }

    convergence[iter] <- best_value

    if (isTRUE(verbose) && (iter %% 10L == 0L || iter == max_iter)) {
      cat("  Iteration", iter, "- best value:", best_value, "\n")
    }
  }

  result <- list(
    best_par = best_position,
    best_value = best_value,
    convergence = convergence,
    optimizer = optimizer,
    lower = lower,
    upper = upper,
    objective = fn,
    n_iter = max_iter
  )

  class(result) <- "met_optimize_result"

  result
}

#' Levy Flight Step
#'
#' Internal helper used by the Secretary Bird Optimization Algorithm.
#'
#' @param dim Problem dimension.
#' @param beta Levy distribution parameter.
#'
#' @return A numeric vector of Levy flight steps.
#' @keywords internal
levy_flight <- function(dim, beta = 1.5) {
  sigma <- (
    gamma(1 + beta) * sin(pi * beta / 2) /
      (gamma((1 + beta) / 2) * beta * 2^((beta - 1) / 2))
  )^(1 / beta)

  u <- stats::rnorm(dim) * sigma
  v <- stats::rnorm(dim)

  u / abs(v)^(1 / beta)
}

#' Secretary Bird Optimization Algorithm Engine
#'
#' Internal implementation of the Secretary Bird Optimization Algorithm for
#' continuous optimization.
#'
#' @param fn Objective function.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param optimizer An SBOA optimizer object.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @keywords internal
sboa_optimize <- function(fn,
                          lower,
                          upper,
                          optimizer,
                          verbose = TRUE,
                          ...) {
  pars <- optimizer$parameters

  pop_size <- pars$pop_size
  max_iter <- pars$max_iter
  dim <- length(lower)

  population <- matrix(NA_real_, nrow = pop_size, ncol = dim)

  for (j in seq_len(dim)) {
    population[, j] <- stats::runif(pop_size, min = lower[j], max = upper[j])
  }

  fitness <- apply(population, 1L, function(x) fn(x, ...))

  if (any(!is.finite(fitness))) {
    stop(
      "The objective function returned non-finite values for the initial population.",
      call. = FALSE
    )
  }

  best_index <- which.min(fitness)
  best_position <- population[best_index, ]
  best_value <- fitness[best_index]

  convergence <- numeric(max_iter)
  convergence[1L] <- best_value

  if (isTRUE(verbose)) {
    cat("SBOA optimization started\n")
    cat("  Initial best value:", best_value, "\n")
  }

  for (iter in seq_len(max_iter)) {
    current_best_index <- which.min(fitness)
    current_best_value <- fitness[current_best_index]

    if (current_best_value < best_value) {
      best_value <- current_best_value
      best_position <- population[current_best_index, ]
    }

    CF <- (1 - iter / max_iter)^(2 * iter / max_iter)

    # Predation strategy
    for (i in seq_len(pop_size)) {
      if (iter < max_iter / 3) {
        random_1 <- sample(seq_len(pop_size), size = 1L)
        random_2 <- sample(seq_len(pop_size), size = 1L)

        R1 <- stats::runif(dim)

        candidate <- population[i, ] +
          (population[random_1, ] - population[random_2, ]) * R1

      } else if (iter > max_iter / 3 && iter < 2 * max_iter / 3) {
        RB <- stats::rnorm(dim)

        candidate <- best_position +
          exp((iter / max_iter)^4) *
          (RB - 0.5) *
          (best_position - population[i, ])

      } else {
        RL <- 0.5 * levy_flight(dim)

        candidate <- best_position +
          CF * population[i, ] * RL
      }

      candidate <- pmin(pmax(candidate, lower), upper)

      candidate_value <- fn(candidate, ...)

      if (!is.finite(candidate_value)) {
        stop(
          "The objective function returned a non-finite value during the predation strategy.",
          call. = FALSE
        )
      }

      if (candidate_value <= fitness[i]) {
        population[i, ] <- candidate
        fitness[i] <- candidate_value

        if (candidate_value < best_value) {
          best_value <- candidate_value
          best_position <- candidate
        }
      }
    }

    # Escape strategy
    r <- stats::runif(1L)
    random_index <- sample(seq_len(pop_size), size = 1L)
    random_position <- population[random_index, ]

    for (i in seq_len(pop_size)) {
      if (r < 0.5) {
        RB <- stats::runif(dim)

        candidate <- best_position +
          (1 - iter / max_iter)^2 *
          (2 * RB - 1) *
          population[i, ]

      } else {
        K <- round(1 + stats::runif(1L))
        R2 <- stats::runif(dim)

        candidate <- population[i, ] +
          R2 * (random_position - K * population[i, ])
      }

      candidate <- pmin(pmax(candidate, lower), upper)

      candidate_value <- fn(candidate, ...)

      if (!is.finite(candidate_value)) {
        stop(
          "The objective function returned a non-finite value during the escape strategy.",
          call. = FALSE
        )
      }

      if (candidate_value <= fitness[i]) {
        population[i, ] <- candidate
        fitness[i] <- candidate_value

        if (candidate_value < best_value) {
          best_value <- candidate_value
          best_position <- candidate
        }
      }
    }

    convergence[iter] <- best_value

    if (isTRUE(verbose) && (iter %% 10L == 0L || iter == max_iter)) {
      cat("  Iteration", iter, "- best value:", best_value, "\n")
    }
  }

  result <- list(
    best_par = best_position,
    best_value = best_value,
    convergence = convergence,
    optimizer = optimizer,
    lower = lower,
    upper = upper,
    objective = fn,
    n_iter = max_iter
  )

  class(result) <- "met_optimize_result"

  result
}

#' Gradient-Based Optimization Engine
#'
#' Internal implementation of gradient-based optimization for differentiable
#' continuous objective functions.
#'
#' @param fn Objective function.
#' @param gr Gradient function.
#' @param lower Numeric vector of lower bounds.
#' @param upper Numeric vector of upper bounds.
#' @param initial Optional numeric vector of initial values.
#' @param optimizer A gradient-based optimizer object.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#' @param ... Additional arguments passed to `fn` and `gr`.
#'
#' @return An object of class `"met_optimize_result"`.
#' @keywords internal
gradient_optimize <- function(fn,
                              gr,
                              lower,
                              upper,
                              initial = NULL,
                              optimizer,
                              verbose = TRUE,
                              ...) {
  if (is.null(gr)) {
    stop(
      "Gradient-based optimizers require a gradient function supplied via 'gr'.",
      call. = FALSE
    )
  }

  if (!is.function(gr)) {
    stop("'gr' must be a function.", call. = FALSE)
  }

  pars <- optimizer$parameters

  dim <- length(lower)

  if (length(upper) != dim) {
    stop("'lower' and 'upper' must have the same length.", call. = FALSE)
  }

  if (any(lower >= upper)) {
    stop("Each lower bound must be smaller than the corresponding upper bound.", call. = FALSE)
  }

  if (is.null(initial)) {
    position <- stats::runif(dim, min = lower, max = upper)
  } else {
    if (!is.numeric(initial) || length(initial) != dim) {
      stop(
        "'initial' must be a numeric vector with the same length as 'lower' and 'upper'.",
        call. = FALSE
      )
    }

    position <- initial
    position <- pmin(pmax(position, lower), upper)
  }

  value <- fn(position, ...)

  if (!is.finite(value)) {
    stop(
      "The objective function returned a non-finite value at the initial point.",
      call. = FALSE
    )
  }

  grad <- gr(position, ...)

  if (!is.numeric(grad) || length(grad) != dim || any(!is.finite(grad))) {
    stop(
      "The gradient function must return a finite numeric vector with the same length as the parameter vector.",
      call. = FALSE
    )
  }

  epochs <- pars$epochs
  learning_rate <- pars$learning_rate

  convergence <- numeric(epochs)

  best_position <- position
  best_value <- value

  if (optimizer$name == "adam") {
    m <- numeric(dim)
    v <- numeric(dim)
    beta1 <- pars$beta1
    beta2 <- pars$beta2
    epsilon <- pars$epsilon
  }

  if (isTRUE(verbose)) {
    cat(toupper(optimizer$name), "optimization started\n")
    cat("  Initial value:", value, "\n")
  }

  for (iter in seq_len(epochs)) {
    grad <- gr(position, ...)

    if (!is.numeric(grad) || length(grad) != dim || any(!is.finite(grad))) {
      stop(
        "The gradient function returned an invalid value during optimization.",
        call. = FALSE
      )
    }

    if (optimizer$name == "sgd") {
      position <- position - learning_rate * grad
    }

    if (optimizer$name == "adam") {
      m <- beta1 * m + (1 - beta1) * grad
      v <- beta2 * v + (1 - beta2) * (grad^2)

      m_hat <- m / (1 - beta1^iter)
      v_hat <- v / (1 - beta2^iter)

      position <- position - learning_rate * m_hat / (sqrt(v_hat) + epsilon)
    }

    position <- pmin(pmax(position, lower), upper)

    value <- fn(position, ...)

    if (!is.finite(value)) {
      stop(
        "The objective function returned a non-finite value during optimization.",
        call. = FALSE
      )
    }

    if (value < best_value) {
      best_value <- value
      best_position <- position
    }

    convergence[iter] <- best_value

    if (isTRUE(verbose) && (iter %% 10L == 0L || iter == epochs)) {
      cat("  Iteration", iter, "- best value:", best_value, "\n")
    }
  }

  result <- list(
    best_par = best_position,
    best_value = best_value,
    convergence = convergence,
    optimizer = optimizer,
    lower = lower,
    upper = upper,
    objective = fn,
    n_iter = epochs
  )

  class(result) <- "met_optimize_result"

  result
}


#' Print a metANN Optimization Result
#'
#' @param x A metANN optimization result object.
#' @param ... Additional arguments, currently unused.
#'
#' @return The input object invisibly.
#' @export
print.met_optimize_result <- function(x, ...) {
  cat("metANN optimization result\n")
  cat("  Optimizer  :", x$optimizer$name, "\n")
  cat("  Best value :", x$best_value, "\n")
  cat("  Dimension  :", length(x$best_par), "\n")
  cat("  Iterations :", x$n_iter, "\n")

  invisible(x)
}

#' Summarize a metANN Optimization Result
#'
#' @param object A metANN optimization result object.
#' @param ... Additional arguments, currently unused.
#'
#' @return A list containing the main optimization results.
#' @export
summary.met_optimize_result <- function(object, ...) {
  cat("metANN optimization summary\n")
  cat("  Optimizer        :", object$optimizer$name, "\n")
  cat("  Optimizer type   :", object$optimizer$type, "\n")
  cat("  Best value       :", object$best_value, "\n")
  cat("  Dimension        :", length(object$best_par), "\n")
  cat("  Iterations       :", object$n_iter, "\n")
  cat("  Initial value    :", object$convergence[1L], "\n")
  cat("  Final value      :", object$convergence[length(object$convergence)], "\n")

  improvement <- object$convergence[1L] - object$convergence[length(object$convergence)]
  cat("  Improvement      :", improvement, "\n")

  cat("\nBest parameter vector:\n")
  print(object$best_par)

  invisible(
    list(
      optimizer = object$optimizer$name,
      optimizer_type = object$optimizer$type,
      best_value = object$best_value,
      best_par = object$best_par,
      dimension = length(object$best_par),
      iterations = object$n_iter,
      convergence = object$convergence,
      improvement = improvement
    )
  )
}


#' Extract the Best Parameters from a metANN Optimization Result
#'
#' @param object A metANN optimization result object.
#' @param ... Additional arguments, currently unused.
#'
#' @return A numeric vector containing the best solution found.
#' @export
coef.met_optimize_result <- function(object, ...) {
  object$best_par
}


#' Plot Optimization Convergence
#'
#' Plots the convergence curve of a metANN optimization result.
#'
#' @param x An object of class `"met_optimize_result"`.
#' @param log Logical. If `TRUE`, the y-axis is shown on a logarithmic scale.
#' Only positive convergence values can be displayed on a log scale.
#' @param ... Additional graphical arguments passed to `plot()`.
#'
#' @return The input object invisibly.
#' @export
plot.met_optimize_result <- function(x, log = FALSE, ...) {
  if (!is.logical(log) || length(log) != 1L) {
    stop("'log' must be a single logical value.", call. = FALSE)
  }

  convergence <- x$convergence

  if (isTRUE(log)) {
    if (any(convergence <= 0, na.rm = TRUE)) {
      stop(
        "Log-scale plotting requires all convergence values to be positive.",
        call. = FALSE
      )
    }

    graphics::plot(
      convergence,
      type = "l",
      log = "y",
      xlab = "Iteration",
      ylab = "Best objective value",
      main = paste("Convergence -", toupper(x$optimizer$name)),
      ...
    )
  } else {
    graphics::plot(
      convergence,
      type = "l",
      xlab = "Iteration",
      ylab = "Best objective value",
      main = paste("Convergence -", toupper(x$optimizer$name)),
      ...
    )
  }

  invisible(x)
}
