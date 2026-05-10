#' Create an Optimizer Object
#'
#' Internal helper for constructing optimizer objects.
#'
#' @param name A character string specifying the optimizer name.
#' @param type A character string specifying the optimizer type.
#' @param parameters A list of optimizer-specific parameters.
#'
#' @return An object of class `"met_optimizer"`.
#' @keywords internal
new_optimizer <- function(name, type, parameters = list()) {
  if (!is.character(name) || length(name) != 1L) {
    stop("'name' must be a single character string.", call. = FALSE)
  }

  if (!is.character(type) || length(type) != 1L) {
    stop("'type' must be a single character string.", call. = FALSE)
  }

  if (!type %in% c("metaheuristic", "gradient", "hybrid")) {
    stop("'type' must be one of 'metaheuristic', 'gradient', or 'hybrid'.", call. = FALSE)
  }

  if (!is.list(parameters)) {
    stop("'parameters' must be a list.", call. = FALSE)
  }

  structure(
    list(
      name = name,
      type = type,
      parameters = parameters
    ),
    class = c("met_optimizer", paste0("met_", type, "_optimizer"))
  )
}


#' Check Whether an Object is a metANN Optimizer
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` is a metANN optimizer object; otherwise `FALSE`.
#' @export
#'
#' @examples
#' is_optimizer(optimizer_pso())
is_optimizer <- function(x) {
  inherits(x, "met_optimizer")
}


#' Particle Swarm Optimization Optimizer
#'
#' Creates a Particle Swarm Optimization optimizer object.
#'
#' @param pop_size Population size.
#' @param max_iter Maximum number of iterations.
#' @param w Inertia weight.
#' @param c1 Cognitive acceleration coefficient.
#' @param c2 Social acceleration coefficient.
#' @param velocity_clamp Optional maximum absolute velocity. If `NULL`,
#' velocity is not clamped.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Kennedy, J., and Eberhart, R. (1995). Particle Swarm Optimization.
#' Proceedings of ICNN'95 - International Conference on Neural Networks,
#' 4, 1942--1948. doi:10.1109/ICNN.1995.488968
#' @export
#'
#' @examples
#' optimizer_pso()
optimizer_pso <- function(pop_size = 30,
                          max_iter = 100,
                          w = 0.7,
                          c1 = 1.5,
                          c2 = 1.5,
                          velocity_clamp = NULL) {
  if (!is.numeric(pop_size) || length(pop_size) != 1L || pop_size <= 0 || pop_size != as.integer(pop_size)) {
    stop("'pop_size' must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(max_iter) || length(max_iter) != 1L || max_iter <= 0 || max_iter != as.integer(max_iter)) {
    stop("'max_iter' must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(w) || length(w) != 1L || w < 0) {
    stop("'w' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.numeric(c1) || length(c1) != 1L || c1 < 0) {
    stop("'c1' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.numeric(c2) || length(c2) != 1L || c2 < 0) {
    stop("'c2' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.null(velocity_clamp)) {
    if (!is.numeric(velocity_clamp) || length(velocity_clamp) != 1L || velocity_clamp <= 0) {
      stop("'velocity_clamp' must be NULL or a single positive numeric value.", call. = FALSE)
    }
  }

  new_optimizer(
    name = "pso",
    type = "metaheuristic",
    parameters = list(
      pop_size = as.integer(pop_size),
      max_iter = as.integer(max_iter),
      w = w,
      c1 = c1,
      c2 = c2,
      velocity_clamp = velocity_clamp
    )
  )
}

#' Differential Evolution Optimizer
#'
#' Creates a Differential Evolution optimizer object.
#'
#' @param pop_size Population size.
#' @param max_iter Maximum number of iterations.
#' @param F Differential weight. Common values are between 0.4 and 1.
#' @param CR Crossover probability. Must be between 0 and 1.
#' @param strategy Differential evolution strategy. Currently only
#' `"rand/1/bin"` is supported.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Storn, R., and Price, K. (1997). Differential Evolution -- A Simple and
#' Efficient Heuristic for Global Optimization over Continuous Spaces.
#' Journal of Global Optimization, 11, 341--359.
#' doi:10.1023/A:1008202821328
#'
#' Ilonen, J., Kamarainen, J.-K., and Lampinen, J. (2003). Differential
#' Evolution Training Algorithm for Feed-Forward Neural Networks.
#' Neural Processing Letters, 17, 93--105.
#' doi:10.1023/A:1022995128597
#' @export
#'
#' @examples
#' optimizer_de()
optimizer_de <- function(pop_size = 30,
                         max_iter = 100,
                         F = 0.5,
                         CR = 0.9,
                         strategy = "rand/1/bin") {
  if (!is.numeric(pop_size) || length(pop_size) != 1L || pop_size <= 3 || pop_size != as.integer(pop_size)) {
    stop("'pop_size' must be a single integer greater than 3.", call. = FALSE)
  }

  if (!is.numeric(max_iter) || length(max_iter) != 1L || max_iter <= 0 || max_iter != as.integer(max_iter)) {
    stop("'max_iter' must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(F) || length(F) != 1L || F <= 0) {
    stop("'F' must be a single positive numeric value.", call. = FALSE)
  }

  if (!is.numeric(CR) || length(CR) != 1L || CR < 0 || CR > 1) {
    stop("'CR' must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  if (!is.character(strategy) || length(strategy) != 1L) {
    stop("'strategy' must be a single character string.", call. = FALSE)
  }

  if (!strategy %in% c("rand/1/bin")) {
    stop("Currently, only strategy = 'rand/1/bin' is supported.", call. = FALSE)
  }

  new_optimizer(
    name = "de",
    type = "metaheuristic",
    parameters = list(
      pop_size = as.integer(pop_size),
      max_iter = as.integer(max_iter),
      F = F,
      CR = CR,
      strategy = strategy
    )
  )
}

#' Genetic Algorithm Optimizer
#'
#' Creates a real-coded Genetic Algorithm optimizer object.
#'
#' @param pop_size Population size.
#' @param max_iter Maximum number of iterations.
#' @param crossover_rate Probability of crossover.
#' @param mutation_rate Probability of mutating each parameter.
#' @param mutation_sd Standard deviation of Gaussian mutation noise.
#' @param elitism Logical. Whether to preserve the best solution in each
#' generation.
#' @param selection Selection method. Currently only `"tournament"` is
#' supported.
#' @param tournament_size Number of individuals used in tournament selection.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Goldberg, D. E. (1989). Genetic Algorithms in Search, Optimization, and
#' Machine Learning. Addison-Wesley, Reading, MA.
#'
#' Montana, D. J., and Davis, L. (1989). Training Feedforward Neural Networks
#' Using Genetic Algorithms. Proceedings of the 11th International Joint
#' Conference on Artificial Intelligence, 762--767.
#' @export
#'
#' @examples
#' optimizer_ga()
optimizer_ga <- function(pop_size = 30,
                         max_iter = 100,
                         crossover_rate = 0.8,
                         mutation_rate = 0.1,
                         mutation_sd = 0.1,
                         elitism = TRUE,
                         selection = "tournament",
                         tournament_size = 2) {
  if (!is.numeric(pop_size) || length(pop_size) != 1L || pop_size <= 3 || pop_size != as.integer(pop_size)) {
    stop("'pop_size' must be a single integer greater than 3.", call. = FALSE)
  }

  if (!is.numeric(max_iter) || length(max_iter) != 1L || max_iter <= 0 || max_iter != as.integer(max_iter)) {
    stop("'max_iter' must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(crossover_rate) || length(crossover_rate) != 1L || crossover_rate < 0 || crossover_rate > 1) {
    stop("'crossover_rate' must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(mutation_rate) || length(mutation_rate) != 1L || mutation_rate < 0 || mutation_rate > 1) {
    stop("'mutation_rate' must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(mutation_sd) || length(mutation_sd) != 1L || mutation_sd <= 0) {
    stop("'mutation_sd' must be a single positive numeric value.", call. = FALSE)
  }

  if (!is.logical(elitism) || length(elitism) != 1L) {
    stop("'elitism' must be a single logical value.", call. = FALSE)
  }

  if (!is.character(selection) || length(selection) != 1L) {
    stop("'selection' must be a single character string.", call. = FALSE)
  }

  if (!selection %in% c("tournament")) {
    stop("Currently, only selection = 'tournament' is supported.", call. = FALSE)
  }

  if (!is.numeric(tournament_size) || length(tournament_size) != 1L ||
      tournament_size < 2 || tournament_size > pop_size ||
      tournament_size != as.integer(tournament_size)) {
    stop("'tournament_size' must be an integer between 2 and 'pop_size'.", call. = FALSE)
  }

  new_optimizer(
    name = "ga",
    type = "metaheuristic",
    parameters = list(
      pop_size = as.integer(pop_size),
      max_iter = as.integer(max_iter),
      crossover_rate = crossover_rate,
      mutation_rate = mutation_rate,
      mutation_sd = mutation_sd,
      elitism = elitism,
      selection = selection,
      tournament_size = as.integer(tournament_size)
    )
  )
}

#' Artificial Bee Colony Optimizer
#'
#' Creates an Artificial Bee Colony optimizer object for continuous
#' optimization problems.
#'
#' @param colony_size Total colony size. Half of the colony is used as employed
#' bees and half as onlooker bees.
#' @param max_iter Maximum number of iterations.
#' @param limit Number of unsuccessful trials before a food source is abandoned.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Karaboga, D., and Basturk, B. (2007). A Powerful and Efficient Algorithm
#' for Numerical Function Optimization: Artificial Bee Colony (ABC) Algorithm.
#' Journal of Global Optimization, 39, 459--471.
#' doi:10.1007/s10898-007-9149-x
#'
#' Karaboga, D., and Ozturk, C. (2009). Neural Networks Training by
#' Artificial Bee Colony Algorithm on Pattern Classification.
#' Neural Network World, 19(3), 279--292.
#' @export
#'
#' @examples
#' optimizer_abc()
optimizer_abc <- function(colony_size = 30,
                          max_iter = 100,
                          limit = NULL) {
  if (!is.numeric(colony_size) ||
      length(colony_size) != 1L ||
      colony_size <= 3 ||
      colony_size != as.integer(colony_size)) {
    stop("'colony_size' must be a single integer greater than 3.", call. = FALSE)
  }

  if (colony_size %% 2L != 0L) {
    stop("'colony_size' must be an even integer.", call. = FALSE)
  }

  if (!is.numeric(max_iter) ||
      length(max_iter) != 1L ||
      max_iter <= 0 ||
      max_iter != as.integer(max_iter)) {
    stop("'max_iter' must be a single positive integer.", call. = FALSE)
  }

  if (is.null(limit)) {
    limit <- as.integer((colony_size / 2L) * length(seq_len(1L)))
    limit <- max(10L, limit)
  }

  if (!is.numeric(limit) ||
      length(limit) != 1L ||
      limit <= 0 ||
      limit != as.integer(limit)) {
    stop("'limit' must be NULL or a single positive integer.", call. = FALSE)
  }

  new_optimizer(
    name = "abc",
    type = "metaheuristic",
    parameters = list(
      colony_size = as.integer(colony_size),
      max_iter = as.integer(max_iter),
      limit = as.integer(limit)
    )
  )
}

#' Grey Wolf Optimizer
#'
#' Creates a Grey Wolf Optimizer object for continuous optimization problems.
#'
#' @param pop_size Population size.
#' @param max_iter Maximum number of iterations.
#' @param a_start Initial value of the control parameter `a`.
#' @param a_end Final value of the control parameter `a`.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Mirjalili, S., Mirjalili, S. M., and Lewis, A. (2014). Grey Wolf Optimizer.
#' Advances in Engineering Software, 69, 46--61.
#' doi:10.1016/j.advengsoft.2013.12.007
#'
#' Mirjalili, S. (2015). How Effective is the Grey Wolf Optimizer in Training
#' Multi-Layer Perceptrons. Applied Intelligence, 43, 150--161.
#' doi:10.1007/s10489-014-0645-7
#' @export
#'
#' @examples
#' optimizer_gwo()
optimizer_gwo <- function(pop_size = 30,
                          max_iter = 100,
                          a_start = 2,
                          a_end = 0) {
  if (!is.numeric(pop_size) ||
      length(pop_size) != 1L ||
      pop_size <= 3 ||
      pop_size != as.integer(pop_size)) {
    stop("'pop_size' must be a single integer greater than 3.", call. = FALSE)
  }

  if (!is.numeric(max_iter) ||
      length(max_iter) != 1L ||
      max_iter <= 0 ||
      max_iter != as.integer(max_iter)) {
    stop("'max_iter' must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(a_start) || length(a_start) != 1L || a_start < 0) {
    stop("'a_start' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.numeric(a_end) || length(a_end) != 1L || a_end < 0) {
    stop("'a_end' must be a single non-negative numeric value.", call. = FALSE)
  }

  new_optimizer(
    name = "gwo",
    type = "metaheuristic",
    parameters = list(
      pop_size = as.integer(pop_size),
      max_iter = as.integer(max_iter),
      a_start = a_start,
      a_end = a_end
    )
  )
}

#' Whale Optimization Algorithm Optimizer
#'
#' Creates a Whale Optimization Algorithm optimizer object for continuous
#' optimization problems.
#'
#' @param pop_size Population size.
#' @param max_iter Maximum number of iterations.
#' @param a_start Initial value of the control parameter `a`.
#' @param a_end Final value of the control parameter `a`.
#' @param b Constant defining the spiral shape in the bubble-net mechanism.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Mirjalili, S., and Lewis, A. (2016). The Whale Optimization Algorithm.
#' Advances in Engineering Software, 95, 51--67.
#' doi:10.1016/j.advengsoft.2016.01.008
#' @export
#'
#' @examples
#' optimizer_woa()
optimizer_woa <- function(pop_size = 30,
                          max_iter = 100,
                          a_start = 2,
                          a_end = 0,
                          b = 1) {
  if (!is.numeric(pop_size) ||
      length(pop_size) != 1L ||
      pop_size <= 1 ||
      pop_size != as.integer(pop_size)) {
    stop("'pop_size' must be a single integer greater than 1.", call. = FALSE)
  }

  if (!is.numeric(max_iter) ||
      length(max_iter) != 1L ||
      max_iter <= 0 ||
      max_iter != as.integer(max_iter)) {
    stop("'max_iter' must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(a_start) || length(a_start) != 1L || a_start < 0) {
    stop("'a_start' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.numeric(a_end) || length(a_end) != 1L || a_end < 0) {
    stop("'a_end' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.numeric(b) || length(b) != 1L || b <= 0) {
    stop("'b' must be a single positive numeric value.", call. = FALSE)
  }

  new_optimizer(
    name = "woa",
    type = "metaheuristic",
    parameters = list(
      pop_size = as.integer(pop_size),
      max_iter = as.integer(max_iter),
      a_start = a_start,
      a_end = a_end,
      b = b
    )
  )
}

#' Teaching-Learning-Based Optimization Optimizer
#'
#' Creates a Teaching-Learning-Based Optimization optimizer object for
#' continuous optimization problems.
#'
#' @param pop_size Population size.
#' @param max_iter Maximum number of iterations.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Rao, R. V., Savsani, V. J., and Vakharia, D. P. (2011).
#' Teaching-Learning-Based Optimization: A Novel Method for Constrained
#' Mechanical Design Optimization Problems. Computer-Aided Design, 43,
#' 303--315. doi:10.1016/j.cad.2010.12.015
#' @export
#'
#' @examples
#' optimizer_tlbo()
optimizer_tlbo <- function(pop_size = 30,
                           max_iter = 100) {
  if (!is.numeric(pop_size) ||
      length(pop_size) != 1L ||
      pop_size <= 1 ||
      pop_size != as.integer(pop_size)) {
    stop("'pop_size' must be a single integer greater than 1.", call. = FALSE)
  }

  if (!is.numeric(max_iter) ||
      length(max_iter) != 1L ||
      max_iter <= 0 ||
      max_iter != as.integer(max_iter)) {
    stop("'max_iter' must be a single positive integer.", call. = FALSE)
  }

  new_optimizer(
    name = "tlbo",
    type = "metaheuristic",
    parameters = list(
      pop_size = as.integer(pop_size),
      max_iter = as.integer(max_iter)
    )
  )
}

#' Secretary Bird Optimization Algorithm Optimizer
#'
#' Creates a Secretary Bird Optimization Algorithm optimizer object for
#' continuous optimization problems.
#'
#' @param pop_size Population size.
#' @param max_iter Maximum number of iterations.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Fu, Y., Liu, D., Chen, J., and He, L. (2024). Secretary Bird Optimization
#' Algorithm: A New Metaheuristic for Solving Global Optimization Problems.
#' Artificial Intelligence Review, 57, 123.
#' doi:10.1007/s10462-024-10729-y
#'
#' Dilber, B., and Ozdemir, A. F. (2026). A novel approach to training
#' feed-forward multi-layer perceptrons with recently proposed secretary bird
#' optimization algorithm. Neural Computing and Applications, 38(5).
#' doi:10.1007/s00521-026-11874-x
#' @export
#'
#' @examples
#' optimizer_sboa()
optimizer_sboa <- function(pop_size = 30,
                           max_iter = 100) {
  if (!is.numeric(pop_size) ||
      length(pop_size) != 1L ||
      pop_size <= 1 ||
      pop_size != as.integer(pop_size)) {
    stop("'pop_size' must be a single integer greater than 1.", call. = FALSE)
  }

  if (!is.numeric(max_iter) ||
      length(max_iter) != 1L ||
      max_iter <= 0 ||
      max_iter != as.integer(max_iter)) {
    stop("'max_iter' must be a single positive integer.", call. = FALSE)
  }

  new_optimizer(
    name = "sboa",
    type = "metaheuristic",
    parameters = list(
      pop_size = as.integer(pop_size),
      max_iter = as.integer(max_iter)
    )
  )
}


#' Stochastic Gradient Descent Optimizer
#'
#' Creates a stochastic gradient descent optimizer object.
#'
#' @param learning_rate Learning rate.
#' @param epochs Number of training epochs.
#' @param batch_size Mini-batch size. If `NULL`, full-batch training is used.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Robbins, H., and Monro, S. (1951). A Stochastic Approximation Method.
#' The Annals of Mathematical Statistics, 22(3), 400--407.
#' doi:10.1214/aoms/1177729586
#' @export
#'
#' @examples
#' optimizer_sgd()
optimizer_sgd <- function(learning_rate = 0.01,
                          epochs = 100,
                          batch_size = NULL) {
  if (!is.numeric(learning_rate) || length(learning_rate) != 1L || learning_rate <= 0) {
    stop("'learning_rate' must be a single positive numeric value.", call. = FALSE)
  }

  if (!is.numeric(epochs) || length(epochs) != 1L || epochs <= 0 || epochs != as.integer(epochs)) {
    stop("'epochs' must be a single positive integer.", call. = FALSE)
  }

  if (!is.null(batch_size)) {
    if (!is.numeric(batch_size) || length(batch_size) != 1L || batch_size <= 0 || batch_size != as.integer(batch_size)) {
      stop("'batch_size' must be NULL or a single positive integer.", call. = FALSE)
    }
    batch_size <- as.integer(batch_size)
  }

  new_optimizer(
    name = "sgd",
    type = "gradient",
    parameters = list(
      learning_rate = learning_rate,
      epochs = as.integer(epochs),
      batch_size = batch_size
    )
  )
}


#' Adam Optimizer
#'
#' Creates an Adam optimizer object.
#'
#' @param learning_rate Learning rate.
#' @param beta1 Exponential decay rate for the first moment estimates.
#' @param beta2 Exponential decay rate for the second moment estimates.
#' @param epsilon Small positive constant for numerical stability.
#' @param epochs Number of training epochs.
#' @param batch_size Mini-batch size. If `NULL`, full-batch training is used.
#'
#' @return An object of class `"met_optimizer"`.
#' @references
#' Kingma, D. P., and Ba, J. (2015). Adam: A Method for Stochastic
#' Optimization. International Conference on Learning Representations.
#' @export
#'
#' @examples
#' optimizer_adam()
optimizer_adam <- function(learning_rate = 0.001,
                           beta1 = 0.9,
                           beta2 = 0.999,
                           epsilon = 1e-8,
                           epochs = 100,
                           batch_size = NULL) {
  if (!is.numeric(learning_rate) || length(learning_rate) != 1L || learning_rate <= 0) {
    stop("'learning_rate' must be a single positive numeric value.", call. = FALSE)
  }

  if (!is.numeric(beta1) || length(beta1) != 1L || beta1 < 0 || beta1 >= 1) {
    stop("'beta1' must be a single numeric value in [0, 1).", call. = FALSE)
  }

  if (!is.numeric(beta2) || length(beta2) != 1L || beta2 < 0 || beta2 >= 1) {
    stop("'beta2' must be a single numeric value in [0, 1).", call. = FALSE)
  }

  if (!is.numeric(epsilon) || length(epsilon) != 1L || epsilon <= 0) {
    stop("'epsilon' must be a single positive numeric value.", call. = FALSE)
  }

  if (!is.numeric(epochs) || length(epochs) != 1L || epochs <= 0 || epochs != as.integer(epochs)) {
    stop("'epochs' must be a single positive integer.", call. = FALSE)
  }

  if (!is.null(batch_size)) {
    if (!is.numeric(batch_size) || length(batch_size) != 1L || batch_size <= 0 || batch_size != as.integer(batch_size)) {
      stop("'batch_size' must be NULL or a single positive integer.", call. = FALSE)
    }
    batch_size <- as.integer(batch_size)
  }

  new_optimizer(
    name = "adam",
    type = "gradient",
    parameters = list(
      learning_rate = learning_rate,
      beta1 = beta1,
      beta2 = beta2,
      epsilon = epsilon,
      epochs = as.integer(epochs),
      batch_size = batch_size
    )
  )
}


#' Hybrid Optimizer
#'
#' Creates a hybrid optimizer object by combining a global optimizer and a
#' local optimizer.
#'
#' @param global A metaheuristic optimizer object.
#' @param local A gradient-based optimizer object.
#' @param strategy Hybrid training strategy. Currently `"sequential"` is used
#' as the default strategy.
#'
#' @return An object of class `"met_optimizer"`.
#' @export
#'
#' @examples
#' optimizer_hybrid(
#'   global = optimizer_pso(max_iter = 10),
#'   local = optimizer_adam(epochs = 10)
#' )
optimizer_hybrid <- function(global = optimizer_pso(),
                             local = optimizer_adam(),
                             strategy = "sequential") {
  if (!is_optimizer(global)) {
    stop("'global' must be a metANN optimizer object.", call. = FALSE)
  }

  if (!is_optimizer(local)) {
    stop("'local' must be a metANN optimizer object.", call. = FALSE)
  }

  if (global$type != "metaheuristic") {
    stop("'global' must be a metaheuristic optimizer.", call. = FALSE)
  }

  if (local$type != "gradient") {
    stop("'local' must be a gradient-based optimizer.", call. = FALSE)
  }

  if (!is.character(strategy) || length(strategy) != 1L) {
    stop("'strategy' must be a single character string.", call. = FALSE)
  }

  if (!strategy %in% c("sequential")) {
    stop("Currently, only strategy = 'sequential' is supported.", call. = FALSE)
  }

  new_optimizer(
    name = "hybrid",
    type = "hybrid",
    parameters = list(
      global = global,
      local = local,
      strategy = strategy
    )
  )
}


#' Convert Character Input to an Optimizer Object
#'
#' Converts a character string such as `"pso"` into the corresponding optimizer
#' object.
#'
#' @param optimizer A character string or an object of class `"met_optimizer"`.
#'
#' @return An object of class `"met_optimizer"`.
#' @export
#'
#' @examples
#' as_optimizer("pso")
#' as_optimizer(optimizer_adam())
as_optimizer <- function(optimizer) {
  if (is_optimizer(optimizer)) {
    return(optimizer)
  }

  if (!is.character(optimizer) || length(optimizer) != 1L) {
    stop(
      "'optimizer' must be a single character string or a met_optimizer object.",
      call. = FALSE
    )
  }

  optimizer <- tolower(optimizer)

  switch(
    optimizer,
    pso = optimizer_pso(),
    de = optimizer_de(),
    ga = optimizer_ga(),
    abc = optimizer_abc(),
    gwo = optimizer_gwo(),
    woa = optimizer_woa(),
    tlbo = optimizer_tlbo(),
    sboa = optimizer_sboa(),
    sgd = optimizer_sgd(),
    adam = optimizer_adam(),
    stop("Unknown optimizer: '", optimizer, "'.", call. = FALSE)
  )
}


#' Print a metANN Optimizer
#'
#' @param x A metANN optimizer object.
#' @param ... Additional arguments, currently unused.
#'
#' @return The input object invisibly.
#' @export
print.met_optimizer <- function(x, ...) {
  cat("metANN optimizer\n")
  cat("  Name :", x$name, "\n")
  cat("  Type :", x$type, "\n")

  if (length(x$parameters) > 0L) {
    cat("  Parameters:\n")
    for (nm in names(x$parameters)) {
      value <- x$parameters[[nm]]

      if (is_optimizer(value)) {
        cat("    ", nm, ": ", value$name, " (", value$type, ")\n", sep = "")
      } else {
        cat("    ", nm, ": ", paste(value, collapse = ", "), "\n", sep = "")
      }
    }
  }

  invisible(x)
}
