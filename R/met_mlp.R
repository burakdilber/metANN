#' Train a Feed-Forward Multilayer Perceptron
#'
#' Convenience wrapper around `metann()` for training feed-forward multilayer
#' perceptrons.
#'
#' @param formula Optional model formula.
#' @param data Optional data frame used with `formula`.
#' @param x Optional numeric input matrix or data frame.
#' @param y Optional response vector.
#' @param architecture Optional MLP architecture object.
#' @param hidden_layers Integer vector giving the number of units in each
#' hidden layer.
#' @param activation Activation function for hidden layers.
#' @param output_activation Optional output activation function. If `NULL`, it
#' is selected automatically based on the task.
#' @param task One of `"auto"`, `"regression"`, or `"classification"`.
#' @param optimizer Optimizer object.
#' @param loss Optional loss function. If `NULL`, it is selected automatically
#' based on the task.
#' @param metrics Optional performance metrics. If `NULL`, default metrics are
#' selected automatically based on the task.
#' @param seed Optional random seed.
#' @param verbose Logical. If `TRUE`, progress information is printed.
#'
#' @return An object of class `"metann"`.
#' @references
#' Montana, D. J., and Davis, L. (1989). Training Feedforward Neural Networks
#' Using Genetic Algorithms. Proceedings of the 11th International Joint
#' Conference on Artificial Intelligence, 762--767.
#'
#' Ilonen, J., Kamarainen, J.-K., and Lampinen, J. (2003). Differential
#' Evolution Training Algorithm for Feed-Forward Neural Networks.
#' Neural Processing Letters, 17, 93--105.
#' doi:10.1023/A:1022995128597
#'
#' Karaboga, D., and Ozturk, C. (2009). Neural Networks Training by
#' Artificial Bee Colony Algorithm on Pattern Classification.
#' Neural Network World, 19(3), 279--292.
#'
#' Mirjalili, S. (2015). How Effective is the Grey Wolf Optimizer in Training
#' Multi-Layer Perceptrons. Applied Intelligence, 43, 150--161.
#' doi:10.1007/s10489-014-0645-7
#'
#' Dilber, B., and Ozdemir, A. F. (2026). A novel approach to training
#' feed-forward multi-layer perceptrons with recently proposed secretary bird
#' optimization algorithm. Neural Computing and Applications, 38(5).
#' doi:10.1007/s00521-026-11874-x
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
#' fit
met_mlp <- function(formula = NULL,
                    data = NULL,
                    x = NULL,
                    y = NULL,
                    architecture = NULL,
                    hidden_layers = NULL,
                    activation = "relu",
                    output_activation = NULL,
                    task = c("auto", "regression", "classification"),
                    optimizer = optimizer_pso(),
                    loss = NULL,
                    metrics = NULL,
                    seed = NULL,
                    verbose = TRUE) {
  task <- match.arg(task)

  metann(
    formula = formula,
    data = data,
    x = x,
    y = y,
    architecture = architecture,
    hidden_layers = hidden_layers,
    activation = activation,
    output_activation = output_activation,
    task = task,
    optimizer = optimizer,
    loss = loss,
    metrics = metrics,
    seed = seed,
    verbose = verbose
  )
}
