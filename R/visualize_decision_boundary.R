#' Visualize Decision Boundary
#'
#' This function visualizes the decision boundary of the perceptron model, which separates the classes based on the learned weights.
#' The decision boundary represents the line (or hyperplane in higher dimensions) where the perceptron model's output changes from one class to another.
#'
#' @param model Trained perceptron model
#' @param X Input numeric matrix of features
#' @param y Target vector
#' @return ggplot object
#' @examples
#' visualize_decision_boundary(model, data$X, data$y)
#' @export

visualize_decision_boundary <- function(model, X, y) {
  df <- data.frame(X1 = X[,1], X2 = X[,2], y = as.factor(y))
  ggplot2::ggplot(df, ggplot2::aes(x = X1, y = X2, color = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = -model$bias / model$weights[2], slope = -model$weights[1] / model$weights[2]) +
    ggplot2::labs(title = "Perceptron Decision Boundary", x = "Feature 1", y = "Feature 2")
}
