#' Predict using Perceptron Model
#'
#' This function makes predictions using the trained perceptron model.
#'
#' @param model Trained perceptron model
#' @param X Input numeric matrix of features
#' @param threshold Threshold value for prediction (default: 0)
#' @return Predicted labels
#' @examples
#' predict_perceptron(model, data$X)
#' @export

predict_perceptron <- function(model, X, threshold = 0) {
  prediction <- ifelse((model$weights %*% t(X) + model$bias) >= threshold, 1, 0)
}
