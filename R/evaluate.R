#' Evaluate Model Performance
#'
#' This function evaluates the performance of a classification model by calculating accuracy and generating a confusion matrix.
#'
#' @param y True labels (ground truth) of the test data.
#' @param y_pred Predicted labels generated by the classification model.
#' @return confusion matrix.
#' @examples
#' data <- read.csv("test_data.csv")
#' y_true <- data$true_labels
#' y_pred <- predict_perceptron(model, data$x)
#' conf_matrix <- evaluate(y_true, y_pred)
#' print(conf_matrix)
#' @export

evaluate <- function(y, y_pred) {

  conf_matrix <- caret::confusionMatrix(factor(y_pred), factor(y))

}
