#' Perceptron Algorithm
#'
#' This function implements the perceptron algorithm.
#'
#' @param X Input numeric matrix of features. Where each row represents a sample and each column represents a feature.
#' @param y Target binary numeric vector. Represents the binary target labels (0 or 1) corresponding to the samples in X.
#' @param max_iter Maximum number of epochs (iterations) for training the perceptron model.
#' @param eta Learning rate (default: 0.01).
#' @param acc_th Threshold for accuracy to break the training loop (default: 0.95).
#' @param threshold Threshold value for prediction (default: 0).
#' @return Trained perceptron model.
#' @examples
#' data(synthetic_dataset)
#' train_perceptron(synthetic_dataset[, c("X1", "X2")], synthetic_dataset$y)
#' @export

train_perceptron <- function(X, y, max_iter = 100, eta = 0.01, acc_th = 0.95, threshold = 0) {
  n_row <- nrow(X)
  n_col <- ncol(X)
  weight <- runif(n_col + 1, -1, 1)  # Generate random weights between -1 and 1, including bias
  errors <- rep(0, max_iter)
  total_sqr_error <- 0

  for (epoch in 1:max_iter) {
    # reshuffle the order of the data points for each epoch
    idx <- sample(1:n_row)

    for (i in idx) {
      y_pred <- weight[1] + sum(weight[-1] * as.numeric(X[i, ]))
      y_pred <- ifelse(y_pred >= threshold , 1, 0)  # Predict 1 if >= 0, otherwise 0

      # Calculate error
      error <- y[i] - y_pred

      # Update weights
      weight[1] <- weight[1] + eta * error * 1.0  # Update bias weight

      for (j in 1:n_col) {
        weight[j + 1] <- weight[j + 1] + eta * error * X[i, j]  # Update feature weights
      }
    }

    # Calculate predictions and accuracy
    y_pred <- weight[1] + weight[2] * X[, 1] + weight[3] * X[, 2]
    y_pred <- ifelse(y_pred >= threshold, 1, 0)
    accuracy <- mean(y_pred == y)

    print(paste("Epoch", epoch, "Accuracy:", accuracy))

    # Check if accuracy meets threshold
    if (accuracy >= acc_th) {
      break
    }
  }

  model <- list(weights = weight[-1], bias = weight[1])
  return(model)
}
