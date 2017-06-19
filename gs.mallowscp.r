#' Determine the Mallow Cp Score of a Model
#'
#' Determines training error of a linear model.
#'
#' @param X matrix containing covariate data.
#' @param Y vector containing response variable data.
#' @param covariates variables employed in the regression model.
#'
#' @return Mallow's Cp Score.

mallowCP = function(X, Y, covariates) {

  #Determines model's Mallow Cp Score using noise of model using all covariates.

  score = trainingError(X, Y, covariates) + (2 * length(covariates) * (1/(nrow(X) - length(X))) * trainingError(X, Y, colnames(X)[-1]))

  return(score)

}

