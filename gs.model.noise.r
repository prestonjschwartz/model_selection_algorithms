#' Determine the Training Error of a Linear Model
#'
#' Determines training error of a linear model.
#'
#' @param X matrix containing covariate data.
#' @param Y vector containing response variable data.
#' @param covariates variables employed in the regression model.
#'
#' @return noise of regression model.

trainingError = function(X, Y, covariates) {

    thin = X[ ,covariates]
    thin = as.matrix(thin)

    betas = (solve(t(thin) %*% thin) %*% t(thin) %*% Y)
    noise = ((thin %*% betas) - Y)

      to_return = 0
      for(i in 1:nrow(X)) {
        to_return = to_return + noise[i]^2
      }

  return(to_return)

}

