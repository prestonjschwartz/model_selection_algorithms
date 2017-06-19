#' Greedy Forward Search Across a Subset of Models using Mallow Cp Score
#'
#' Forward searches across a subset of models to determine best model based on Mallow Cp.
#'
#' @param X matrix containing covariate data.
#' @param Y vector containing response variable data.
#' @param threshold value specifying the minimum information requirement; 0 to 1.
#'
#' @return Best model and associated covariates.

stepwiseMallowCp = function(X_data, Y_data, threshold) {

    X = data.prep(X_data, threshold)
    Y = na.omit(Y_data)

  model = c()
  past_score = +Inf
  resid = greedyForward(X, Y, model)

  score = resid[[1]]
    cov = resid[[2]]

    while(score <= past_score) {
        past_score = score
        model = c(model, cov)
        resid = greedyForward(X, Y, model)

      score = resid[[1]]
        cov = resid[[2]]
    }

    col.space = as.matrix(cbind(rep(1, nrow(X)), X[ ,model]))
    orth.projection = data.frame(solve(t(col.space) %*% col.space) %*% t(col.space) %*% Y)

  colnames(orth.projection)[ncol(orth.projection)] = "coefficient"
  rownames(orth.projection)[1] = "intercept"

  print(orth.projection)

}
