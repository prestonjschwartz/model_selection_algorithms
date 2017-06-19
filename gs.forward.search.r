#' Greedy Forward Search Across a Subset of Models
#'
#' Forward searches across a subset of models.
#'
#' @param X matrix containing covariate data.
#' @param Y vector containing response variable data.
#' @param model list of covariates employed in model.
#'
#' @return Mallow's Cp Score.

greedyForward = function(X, Y, model) {

  #Minimizes Mallow Cp Score across a forward search.
  #Forward Search is executed in greedy fashion.

  score = +Inf
  cov = NULL

  full_model = colnames(X)[-1]
  for(k in full_model[which(!full_model %in% model)]) {
    score_update = mallowCP(X, Y, c(model, k))

      if(score_update < score) {
        cov = k
        score = score_update
      }
  }

  return(list(score, cov))

}


