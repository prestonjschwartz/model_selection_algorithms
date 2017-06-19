#' Add an Intercept Column
#'
#' Add an intercept column. Calculating a regression model as an orthogonal
#' projection of the dependent variable onto the columnspace of the matrix
#' requires a column of 1s.
#'
#' @param data matrix containing covariate data.
#' @param threshold value specifying the minimum information requirement; 0 to 1.
#'
#' @return dataframe ready for least squares matrix algebra containing covariates
#' with less than a defined amount of correlation.

data.prep = function(data, threshold) {

  #Modify the independent variable matrix for later matrix algebra.
  #Modify the matrix for easier interpretation.

    data = remove.correlated(data, threshold)
    X[, (ncol(data) + 1)] = 1

    colnames(data)[ncol(data)] = "intercept"
    data = data[c("intercept", setdiff(colnames(data), "intercept"))]

  return(as.matrix(data))

}


