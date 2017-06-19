#' Remove Repeated Covariate Information
#'
#' Removes repeated covariate information. If a covariate is correlated to
#' another covariate in a multiple regression model, then one of the covariates
#' will be removed to trim the model -- the information of the deleted covariate
#' will be mostly contained in the kept covariate.
#'
#' @param data matrix containing covariate data.
#' @param threshold value specifying the minimum information requirement; 0 to 1.
#'
#' @return dataframe containing covariates that are not correlated beyong a
#' defined threshold value.


remove.correlated = function(data, threshold) {

    data = na.omit(data)

    correlation.cabinet = cor(data)
    correlation.cabinet[upper.tri(correlation.cabinet)] = 0

    diag(correlation.cabinet) = 0

    data = data[,!apply(correlation.cabinet, 2, function(data) any(abs(data) > threshold))]

  return(data)

}


