#' Creates summary data.frame for feature values across all instances.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param type [\code{character(1)}]\cr
#'   Feature type (instance or algorithmic). 
#' @return [\code{data.frame}].
#' @export
summarizeFeatureValues = function(asscenario, type) {
  assertClass(asscenario, "ASScenario")
  assertChoice(type, c("instance", "algorithm"))
  
  if(type == "instance") {
    data = dropNamed(asscenario$feature.values, c("instance_id", "repetition"))
  } else if(type == "algorithm") {
    data = dropNamed(asscenario$algorithm.feature.values, c("algorithm", "repetition"))
  }
  
  s = apply(data, 2, getStatistics)
  return(as.data.frame(t(s)))
}
