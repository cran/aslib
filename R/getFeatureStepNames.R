#' Returns feature step names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param type [\code{character(1)}]
#'   Feature type (instance or algorithmic). 
#' @return [\code{character}].
#' @export
getFeatureStepNames = function(asscenario, type) {
  assertClass(asscenario, "ASScenario")
  assertChoice(type, c("instance", "algorithm"))
  
  if (type == "instance") {
    steps.col = "feature_steps"
  } else if (type == "algorithm") {
    steps.col = "algorithm_feature_steps"
  }
  
  return(names(asscenario$desc[[steps.col]]))
}

