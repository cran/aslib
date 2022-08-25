#' Returns feature names of scenario.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param type [\code{character(1)}]
#'   Feature type (instance or algorithmic). 
#' @return [\code{character}].
#' @export
getFeatureNames = function(asscenario, type) {
  assertClass(asscenario, "ASScenario")
  assertChoice(type, c("instance", "algorithm"))
  
  if (type == "instance") {
    deterministic = "features_deterministic"
    stochastic = "features_stochastic"
  } else if (type == "algorithm") {
    deterministic = "algorithm_features_deterministic"
    stochastic = "algorithm_features_stochastic"
  }
  
  c(asscenario$desc[[deterministic]], asscenario$desc[[stochastic]])
}
