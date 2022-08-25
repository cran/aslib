#' Return features that are useable for a given set of feature steps.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param steps [\code{character}]\cr
#'   Feature steps.
#'   Default are all feature steps.
#' @param type [\code{character(1)}]
#'   Feature type (instance or algorithmic). 
#' @return [\code{character}].
#' @export
getProvidedFeatures = function(asscenario, steps, type) {
  assertClass(asscenario, "ASScenario")
  assertChoice(type, c("instance", "algorithm"))
  
  if (type == "instance") {
    step.col = "feature_steps"
  } else if (type == "algorithm") {
    step.col = "algorithm_feature_steps"
  }
  
  if (missing(steps))
    steps = names(asscenario$desc[[step.col]])
  else
    assertSubset(steps, names(asscenario$desc[[step.col]]))
  allfeats = getFeatureNames(asscenario, type)
  step.list = asscenario$desc[[step.col]]
  allsteps = names(step.list)
  notsteps = setdiff(allsteps, steps)
  notfeatures = unlist(Reduce(union, step.list[notsteps]))
  return(setdiff(allfeats, notfeatures))
}


