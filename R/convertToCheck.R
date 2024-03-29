convertToCheck = function(asscenario, measure, feature.steps, add.feature.costs) {
  assertClass(asscenario, "ASScenario")
  if (missing(measure)) {
    if (length(asscenario$desc$performance_measures) > 1L) {
        warningf("More than one performance measure, taking the first one.")
    }
    measure = asscenario$desc$performance_measures[1L]
  } else {
    assertString(measure)
  }
  allsteps = names(asscenario$desc$feature_steps)
  algo.allsteps = names(asscenario$desc$algorithm_feature_steps)
  if (missing(feature.steps))
    feature.steps = getDefaultFeatureStepNames(asscenario)
  else
    assertSubset(feature.steps, c(allsteps, algo.allsteps))
  assertFlag(add.feature.costs)
  if (add.feature.costs && is.null(asscenario$feature.costs) && is.null(asscenario$algorithm.feature.costs))
    warningf("Requested to add feature costs, but none in scenario. Adding always 0 feature costs.")
  return(list(measure = measure, feature.steps = feature.steps))
}
