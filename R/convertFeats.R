# helper to convert feats to llama or mlr
convertFeats = function(asscenario, feature.steps, with.id, type) {
  assertChoice(type, c("instance", "algorithm"))
  
  if (type == "instance") {
    feature.col = "feature.values"
    step.col = "feature_steps"
    id.col = "instance_id"
    sortBy = c("instance_id", "repetition")
  } else if (type == "algorithm") {
    feature.col = "algorithm.feature.values"
    step.col = "algorithm_feature_steps"
    id.col = "algorithm"
    sortBy = c("algorithm", "repetition")
  }
  
  # simply return NULL if algorithm features are needed but not present
  if (is.null(asscenario[[feature.col]]) && type == "algorithm") {
    return(NULL)
  }
  
  # reduce to inst + rep + allowed features
  # note that feats is ordered by instance, then repetition
  allowed.features = getProvidedFeatures(asscenario, type = type)
  feats = asscenario[[feature.col]]
  feats = feats[, c(sortBy, allowed.features), drop = FALSE]

  # aggregate features, only do this if repeated measurements to save time
  if (type == "instance") {
    if (max(feats$repetition) > 1L) {
      feats = ddply(feats, c("instance_id"), function(d) {
        colMeans(d[, allowed.features, drop = FALSE])
      })
    } else {
      feats$repetition = NULL
    }
  }


  # FIXME:
  # remove constant features, currently we do not count NAs as an extra-level
  # the feature would still be completely constant if we impute just with mean
  # THIS CHANGES IF WE CREATE  DUMMIES FOR NAs LIKE WE SHOULD!
  feats = removeConstScenFeats(feats, id = id.col)
  if (!with.id)
    feats[[id.col]] = NULL


  return(feats)
}
