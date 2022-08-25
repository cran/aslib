#FIXME: what do we do if we have reps here????
#FIXME: add costs to algorithmic features

#' Return whether an instance was presolved and which step did it.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @param feature.steps [\code{character}]\cr
#'   Which feature steps are allowed?
#'   Default is all steps.
#' @param type [\code{character(1)}]\cr
#'   Feature type (instance or algorithmic). 
#' @return [\code{list}]. Below, \code{n} is the number of instances. All following object are ordered by \dQuote{instance_id}.
#'   \item{is.presolved [logical(n)]}{Was instance presolved? Named by instance ids.}
#'   \item{solve.steps [character(n)]}{Which step solved it? NA if no step did it. Named by instance ids.}
#'   \item{costs [numeric(n)]}{Feature costs for using the steps. Named by instance ids. NULL if no costs are present.}
#' @export
getCostsAndPresolvedStatus = function(asscenario, feature.steps, type) {
  assertClass(asscenario, "ASScenario")
  assertChoice(type, c("instance", "algorithm"))
  
  if (type == "instance") {
    status.col = "feature.runstatus"
    cost.col = "feature.costs"
    step.col = "feature_steps"
    id.col = "instance_id"
  } else if (type == "algorithm") {
    status.col = "algorithm.feature.runstatus"
    cost.col = "algorithm.feature.costs"
    step.col = "algorithm_feature_steps"
    id.col = "algorithm"
  }
  
  allsteps = getFeatureStepNames(asscenario, type = type)
  if (missing(feature.steps))
    feature.steps = allsteps
  else {
    feature.steps = intersect(feature.steps, names(asscenario$desc[[step.col]]))
    assertSubset(feature.steps, allsteps)
  }

  frs = asscenario[[status.col]]
  #FIXME:
  stopifnot(max(frs$repetition) == 1L)

  # note that frs and costs are ordered by instance_id
  iids = frs[[id.col]]
  # reduce to allowed feature steps
  frs = frs[, feature.steps, drop = FALSE]
  isps = frs == "presolved"
  ps = apply(isps, 1, any)
  # steps are in correct order of execution after parseASScenario
  # now get the first step that solves as index, or NA
  solve.steps1 = apply(isps, 1, function(x)
    ifelse(any(x), which.first(x), NA_integer_))
  # set NA to nr of steps (means we use all in costs)
  solve.steps2 = ifelse(is.na(solve.steps1), length(feature.steps), solve.steps1)
  if (!is.null(asscenario[[cost.col]])) {
    costs = asscenario[[cost.col]][, feature.steps, drop = FALSE]
    # add up costs to solving step (or add up all), removing any NAs
    costs = sapply(seq_row(costs), function(i) sum(costs[i, 1:solve.steps2[i]], na.rm = TRUE))
    costs = setNames(costs, iids)
  } else {
    costs = NULL
  }
  list(
    is.presolved = setNames(ps, iids),
    solve.steps = setNames(feature.steps[solve.steps1], iids),
    costs = costs
  )
}
