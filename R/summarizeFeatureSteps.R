#' Creates a data.frame that summarizes the feature steps.
#'
#' @param asscenario [\code{\link{ASScenario}}]\cr
#'   Algorithm selection scenario.
#' @return [\code{data.frame}].
#' @export
summarizeFeatureSteps= function(asscenario) {
  assertClass(asscenario, "ASScenario")
  rs = asscenario$feature.runstatus
  # First 2 cols are instance id and rep
  # create prop table for the subsequent step columns over their levels
  n = ncol(rs)
  res = sapply(rs[, 3:n, drop = FALSE], function(x)
    100 * as.numeric(prop.table(table(x))))
  # add size of step = number of features in it
  size = sapply(asscenario$desc$feature_steps, length)
  res = as.data.frame(setColNames(t(res), levels(rs[, 3])))
  res = cbind(size = size, res)
  fc = asscenario$feature.costs
  if (!is.null(fc)) {
    res2 = t(sapply(fc[, 3:n, drop = FALSE], function(x) c(
      cost_min = min(x, na.rm = TRUE),
      cost_mean = mean(x, na.rm = TRUE),
      cost_max = max(x, na.rm = TRUE),
      cost_na = mean(is.na(x))
    )))
    res = cbind(res, res2)
  }
  
  if (!is.null(asscenario$algorithm.feature.runstatus)) {
    algo.rs = asscenario$algorithm.feature.runstatus
    # First 2 cols are algorithm id and rep
    # create prop table for the subsequent step columns over their levels
    n = ncol(algo.rs)
    algo.res = sapply(algo.rs[, 3:n, drop = FALSE], function(x)
      100 * as.numeric(prop.table(table(x))))
    # add size of step = number of features in it
    size = sapply(asscenario$desc$algorithm_feature_steps, length)
    algo.res = as.data.frame(setColNames(t(algo.res), levels(algo.rs[, 3])))
    algo.res = cbind(size = size, algo.res)
    fc = asscenario$algorithm.feature.costs
    if (!is.null(fc)) {
      algo.res2 = t(sapply(fc[, 3:n, drop = FALSE], function(x) c(
        cost_min = min(x, na.rm = TRUE),
        cost_mean = mean(x, na.rm = TRUE),
        cost_max = max(x, na.rm = TRUE),
        cost_na = mean(is.na(x))
      )))
      algo.res = cbind(algo.res, algo.res2)
    }
    res.names = rownames(res)
    algo.res.names = rownames(algo.res)
    res = rbind.fill(res, algo.res)
    rownames(res) = c(res.names, algo.res.names)
  }
  return(res)
}
