# @export
convertToMlr = function(asscenario, measure, feature.steps, add.feature.costs = TRUE) {
  ch = convertToCheck(asscenario, measure, feature.steps, add.feature.costs)
  desc = asscenario$desc
  measure = ch$measure; feature.steps = ch$feature.steps

  ### scenario
  feats = convertFeats(asscenario, with.instance.id = FALSE)
  cp = convertPerf(asscenario, measure = measure, feature.steps = feature.steps,
    add.feature.costs = add.feature.costs, with.instance.id = FALSE)
  costs = if (desc$maximize[[measure]])
    -cp$perf
  else
    cp$perf
  mlr.scenario = makeCostSensTask(id = desc$scenario_id, data = feats, costs = as.matrix(costs))

  ### CV
  cv.splits = asscenario$cv.splits
  folds = max(cv.splits$fold)
  n = length(unique(cv.splits$instance_id))
  rdesc = makeResampleDesc("CV", iters = folds)
  rin = makeResampleInstance(rdesc, size = n)
  all = 1:n
  # these are sorted, like our data
  all.ids = getInstanceNames(asscenario)
  # get position into sorted ids
  idsToIndices = function(ids) match(ids, all.ids)
  for (i in 1:folds) {
    s = cv.splits[cv.splits$fold == i, , drop = FALSE]
    test.ids = s$instance_id
    train.ids = setdiff(cv.splits$instance_id, test.ids)
    rin$train.inds[[i]] = idsToIndices(train.ids)
    rin$test.inds[[i]] = idsToIndices(test.ids)
  }
  list(mlr.scenario = mlr.scenario, mlr.rin = rin)
}


