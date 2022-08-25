context("runLlamaModels")
aggrShort = function(job, res) {
  return(list(succ = res$succ, par10 = res$par10, mcp = res$mcp))
}

test_that("runLlamaModels", {
  skip_on_cran()
  unlink("run_llama_models", recursive = TRUE)
  fs = setNames(list(getFeatureStepNames(testscenario1, "instance")), testscenario1$desc$scenario_id)
  reg = runLlamaModels(list(testscenario1), feature.steps.list = fs,
    baselines = "vbs",
    learners = list(makeLearner("classif.rpart"),
                 makeLearner("regr.rpart"),
                 makeLearner("cluster.SimpleKMeans")),
    par.sets = list(ParamHelpers::makeParamSet(), ParamHelpers::makeParamSet(), ParamHelpers::makeParamSet())
  )
  submitJobs(reg = reg)
  waitForJobs(reg = reg)
  errors = getErrorMessages(reg = reg)
  expect_true(sum(errors$error) == 0)
  res = summarizeLlamaExps(reg, fun = aggrShort)
  expect_true(is.data.frame(res) && nrow(res) == 4L)
  expect_true(abs(res[1,]$par10 - 8337.099) < .1)

  resLong = reduceResultsList(reg = reg, ids = findDone())
  expect_equal(length(resLong), 4)
  expect_true(is.data.frame(resLong[[2]]$predictions))
})

test_that("runLlamaModels w/ costs", {
  skip_on_cran()
  unlink("run_llama_models", recursive = TRUE)
  fs = setNames(list(getFeatureStepNames(testscenario2, "instance")), testscenario2$desc$scenario_id)
  reg = runLlamaModels(list(testscenario2), feature.steps.list = fs,
    baselines = "vbs",
    learners = list(makeLearner("classif.OneR")),
    par.sets = list(ParamHelpers::makeParamSet())
  )
  submitJobs(reg = reg)
  waitForJobs(reg = reg)
  res = summarizeLlamaExps(reg = reg, fun = aggrShort)
  expect_true(is.data.frame(res) && nrow(res) == 2L)
  expect_true(abs(res[1,]$par10 - 2221.497) < .1)
  # greater than without costs
  expect_true(res[2,]$par10 > 3274.425)
})
