context("getProvidedFeatures")

test_that("getProvidedFeatures", {
  expect_equal(getProvidedFeatures(testscenario1, steps = character(0), type = "instance"), character(0))
  expect_equal(getProvidedFeatures(testscenario1, type = "instance"), getFeatureNames(testscenario1, "instance"))

  # test with fake steps
  testscenario = testscenario1

  testscenario$desc$feature_steps = list(s1 = c("a", "b"), s2 = c("b", "c"))
  testscenario$desc$features_deterministic = c("a", "b", "c")
  testscenario$desc$features_stochastic = character(0)
  expect_equal(getProvidedFeatures(testscenario, "s1", "instance"), c("a"))
  expect_equal(getProvidedFeatures(testscenario, "s2", "instance"), c("c"))

  expect_equal(getProvidedFeatures(testscenario6, steps = character(0), type = "algorithm"), character(0))
  expect_equal(getProvidedFeatures(testscenario6, type = "algorithm"), getFeatureNames(testscenario6, "algorithm"))
  expect_equal(getProvidedFeatures(testscenario6, type = "algorithm"), getFeatureNames(testscenario6, type = "algorithm"))

  # test with fake steps
  testscenario = testscenario6

  testscenario$desc$algorithm_feature_steps = list(s1 = c("a", "b"), s2 = c("b", "c"))
  testscenario$desc$algorithm_features_deterministic = c("a", "b", "c")
  testscenario$desc$algorithm_features_stochastic = character(0)
  expect_equal(getProvidedFeatures(testscenario, "s1", "algorithm"), c("a"))
  expect_equal(getProvidedFeatures(testscenario, "s2", "algorithm"), c("c"))
})
