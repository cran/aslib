context("getCostsAndPresolvedStatus")

test_that("getCostsAndPresolvedStatus", {
  ps = getCostsAndPresolvedStatus(testscenario1, type = "instance")
  insts = getInstanceNames(testscenario1)
  n = length(insts)
  expect_is(ps$is.presolved, "logical")
  expect_equal(length(ps$is.presolved), n)
  expect_equal(names(ps$is.presolved), insts)
  expect_is(ps$solve.step, "character")
  expect_equal(length(ps$solve.step), n)
  expect_equal(names(ps$solve.step), insts)
  expect_equal(ps$costs, NULL)

  ps = getCostsAndPresolvedStatus(testscenario2, type = "instance")
  insts = getInstanceNames(testscenario2)
  n = length(insts)
  expect_is(ps$is.presolved, "logical")
  expect_equal(length(ps$is.presolved), n)
  expect_equal(names(ps$is.presolved), insts)
  expect_is(ps$solve.step, "character")
  expect_equal(length(ps$solve.step), n)
  expect_equal(names(ps$solve.step), insts)
  expect_is(ps$costs, "numeric")
  expect_equal(length(ps$costs), n)
  expect_equal(names(ps$costs), insts)
  
  ps = getCostsAndPresolvedStatus(testscenario6, type = "instance")
  insts = getInstanceNames(testscenario6)
  n = length(insts)
  expect_is(ps$is.presolved, "logical")
  expect_equal(length(ps$is.presolved), n)
  expect_equal(names(ps$is.presolved), insts)
  expect_is(ps$solve.step, "character")
  expect_equal(length(ps$solve.step), n)
  expect_equal(names(ps$solve.step), insts)
  expect_null(ps$costs, NULL)
  expect_equal(length(ps$costs), 0L)

  ps = getCostsAndPresolvedStatus(testscenario3, type = "instance")
  expect_equal(ps$is.presolved, c(i1 = FALSE, i2 = TRUE, i3 = FALSE))
  expect_equal(ps$solve.steps, c(i1 = NA, i2 = "s1", i3 = NA))
  expect_equal(ps$costs, c(i1 = 30, i2 = 30, i3 = 10))

  ps = getCostsAndPresolvedStatus(testscenario3, feature.steps = "s1", type = "instance")
  expect_equal(ps$is.presolved, c(i1 = FALSE, i2 = TRUE, i3 = FALSE))
  expect_equal(ps$solve.steps, c(i1 = NA, i2 = "s1", i3 = NA))
  expect_equal(ps$costs, c(i1 = 20, i2 = 30, i3 = 0))

  ps = getCostsAndPresolvedStatus(testscenario3, feature.steps = "s2", type = "instance")
  expect_equal(ps$is.presolved, c(i1 = FALSE, i2 = FALSE, i3 = FALSE))
  expect_equal(ps$solve.steps, c(i1 = NA_character_, i2 = NA_character_, i3 = NA_character_))
  expect_equal(ps$costs, c(i1 = 10, i2 = 20, i3 = 10))
})

