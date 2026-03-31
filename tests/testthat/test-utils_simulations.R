### Test 'checkGoal'

test_that("test checkGoal", {


  goal1 <- createGoal(parametersA="aa", parametersB="bb",
                      goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                      ropeExclusive=T, ci=0.95)
  goal2 <- createGoal(parametersA="aa", parametersB="bb",
                      goalType="rope", ropeType="exclude", ropeLower=-1, ropeUpper = 1,
                      ropeExclusive=T, ci=0.95)
  goal3 <- createGoal(parametersA="aa", parametersB="bb",
                      goalType="rope", ropeType="include", ropeLower=-0.5, ropeUpper = 0.5,
                      ropeExclusive=T, ci=0.95)
  goal4 <- createGoal(parametersA="aa", parametersB="bb",
                      goalType="precision", precisionWidth =1,
                      ci=0.95)
  goal5 <- createGoal(parametersA="aa",
                      goalType="precision", precisionWidth =0.5,
                      ropeExclusive=T, ci=0.6)

  set.seed(1234)
  params <- matrix(c(rnorm(4000, 0, 0.1), rnorm(4000, 0.5, 0.1)), 4000, 2)
  dimnames(params) <- list(iterations = NULL,
                           parameters = c("aa","bb"))

  c1 <- checkGoal(goal1, params)
  c2 <- checkGoal(goal2, params)
  c3 <- checkGoal(goal3, params)
  c4 <- checkGoal(goal4, params)
  c5 <- checkGoal(goal5, params)

  expect_equal(c1$goalAchievement, 0.99975, tolerance = 1e-4)
  expect_equal(c1$counterVal, 1)

  expect_equal(c2$goalAchievement, 0.00025, tolerance = 1e-4)
  expect_equal(c2$counterVal, 0)

  expect_equal(c3$goalAchievement, 0.498, tolerance = 1e-4)
  expect_equal(c3$counterVal, 0)

  expect_equal(c4$goalAchievement, 0.6126774, tolerance = 1e-4)
  expect_equal(c4$counterVal, 1)

  expect_equal(c5$goalAchievement, 0.6606021, tolerance = 1e-4)
  expect_equal(c5$counterVal, 1)
})
