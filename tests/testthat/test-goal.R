### Test 'createGoal'

test_that("test createGoal", {

  goal1 <- createGoal(
    parametersA = "a",
    goalType = "rope", ci=0.95,
    ropeType = "exclude", ropeLower = 0, ropeUpper = 1,
    ropeExclusive = T)

  goal2 <- createGoal(
    parametersA = "a",
    goalType = "rope", ci=0.9,
    ropeType = "include", ropeLower = 0, ropeUpper = 1,
    ropeExclusive = T)

  goal3 <- createGoal(
    parametersA = "a",
    goalType = "rope", ci=0.85,
    ropeType = "include", ropeLower = 0, ropeUpper = 1,
    ropeExclusive = F)

  goal3 <- createGoal(
    parametersA = "a",
    goalType = "precision", ci=0.8,
    precisionWidth=1)

  goal4 <- createGoal(
    parametersA = "a", parametersB="b",
    goalType = "rope", ci=0.75,
    ropeType = "exclude", ropeLower = 0, ropeUpper = 1,
    ropeExclusive = T)

  goal5 <- createGoal(
    parametersA = "a", parametersB="b",
    goalType = "precision", ci=0.7,
    precisionWidth=1)


  expect_true("bayesianssdgoal" %in% class(goal1))
  expect_true("bayesianssdgoal" %in% class(goal2))
  expect_true("bayesianssdgoal" %in% class(goal3))
  expect_true("bayesianssdgoal" %in% class(goal4))
  expect_true("bayesianssdgoal" %in% class(goal5))
})

