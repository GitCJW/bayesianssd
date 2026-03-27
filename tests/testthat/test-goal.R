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


  expect_true(inherits(goal1, "bayesianssdgoal"))
  expect_true(inherits(goal2, "bayesianssdgoal"))
  expect_true(inherits(goal3, "bayesianssdgoal"))
  expect_true(inherits(goal4, "bayesianssdgoal"))
  expect_true(inherits(goal5, "bayesianssdgoal"))

})


### Test 'goalList'

test_that("test goalList", {

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

  gList1 <- goalList(goal1, goal2)

  expect_true(inherits(gList1, "bayesianssdgoallist"))
  expect_error(goalList())
  expect_error(goalList(goal1, list(a=1)))
})


