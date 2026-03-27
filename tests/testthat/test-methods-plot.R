

### Test 'plot'

test_that("test plot.bayesianssd", {

  powerDesired <- 0.8
  resultsSSD <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,80),
    power = c(1, 0.7, 0.81, 0.8125),
    certainty = c(F,F,F,F),
    tendency = c(1, -1, 0, 1)
  )

  resultsPowerBinomial <- data.frame(
    N = c(99, 39, 48, 51),
    powerMean = c(0.9166667, 0.6875000, 0.7934783, 0.8048780),
    powerLow = c(0.8389251, 0.5937902, 0.7450150, 0.7547323),
    powerHigh = c(0.9955778, 0.7824342, 0.8425751, 0.8557615)
  )

  furtherArgs <- list(
    acceptHDIwidth = c(upper = 0.13)
  )

  ssd <- list(
    intern = list(
      powerDesired = powerDesired,
      resultsSSD = resultsSSD,
      resultsPowerBinomial = resultsPowerBinomial,
      furtherArgs = furtherArgs
    )
  )
  class(ssd) <- c("bayesianssd", class(ssd))
  gg <- plot(ssd)
  expect_true("ggplot" %in% class(gg))
})


test_that("test plot.bayesianssdgoal ", {

  testthat::skip_if_not_installed("rstanarm")

  goal1 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                      ropeExclusive=T, ci=0.95)
  goal2 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="exclude", ropeLower=-1, ropeUpper = 1,
                      ropeExclusive=T, ci=0.95)
  goal3 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="include", ropeLower=-0.5, ropeUpper = 0.5,
                      ropeExclusive=T, ci=0.95)
  goal4 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="precision", precisionWidth =1,
                      ropeExclusive=T, ci=0.95)
  goal5 <- createGoal(parametersA="treatmentcontrol",
                      goalType="precision", precisionWidth =0.5,
                      ropeExclusive=T, ci=0.6)

  dataCreationFunction <- function(N){
    group_effects <- c(
      control = 9,
      drug = 7
    )
    treatment <- rep(c("control", "drug"), length.out=N)
    y <- rpois(N, group_effects)

    data <- data.frame(
      treatment = treatment,
      y = y
    )
    data
  }

  suppressWarnings({
    model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20),
                                family=poisson(), iter=1000, chains=2, refresh=0, seed=123)
  })


  goals <- goalList(goal1, goal2, goal3, goal4, goal5)
  N <- 100
  ggs <- plot(goals, dataCreationFunction, model, N)

  expect_true(inherits(ggs, "gtable"))
})


test_that("test plot.bayesianssdgoal simple", {

  goal1 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                      ropeExclusive=T, ci=0.95)
  goal2 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="exclude", ropeLower=-1, ropeUpper = 1,
                      ropeExclusive=T, ci=0.95)
  goal3 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="include", ropeLower=-0.5, ropeUpper = 0.5,
                      ropeExclusive=T, ci=0.95)
  goal4 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="precision", precisionWidth =1,
                      ropeExclusive=T, ci=0.95)
  goal5 <- createGoal(parametersA="treatmentcontrol",
                      goalType="precision", precisionWidth =0.5,
                      ropeExclusive=T, ci=0.6)

  goals <- goalList(goal1, goal2, goal3, goal4, goal5)
  ggs <- plot(goals)

  expect_true(inherits(ggs, "gtable"))
})


test_that("test plot.bayesianssdgoallist", {

  goal1 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                      ropeExclusive=T, ci=0.95)
  goal2 <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                      goalType="rope", ropeType="exclude", ropeLower=-1, ropeUpper = 1,
                      ropeExclusive=T, ci=0.95)

  goals <- goalList(goal1, goal2)
  ggs <- plot(goals)
  expect_true(inherits(ggs, "gtable"))
})
