if(F){

  ################################################################################
  files <- list.files(paste0(getwd(),"/R"))
  files <- paste0(getwd(),"/R/", files[files != "aa_EXAMPLE.R"])
  for(file in files){
    source(file)
  }
  library(rstan)
  library(doFuture)
  library(HDInterval)
  library(gridExtra)
  library(ggplot2)
  library(rstanarm)
  library(testthat)
  ################################################################################
  ###################################### or ######################################
  ################################################################################
  devtools::test()
  usethis::use_version()
  devtools::document()
  devtools::build()
  devtools::install()
  devtools::check()
  library(bayesianssd)
  ################################################################################

  library(bayesianssd)
  library(rstanarm)

  # Assume we want to study the vocal activity of a bird species in two different environments:
  # Urban parks (higher noise pollution)
  # Forest areas (quieter, more natural habitat)
  # We are counting the number of calls per minute from a fixed observation point.
  # In forests, birds are more active, so the mean number of calls per minute is assumed to be 9.
  # In urban parks, noise and disturbances reduce bird vocalizations, leading to a assumed mean of 7 calls per minute.
  # Since bird calls occur as discrete events in a fixed time window, their distribution follows a Poisson process.

  # Function that generates 'N' samples of two groups 'forest' and 'parks'
  # Assumed effect (-sizes) have to be defined within this function
  dataCreationFunction <- function(N){
    group_effects <- c(
      forest = 9, # Expected mean for bird calls in a wild forest
      parks = 7 # Expected mean for bird calls in a park
    )
    treatment <- rep(c("forest", "park"), length.out=N)
    y <- rpois(N, group_effects)

    data <- data.frame(
      treatment = treatment,
      y = y
    )
    data
  }

  # The probabilistic model that will be used for the future real data and used for simulation
  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20),
                              family=poisson(), prior = normal(2,3))


  # We define a condition that must be met: the difference in expected values between the 'forest' and 'park' groups must be non-zero.
  # To ensure this, we establish a Region of Practical Equivalence (ROPE) around zero, which must be excluded by the 95% credible interval of the effect (difference between the two groups).
  # Multiple goals can be created and tested.
  goal <- createGoal(parametersA="treatmentforest", parametersB="treatmentpark",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)

  # Visualize goal(s)
  plot(goal, dataCreationFunction, model, N=100)


  # Checks whether everything is well-defined
  checkSettings(model, dataCreationFunction, 2, goals=list(goal))

  # Uses parallelization
  plan("multisession")

  # Runs the sample size determination / power analysis
  # The minimum required sample size is 2 per group, with a maximum of 100 due to cost considerations.
  # The factorN = 2 parameter ensures an equal number of samples for both groups.
  ssd <- runSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    minN = 60,
    maxN = 100,
    factorN = 5,
    goals = list(goal),
    con = 20,
    iParallel = 20)

  # Plots the final simulation step
  plot(ssd)

  # Prints the estimated sample size required to achieve the desired power of 80% with the specified goal.
  print(ssd)




  ##############################################################################
  seed <- 127 #con=100: 125, 129 interesting; con=10: 127 interesting
  while(T){
    print(seed)
    set.seed(seed)
    ssd <- startSSD(model, dataCreationFunction, 0.8, 100, 2,
                   list(goal), NULL, 10, 20, F)
    ssd_list <- list(ssd)
    index <- 1
    while(ssd$extern$continue){
      ssd <- updateSSD(ssd, dataCreationFunction, F)
      print(plot(ssd))
      index <- index +1
      ssd_list[[index]] <- ssd
    }
    seed <- seed +1
  }

  ##############################################################################
  dataCreationFunction <- function(N){
    group_effects <- c(
      a = 12.7,
      b = 7.8
    )
    strain <- rep(c("a", "b"), length.out=N)
    y <- rnbinom(N, mu=group_effects, size=24)

    data <- data.frame(
      strain = strain,
      entries = y
    )
    data
  }

  prior <- brms::get_prior("entries~-1+strain", data=dataCreationFunction(20),
                           family=brms::negbinomial(link="identity"))
  prior[2,1] <- prior[3,1] <- "lognormal(2.3,1)"
  prior[4,1] <- "exponential(0.1)"

  # The probabilistic model that will be used for the future real data and used for simulation
  model <- brms::brm("entries~-1+strain", data=dataCreationFunction(20),
                     family=brms::negbinomial(link="identity"),
                     prior = prior)


  # We define a condition that must be met: the difference in expected values between the 'forest' and 'park' groups must be non-zero.
  # To ensure this, we establish a Region of Practical Equivalence (ROPE) around zero, which must be excluded by the 95% credible interval of the effect (difference between the two groups).
  # Multiple goals can be created and tested.
  goal <- createGoal(parametersA="b_straina", parametersB="b_strainb",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)

  # Visualize goal(s)
  plot(goal, dataCreationFunction, model, N=100)



  # Uses parallelization
  plan("multisession")

  # Runs the sample size determination / power analysis
  # The minimum required sample size is 2 per group, with a maximum of 100 due to cost considerations.
  # The factorN = 2 parameter ensures an equal number of samples for both groups.
  ssd <- runSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    minN = 2,
    maxN = 30,
    factorN = 1,
    goals = list(goal),
    con = 200,
    iParallel = 20)

  # Plots the final simulation step
  plot(ssd)

  # Prints the estimated sample size required to achieve the desired power of 80% with the specified goal.
  print(ssd)

  power.t.test(delta=4.86, sd=4.49, power=0.8, sig.level=0.05)

}
