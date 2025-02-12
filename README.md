# bayesianssd

Perform sample size determination (power analysis) through simulations for a probabilistic model using packages such as rstan, rstanarm, or brms.


## Installation

To install the latest development version from GitHub:

```r
  library(devtools)
  install_github('GitCJW/bayesianssd')
```

## Example

```r

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
    environment <- rep(c("forest", "park"), length.out=N)
    y <- rpois(N, group_effects)

    data <- data.frame(
      environment = environment,
      y = y
    )
    data
  }

  # The probabilistic model that will be used for the future real data and used for simulation
  model <- rstanarm::stan_glm("y~-1+environment", data=dataCreationFunction(20),
                              family=poisson(), prior = normal(2,3))


  # We define a condition that must be met: the difference in expected values between the 'forest' and 'park' groups must be non-zero.
  # To ensure this, we establish a Region of Practical Equivalence (ROPE) around zero, which must be excluded by the 95% credible interval of the effect (difference between the two groups).
  # Multiple goals can be created and tested.
  goal <- createGoal(parametersA="environmentforest", parametersB="environmentpark",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)

  # Visualize goal(s)
  plotGoal(goal, dataCreationFunction, model, N=100)


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
    minN = 2,
    maxN = 100,
    factorN = 2,
    goals = list(goal),
    con = 200,
    iParallel = 20)

  # Plots the final simulation step
  plotResults(ssd)

  # Prints the estimated sample size required to achieve the desired power of 80% with the specified goal.
  printSSD(ssd)


```
