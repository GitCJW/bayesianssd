% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/goal.R
\name{plotGoal}
\alias{plotGoal}
\title{Plot goals for a sample size determination}
\usage{
\code{createGoal(
  parametersA = c(),
  parametersB = NULL,
  goalType = c("rope","precision"),
  ci = 0.95,
  ropeType = c("exclude","include"),
  ropeLower = NULL,
  ropeUpper = NULL,
  ropeExclusive = T,
  precisionWidth = NULL
)}
}
\arguments{
\item{goals}{The condition be tested. Use the function \link[ssdbayas]{createGoal} to create such goals.}

\item{dataCreationFunction}{A function that accepts a single parameter, \code{N}, and generates N values in the same manner as the given \code{model}.}

\item{model}{A object of class \code{stanmodel, stanreg or brmsfit}.}

\item{N}{The exemplary sample size.}
}
\value{
A gtable object.
}
\description{
Plots all goals created by \code{createGoal}.
If the additional arguments are provided, the goals are plotted alongside example data.
}
\examples{
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

model <- stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())

goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                   goalType="rope", ropeType="exclude", ropeLower=-0.1, ropeUpper = 0.1,
                   ropeExclusive=T, ci=0.95)
plotGoal(goal, dataCreationFunction, model, N=100)
}
