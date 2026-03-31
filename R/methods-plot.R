
#' Plots the sample size determination
#'
#' @param ssd Object returned by \code{runSSD()}.
#' @param plotTriangles Boolean (default: TRUE) indicating whether to draw
#' triangles representing the maximum accepted power width
#' @param theme A list given the colors for the plot.
#' Default: \code{list(main="#56B4E9", current="black", default="darkgrey",
#' mean="white")}.
#'
#' @returns A ggplot of the final sample size determination.
#'
#' @importFrom rlang .data
#' @export
#' @examplesIf rlang::is_installed("rstanarm") && interactive()
#'dataCreationFunction <- function(N){
#'  group_effects <- c(
#'    control = 13,
#'    drug = 7
#'  )
#'  treatment <- rep(c("control", "drug"), length.out=N)
#'  y <- rpois(N, group_effects)
#'
#'  data <- data.frame(
#'    treatment = treatment,
#'    y = y
#'  )
#'  data
#'}
#'
#'model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
#'
#'goal <- createGoal(parametersA = "treatmentcontrol", parametersB = "treatmentdrug",
#'                   goalType = "rope", ropeType = "exclude", ropeLower = 0, ropeUpper = 0, ci = 0.95)
#'
#'checkSettings(model, dataCreationFunction, 2, list(goal))
#'
#'ssd <- runSSD(
#'  model = model,
#'  dataCreationFunction = dataCreationFunction,
#'  powerDesired = 0.8,
#'  minN = 2,
#'  maxN = 20,
#'  factorN = 5,
#'  goals = list(goal),
#'  con = 20,
#'  iParallel = 20)
#'
#'plot(ssd)
plot.bayesianssd <- function (ssd, plotTriangles = TRUE,
                              theme=list(main="#56B4E9", current="black",
                                         default="darkgrey", mean="white")) {

  if(is.null(ssd) || !"bayesianssd" %in% class(ssd))
    stop("'ssd' must be a object of class 'bayesianssd'.")

  powerDesired <- ssd$intern$powerDesired
  resultsSSD <- ssd$intern$resultsSSD
  resultsPowerBinomial <- ssd$intern$resultsPowerBinomial
  furtherArgs <- ssd$intern$furtherArgs
  acceptedWidth <- furtherArgs$acceptHDIwidth

  NLastChecked <- utils::tail(resultsSSD$N, 1)

  onlyCertainAndHigh <- resultsSSD[resultsSSD$certainty==TRUE &
                                     resultsSSD$tendency == 1,]
  bestN <- NULL
  noBlackHighlight <- FALSE
  if (dim(onlyCertainAndHigh)[1] > 0){
    bestN <- min(onlyCertainAndHigh$N)
    lowN <- bestN-1
    if (lowN %in% resultsSSD[resultsSSD$certainty==TRUE &
                             resultsSSD$power<powerDesired,]$N)
      noBlackHighlight <- TRUE
  }

  gg <- ggplot2::ggplot(resultsPowerBinomial) +
    ggplot2::theme_bw() +
    ggplot2::xlab("N") +
    ggplot2::ylab("Power") +
    ggplot2::geom_hline(yintercept=powerDesired, linetype="dashed",
                        color = theme$main)

  for (n in unique(resultsSSD$N)) {
    subset <-  resultsSSD[resultsSSD$N==n,]
    subset <- utils::tail(subset, 1)

    powerBinomN <- resultsPowerBinomial[resultsPowerBinomial$N==n,]
    subset$minPower <- powerBinomN$powerLow
    subset$maxPower <- powerBinomN$powerHigh

    col <- theme$default
    if (!noBlackHighlight && length(NLastChecked) > 0 && n==NLastChecked)
      col <- theme$current
    if (!is.null(bestN) && n==bestN) col <- theme$main

    gg <- gg + ggplot2::geom_pointrange(
      data=subset,
      mapping=ggplot2::aes(
        x=.data$N,
        y=.data$power,
        ymin=.data$minPower,
        ymax=.data$maxPower,
        size=3),
      shape = 21, size=1, linewidth=1,
      color=col, fill=theme$mean)

    if (plotTriangles){
      acceptedWidthUpper <- mean(c(subset$minPower, subset$maxPower)) + acceptedWidth/2
      acceptedWidthLower <- mean(c(subset$minPower, subset$maxPower)) - acceptedWidth/2

      acceptedWidthData <- data.frame(N=n, acc=c(acceptedWidthUpper, acceptedWidthLower),
                                      type=c("upper","lower"))

      gg <- gg +
        ggplot2::geom_point(data=acceptedWidthData,
                            mapping=ggplot2::aes(x=.data$N,
                                                 y=.data$acc, shape=c(25,24)),
                            size=2, fill=theme$main, color=theme$main, alpha=0.5)

    }

  }
  gg <- gg + ggplot2::scale_shape_identity()
  gg
}


#' @title
#' Plot goals for a sample size determination
#'
#'@description
#' Plots a single goal created by \code{createGoal}.
#' If the additional arguments are provided, the goals are plotted alongside example data.
#'
#' @param x The condition be tested. Use the function
#' \link[bayesianssd]{createGoal} to create such goals.
#' @param dataCreationFunction A function that accepts a single parameter,
#' \code{N}, and generates N values in the same manner as the given \code{model}.
#' @param model A object of class \code{stanmodel, stanreg or brmsfit}.
#' @param N The exemplary sample size.
#'
#' @returns An object of class \code{ggplot}.
#' @method plot bayesianssdgoal
#' @export
#'
#' @examples
#' goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
#'                    goalType="rope", ropeType="include", ropeLower=-2, ropeUpper=2,
#'                    ropeExclusive=TRUE, ci=0.95)
#' plot(goal)
#'
# goal_prec <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
# goalType="precision", precisionWidth=2, ci=0.95)
# plot(goal_prec)
#'
#' @examplesIf rlang::is_installed("rstanarm")
#' dataCreationFunction <- function(N){
#'   group_effects <- c(
#'     control = 9,
#'     drug = 7
#'   )
#'   treatment <- rep(c("control", "drug"), length.out=N)
#'   y <- rpois(N, group_effects)
#'
#'   data <- data.frame(
#'     treatment = treatment,
#'     y = y
#'   )
#'   data
#' }
#'
#' model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
#'
#' goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
#'                    goalType="rope", ropeType="exclude", ropeLower=-0.1, ropeUpper = 0.1,
#'                    ropeExclusive=TRUE, ci=0.95)
#' plot(goal, dataCreationFunction, model, N=100)
plot.bayesianssdgoal <- function(x, dataCreationFunction=NULL,
                                 model=NULL, N=NULL){
  goal <- singleGoalAsList(x)

  verifySSDInputs.goal(goal)

  fit <- plotGoal.fit(goal, dataCreationFunction, model, N)

  ggs <- list()

  colors <- bayesianSSDColors()

  goal <- goal[[1]]
  genData <- NULL
  if(!is.null(fit)){
    params <- as.matrix(fit)
    gA <- rep(0, length=dim(params)[1])
    gB <- rep(0, length=dim(params)[1])
    for(p in goal$parametersA){
      gA <- gA+params[, p]
    }
    for(p in goal$parametersB){
      gB <- gB+params[, p]
    }
    genData <- gA-gB
  }

  gg <- ggplot2::ggplot()

  if (goal$type=="rope"){
    gg <- plotGoal.rope(colors, genData, goal)
  }else if (goal$type=="precision"){
    gg <- plotGoal.precision(colors, genData, goal)
  }
  gg
}


#' @title
#' Plot goals for a sample size determination
#'
#'@description
#' Plots all goals created by \code{goalList}.
#' If the additional arguments are provided, the goals are plotted alongside example data.
#'
#' @param x The condition be tested. Use the function
#' \link[bayesianssd]{createGoal} to create such goals.
#' @param dataCreationFunction A function that accepts a single parameter,
#' \code{N}, and generates N values in the same manner as the given \code{model}.
#' @param model A object of class \code{stanmodel, stanreg or brmsfit}.
#' @param N The exemplary sample size.
#'
#' @returns An object of class \code{gtable}.
#' @method plot bayesianssdgoallist
#' @export
#'
#' @examples
#' goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
#'                    goalType="rope", ropeType="include", ropeLower=-2, ropeUpper=2,
#'                    ropeExclusive=TRUE, ci=0.95)
#' plot(goal)
#'
# goal_prec <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
# goalType="precision", precisionWidth=2, ci=0.95)
# plot(goal_prec)
#'
#' @examplesIf rlang::is_installed("rstanarm") && interactive()
#' dataCreationFunction <- function(N){
#'   group_effects <- c(
#'     control = 9,
#'     drug = 7
#'   )
#'   treatment <- rep(c("control", "drug"), length.out=N)
#'   y <- rpois(N, group_effects)
#'
#'   data <- data.frame(
#'     treatment = treatment,
#'     y = y
#'   )
#'   data
#' }
#'
#' model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
#'
#' goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
#'                    goalType="rope", ropeType="exclude", ropeLower=-0.1, ropeUpper = 0.1,
#'                    ropeExclusive=TRUE, ci=0.95)
#' plot(goal, dataCreationFunction, model, N=100)
plot.bayesianssdgoallist <- function(x, dataCreationFunction=NULL,
                                 model=NULL, N=NULL){
  verifySSDInputs.goal(x)

  fit <- plotGoal.fit(x, dataCreationFunction, model, N)

  ggs <- list()

  colors <- bayesianSSDColors()

  ggs <- lapply(seq_along(x), function(i){
    goal <- x[[i]]
    plot(goal)
  })

  n <- length(ggs)
  nCol <- floor(sqrt(n))
  suppressWarnings(
    rlang::exec(gridExtra::grid.arrange, !!!ggs, ncol = nCol)
  )
}


#' Plots a single goal of type 'rope'.
#' @importFrom rlang .data
#' @noRd
plotGoal.rope <- function(colors, genData, goal){
  gg <- ggplot2::ggplot()
  gg <- gg +
    ggplot2::scale_fill_manual(breaks = c("color1","color2","color3","color4"),
                               values=c(colors$goal1,
                                        colors$goal2,
                                        colors$goal3,
                                        colors$goal4),
                               name="Model", label=c("","","","Data generation"))

  cc1 <- 1
  color <- colors$rope1

  if(goal$ropeExclude == "include") color <- colors$rope2

  if(!is.null(genData)){
    densGen <- stats::density(genData)
    limits <- HDInterval::hdi(genData, credMass=goal$hdi)
    genData <- genData[genData>=limits[1] & genData<=limits[2]]
    bins <- min(50, length(unique(genData)))


    colGen <- "color4"

    gg <- gg +
      ggplot2::geom_histogram(data=data.frame(x=genData, col=colGen),
                              ggplot2::aes(x=.data$x, fill=colGen),
                              bins=bins,
                              alpha=0.5)
    lims <- ggplot2::ggplot_build(gg)$layout$panel_scales_x[[1]]$range$range
    gg <- gg +
      ggplot2::xlim(c(min(lims[1],goal$ropeLower), max(lims[2],goal$ropeUpper)))

    ggb <- ggplot2::ggplot_build(gg)
    ggb_data <- ggb$data
    cc1 <- max(ggb_data[[1]]$count)
  }

  gg <- gg +
    ggplot2::geom_rect(ggplot2::aes(xmin = goal$ropeLower, xmax = goal$ropeUpper,
                                    ymin = 0, ymax = cc1*1.1), alpha=0.3,
                       color=color, fill=color)

  gg <- gg + ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_y_continuous(breaks=NULL)  +
    ggplot2::theme(legend.position="none")
  gg
}

#' Plots a single goal of type 'precision'.
#' @importFrom rlang .data
#' @noRd
plotGoal.precision <- function(colors, genData, goal){
  if(is.null(genData))
    genData <- stats::rnorm(1e3)

  limits <- HDInterval::hdi(genData, credMass=goal$hdi)

  if(is.na(limits[1])){
    if(goal$hdi < 0.5){
      limits[1] <- -0.001
    }else{
      limits[1] <- min(genData)
    }
  }
  if(is.na(limits[2])){
    if(goal$hdi < 0.5){
      limits[2] <- 0.001
    }else{
      limits[2] <- max(genData)
    }
  }

  data <- data.frame(
    x = genData,
    col="color4"
  )
  data$col[data$x <= limits[1]] <- "color1"
  data$col[data$x >= limits[2]] <- "color1"
  bins <- 50

  breaks <- seq(from=min(genData), to=max(genData), length=bins-2)
  breaks <- c(breaks, limits[1], limits[2])
  breaks <- unique(breaks)

  gg <- ggplot2::ggplot()
  gg <- gg +
    ggplot2::geom_histogram(data=data,
                            ggplot2::aes(x=.data$x, fill=.data$col,
                                         color=.data$col),
                            breaks=breaks,
                            alpha=0.5) +
    ggplot2::scale_fill_manual(breaks = c("color1","color2","color3", "color4"),
                               values=c(colors$goal1,
                                        colors$goal2,
                                        colors$goal3,
                                        colors$goal4)) +
    ggplot2::scale_color_manual(breaks = c("color1","color2","color3", "color4"),
                                values=c(colors$goal1,
                                         colors$goal2,
                                         colors$goal3,
                                         colors$goal4))


  ggb <- ggplot2::ggplot_build(gg)
  ggb_data <- ggb$data
  cc1 <- ggb_data[[1]]$count
  height <- max(cc1) * 0.1

  gg <- gg +
    ggplot2::geom_segment(ggplot2::aes(x = limits[1], y = height,
                                       xend = limits[2], yend = height),
                          linewidth=2) +
    ggplot2::annotate("text", x = mean(genData), y=height*0.5, size = 5,
                      label=paste0("\u2264 ", goal$precWidth))

  gg <- gg + ggplot2::ylab("") + ggplot2::xlab("Posterior distribution") +
    ggplot2::scale_y_continuous(breaks=NULL) +
    ggplot2::theme(legend.position = "none",
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank())

  gg
}

#' If provided, fits the model to an example dataset of length 'N' generated by
#' 'dataCreationFunction'.
#' @noRd
plotGoal.fit <- function(goals, dataCreationFunction=NULL, model=NULL, N=NULL){
  check <- c(!is.null(dataCreationFunction),!is.null(model),!is.null(N))
  fit <- NULL
  if(any(check)){
    if(!all(check)){
      warning("If any argument of 'dataCreationFunction', 'model', or 'N' is set, all of them must be set.")
    }else{

      check <- checkSettings(model, dataCreationFunction, N, goals)
      if(!check){
        stop("Invalid inputs, check with 'checkSettings'.")
      }

      try({
        data <- dataCreationFunction(N)
      })
      if(is.null(data)){
        stop("Couldn't generate data with the provided 'dataCreationFunction' and 'N'")
      }
      fit <- NULL
      try({
        if ("stanmodel" %in% class(model)){
          fit <- rstan::sampling(model, data=data)
        }else if ("stanreg" %in% class(model)){
          fit <- stats::update(model, data=data)
        }else{
          fit <- stats::update(model, newdata=data)
        }
      })
      if(is.null(data)){
        stop("Couldn't fit the provided model.")
      }
    }
  }
  return(fit)
}
