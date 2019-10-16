#' Expected value of perfect information (EVPI) for a simple
#' model with the predictor variable sampled from a normal
#' distribution with.
#'
#' The Expected Value of Perfect Information is a concept in
#' decision analysis. It measures the expected loss of gain
#' (expected opportunity loss, EOL) that is incurred because
#' the decision-maker does not have perfect information
#' about a paricular variable. It is determined by examining
#' the influence of that variable on the output value of a
#' decision model. Its value is best illustrated by a plot
#' of weighed decision outcomes as a function of the
#' variable in question. If this curve intersects zero and
#' the recommendation without perfect information is to go
#' ahead with the project, the EVPI is the negative area
#' under the curve, or the positive area if the
#' recommendation is not to go ahead. If there is no
#' intersection point, the EVPI is zero.
#'
#' The EVPI is often calculated by assuming that all
#' variables except the one being tested take their best
#' estimate. This makes it possible to calculate the EVPI
#' very quickly, but at a high price: the assumption that
#' many variables simply take their best value ignores
#' uncertainties about all these variables. In the present
#' implementation, this problem is addressed by using the
#' outputs of a Monte Carlo simulation and assessing the
#' EVPI empirically. In the first step, the output variable
#' is smoothed using a loess regression with an automated
#' optimization of the bandwidth parameter, based on a
#' generalized cross validation procedure. Then the values
#' are weighted according to the probability density
#' function that has been used for Monte Carlo sampling
#' (i.e. a normal distribution, with mean and standard
#' deviation being estimated automatically) and the
#' resulting positive and negative areas under the curve are
#' calculated. After this, the expected gain (exptected mean
#' value - EMV) without perfect information (PI) is
#' calculated, the recommendation whether to go ahead with
#' the project without PI determine and the EVPI returned by
#' the function.
#'
#' @param mc output table from a Monte Carlo simulation,
#'   e.g. as realized with the decisionSupport package
#' @param test_var_name character; name of an independent
#'   variable in mc, sampled from a normal distribution
#' @param out_var_name character; name of a dependent
#'   variable in mc
#' @param object EVPI_res object (produced with
#'   empirical_EVPI) as input to the summary function.
#' @param x EVPI_res object (produced with empirical_EVPI)
#'   as input to the plotting function.
#' @param res boolean parameter indicating whether the plot
#'   function should output a plot of opportunity losses and
#'   gains  (res = TRUE) or a plot of the original data with
#'   the loess prediction (res = FALSE).
#' @param ...	Arguments to be passed to methods, such as
#'   graphical parameters (see par).
#' @return list of 11 elements: 
#'   (1) expected_gain: expected gain when project is
#'   implemented, without knowing the value of the test
#'   variable, equals NA when there is no variation in the
#'   output variable (2) recommendation: should project be
#'   implemented? Decision without knowing the value of the
#'   test variable (3) EVPI_do: the Expected Value of
#'   Perfect Information (EVPI) for this variable, if the
#'   recommended decision is to implement the project. (4)
#'   EVPI_dont: the Expected Value of Perfect Information
#'   (EVPI) for this variable, if the recommended decision
#'   is not to implement the project. (5) tests_var_data:
#'   values of the test variable (6) out_var_data: values of
#'   the outcome variable (7) out_var_sm: results of loess
#'   regression = smoothed outcome variable (8) weight:
#'   values by which smoothed outcome variable is weighted
#'   (9) out_var_weight: smoothed and weighted outcome
#'   variable (10) test_var_name: variable name of test data
#'   (11) out_var_name: variable name of outcome data
#'
#'
#'   
#' @importFrom stats predict
#' @importFrom graphics box legend mtext plot strwidth text
#'
#' @author Eike Luedeling, Katja Schiffers
#' @keywords "Value of Information"
#' @examples
#'
#'
#' ### In the following example, the sign of the calculation
#' ### is entirely determined by the predictor variable
#' ### 'indep1', so this should be expected to have a high
#' ### EVPI.
#'
#' montecarlo <- data.frame(indep1 = rnorm(1000), indep2 = rlnorm(1000))
#' montecarlo[, 'output1'] <- montecarlo[, 'indep1'] * montecarlo[, 'indep2']
#'
#' evpi1 <- empirical_EVPI(mc = montecarlo, test_var_name = 'indep1', out_var_name = 'output1')
#' summary(evpi1)
#' plot(evpi1, res = FALSE)
#' plot(evpi1, res = TRUE)
#' 
#' 
#' ### In this example, the sign of the output variable does not change depending on the
#' ### predictor variable 'indep1' so the EVPI should be zero.
#' montecarlo[, 'output2'] <- (montecarlo[, 'indep1'] * (montecarlo[, 'indep2']) + 10)
#' evpi2 <- empirical_EVPI(mc = montecarlo, test_var_name = 'indep1', out_var_name = 'output2')
#' summary(evpi2)
#' plot(evpi2, res = FALSE)
#' plot(evpi2, res = TRUE)

#' @export empirical_EVPI
empirical_EVPI <- function(mc, test_var_name, out_var_name) {
  
  # calculate parameters of the normal distribution of the test_var
  sd_tv <- sd(mc[, test_var_name])
  mean_tv <- mean(mc[, test_var_name])
  
  # if there is no variation in the test variable, return NA
  if (sd_tv == 0) {
    return(list(expected_gain = NA, EVPI_do = 0, EVPI_dont = 0))
  }
  
  #  otherwise start with EVPI calculation
  mcs <- mc[order(mc[, test_var_name]), ]  # order MC results by test variable
  # plot(1:length(mcs$test_var_name), mcs$test_var_name)
  # fit a local polynomial regression on the out_var, with
  # automatic smoothing parameter selection based on a
  # generalized cross validation procedure (gcv)
  loessmod <- fANCOVA::loess.as(mcs[,test_var_name], mcs[, out_var_name], criterion = "gcv")
  out_var_sm <- predict(loessmod, mcs[,test_var_name])
  
  # weight smoothed output variable with probability distribution of test
  # variable
  
  weight <- stats::dnorm(mcs[,test_var_name], mean = mean_tv, sd = sd_tv)
  out_var_weight <- out_var_sm * weight
  
  # calculate auc for positive part of curve
  out_var_pos <- ifelse(out_var_weight>=0, out_var_weight, 0)
  diffsx <-mcs[,test_var_name]-c(mcs[c(1,1:(nrow(mcs)-1)),test_var_name])
  auc_pos <- sum(out_var_pos * diffsx)
  
  
  # calculate auc for negative part of curve
  out_var_neg <- ifelse(out_var_weight<0, out_var_weight, 0)
  auc_neg <- sum(out_var_neg * diffsx)
  
  exp_gain <- auc_neg + auc_pos
  
  res <- list(expected_gain = exp_gain,
              recommendation = ifelse(exp_gain > 0, "do", "don't"),
              EVPI_do = - auc_neg,
              EVPI_dont = auc_pos,
              test_var_data = mcs[, test_var_name],
              out_var_data = mcs[, out_var_name],
              out_var_sm = out_var_sm,
              weight = weight,
              out_var_weight = out_var_weight,
              test_var_name = test_var_name,
              out_var_name = out_var_name)
  
  attr(res, "class") <- c("EVPI_res", "list")
  return(invisible(res))
}

#' @rdname empirical_EVPI
#' @name summary.EVPI_res
#' @aliases summary_empirical_EVPI
#' @export
#' 
summary.EVPI_res <- function(object, ...){
  cat("expected gain: ", round(object$expected_gain, digits = 3), "\n", 
      "recommendation: ", object$recommendation, "\n", 
      "EVPI value for recommendation 'do': ", round(object$EVPI_do, digits = 3), "\n", 
      "EVPI value for recommendation 'don't': ", round(object$EVPI_dont, digits = 3))
}

#' @rdname empirical_EVPI
#' @name plot.EVPI_res
#' @aliases plot_empirical_EVPI
#' @export
plot.EVPI_res <- function(x, res = TRUE, ...){
  if(res == FALSE){
    x1 <- data.frame(test_var = x$test_var_data, 
                     out_var = x$out_var_data, 
                     out_var_sm = x$out_var_sm)
    
    ggplot2::ggplot(x1,ggplot2::aes_string("test_var","out_var")) + 
      ggplot2::geom_jitter(alpha = 0.2) +
      ggplot2::geom_line(ggplot2::aes(x = x1$test_var, y = x1$out_var_sm), col = "red") +
      ggplot2::labs(title = "Original data with loess prediction", 
                    x = x$test_var_name, 
                    y = x$out_var_name) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size=14, face="bold"),
        axis.title.x = ggplot2::element_text(size=14),
        axis.title.y = ggplot2::element_text(size=14))
    
  }else{
    
    if(x$recommendation == "do"){
      title <- paste("EVPI: ", round(x$EVPI_do, digits = 2), sep = " ")
    }else{
      title <- paste("EVPI: ", round(x$EVPI_dont, digits = 2), sep = " ")
    }
    
    x1 <- data.frame(test_var = x$test_var_data, 
                     out_var_weight = x$out_var_weight)
    ylab <- paste("Weighted", x$out_var_name, sep = " ")
    
    ggplot2::ggplot(x1,ggplot2::aes_string("test_var","out_var_weight")) + 
      ggplot2::geom_line() +
      ggplot2::geom_area(data=unique(subset(x1, x1$out_var_weight>=0)), fill="lightgreen") +
      ggplot2::geom_area(data=unique(subset(x1, x1$out_var_weight<0)), fill="tomato") +
      ggplot2::labs(title = title, x = x$test_var_name,  y = ylab) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size=14, face="bold"),
        axis.title.x = ggplot2::element_text(size=14),
        axis.title.y = ggplot2::element_text(size=14)
      ) 
  }
}

