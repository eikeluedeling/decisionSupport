#' Expected value of perfect information (EVPI) for a
#' variable, calculated from Monte Carlo output with normal
#' sampling distribution
#'
#' The Expected Value of Perfect Information is a concept in
#' decision analysis. It measures the expected loss of gain
#' (expected opportunity loss, EOL) that is incurred because
#' the decision-maker does not have perfect information
#' about a paricular variable. It is determined by examining
#' the influence of that variable on the output value of a
#' decision model. Its value is best illustrated by a plot
#' of decision outcomes as a function of the variable in
#' question. If this curve intersects zero and the
#' recommendation without perfect information is to go ahead
#' with the project, the EVPI is the negative area under the
#' curve, or the positive area if the recommendation is not
#' to go ahead. If there is no intersection point, the EVPI
#' is zero.
#'
#' The EVPI is often calculatedby assuming that all
#' variables except the one being tested assume their best
#' estimate. This makes it possible to calculate the EVPI
#' very quickly, but at a high price: the assumption that
#' many variables simply assume their best value ignores
#' uncertainties about all these variaables. In the present
#' function, this problem is addressed by using the outputs
#' of a Monte Carlo simulation and assessing the EVPI
#' empirically. In the first step, the outcome variable is
#' smoothed using a loess regression with an automated
#' optimization of the bandwidth parameter, based on a
#' generalized cross validation procedure. After this, the
#' expected mean values (EMV) without perfect information
#' (PI) are calculated for both decision. Based on this the
#' recommendation without PI is determined. Then the
#' positive and negative areas under the curve are
#' calculated and the expected opportunity loss (EOL)
#' returned by the function. Depending on the recommended
#' decision, the EOL for the opposite decision option can
#' then be interpreted as ther EVPI for the tested variable.
#' If no threshold is identified, i.e. the model output
#' variables are either all positive or all negative, the
#' EVPI for this variable is zero, indicating that more
#' information about this particular variable will not
#' change the recommended decision.
#'
#' @param mc output table from a Monte Carlo simulation,
#'   e.g. as realized with the decisionSupport package
#' @param test_var_name character; name of an independent
#'   variable in mc
#' @param out_var_name character; name of a dependent variable in
#'   mc
#' @param object EVPI_res object (produced with EVPI_empi) as input to the
#'   summary function plot.
#' @param x EVPI_res object (produced with EVPI_empi) as input to the
#'   plotting function plot.
#' @param res boolean parameter indicating whether the plot function should
#'   output a plot of opportunity losses and gains  (res = TRUE) or
#'   a plot of the original data with the loess prediction (res = FALSE).
#' @param ...	Arguments to be passed to methods, such as graphical parameters (see par).
#' @return list of 11 elements: 
#'   (1) expected_gain: expected gain when project is
#'   implemented, without knowing the value of the test
#'   variable (2) recommendation: should project be
#'   implemented? Decision without knowing the value of the
#'   test variable (3) EVPI_do: the Expected Value of
#'   Perfect Information (EVPI) for this variable, if the
#'   recommended decision is to implement the project. (4)
#'   EVPI_dont: the Expected Value of Perfect Information
#'   (EVPI) for this variable, if the recommended decision
#'   is not to implement the project. (5) tests_var_data: values
#'   of the test variable (6) out_var_data: values of the
#'   outcome variable (7) out_var_sm: results of loess
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
#'
#' @author Eike Luedeling, Katja Schiffers
#' @keywords "Value of Information"
#' @examples
#'
#'
#' ### In the following example, the sign of the calculation is entirely
#' ### determined by the variable indep1, so this should be expected to have
#' ### a high EVPI. Variable indep2 doesn't affect the sign of the output,
#' ### so it should not have information value.
#'
#' montecarlo <- data.frame(indep1 = rnorm(1000), indep2 = rlnorm(1000))
#' montecarlo[, 'output1'] <- montecarlo[, 'indep1'] * montecarlo[, 'indep2']
#' montecarlo[, 'output2'] <- (montecarlo[, 'indep1'] * (montecarlo[, 'indep2']) + 5)
#'
#' evpi1 <- EVPI_empi(mc = montecarlo, test_var_name = 'indep1', out_var_name = 'output1')
#' summary(evpi1)
#' plot(evpi1, res = FALSE)
#' plot(evpi1, res = TRUE)
#' 
#' evpi2 <- EVPI_empi(mc = montecarlo, test_var_name = 'indep1', out_var_name = 'output2')
#' summary(evpi2)
#' plot(evpi2, res = FALSE)
#' plot(evpi2, res = TRUE)

#' @export EVPI_empi
EVPI_empi <- function(mc, test_var_name, out_var_name) {
 
  # calculate parameters of the normal distribution of the test_var
  sd_tv <- sd(mc[, test_var_name])
  mean_tv <- mean(mc[, test_var_name])
  
  # if there is no variation in the test variable, return 'no variation'
    if (sd_tv == 0) {
    return(list(expected_gain = "no variation", EVPI_do = 0, EVPI_dont = 0))
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
 
#' @rdname EVPI_empi
#' @name summary.EVPI_res
#' @aliases summary
#' @export
summary.EVPI_res <- function(object, ...){
 cat("expected gain: ", round(object$expected_gain, digits = 3), "\n", 
     "recommendation: ", object$recommendation, "\n", 
     "EVPI value for recommendation 'do': ", round(object$EVPI_do, digits = 3), "\n", 
     "EVPI value for recommendation 'don't': ", round(object$EVPI_dont, digits = 3))
}

#' @rdname EVPI_empi
#' @name plot.EVPI_res
#' @aliases plot
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




#' Expected value of perfect information (EVPI) for many variables. This
#' is a wrapper for the EVPI_empi function
#'
#' This function computes the Value of Information for many variables
#' contained in the output table of a Monte Carlo simulation. It applies the
#' \code{\link{EVPI_empi}} function to compute the amount of money that a
#' rational decision-maker should be willing to pay for perfect information
#' on each model input variable. See the documentation of the 
#' \code{\link{EVPI_empi}} function for more details.
#' 
#'
#' @param mc output table from a Monte Carlo simulation,
#'   e.g. as realized with the decisionSupport package
#'   
#' @param first_out_var name of the column in the mc table that contains the first output
#'   variable. Information Values are computed for variables in all earlier columns.
#' @param fileformat format of the output file. This can only be "png" for output as a file
#'   or NA for plotting within R.
#' @param outfolder folder where the outputs should be saved (this is optional).
#' @param write_table boolean parameter indicating whether an output table should be written.
#' @param scale_results boolean parameter indicating whether numbers in the output should be
#'   expressed in thousands, millions, billions etc.
#' @param legend_table data.frame containing translations of input variable names into
#'   labels for the EVPI figure
#' @param output_legend_table data.frame containing translations of output variable names into
#'   labels for the EVPI figure
#' 
#' @return list of as many elements as there are output variables in the Monte Carlo table:
#'   each element refers to one of the output variables and contains a data.frame with four
#'   columns:
#'   (1) variable - the input variable names
#'   (2) expected_gain - expected gain when project is
#'   implemented, without knowing the value of the test
#'   variable
#'   (3) EVPI_do - the Expected Value of Perfect Information on the respective input variable,
#'   if the analysis suggests that the expected value of the decision is likely positive
#'   (e.g. the project should be done)
#'   (4) EVPI_dont - the Expected Value of Perfect Information on the respective input variable,
#'   if the analysis suggests that the expected value of the decision is likely negative
#'   (e.g. the project should not be done)
#'      
#' @importFrom stats predict
#' @importFrom graphics box legend mtext plot strwidth text
#'
#' @author Eike Luedeling, Katja Schiffers
#' @keywords "Value of Information"
#' @examples
#'
#'
#' ### In the following example, the sign of the calculation is entirely
#' ### determined by the variable indep1, so this should be expected to have
#' ### a high EVPI. Variable indep2 doesn't affect the sign of the output,
#' ### so it should not have information value.
#'
#' montecarlo <- data.frame(indep1 = rnorm(1000), indep2 = rlnorm(1000))
#' montecarlo[, 'output1'] <- montecarlo[, 'indep1'] * montecarlo[, 'indep2']
#' montecarlo[, 'output2'] <- (montecarlo[, 'indep1'] * (montecarlo[, 'indep2']) + 5)
#'
#' empirical_EVPI(montecarlo,"output1")
#' @export empirical_EVPI
empirical_EVPI<-function(mc,first_out_var,fileformat=NA,outfolder=NA,
                         write_table=FALSE,scale_results=TRUE,legend_table=NULL,
                         output_legend_table=NULL) 
{
  if(mean(mc[,1]==1:nrow(mc))==1) mc<-mc[,2:ncol(mc)]
  outvar1pos<-which(colnames(mc)==first_out_var)
  outvars<-colnames(mc)[outvar1pos:ncol(mc)]
  print(paste("Processing",length(outvars),"output variables. This can take some time."))
  
  outputs<-list()
  for(out_var in outvars)
  {EOL_do<-abs(sum(mc[which(mc[,out_var]<0),out_var])/nrow(mc))
  EOL_dont<-sum(mc[which(mc[,out_var]>=0),out_var])/nrow(mc)
  if(EOL_do<EOL_dont) decision<-"do" else decision<-"dont"
  
  
  results<-t(sapply(colnames(mc)[1:(outvar1pos-1)],function(x) EVPI_empi(mc,x,out_var)[c("expected_gain","EVPI_do","EVPI_dont")]))
  
  outs<-data.frame(variable=rownames(results),
                   expected_gain=unlist(results[,"expected_gain"]),
                   EVPI_do=unlist(results[,"EVPI_do"]),
                   EVPI_dont=unlist(results[,"EVPI_dont"]))
  
  if(decision=="do") res<-outs[,c("EVPI_do")]
  if(decision=="dont") res<-outs[,c("EVPI_dont")]
  names(res)<-outs[,c("variable")]
  
  toplot<-sort(res[which(res>0)])
  
  x_label<-"Value of Information"           
  suffix<-""             
  if(!length(toplot)==0)
    if(scale_results)
    {scaling_factor<-ceiling(log10(max(abs(toplot))))
    if(scaling_factor>15) {toplot<-toplot/1000000000000000
    suffix<-"(in quadrillions)"} else
      if(scaling_factor>12) {toplot<-toplot/1000000000000
      suffix<-"(in trillions)"} else
        if(scaling_factor>9) {toplot<-toplot/1000000000
        suffix<-"(in billions)"} else
          if(scaling_factor>6) {toplot<-toplot/1000000
          suffix<-"(in millions)"} else
            if(scaling_factor>3) {toplot<-toplot/1000
            suffix<-"(in thousands)"} else
              suffix=""
            
            x_label<-paste("Value of Information",suffix)    
    }
  
  if(!length(toplot)==0) if(!is.null(legend_table)) names(toplot)<-as.character(legend_table[sapply(names(toplot),function(x)  which(as.character(legend_table$variable)==x)),"label"])
  if(!is.null(output_legend_table)) lab_outvar<-as.character(output_legend_table[which(as.character(legend_table$variable)==out_var),"label"]) else
    lab_outvar<-out_var 
  
  
  if (!is.na(fileformat)) if(fileformat=="png") png(file.path(outfolder,paste("EVPI_plot_",out_var,".png",sep="")),height=1000,width=1200)
  
  par(family="serif")
  if(!is.na(fileformat))
  {cexex<-2
  cexexlarge<-3
  space<-4
  if(!length(toplot)==0) textspace<-max(strwidth(names(toplot),units="inches",cex=cexex)) else textspace<-1
  par(mai=c(1.5,0.5+textspace,1,0.5))} else { #6,35,6,1
    cexex<-1
    cexexlarge<-1.5
    space<-2.5
    if(!length(toplot)==0) textspace<-max(strwidth(names(toplot),units="inches",cex=cexex)) else textspace<-1
    par(mai=c(1.5,0.5+textspace,1,0.5))
  }
  
  
  if(!length(toplot)==0) {barplot(toplot,horiz=TRUE,las=1,cex.lab=cexexlarge,cex.axis=cexex,cex.names=cexex,lwd=2,xlab="",main=lab_outvar,cex.main=cexexlarge,
                                  xlim=c(0,max(toplot)*1.04),col="LIGHT BLUE")
    mtext(x_label,1,line=space,cex=cexexlarge)}else
    {plot(1,col="WHITE",axes=FALSE,xlab="",ylab="",main=lab_outvar,cex.main=cexexlarge)
      text(1,1,"Value of information 0 for all variables",cex=cexex)}
  
  if(decision=="do") legend("bottomright",legend=paste("Result probably positive"),cex=cexex,bg="LIGHT GREEN",box.col="LIGHT GREEN")
  if(decision=="dont") legend("bottomright",legend=paste("Result probably negative"),cex=cexex,bg="indianred1",box.col="indianred1")
  box(lwd=2)
  if(!is.na(fileformat)) dev.off()
  outputs[[out_var]]<-outs
  if(write_table) write.csv(res,paste(file.path(outfolder,paste("EVPI_table_",out_var,".csv",sep=""))))
  print(paste("Output variable ",which(out_var==outvars)," (",out_var,") completed.",sep=""))
  }
  return(outputs)
}
