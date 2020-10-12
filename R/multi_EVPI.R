#' Expected value of perfect information (EVPI) for multiple variables. This
#' is a wrapper for the empirical_EVPI function. See the documentation of the 
#' \code{\link{empirical_EVPI}} function for more details.
#' 
#'
#' @param mc output table from a Monte Carlo simulation,
#'   e.g. as realized with the decisionSupport package
#' @param first_out_var name of the column in the mc table that contains the first output
#'   variable. Information Values are computed for variables in all earlier columns.
#' @param write_table boolean parameter indicating whether an output table should be written.
#' @param outfolder folder where the outputs should be saved (this is optional).
#' @return invisible list of as many elements as there are output variables 
#'   in the Monte Carlo table:
#'   each element refers to one of the output variables and contains a data.frame with five
#'   columns:
#'   (1) variable - the input variable names
#'   (2) expected_gain - expected gain when project is
#'   implemented, without knowing the value of the test
#'   variable, equals NA in case there is no variation in the tested variable
#'   (3) EVPI_do - the Expected Value of Perfect Information on the respective input variable,
#'   if the analysis suggests that the expected value of the decision is likely positive
#'   (e.g. the project should be done)
#'   (4) EVPI_dont - the Expected Value of Perfect Information on the respective input variable,
#'   if the analysis suggests that the expected value of the decision is likely negative
#'   (e.g. the project should not be done)
#'   (5) the decision whether to implement with the project based on imperfect information
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
#' ### is entirely determined by the variable indep1, so
#' ### this should be expected to have a high EVPI. Variable
#' ### indep2 doesn't affect the sign of the output, so it
#' ### should not have information value.
#'
#' montecarlo <- data.frame(indep1 = rnorm(1000), indep2 = rnorm(1000, 3))
#' montecarlo[, 'output1'] <- montecarlo[, 'indep1'] * montecarlo[, 'indep2']
#' montecarlo[, 'output2'] <- (montecarlo[, 'indep1'] * (montecarlo[, 'indep2']) + 10)
#'
#' results_all <- multi_EVPI(montecarlo,"output1")
#' summary(results_all)
#' plot(results_all, "output1")
#' plot(results_all, "output2")

#' @keywords "Value of Information"
#' @examples
#'
#'
#' ### In the following example, the sign of the calculation is entirely
#' ### determined by the variable indep1, so this should be expected to have
#' ### a high EVPI. Variable indep2 doesn't affect the sign of the output,
#' ### so it should not have information value.
#'
#' montecarlo <- data.frame(indep1 = rnorm(1000), indep2 = rnorm(1000, mean = 3))
#' montecarlo[, 'output1'] <- montecarlo[, 'indep1'] * montecarlo[, 'indep2']
#' montecarlo[, 'output2'] <- (montecarlo[, 'indep1'] * (montecarlo[, 'indep2']) + 10)
#'
#' results_all <- multi_EVPI(montecarlo,"output1")
#' summary(results_all)
#' plot(results_all, "output1")
#' plot(results_all, "output2")
#' 
#' @export multi_EVPI
multi_EVPI <- function(mc, first_out_var, write_table=FALSE, outfolder=NA) 
{
  if(mean(mc[,1]==1:nrow(mc))==1) mc <- mc[,2:ncol(mc)]
  outvar1pos <- which(colnames(mc)==first_out_var)
  outvars <- colnames(mc)[outvar1pos:ncol(mc)]
  print(paste("Processing",length(outvars),"output variables. This can take some time."))
  
  outputs <- list()
  for(out_var in outvars){

    # call the empirical_EVPI function
    results <- t(sapply(colnames(mc)[1:(outvar1pos-1)], function(x) empirical_EVPI(mc,x,out_var)[c("expected_gain","EVPI_do","EVPI_dont")]))
    
    outs <- data.frame(variable=rownames(results),
                       expected_gain=unlist(results[,"expected_gain"]),
                       EVPI_do=unlist(results[,"EVPI_do"]),
                       EVPI_dont=unlist(results[,"EVPI_dont"]))
    

    decision <- ifelse(mean(outs$expected_gain, na.rm = TRUE) > 0, "do", "dont")
    if(decision=="do") res <- outs[,c("EVPI_do")]
    if(decision=="dont") res <- outs[,c("EVPI_dont")]
    names(res) <- outs[,c("variable")]
    
    outputs[[out_var]] <- outs
    outputs[[out_var]]$EVPI <- res
    outputs[[out_var]]$decision <- decision
    
    # write out res or outs? outs needed for compound figure
    if(write_table){
      if(is.na(outfolder))  outfolder <- "."
      write.csv(outs, paste(file.path(outfolder, 
                                     paste("EVPI_table_", 
                                           out_var, ".csv",
                                           sep=""))))
    }
    
    print(paste("Output variable ", which(out_var==outvars), " (",out_var,") completed.", sep=""))
  }
  attr(outputs, "class") <- c("EVPI_outputs", "list")
  return(invisible(outputs))
}

#' @param object EVPI_res object (produced with multi_EVPI) as input to the
#'   summary function plot.
#' @param ...	Arguments to be passed to methods, such as graphical parameters (see par).
#' 
#' @rdname multi_EVPI
#' @name summary.EVPI_outputs
#' @aliases summary_multi_EVPI
#' @export
summary.EVPI_outputs <- function(object, ...){
  for(i in 1:length(object)){
    print(names(object)[i])
    print(object[[i]][2:4])
    cat("\n")
  }
}

#' @param x object of class EVPI_outputs as produced with the multi_EVPI function
#' @param out_var name of the output variable to be plotted
#' @param fileformat The file format to be used for the outputs. Currently only
#' NA (for R plot output) and "png" (for a PNG file) are implemented. Note that
#' when this is !NA, the outfolder parameter must point to a valid folder.
#' @param scale_results boolean variable indicating if resulting high numbers
#' should be scaled to avoid numbers in the plot that cannot be read easily. If
#' this is TRUE, numbers are divided by an appropriate divisor and a suffix is
#' added to the number in the plot (e.g. "in millions").
#' @param legend_table a data.frame with two columns variable and label. The
#' variable column should contain the name of the independent variables as
#' listed in the Monte Carlo table. The label column should contain the label
#' to be used for this variable in the EVPI plot.
#' @param output_legend_table a data.frame with two columns variable and label.
#' The variable column should contain the name of the dependent variables as
#' listed in the Monte Carlo table. The label column should contain the label
#' to be used for this variable in the EVPI plot. Note that labels for both
#' dependent and independent variables can be provided in the same table. Then
#' both parameters legend_table and output_legend_table can point to the same
#' table. 
#'   
#' @rdname multi_EVPI
#' @name plot.EVPI_outputs
#' @aliases plot_multi_EVPI
#' @export
plot.EVPI_outputs <- function(x, 
                              out_var,
                              fileformat=NA,
                              outfolder=NA,
                              scale_results=TRUE,
                              legend_table=NULL,
                              output_legend_table=NULL,
                              ...){
  
  toplot<-t(x[[out_var]][5])
  names(toplot)<-x[[out_var]]$variable
  
  x_label<-"Expected Value of Perfect Information"           
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
            
            x_label<-paste("Expected Value of Perfect Information",suffix)    
    }
  
  if(!is.null(toplot)) if(!is.null(legend_table)) names(toplot)<-as.character(legend_table[sapply(names(toplot),function(x)  which(as.character(legend_table$variable)==x)),"label"])
  
  if(!is.null(output_legend_table)) lab_outvar<-as.character(output_legend_table[which(as.character(legend_table$variable)==out_var),"label"]) else  lab_outvar <- out_var 
  
  
  if(!is.na(fileformat)) if(fileformat=="png") png(file.path(outfolder,paste("EVPI_plot_",out_var,".png",sep="")),height=1000,width=1200)
  
  par(family="serif")
  
  if(!is.na(fileformat)){
    cexex <- 2
    cexexlarge <- 3
    space <- 4
    if(!length(toplot)==0) textspace<-max(strwidth(names(toplot),units="inches",cex=cexex))
    else textspace <- 1
    par(mai=c(1.5,0.5+textspace,1,0.5))} 
  else { #6,35,6,1
    cexex <- 1
    cexexlarge <- 1.5
    space <- 2.5
    if(!length(toplot)==0){
      textspace <- max(strwidth(names(toplot),units="inches",cex=cexex))
    }else{
      textspace <- 1
    }
    par(mai=c(1.5, 0.5+textspace, 1, 0.5))
  }
  
  
  if(!length(toplot)==0) {
    barplot(toplot, horiz=TRUE, las=1, cex.lab=cexexlarge, cex.axis=cexex,
            cex.names=cexex, lwd=2, xlab="", main=lab_outvar,
            cex.main=cexexlarge, xlim=c(0,max(toplot)*1.04), 
            col="LIGHT BLUE")
    mtext(x_label,1,line=space,cex=cexexlarge)
  }else{
    plot(1,col="WHITE",axes=FALSE,xlab="",ylab="",main=lab_outvar,cex.main=cexexlarge)
    text(1,1,"Value of information 0 for all variables",cex=cexex)
  }
  
  if(unique(x[[out_var]][6])=="do"){
    legend("bottomright",legend=paste("Recommendation: do"),
           cex=cexex,bg="LIGHT GREEN",box.col="LIGHT GREEN")
  }else{
    legend("bottomright",legend=paste("Recommendation: don't"),
           cex=cexex,bg="indianred1",box.col="indianred1")
  }
  box(lwd=2)
  if(!is.na(fileformat)) dev.off()
  
}
