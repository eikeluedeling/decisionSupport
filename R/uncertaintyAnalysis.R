#
# file: uncertaintyAnalysis.R
#
# This file is part of the R-package decisionSupport
#
# Authors:
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <E.Luedeling@cgiar.org>
#
# Copyright (C) 2015 World Agroforestry Centre (ICRAF)
#	http://www.worldagroforestry.org
#
# The R-package decisionSupport is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# The R-package decisionSupport is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the R-package decisionSupport.  If not, see <http://www.gnu.org/licenses/>.
#
##############################################################################################
#' @include estimate_read_csv_old.R
#' @include individualEvpiSimulation.R
NULL
##############################################################################################
# uncertaintyAnalysis(inputFilePath, outputPath, modelFunction, numberOfSimulations,
#                     randomMethod="calculate",	functionSyntax="globalNames",
#                     write_table=TRUE, indicators=FALSE, log_scales=FALSE,
#                     oldInputStandard=FALSE)
##############################################################################################
#' Uncertainty Analysis Wrapper Function.
#'
#' This function performs a Monte Carlo simulation from input files and analyses the results
#' via Partial Least Squares Regression (PLSR) and calculates the Variable Importance on Projection
#' (VIP). Results are safed as plots.
#' @param inputFilePath Path to input csv file, which gives the input \code{\link{estimate}}.
#' @param outputPath Path were the result plots and tables are safed.
#' @param modelFunction The model function.
#' @param numberOfSimulations The number of Monte Carlo simulations to be performed.
#' @param randomMethod ToDo
#' @param functionSyntax ToDo
#' @param write_table \code{logical}: If the full Monte Carlo simulation results and PLSR results should be
#'  written to file.
#' @param plsrVipAnalysis \code{logical}: If PLSR-VIP analysis shall be performed.
#' @param indicators \code{logical}: If indicator variables should be respected specially.
#' @param log_scales \code{logical}: If the scales in the pls plots should be logarithmic.
#' @param oldInputStandard \code{logical}: If the old input standard should be used
#' 	(\code{\link{estimate_read_csv_old}}).
#' @param verbosity \code{integer}: if \code{0} the function is silent; the larger the value the
#'   more verbose is output information.
#' 	@seealso \code{\link{mcSimulation}}, \code{\link{estimate}}, \code{\link{estimate_read_csv}}, 
#' 	\code{\link{plsr.mcSimulation}}, \code{\link[chillR:VIP]{VIP}}
#' @export
uncertaintyAnalysis <- function(inputFilePath, outputPath, modelFunction, numberOfSimulations,
                                randomMethod="calculate",	functionSyntax="globalNames",
                                write_table=TRUE, 
                                plsrVipAnalysis=TRUE,
                                indicators=FALSE, log_scales=FALSE,
                                oldInputStandard=FALSE,
                                verbosity=1){
  # Read estimate from file:
  if(!oldInputStandard){
    #	print("newInputStandard")
    estimateObject<-estimate_read_csv(fileName=inputFilePath)
  }else{
    #	print("oldInputStandard")
    estimateObject<-estimate_read_csv_old(fileName=inputFilePath)
  }
  if(verbosity > 0)
    cat("Estimate read from file: ",inputFilePath, "\n")
  if(verbosity > 1)
    print(estimateObject)
  # Run Monte Carlo simulation:
  mcResults<-mcSimulation(estimate=estimateObject,
                          model_function=modelFunction,
                          numberOfSimulations=numberOfSimulations,
                          randomMethod=randomMethod,
                          functionSyntax=functionSyntax)
  if(verbosity > 0)
    cat("Monte Carlo Simulation done.\n")
  if(verbosity > 1)
    print(summary(mcResults))
  # Write histogram of results to png files:
  if ( !file.exists(outputPath) )
    dir.create(outputPath, recursive=TRUE)
  for(i in names(mcResults$y)) {
    png(file.path(outputPath, paste(i, "_distribution.png",sep="")), width=1000, height=500)
    par(mar=c(5.1,5.1,4.1,2.1))
    hist(mcResults, lwd=3, cex.lab=2 ,cex.axis=2, prob=TRUE, resultName=i)
    dev.off()
  }
  # Write the summary of the resulting distributions to file:
  mcSummary<-summary(mcResults, digits=2)
  write.csv(mcSummary$summary,file.path(outputPath,"summary_cooperation.csv"))
  if (verbosity > 0)
    cat("Monte Carlo results written into directory: ", outputPath, "\n")
  # Partial lest squares analysis:
  ## ToDo
  #if(0){
  if(plsrVipAnalysis){
    if (!requireNamespace("chillR", quietly = TRUE)) 
      stop("Package \"chillR\" needed. Please install it.",
           call. = FALSE)
    for(i in names(mcResults$y)) {
      # Run PLSR on the Monte Carlo result:
      plsrResult<-plsr.mcSimulation(mcResults, resultName=i)
      # Run VIP analysis on the PLSR result:
      vipResult<-chillR::VIP(plsrResult)["Comp 1",]
      # Prepare plotting of PLSR-VIP results:
      coef<-plsrResult$coefficients[,,1]
      color_bars<-chillR::color_bar_maker(vipResult, coef, threshold=0.8, 
                                          col1="RED", col2="DARK GREEN", col3="DARK GREY")
      vipResult_select<-vipResult[order(vipResult,decreasing=TRUE)[1:50]]
      if(length(vipResult)<50) vipResult_select<-vipResult_select[1:length(vipResult)]
      col_select<-color_bars[order(vipResult,decreasing=TRUE)[1:50]]
      if(length(vipResult)<50) col_select<-col_select[1:length(vipResult)]
      # Plot PLSR-VIP results to file:
      png(file.path(outputPath, paste(i, "_PLS_VIP.png",sep="")),height=1400,width=1400)
      par(mar=c(5.1,55,4.1,2.1))
      barplot(rev(vipResult_select),horiz=TRUE,las=1,col=rev(col_select),cex.names=2,cex.axis=1,main="VIP for most important variables",cex.main=2,axes=FALSE)
      axis(side=1,cex.axis=2,lwd=5,padj=0.7)
      abline(v=0.8,lwd=3)
      dev.off()
      
      if (write_table){
        vipPlsResultTable<-cbind(vipResult,coef)
        colnames(vipPlsResultTable)<-c("VIP","Coefficient")
        #         if (write_table) write.csv(pls_tab,paste(result_path,resultnames[ress],"_pls_results.csv",sep=""))
        write.csv(vipPlsResultTable,file.path(outputPath, paste(i,"_pls_results.csv",sep="")))
      }
      if (0){
        ##         vip<-VIP(pls_out)["Comp 1",]
        ##         coef<-pls_out$coefficients[,,1]
        ##         color_bars<-color_bar_maker(vip,coef,threshold=0.8,col1="RED",col2="DARK GREEN",col3="DARK GREY")
        #         
        #         #barplot(coef,horiz=TRUE,las=1,col=color_bars,cex.names=4,cex.axis=3,main="Model coefficient",cex.main=5,axes=FALSE)
        #         
        ##         vip_select<-vip[order(vip,decreasing=TRUE)[1:50]]
        ##         if(length(vip)<50) vip_select<-vip_select[1:length(vip)]
        ##         col_select<-color_bars[order(vip,decreasing=TRUE)[1:50]]
        ##         if(length(vip)<50) col_select<-col_select[1:length(vip)]
        #         #par(mar=c(5.1,20,4.1,4.1))
        #         
        ##         png(paste(result_path,resultnames[ress],"_PLS_VIP.png",sep=""),height=1400,width=1400)
        ##         par(mar=c(5.1,55,4.1,2.1))
        ##         barplot(rev(vip_select),horiz=TRUE,las=1,col=rev(col_select),cex.names=2,cex.axis=1,main="VIP for most important variables",cex.main=2,axes=FALSE)
        ##         axis(side=1,cex.axis=2,lwd=5,padj=0.7)
        ##         abline(v=0.8,lwd=3)
        ##         dev.off()
        #         
        ##         pls_tab<-cbind(vip,coef)
        ##         colnames(pls_tab)<-c("VIP","Coefficient")
        ##         if (write_table) write.csv(pls_tab,paste(result_path,resultnames[ress],"_pls_results.csv",sep=""))
        #         #par(mar=c(5.1,4.1,4.1,2.1))
      }
    }
    
    if (verbosity > 0)
      cat("VIP PLSR results written into directory: ", outputPath, "\n")
  }
  
  #}
  
}
