#
# file: tests/testthat/test_individualEvpiSimulation.R
#
# This file is part of the R-package decisionSupport
# 
# Authors: 
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <eike@eikeluedeling.com>
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
context("Testing individualEvpiSimulation()")

set.seed(100)
# Number of model runs for the underlying Monte Carlo simulation:
n= 1000
tolerance=3/sqrt(n)

test_that("Individual EVPI is calculated for 2-D correltated current estimate.",{
  # Define the model for the profit:
  profitModel <- function(x){
    x$revenue-x$costs
  }
  # Read the estimate for revenue and costs:
  profitEstimate<-estimate_read_csv("profit-2.csv")
  # Run the EVPI Simulation:
  evpiSimulation <- individualEvpiSimulation(welfare=profitModel, 
                                             currentEstimate=profitEstimate, 
                                             numberOfModelRuns=n,
                                             randomMethod="calculate",
                                             functionSyntax="data.frameNames")
})
test_that("Individual EVPI is calculated for 3-D correlated current estimate.",{
  # Define the model for the profit:
  profitModel <- function(x){
    x$revenue1 + x$revenue2 -x$costs1
  }
  # Create an estimate from text (with correlated components):
  estimateTextMarg<-"variable,  distribution, lower,  upper
                     revenue1,          norm,   500,   1000
                     revenue2,          norm,    50,   2000
                       costs1,          norm,    50,   1100"
  estimateTextCor<-",         revenue1, revenue2, costs1
                  revenue1,          1,     -0.3,    0.5
                  revenue2,       -0.3,        1,    0.2
                  costs1,          0.5,      0.2,      1"
  profitEstimate<-as.estimate(read.csv(header=TRUE, text=estimateTextMarg,
                                       strip.white=TRUE, stringsAsFactors=FALSE),
                              correlation_matrix=data.matrix(read.csv(text=estimateTextCor,
                                                                      row.names=1,
                                                                      strip.white=TRUE)))
  # Run the EVPI Simulation:
  evpiSimulation <- individualEvpiSimulation(welfare=profitModel, 
                                             currentEstimate=profitEstimate, 
                                             numberOfModelRuns=n,
                                             randomMethod="calculate",
                                             functionSyntax="data.frameNames")
})
test_that("Individual EVPI is calculated for 4-D partly correlated current estimate.",{
  # Define the model for the profit:
  profitModel <- function(x){
    x$revenue1 + x$revenue2 - (x$costs1 + x$costs2)
  }
  # Create an estimate from text (with correlated components):
  estimateTextMarg<-"variable,  distribution,    lower,   upper
                    revenue1,           norm,      100,    1000
                    revenue2,           norm,       50,    2000
                      costs1,           norm,       50,    2000
                      costs2,           norm,      100,    1000"
  estimateTextCor<-",         revenue1, costs2
                  revenue1,        1,   -0.3
                  costs2,       -0.3,      1"
  profitEstimate<-as.estimate(read.csv(header=TRUE, text=estimateTextMarg,
                                       strip.white=TRUE, stringsAsFactors=FALSE),
                              correlation_matrix=data.matrix(read.csv(text=estimateTextCor,
                                                                      row.names=1,
                                                                      strip.white=TRUE)))
  # Run the EVPI Simulation:
  evpiSimulation <- individualEvpiSimulation(welfare=profitModel, 
                                             currentEstimate=profitEstimate, 
                                             numberOfModelRuns=n,
                                             randomMethod="calculate",
                                             functionSyntax="data.frameNames")
})
test_that("Individual EVPI is calculated for 4-D uncorrelated current estimate
           and 2 dimensional unnamed model function
          (randomMethod=\"calculate\", functionSyntax=\"globalNames\") (1).",{
            # Number of simulations:
            n=10
            # Create the current estimate from text:
            estimateText<-"variable,  distribution, lower, upper
            revenue1,  posnorm,      100,   1000
            revenue2,  posnorm,      50,    2000
            costs1,    posnorm,      50,    2000
            costs2,    posnorm,      100,   1000"
            currentEstimate<-as.estimate(read.csv(header=TRUE, text=estimateText,
                                                  strip.white=TRUE, stringsAsFactors=FALSE))
            # The welfare function:
            profitModel <- function(x,varnames){
              # Assign the variable names to the function environement:
              tt<-table(varnames)
              for (t in 1:length(tt))
                assign(names(tt[t]),as.numeric(x[which(varnames==names(tt[t]))]))
              
              list(revenue1 + revenue2 - costs1 - costs2, revenue1)
            }
            # Calculate the Individual EVPI:
            individualEvpiResult<-individualEvpiSimulation(welfare=profitModel,
                                                           currentEstimate=currentEstimate,
                                                           numberOfModelRuns=n,
                                                           functionSyntax="globalNames",
                                                           verbosity=0)
          })
