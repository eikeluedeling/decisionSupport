#
# file: tests/testthat/test_mcSimulation.R
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
context("Testing mcSimulation()")

set.seed(100)
# Number of Monte Carlo Simulations:
n= 1000000
tolerance=3/sqrt(n)

test_that("Difference of two uncorrelated normally distributed variables is normally distributed
          (randomMethod=\"calculate\", functionSyntax=\"data.frameNames\") (1).",{
            # Define the model for the profit:
            profitModel <- function(x){
              x$revenue-x$costs
            }
            # Read the estimate for revenue and costs:
            profitEstimate<-estimate_read_csv("profit-1.csv")
            # Calculate means from 95%-confidence intervalls:
            meanRevenue <- mean(c(profitEstimate$base["revenue","lower"], profitEstimate$base["revenue","upper"]) ) 
            meanCosts <- mean(c(profitEstimate$base["costs","lower"], profitEstimate$base["costs","upper"]) ) 
            # Calculate standard deviations from 95%-confidence intervalls:
            sdRevenue <- 0.5 * (profitEstimate$base["revenue","upper"] - profitEstimate$base["revenue","lower"]) / qnorm(0.95)
            sdCosts <- 0.5 * (profitEstimate$base["costs","upper"] - profitEstimate$base["costs","lower"]) / qnorm(0.95)
            # Calculate expected moments for profit = revenue - costs:
            meanProfitExpected <- meanRevenue - meanCosts
            sdProfitExpected <- sqrt( sdRevenue^2 + sdCosts^2)
            # Run the Monte Carlo Simulation:
            profitSimulation <- mcSimulation(estimate=profitEstimate, 
                                           model_function=profitModel, 
                                           numberOfSimulations=n,
                                           randomMethod="calculate",
                                           functionSyntax="data.frameNames")
            expect_equal(mean(profitSimulation$y$y), meanProfitExpected, tolerance=tolerance)
            expect_equal(sd(profitSimulation$y$y), sdProfitExpected, tolerance=tolerance)
          })

test_that("Difference of two uncorrelated normally distributed variables is normally distributed
          (randomMethod=\"calculate\", functionSyntax=\"data.frameNames\") (2).",{
            # Define the model for the profit:
            profitModel <- function(x){
              x[["revenue"]]-x[["costs"]]
            }
            # Read the estimate for revenue and costs:
            profitEstimate<-estimate_read_csv("profit-1.csv")
            # Calculate means from 95%-confidence intervalls:
            meanRevenue <- mean(c(profitEstimate$base["revenue","lower"], profitEstimate$base["revenue","upper"]) ) 
            meanCosts <- mean(c(profitEstimate$base["costs","lower"], profitEstimate$base["costs","upper"]) ) 
            # Calculate standard deviations from 95%-confidence intervalls:
            sdRevenue <- 0.5 * (profitEstimate$base["revenue","upper"] - profitEstimate$base["revenue","lower"]) / qnorm(0.95)
            sdCosts <- 0.5 * (profitEstimate$base["costs","upper"] - profitEstimate$base["costs","lower"]) / qnorm(0.95)
            # Calculate expected moments for profit = revenue - costs:
            meanProfitExpected <- meanRevenue - meanCosts
            sdProfitExpected <- sqrt( sdRevenue^2 + sdCosts^2)
            # Run the Monte Carlo Simulation:
            profitSimulation <- mcSimulation(estimate=profitEstimate, 
                                             model_function=profitModel, 
                                             numberOfSimulations=n,
                                             randomMethod="calculate",
                                             functionSyntax="data.frameNames")
            expect_equal(mean(profitSimulation$y$y), meanProfitExpected, tolerance=tolerance)
            expect_equal(sd(profitSimulation$y$y), sdProfitExpected, tolerance=tolerance)
          })

test_that("Difference of two uncorrelated normally distributed variables is normally distributed
          (randomMethod=\"calculate\", functionSyntax=\"matrixNames\").",{
            # Define the model for the profit:
            profitModel <- function(x){
              x[,"revenue"]-x[,"costs"]
            }
            # Read the estimate for revenue and costs:
            profitEstimate<-estimate_read_csv("profit-1.csv")
            # Calculate means from 95%-confidence intervalls:
            meanRevenue <- mean(c(profitEstimate$base["revenue","lower"], profitEstimate$base["revenue","upper"]) ) 
            meanCosts <- mean(c(profitEstimate$base["costs","lower"], profitEstimate$base["costs","upper"]) ) 
            # Calculate standard deviations from 95%-confidence intervalls:
            sdRevenue <- 0.5 * (profitEstimate$base["revenue","upper"] - profitEstimate$base["revenue","lower"]) / qnorm(0.95)
            sdCosts <- 0.5 * (profitEstimate$base["costs","upper"] - profitEstimate$base["costs","lower"]) / qnorm(0.95)
            # Calculate expected moments for profit = revenue - costs:
            meanProfitExpected <- meanRevenue - meanCosts
            sdProfitExpected <- sqrt( sdRevenue^2 + sdCosts^2)
            # Run the Monte Carlo Simulation:
            profitSimulation <- mcSimulation(estimate=profitEstimate, 
                                             model_function=profitModel, 
                                             numberOfSimulations=n,
                                             randomMethod="calculate",
                                             functionSyntax="matrixNames")
            expect_equal(mean(profitSimulation$y$y), meanProfitExpected, tolerance=tolerance)
            expect_equal(sd(profitSimulation$y$y), sdProfitExpected, tolerance=tolerance)
          })

test_that("Difference of two correlated normally distributed variables is normally distributed
          (randomMethod=\"calculate\", functionSyntax=\"data.frameNames\") (1).",{
          	# Define the model for the profit:
          	profitModel <- function(x){
          		x$revenue-x$costs
          	}
          	# Read the estimate for revenue and costs:
          	profitEstimate<-estimate_read_csv("profit-2.csv")
          	# Calculate means from 95%-confidence intervalls:
          	meanRevenue <- mean(c(profitEstimate$base["revenue","lower"], profitEstimate$base["revenue","upper"]) ) 
          	meanCosts <- mean(c(profitEstimate$base["costs","lower"], profitEstimate$base["costs","upper"]) ) 
          	# Calculate standard deviations from 95%-confidence intervalls:
          	sdRevenue <- 0.5 * (profitEstimate$base["revenue","upper"] - profitEstimate$base["revenue","lower"]) / qnorm(0.95)
          	sdCosts <- 0.5 * (profitEstimate$base["costs","upper"] - profitEstimate$base["costs","lower"]) / qnorm(0.95)
          	covRevenueCosts <- sdRevenue * sdCosts * profitEstimate$correlation_matrix["revenue","costs"]
          	# Calculate expected moments for profit = revenue - costs:
          	meanProfitExpected <- meanRevenue - meanCosts
          	sdProfitExpected <- sqrt( sdRevenue^2 - 2*covRevenueCosts + sdCosts^2)
          	# Run the Monte Carlo Simulation:
          	profitSimulation <- mcSimulation(estimate=profitEstimate, 
          																	 model_function=profitModel, 
          																	 numberOfSimulations=n,
          																	 randomMethod="calculate",
          																	 functionSyntax="data.frameNames")
          	expect_equal(mean(profitSimulation$y$y), meanProfitExpected, tolerance=tolerance)
          	expect_equal(sd(profitSimulation$y$y), sdProfitExpected, tolerance=tolerance)
          })
