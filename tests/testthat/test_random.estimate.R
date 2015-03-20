#
# file: test_random.estimate.R
#
# R package: decisionSupport
# 
# Authors (ToDo order?): 
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <E.Luedeling@cgiar.org>
#
# Affiliation: ToDo
# 
# License: ToDo
#
##############################################################################################
context("Testing random.estimate()")

set.seed(100)
# Number of random numbers to be generated:
n= 10000
tolerance=3/sqrt(n)

test_that("3d - standard normal distribution (correlated) is generated correctly from the 0.05 and 0.95 quantiles", {
	# Read the estimate for revenue and costs:
	profitEstimate<-estimate_read_csv("profit-4.csv")
	# Calculate means from 95%-confidence intervalls:
	meanProductPrice <- mean(c(profitEstimate$base["productprice","lower"], profitEstimate$base["productprice","upper"]) ) 
	meanCostPrice <- mean(c(profitEstimate$base["costprice","lower"], profitEstimate$base["costprice","upper"]) ) 
	meanSales <- mean(c(profitEstimate$base["sales","lower"], profitEstimate$base["sales","upper"]) )
	mean <- c(productprice=meanProductPrice, costprice=meanCostPrice, sales=meanSales)
	# Calculate standard deviations from 95%-confidence intervalls:
	sdProductPrice <- 0.5 * (profitEstimate$base["productprice","upper"] - profitEstimate$base["productprice","lower"]) / qnorm(0.95)
	sdCostPrice <- 0.5 * (profitEstimate$base["costprice","upper"] - profitEstimate$base["costprice","lower"]) / qnorm(0.95)
	sdSales <- 0.5 * (profitEstimate$base["sales","upper"] - profitEstimate$base["sales","lower"]) / qnorm(0.95)
	sd <- c(productprice=sdProductPrice, costprice=sdCostPrice, sales=sdSales)
	# Extract the full correlation matrix:
	cor<-corMat(profitEstimate)
	# Generate the random numbers:
	x<-random(rho=profitEstimate,n=n)
	# Test:
	expect_equal(colMeans(x), mean, tolerance=tolerance)	
	expect_equal(apply(X=x, MARGIN=2, sd), sd, tolerance=tolerance)
	expect_equal(cor(x),cor, tolerance=0.05) 
})
