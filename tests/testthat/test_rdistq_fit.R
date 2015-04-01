#
# file: test_rdistq_fit.R
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
# ToDo: is this necessary?
library(decisionSupport)

##############################################################################################
# Test rdistq_fit(distribution, n, percentiles, quantiles)
##############################################################################################
context("Checking rdistq_fit()")

n=10000
tolerance=2/sqrt(n)

test_that("Standard normal distribution is generated correctly from the 0.05 and 0.95 quantiles", {
  mean=0
  sd=1
  percentiles=c(0.05, 0.95)
  (quantiles=qnorm(p=percentiles, mean=mean, sd=sd))
  x<-rdistq_fit(distribution="norm", n=n, percentiles=percentiles, quantiles=quantiles)
  expect_equal(mean(x), mean, tolerance=tolerance)
  expect_equal(sd(x), sd, tolerance=tolerance)
})

test_that("A normal distribution is generated correctly from the 0.05 and 0.95 quantiles (1)", {
  mean=3
  sd=0.5
  percentiles=c(0.05, 0.95)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
  x<-rdistq_fit(distribution="norm", n=n, percentiles=percentiles, quantiles=quantiles)
  expect_equal(mean(x), mean, tolerance=tolerance)
  expect_equal(sd(x), sd, tolerance=tolerance)
})

test_that("A normal distribution is generated correctly from the 0.05 and 0.95 quantiles, if no warning occurs (1)", {
  mean=60000
  sd=24000
  percentiles=c(0.05, 0.95)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
  if( inherits(try(expr=expect_warning(x<-rdistq_fit(distribution="norm", n=n, percentiles=percentiles, quantiles=quantiles))),
               "try-error")){
    expect_equal(mean(x), mean, tolerance=tolerance)
    expect_equal(sd(x), sd, tolerance=tolerance)
  }
})


test_that("A normal distribution is generated correctly from the 0.05, 0.10, 0.17, 0.45 and 0.95 quantiles", {
  mean=3
  sd=0.5
  percentiles=c(0.05, 0.10, 0.17, 0.45, 0.95)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
  x<-rdistq_fit(distribution="norm", n=n, percentiles=percentiles, quantiles=quantiles)
  expect_equal(mean(x), mean, tolerance=tolerance)
  expect_equal(sd(x), sd, tolerance=tolerance)
})

test_that("A normal distribution cannot be generated from only one quantile, here 0.05", {
  mean=3
  sd=0.5
  percentiles=c(0.05)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
  expect_error(rdistq_fit(distribution="norm", n=n, percentiles=percentiles, quantiles=quantiles)
  )
})
