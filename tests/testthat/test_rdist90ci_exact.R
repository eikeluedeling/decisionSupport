#
# file: test_rdist90ci_exact.R
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
# Test rdist90ci_exact(distribution, n, percentiles, quantiles)
##############################################################################################
context("Checking rdist90ci_exact()")

set.seed(100)
n=10000
tolerance=2/sqrt(n)

test_that("Standard normal distribution is generated correctly from the 0.05 and 0.95 quantiles", {
  mean=0
  sd=1
  percentiles=c(0.05, 0.95)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
  x<-rdist90ci_exact(distribution="norm", n=n, lower=quantiles[[1]], upper=quantiles[[2]])
  expect_equal(mean(x), mean, tolerance=tolerance)
  expect_equal(sd(x), sd, tolerance=tolerance)
})

test_that("A normal distribution is generated correctly from the 0.05 and 0.95 quantiles (1)", {
  mean=3
  sd=0.5
  percentiles=c(0.05, 0.95)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
  x<-rdist90ci_exact(distribution="norm", n=n, lower=quantiles[[1]], upper=quantiles[[2]])
  expect_equal(mean(x), mean, tolerance=tolerance)
  expect_equal(sd(x), sd, tolerance=tolerance)
})

test_that("A normal distribution is generated correctly from the 0.05 and 0.95 quantiles (2)", {
  mean=60000
  sd=24000
  percentiles=c(0.05, 0.95)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
  x<-rdist90ci_exact(distribution="norm", n=n, lower=quantiles[[1]], upper=quantiles[[2]])
  expect_equal(mean(x), mean, tolerance=tolerance)
  expect_equal(sd(x), sd, tolerance=tolerance)
})

test_that("A normal distribution is generated correctly from the 0.05 and 0.95 quantiles (3)", {
  lower=20000
  upper=100000
  percentiles=c(0.05, 0.95)
  x<-rdist90ci_exact(distribution="norm", n=n, lower=lower, upper=upper)
  expect_equal(quantile(x,probs=0.05)[["5%"]], lower, tolerance=1.5*tolerance)
  expect_equal(quantile(x,probs=0.95)[["95%"]], upper, tolerance=1.5*tolerance)
})

test_that("A normal distribution cannot be generated from the lower CI value (upper not given)", {
  mean=3
  sd=0.5
  percentiles=c(0.05)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
  expect_error(rdist90ci_exact(distribution="norm", n=n, lower=quantiles[[1]]))
})

test_that("A normal distribution cannot be generated from the lower CI value (upper=NULL)", {
  mean=3
  sd=0.5
  percentiles=c(0.05)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
    expect_error(rdist90ci_exact(distribution="norm", n=n, lower=quantiles[[1]], upper=NULL))
})

test_that("A normal distribution cannot be generated from the lower CI value (upper=NA)", {
  mean=3
  sd=0.5
  percentiles=c(0.05)
  quantiles=qnorm(p=percentiles, mean=mean, sd=sd)
  expect_error(rdist90ci_exact(distribution="norm", n=n, lower=quantiles[[1]], upper=NA))
})
