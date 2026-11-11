#
# file: tests/testthat/test_rmetalog90ci.R
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
# method: rmetalog90ci() / distribution="metalog" for random.estimate1d()
##############################################################################################
context("Testing rmetalog90ci() / distribution=\"metalog\"")

testthat::skip_if_not_installed("rmetalog")

set.seed(100)
# Number of random numbers to be generated:
n<-100000
tolerance<-0.05

test_that("rmetalog90ci() recovers the supplied quantiles (unbounded)", {
  lower<-100
  median<-250
  upper<-800
  x<-rmetalog90ci(n=n, lower=lower, median=median, upper=upper)
  q<-quantile(x, probs=c(0.05, 0.5, 0.95))
  expect_equal(as.numeric(q), c(lower, median, upper), tolerance=tolerance)
})

test_that("rmetalog90ci() respects a semi-bounded-lower boundedness", {
  lower<-100
  median<-250
  upper<-800
  x<-rmetalog90ci(n=n, lower=lower, median=median, upper=upper, 
                  boundedness="sl", bounds_lower=0)
  expect_true(all(x > 0))
  q<-quantile(x, probs=c(0.05, 0.5, 0.95))
  expect_equal(as.numeric(q), c(lower, median, upper), tolerance=tolerance)
})

test_that("rmetalog90ci() respects a bounded boundedness", {
  lower<-100
  median<-250
  upper<-800
  x<-rmetalog90ci(n=n, lower=lower, median=median, upper=upper, 
                  boundedness="b", bounds_lower=0, bounds_upper=1000)
  expect_true(all(x > 0 & x < 1000))
})

test_that("rmetalog90ci() errors if median is missing", {
  expect_error(rmetalog90ci(n=n, lower=100, median=NA, upper=800))
})

test_that("rmetalog90ci() errors for an unknown boundedness or missing bounds", {
  expect_error(rmetalog90ci(n=n, lower=100, median=250, upper=800, boundedness="unknown"))
  expect_error(rmetalog90ci(n=n, lower=100, median=250, upper=800, boundedness="sl"))
  expect_error(rmetalog90ci(n=n, lower=100, median=250, upper=800, boundedness="su"))
  expect_error(rmetalog90ci(n=n, lower=100, median=250, upper=800, boundedness="b", bounds_lower=0))
})

test_that("distribution=\"metalog\" works via estimate1d()/random()", {
  est<-estimate1d("metalog", lower=100, upper=800, median=250)
  x<-random(est, n=n)
  q<-quantile(x, probs=c(0.05, 0.5, 0.95))
  expect_equal(as.numeric(q), c(100, 250, 800), tolerance=tolerance)
})

test_that("distribution=\"metalog\" errors via estimate1d()/random() if median is missing", {
  est<-estimate1d("metalog", lower=100, upper=800)
  expect_error(random(est, n=n))
})

test_that("distribution=\"metalog\" works alongside other distributions in a multivariate estimate()", {
  estimateObject<-as.estimate(
    variable=c("revenue", "costs"),
    distribution=c("metalog", "norm"),
    lower=c(100, 5000),
    upper=c(800, 50000),
    median=c(250, NA)
  )
  x<-random(estimateObject, n=n)
  expect_equal(colnames(x), c("revenue", "costs"))
  q<-quantile(x[,"revenue"], probs=c(0.05, 0.5, 0.95))
  expect_equal(as.numeric(q), c(100, 250, 800), tolerance=tolerance)
})
