#
# file: tests/testthat/test_rposnorm90ci.R
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
# ToDo: is this necessary?
library(decisionSupport)

##############################################################################################
# Test rposnorm90ci.R(n, lower, upper)
##############################################################################################
context("Checking rposnorm90ci(method=\"numeric\")")

set.seed(100)
n=10000
tolerance=5/sqrt(n)

test_that("A positiv normal distribution is generated correctly from the 0.05 and 0.95 quantiles (1)", {
  lower=20000
  upper=100000
  percentiles=c(0.05, 0.95)
  x<-rposnorm90ci(n=n, lower=lower, upper=upper, relativeTolerance=tolerance, method="numeric")
  expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance, scale=min(abs(c(lower,upper))))
})

test_that("A positiv normal distribution is generated correctly from the 0.05 and 0.95 quantiles (2)", {
  lower=10000
  upper=100000
  percentiles=c(0.05, 0.95)
  x<-rposnorm90ci(n=n, lower=lower, upper=upper, relativeTolerance=tolerance, method="numeric")
  expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance, scale=min(abs(c(lower,upper))))
})
test_that("A positiv normal distribution is generated correctly from the 0.05 and 0.95 quantiles (3)", {
  lower=1000
  upper=100000
  percentiles=c(0.05, 0.95)
  x<-tryCatch(
    rposnorm90ci(n=n, lower=lower, upper=upper, method="numeric", relativeTolerance=tolerance),
    warning=function(w) FALSE  
  )
  if( is.numeric(x) )
    expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance)
})
