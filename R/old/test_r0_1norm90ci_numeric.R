#
# file: tests/testthat/test_r0_1norm90ci_numeric.R
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
# Test rtnorm_0_1_90ci(n, lower, upper, method="numeric")
##############################################################################################
context("Checking rtnorm_0_1_90ci(method=\"numeric\")")

set.seed(100)
n=10000
tolerance=5/sqrt(n)

test_that("Preconditions are checked: : upper value must be less than one.", {
  lower=20000
  upper=100000
  percentiles=c(0.05, 0.95)
  expect_error(rtnorm_0_1_90ci(n=n, lower=lower, upper=upper, method="numeric", relativeTolerance=tolerance))
})
test_that("A 0-1-truncated normal distribution is generated correctly from the 0.05 and 0.95 quantiles (1)", {
  lower=0.1
  upper=0.5
  percentiles=c(0.05, 0.95)
  x<-rtnorm_0_1_90ci(n=n, lower=lower, upper=upper, method="numeric", relativeTolerance=tolerance)
  expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance, scale=min(abs(c(lower,upper))))
})
test_that("A 0-1-truncated  normal distribution is generated correctly from the 0.05 and 0.95 quantiles (2)", {
  lower=0.01
  upper=0.5
  percentiles=c(0.05, 0.95)
  x<-tryCatch(
    rtnorm_0_1_90ci(n=n, lower=lower, upper=upper, method="numeric", relativeTolerance=tolerance),
    warning=function(w) FALSE 
  )
  if( is.numeric(x) )
    expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance)
})
