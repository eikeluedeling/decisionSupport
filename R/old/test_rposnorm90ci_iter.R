<<<<<<< HEAD
#
# file: test_rposnorm90ci_iter.R
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
# Test rposnorm90ci_iter(n, lower, upper)
##############################################################################################
context("Checking rposnorm90ci_iter()")

set.seed(100)
n=10000
tolerance=3/sqrt(n)

test_that("A positiv normal distribution is generated correctly from the 0.05 and 0.95 quantiles (1)", {
	lower=20000
	upper=100000
	percentiles=c(0.05, 0.95)
	x<-rposnorm90ci_iter(n=n, lower=lower, upper=upper, relativeTolerance=tolerance)
	expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance, scale=min(abs(c(lower,upper))))
})

test_that("A positiv normal distribution is generated correctly from the 0.05 and 0.95 quantiles (2)", {
	lower=10000
	upper=100000
	percentiles=c(0.05, 0.95)
	x<-rposnorm90ci_iter(n=n, lower=lower, upper=upper, relativeTolerance=tolerance)
	expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance, scale=min(abs(c(lower,upper))))
})

test_that("A positiv normal distribution is generated correctly from the 0.05 and 0.95 quantiles (3)", {
	lower=1000
	upper=100000
	percentiles=c(0.05, 0.95)
	if( inherits(try(expr=expect_warning(x<-rposnorm90ci_iter(n=n, lower=lower, upper=upper, relativeTolerance=tolerance))),
																			 "try-error")){																		 
	expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance, scale=min(abs(c(lower,upper))))
	}
})

=======
#
# file: test_rposnorm90ci_iter.R
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
# Test rposnorm90ci_iter(n, lower, upper)
##############################################################################################
context("Checking rposnorm90ci_iter()")

set.seed(100)
n=10000
tolerance=3/sqrt(n)

test_that("A positiv normal distribution is generated correctly from the 0.05 and 0.95 quantiles (1)", {
	lower=20000
	upper=100000
	percentiles=c(0.05, 0.95)
	x<-rposnorm90ci_iter(n=n, lower=lower, upper=upper, relativeTolerance=tolerance)
	expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance, scale=min(abs(c(lower,upper))))
})

test_that("A positiv normal distribution is generated correctly from the 0.05 and 0.95 quantiles (2)", {
	lower=10000
	upper=100000
	percentiles=c(0.05, 0.95)
	x<-rposnorm90ci_iter(n=n, lower=lower, upper=upper, relativeTolerance=tolerance)
	expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance, scale=min(abs(c(lower,upper))))
})

test_that("A positiv normal distribution is generated correctly from the 0.05 and 0.95 quantiles (3)", {
	lower=1000
	upper=100000
	percentiles=c(0.05, 0.95)
	if( inherits(try(expr=expect_warning(x<-rposnorm90ci_iter(n=n, lower=lower, upper=upper, relativeTolerance=tolerance))),
																			 "try-error")){																		 
	expect_equal(quantile(x=x, probs=c(0.05, 0.95)), c("5%"=lower, "95%"=upper), tolerance=tolerance, scale=min(abs(c(lower,upper))))
	}
})

>>>>>>> 48e144dcc6c2a9ad66698d489d3691d829d8cd4d
