#
# file: rmetalog90ci.R
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
##############################################################################################
# rmetalog90ci(n, lower, median, upper, boundedness, bounds_lower, bounds_upper, relativeTolerance)
##############################################################################################
#' 90\%-confidence interval based Metalog random number generation.
#' 
#' \code{rmetalog90ci} generates random numbers from a \href{https://en.wikipedia.org/wiki/Metalog_distribution}{Metalog
#' distribution} that is fitted to the 5\%-, 50\%- and 95\%-quantiles, i.e. \code{lower}, \code{median}
#' and \code{upper}.
#' @param n \code{integer}: number of observations to be generated.
#' @param lower \code{numeric}: 5\%-quantile of the distribution to be fitted.
#' @param median \code{numeric}: 50\%-quantile of the distribution to be fitted. Unlike the other
#'   distributions supported by \code{\link{random.estimate1d}}, \code{median} is mandatory for the
#'   Metalog distribution, as \code{\link[rmetalog]{metalog}} requires at least 3 quantiles for fitting.
#' @param upper \code{numeric}: 95\%-quantile of the distribution to be fitted.
#' @param boundedness \code{character}: boundedness of the Metalog distribution to be fitted; one of
#'   \code{"u"} (unbounded, the default), \code{"sl"} (semi-bounded above \code{bounds_lower}),
#'   \code{"su"} (semi-bounded below \code{bounds_upper}) or \code{"b"} (bounded between
#'   \code{bounds_lower} and \code{bounds_upper}).
#' @param bounds_lower \code{numeric}: lower bound of the Metalog distribution; mandatory for
#'   \code{boundedness} \code{"sl"} and \code{"b"}.
#' @param bounds_upper \code{numeric}: upper bound of the Metalog distribution; mandatory for
#'   \code{boundedness} \code{"su"} and \code{"b"}.
#' @param relativeTolerance \code{numeric}: the relative tolerance level of deviation of the
#'   generated quantiles from the specified quantiles. If this deviation is greater than
#'   \code{relativeTolerance} a warning is given.
#' @details
#'   The Metalog distribution is fitted with \code{\link[rmetalog]{metalog}} (as a 3-term Metalog,
#'   since exactly 3 quantiles are supplied) and sampled with \code{\link[rmetalog]{rmetalog}} from
#'   package \pkg{rmetalog}, which must be installed to use this function.
#' @return A \code{numeric} vector of length \code{n} with the sampled values.
#' @seealso \code{\link[rmetalog]{metalog}}, \code{\link[rmetalog]{rmetalog}}
#' @examples
#' if ( requireNamespace("rmetalog", quietly = TRUE) ){
#'   x <- rmetalog90ci(n=10000, lower=100, median=250, upper=800, boundedness="sl", bounds_lower=0)
#'   quantile(x, probs=c(0.05, 0.5, 0.95))
#' }
#' @export
rmetalog90ci <- function(n, lower, median=NULL, upper,
                         boundedness=NA, bounds_lower=NA, bounds_upper=NA,
                         relativeTolerance=0.05){
  # Check preconditions:
  ## Namespace requirements:
  if (!requireNamespace("rmetalog", quietly = TRUE)) 
    stop("Package \"rmetalog\" needed for distribution=\"metalog\". Please install it.",
         call. = FALSE)
  if ( is.null(median) || length(median) != 1 || is.na(median) )
    stop("\"median\" must be supplied for distribution=\"metalog\" (at least 3 quantiles are needed to fit it).")
  lower<-as.numeric(lower)
  median<-as.numeric(median)
  upper<-as.numeric(upper)
  percentiles<-c(0.05, 0.5, 0.95)
  quantiles<-c(lower, median, upper)
  ## The Metalog distribution is always fitted with 3 terms, as exactly 3 quantiles are supplied:
  term_limit<-3
  ## Determine the boundedness and bounds of the Metalog distribution to be fitted:
  if ( is.null(boundedness) || length(boundedness) != 1 || is.na(boundedness) || boundedness == "" )
    boundedness<-"u"
  bounds_lower<-suppressWarnings(as.numeric(bounds_lower))
  bounds_upper<-suppressWarnings(as.numeric(bounds_upper))
  bounds<-switch(boundedness,
                "u" = c(0,1),
                "sl" = {
                  if ( length(bounds_lower) != 1 || is.na(bounds_lower) )
                    stop("\"bounds_lower\" must be supplied for boundedness=\"sl\".")
                  bounds_lower
                },
                "su" = {
                  if ( length(bounds_upper) != 1 || is.na(bounds_upper) )
                    stop("\"bounds_upper\" must be supplied for boundedness=\"su\".")
                  bounds_upper
                },
                "b" = {
                  if ( length(bounds_lower) != 1 || is.na(bounds_lower) ||
                       length(bounds_upper) != 1 || is.na(bounds_upper) )
                    stop("Both \"bounds_lower\" and \"bounds_upper\" must be supplied for boundedness=\"b\".")
                  c(bounds_lower, bounds_upper)
                },
                stop("\"boundedness\" must be one of \"u\", \"sl\", \"su\" or \"b\"."))
  # Fit the Metalog distribution to the given quantiles:
  metalogObject<-rmetalog::metalog(x=quantiles,
                                   probs=percentiles,
                                   term_limit=term_limit,
                                   term_lower_bound=term_limit,
                                   boundedness=boundedness,
                                   bounds=bounds)
  # Generate the random numbers:
  x<-rmetalog::rmetalog(metalogObject, n=n, term=term_limit)
  # Check postcondition (i.e. goodness of fit):
  quantiles_calc<-quantile(x=x, probs=percentiles)
  for ( j in seq_along(quantiles) ){
    scale<-if ( abs(quantiles[[j]]) > 0 ) abs(quantiles[[j]]) else NULL
    if ( !isTRUE( msg<-all.equal(quantiles[[j]], quantiles_calc[[j]], scale=scale, tolerance=relativeTolerance) ) ){
      warning("Fitted value of ", 100*percentiles[[j]], "%-quantile: ", quantiles_calc[[j]], "\n  ",
              "Target value of ", 100*percentiles[[j]], "%-quantile: ", quantiles[[j]], "\n  ",
              msg)
    }
  }
  # Return the sampled values:
  x
}
