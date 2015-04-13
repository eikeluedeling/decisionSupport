#
# file: estimate.R
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
#' @include rmvnorm90ci_exact.R
#' @include random.R
#' @include estimate1d.R
NULL
# Define global variables:
if(getRversion() >= "2.15.1")  utils::globalVariables(c("variable",
                                                        "distribution"))
##############################################################################################
# estimate(..., correlation_matrix)
# ToDo: review documentation (if pre and postconditions are correct)
##############################################################################################
#' Create an Estimate Object
#'
#' This function creates an object of class \code{estimate}. #ToDo: detailed description
#' #ToDo: Implement characterization of distribution by mean and sd. Eventually, also by other quantiles.
#' @param ... arguments that can be coerced to a data frame comprising the base of the estimate.
#' @param correlation_matrix numeric matrix containing the correlations of the variables.
#' @details The parameters in \code{...} provide the base information of an estimate.
#' \subsection{The structure of the estimate base information (mandatory)}{
#'    Mandatory columns:
#'    \tabular{lll}{
#'      Column name         \tab  R-type    \tab Explanation\cr
#'      \code{distribution} \tab  \code{character} \tab  Distribution types \cr
#'      \code{variable}     \tab  \code{character} \tab  Variable names
#'    }
#' }
#' @return An object of type \code{estimate} which is a list whith components \code{base} and \code{correlation_matrix}.
#' \code{base} is a \code{\link{data.frame}} with mandatory column \code{distribution}. The \code{\link{row.names}} are the
#' names of the variables. \code{correlation_matrix} is a symmetric matrix with row and column names being the subset of
#' the variables supplied in \code{base} which are correlated. Its elements are the corresponding correlations.
#' @seealso \code{\link{row.names.estimate}}, \code{\link{names.estimate}}, \code{\link{corMat}}, \code{\link{estimate_read_csv}},
#' \code{\link{estimate_write_csv}}, \code{\link{random.estimate}}
#' @export
estimate<-function(..., correlation_matrix=NULL){
  base<-data.frame(..., stringsAsFactors=FALSE)
  if( !is.null(base$variable) ){
    rownames(base)<-base$variable
    base<-base[!colnames(base) %in% "variable"]
  }
  # Drop rows without variable name:
  base<-subset(base, row.names(base) != "")
  if( is.null(base$distribution) )
    stop("base must be supplied with a distribution column.")
  # Check preconditions of correlation_matrix:
  if( !is.null(correlation_matrix)){
    if( !is.matrix(correlation_matrix) )
      correlation_matrix<-as.matrix(correlation_matrix)
    if( !identical( correlation_matrix, t(correlation_matrix) ) )
      stop("correlationMatrix must be a symmetric matrix.")
    if( !identical( as.vector(diag(correlation_matrix)), rep(1, nrow(correlation_matrix)) ) )
      stop("All diagonal elements of correlation_matrix must be equal to 1.")
    #ToDo: check that all elements are between -1 and 1.
    #ToDo: Check that all rows are named
    #ToDo: check that rownames(correlation_matrix) is a subset of base names
  }
  # Return object:
  returnObject=list(base=base , correlation_matrix=correlation_matrix)
  class(returnObject)<-"estimate"
  returnObject
}
##############################################################################################
# row.names.estimate(x)
##############################################################################################
#' Return the variable names of an \code{estimate} object.
#'
#' This function returns the variable names of an \code{\link{estimate}} object which is identical to
#' \code{row.names(x$base)}.
#' @param x an \code{\link{estimate}} object.
#' @seealso \code{\link{estimate}}, \code{\link{names.estimate}}, \code{\link{corMat.estimate}}
#' @export
row.names.estimate<-function(x){
  row.names(x$base)
}
##############################################################################################
# names.estimate(x)
##############################################################################################
#' Return the column names of an \code{estimate} object.
#'
#' This function returns the column names of an \code{\link{estimate}} object which is identical to
#' \code{names(x$base)}.
#' @param x an \code{\link{estimate}} object.
#' @seealso \code{\link{estimate}}, \code{\link{row.names.estimate}}, \code{\link{corMat.estimate}}
#' @export
names.estimate<-function(x){
  names(x$base)
}
##############################################################################################
# generic: corMat(rho)
##############################################################################################
#' Return the Correlation Matrix of x.
#'
#' Return the correlation matrix of x.
#' @param rho a distribution.
#' @export
corMat <- function(rho) UseMethod("corMat")
##############################################################################################
# corMat.estimate(rho)
##############################################################################################
#' Return the correlation matrix of an \code{estimate} object.
#'
#' This function returns the full correlation matrix of an \code{\link{estimate}} object.
#' @param rho an \code{\link{estimate}} object.
#' @seealso \code{\link{estimate}}, \code{\link{row.names.estimate}}, \code{\link{names.estimate}}
#' @export
corMat.estimate<-function(rho){
  # Create identity matrix:
  corMat<-diag(nrow=length(row.names(rho)))
  dimnames(corMat)<-list(row.names(rho), row.names(rho))
  # Replace the values for correlated elements:
  namesCorrelated<-row.names(rho$correlation_matrix)
  corMat[namesCorrelated, namesCorrelated]<-rho$correlation_matrix[namesCorrelated, namesCorrelated]
  # Return full correlation matrix:
  corMat
}
###############################################################################################
# estimate_read_csv(fileName, strip.white=TRUE, ...)
# ToDo: review documentation (if pre and postconditions are correct)
##############################################################################################
#' Read an Estimate from CSV - File.
#'
#' This function reads an \code{\link{estimate}} from the specified csv files. In this context, an estimate of a variable is
#' defined by its distribution type, its 90\%-confidence interval \code{[lower,upper]} and its correlation to other variables.
#' #ToDo: Implement characterization of distribution by mean and sd. Eventually, also by other quantiles.
#' @param fileName Name of the file containing the base information of the estimate that should be read.
#' @param strip.white logical. Allows the stripping of leading and trailing white space from unquoted character fields
#' (numeric fields are always stripped). See \code{\link[base]{scan}} for further details (including the exact meaning of 'white space'),
#'  remembering that the columns may include the row names.
#'  @param ... Further parameters to be passed to \code{\link[utils]{read.csv}}.
#' @return An object of type \code{\link{estimate}}.
#' @details An estimate might consists of uncorrelated and correlated variables. This is reflected in the input file structure, which
#' is described in the following.
#' @section CSV input file structures:
#' The estimate is read from one or two csv files: the basic csv file which is mandatory and the correlation csv file which is optional.
#' The basic csv file contains the definition of the distribution of all variables ignoring potential correlations. The correlation csv
#' file only defines correlations.
#' \subsection{The structure of the basic input file (mandatory)}{
#'    File name structure: \code{<basic-filename>.csv}\cr
#'    Mandatory columns:
#'    \tabular{lll}{
#'      Column name         \tab  R-type    \tab Explanation\cr
#'      \code{lower}        \tab  \code{numeric}   \tab  ToDo \cr
#'      \code{upper}        \tab  \code{numeric}   \tab  ToDo \cr
#'      \code{distribution} \tab  \code{character} \tab  ToDo \cr
#'      \code{variable}     \tab  \code{character} \tab  ToDo
#'    }
#'    Optional columns:
#'    \tabular{lll}{
#'      Column name         \tab  R-type  \tab  Explanation \cr
#'      \code{description}  \tab  \code{character}  \tab  ToDo\cr
#'      \code{median}       \tab  \code{numeric}    \tab  ToDo\cr
#'      \code{start}        \tab  \code{integer}    \tab  ToDo\cr
#'      \code{end}          \tab  \code{integer}    \tab  ToDo\cr
#'      \code{indicator}    \tab  \code{logical}    \tab  ToDo
#'    }
#'    Columns without names are ignored. Rows where the \code{variable} field is empty are also dropped.
#' }
#' \subsection{The structure of the correlation file (optional)}{
#'    File name structure: \code{<basic-filename>_cor.csv}\cr
#'    Columns and rows are named by the corresponding variables. Only those variables need to be present which are correlated with others.
#'    The element \code{["rowname","columnname"]} contains the correlation between the variables \code{rowname} and \code{columnname}.
#'    Uncorrelated elements can be left empty, i.e. as \code{NA}, or defined as \code{0}. The element \code{["name","name"]} has to be
#'    set to \code{1}. The matrix must be given in symmetric form.
#' }
#' @seealso \code{\link{estimate_write_csv}}, \code{\link[utils]{read.csv}}, \code{\link{estimate}}
#' @export
estimate_read_csv <- function(fileName, strip.white=TRUE, ...){
  base<-NULL
  correlation_matrix<-NULL
  baseFilename<-fileName
  # Read basic data:
  #base<-read.csv(baseFilename,row.names="variable", strip.white=strip.white, stringsAsFactors=FALSE, ...)
  base<-read.csv(baseFilename, strip.white=strip.white, stringsAsFactors=FALSE, ...)
  # ToDo: replace subset() such that reference to global variable "variable" becomes obsolete:
  base<-subset(base,variable!="")
  base<-data.frame(base,row.names="variable")
  # Read correlation data:
  # Generate correlation filename:
  correlationFilename<-gsub(".csv","_cor.csv",baseFilename)
  # Read correlation file if it exists:
  if(file.exists(correlationFilename))
    correlation_matrix<-data.matrix(read.csv(correlationFilename, row.names=1))
  
  # Return object
  estimate(base, correlation_matrix=correlation_matrix)
}
###############################################################################################
# estimate_write_csv(estimate, fileName, strip.white=TRUE, ...)
# ToDo: review documentation (if pre and postconditions are correct)
##############################################################################################
#' Write an Estimate to CSV - File.
#'
#' This function writes an \code{\link{estimate}} to the specified csv file(s).
#' @param fileName \code{character}. Output file name which must end with \code{.csv}.
#' @param estimate  Estimate object to write to file \code{fileName}.
#' @param varNamesAsColumn \code{logical}; If \code{TRUE} the variable names will be written as a
#' separate column, otherwise as row names.
#' @param quote a \code{logical} value (TRUE or FALSE) or a numeric vector. If
#'   TRUE, any character or factor columns will be surrounded by double quotes.
#'   If a numeric vector, its elements are taken as the indices of columns to
#'   quote. In both cases, row and column names are quoted if they are written.
#'   If FALSE, nothing is quoted. Parameter is passed on to \code{\link{write.csv}}.
#' @param ... Further parameters to be passed to \code{\link[utils]{write.csv}}.
#' @return An object of type \code{\link{estimate}}.
#' @seealso \code{\link{estimate_read_csv}}, \code{\link{estimate}}, \code{\link[utils]{write.csv}}
#' @export
estimate_write_csv <- function(estimate, fileName, varNamesAsColumn=TRUE, quote=FALSE, ...){
  baseFilename=fileName
  # Write basic data to file:
  if (varNamesAsColumn){
    base<-cbind(estimate$base,variable=row.names(estimate))
    row.names(base)<-NULL
  } else
    base<-estimate$base
  write.csv(x=base, file=baseFilename, row.names=!varNamesAsColumn,  quote=FALSE, ...)
  # Write correlation data if exists:
  if( !is.null(estimate$correlation_matrix) ){
    # Generate correlation filename:
    correlationFilename<-gsub(".csv","_cor.csv",baseFilename)
    # Wirte correlation file:
    write.csv(x=estimate$correlation_matrix, file=correlationFilename, quote=FALSE, ...)
  }
}
##############################################################################################
# random.estimate(rho,n,method, ...)
##############################################################################################
#' Generate Random Numbers for an Estimate.
#'
#' This function generates random numbers for general multivariate
#' distributions that are defined as an \code{\link{estimate}}.
#' @param rho \code{estimate} object; Multivariate distribution to be randomly sampled.
#' @param n Number of generated observations
#' @param method Particular method to be used for random number generation.
#' @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the
#'   generated confidence interval from the specified interval. If this deviation is greater than
#'   \code{relativeTolerance} a warning is given.
#' @param ... Optional arguments to be passed to the particular random number
#'  generating function.
#' @details
#' 	\subsection{Generation of uncorrelated components}{
#' 		Implementation: \code{\link{random.estimate1d}}
#'
#' 	}
#' 	\subsection{Generation of correlated components}{
#' 		Implementation: \code{\link{rmvnorm90ci_exact}}
#' 	}
#' @examples
#'	variable=c("revenue","costs")
#'	distribution=c("norm","norm")
#'  lower=c(10000,  5000)
#'  upper=c(100000, 50000)
#'  estimateObject<-estimate(variable, distribution, lower, upper)
#'  x<-random(rho=estimateObject, n=10000)
#'  apply(X=x, MARGIN=2, FUN=quantile, probs=c(0.05, 0.95))
#'  cor(x)
#'  colnames(x)
#'  summary(x)
#'  hist(x[,"revenue"])
#'  hist(x[,"costs"])
#' @seealso \code{\link{estimate}}
#' @export
random.estimate <- function(rho,n,method="calculate", relativeTolerance=0.05, ...){
  #ToDo: implement generation of correlated variables
  #ToDo: test
  x<-NULL
  if ( !is.null(rho$correlation_matrix) ){
    # Select correlated variables:
    rhoCorrelated<-list(base=NULL,correlation_matrix=NULL)
    class(rhoCorrelated)<-"estimateCorrelated"
    namesCorrelated<-row.names(rho$correlation_matrix)
    rhoCorrelated$base<-rho$base[namesCorrelated, ]
    rhoCorrelated$correlation_matrix<-rho$correlation_matrix
    # Generate correlated variables
    x<-random(rho=rhoCorrelated,
              n=n,
              method=method,
              relativeTolerance=relativeTolerance,
              ...)
    # Select uncorrelated variables if there are any:
    if( length(namesUnCorrelated
               <-row.names(rho$base[!(row.names(rho$base) %in% namesCorrelated ),]) ) ){
      rhoUnCorrelated<-rho$base[namesUnCorrelated, ]
      class(rhoUnCorrelated)<-c("estimateUnCorrelated", class(rhoUnCorrelated))
      x<-cbind(x, random(rho=rhoUnCorrelated, 
                         n=n, 
                         method=method,
                         relativeTolerance=relativeTolerance,
                         ...))
    }
  } else {
    class(rho$base)<-c("estimateUnCorrelated", class(rho$base))
    x<-random(rho=rho$base, 
              n=n, 
              method=method, 
              relativeTolerance=relativeTolerance,
              ...)
  }
  # Return the generated random variables:
  x
}

##############################################################################################
# random.estimateCorrelated(rho,n,method, relativeTolerance, ...)
##############################################################################################
# Generate random numbers based on the first two moments of a certain probability distribution.
#
# This function generates random numbers for general multivariate
# distributions that can be characterized by the joint first two moments, viz.
# the mean and covariance.
# @param rho \code{estimateCorrelated} object; Multivariate distribution to be randomly sampled.
# @param n Number of generated observations
# @param method Particular method to be used for random number generation.
# @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the
#   generated confidence interval from the specified interval. If this deviation is greater than
#   \code{relativeTolerance} a warning is given.
# @param ... Optional arguments to be passed to the particular random number
#  generating function.
random.estimateCorrelated <- function(rho, n, method, relativeTolerance=0.05, ...){
  x<-NULL
  if(method=="calculate"){
    if( identical( rho$base$distribution, rep("norm", nrow(rho$base)) ) ){
      x<-rmvnorm90ci_exact(n=n,
                           lower=data.matrix(rho$base["lower"]),
                           upper=data.matrix(rho$base["upper"]),
                           correlationMatrix=rho$correlation_matrix)
    }
    else
      stop("correlated variables must all be of type \"norm\".")
  }
  else
    stop ("method must be  \"calculate\".")
  # Return the generated random numbers:
  x
}
##############################################################################################
# random.estimateUnCorrelated(rho , n,method, relativeTolerance, ...)
##############################################################################################
# Generate random numbers based on the first two moments of a uncorrelated probability distribution.
#
# This function generates random numbers for uncorrelated general multivariate
# distributions that can be characterized by the joint first two moments, viz.
# the mean and covariance.
# @param rho \code{estimateCorrelated} object; Multivariate distribution to be randomly sampled.
# @param n Number of generated observations
# @param method Particular method to be used for random number generation.
# @param relativeTolerance \code{numeric}; the relative tolerance level of deviation of the
#   generated confidence interval from the specified interval. If this deviation is greater than
#   \code{relativeTolerance} a warning is given.
# @param ... Optional arguments to be passed to the particular random number
#  generating function.
random.estimateUnCorrelated <- function(rho, n, method="calculate", relativeTolerance=0.05, ...){
  defaultMethod<-method
  x<-NULL
  #x<-apply(X=rho, MARGIN=1, FUN=random_estimate_1d, n=n, method=method)
  if(0){
#     x<-apply(X=rho, MARGIN=1,
#              FUN=function(rho, n, method, ...)
#                withCallingHandlers(random_estimate_1d(rho=rho, n=n, method=method, relativeTolerance=relativeTolerance, ...),
#                                    warning=function(w) {warning("Variable: ", print(rho), print(row.names(rho)), "\n", w$message, noBreaks. = TRUE)}),
#              n=n, method=method)
  }
  # Check if the estimates variables are supplied with individual methods, if yes, use them,
  # i.e. overwrite the option set with the function call (ToDo: move into random.estimate1d()):
  method<-rep(defaultMethod,length=length(row.names(rho)))
  names(method)<-row.names(rho)
  if( match("method", names(rho), nomatch = 0) ){
    indexOverwriteMethod<-!is.null(rho[,"method"]) & is.character(as.character(rho[,"method"])) & rho[,"method"]!=""
    method[indexOverwriteMethod] <- rho[indexOverwriteMethod,"method"]
  }
  for(i in row.names(rho)){
    if(0){
#       x<-cbind(x,matrix(withCallingHandlers(random_estimate_1d(rho=rho[i,], n=n, method=method[i], relativeTolerance=relativeTolerance, ...),
#                                             warning=function(w) warning("Variable: ", i, "\n", w$message, call. = FALSE, immediate.=TRUE)
#       ), nrow=n, ncol=1, dimnames=list(NULL,i)), deparse.level=1
#       )
    }
    x<-cbind(x,matrix(withCallingHandlers(random(rho=as.estimate1d(rho[i,]), n=n, method=method[i], relativeTolerance=relativeTolerance, ...),
                                          warning=function(w) warning("Variable: ", i, "\n", w$message, call. = FALSE, immediate.=TRUE),
                                          error=function(e) stop("Variable: ", i, "\n", e$message)
    ), nrow=n, ncol=1, dimnames=list(NULL,i)), deparse.level=1
    )
  }
  #  Return the sampled multivariate values:
  x
}
