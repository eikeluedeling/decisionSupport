#
# file: estimate_read_csv_old.R
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
#' @include estimate.R
NULL
###############################################################################################
# estimate_read_csv_old(fileName, strip.white=TRUE, ...)
# ToDo: review documentation (if pre and postconditions are correct)
##############################################################################################
#' Read an Estimate from CSV - File (depreciated standard).
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
#'    File name structure: \code{<basic-filename>.csv_correlations.csv}\cr
#'    #ToDo 
#' }
#' @seealso \code{\link{estimate_read_csv}}, \code{\link[utils]{read.csv}}, \code{\link{estimate}}
#' @export
estimate_read_csv_old<-function(fileName, strip.white=TRUE, ...){
	base<-NULL
	#	correlation_matrix<-NULL
	baseFilename=fileName
	# Read basic data:
	base<-read.csv(baseFilename, strip.white=strip.white, stringsAsFactors=FALSE, ...)
	base<-subset(base,variable!="")
	base<-data.frame(base,row.names="variable")
	# Transform number format: remove ',' as thousand separator:
	base[,"lower"]<-as.numeric(as.numeric(gsub(",","", as.character(base[,"lower"]))))
	base[,"upper"]<-as.numeric(as.numeric(gsub(",","", as.character(base[,"upper"]))))


	# Read correlation file:
	if( length(base$distribution[base$distribution=="correlated"]) > 0 ){
		correlated_variables<-estimate_read_csv_old_correlation(paste(fileName,"_correlations.csv",sep=""))
	}

	# Merge basic information and correlation information (ToDo: check):
	if( !setequal(intersect(row.names(base), row.names(correlated_variables$base)),
					 row.names(subset(x=base,subset=(distribution=="correlated")))) )
		stop("Variables in base file indicated as correlated are not the same as in correlation file.")


	base<-subset(x=base,subset=(distribution!="correlated"))
	baseCor<-correlated_variables$base
#	mergedBase<-data.frame(merge(base,baseCor,by=c("row.names",intersect(names(base),names(baseCor)))),row.names="Row.names")
	mergedBase<-data.frame(merge(base,baseCor,by=c("row.names",intersect(names(base),names(baseCor))),all.y=TRUE),row.names="Row.names")
	base<-rbind(subset(x=base,subset=(distribution!="correlated")),
							mergedBase)

	# Transform old to new syntax
	base$distribution[base$distribution=="constant"]<-"const"
	base$distribution[base$distribution=="normal"]<-"norm"
	base$distribution[base$distribution=="pos_normal"]<-"posnorm"
	base$distribution[base$distribution=="normal_0_1"]<-"0_1norm"

	# Return:
	estimate(base,correlation_matrix=correlated_variables$correlation_matrix)
}

###############################################################################################
# estimate_read_csv_old_correlation(inFileName)
# ToDo: review documentation (if pre and postconditions are correct)
##############################################################################################
# Read correlated variables:
estimate_read_csv_old_correlation<-function(inFileName){
	# Initialization:
	correlationMatrix<-NULL
	base<-NULL
	
	tabl<-read.csv(inFileName, header=FALSE,stringsAsFactors=FALSE)
	block_starts<-which(tabl[,1]=="variable")
	if(length(block_starts)==1) block_ends<-nrow(tabl) else
		block_ends<-c(block_starts[2:length(block_starts)]-1,nrow(tabl))
	
	for(i in 1:length(block_starts)){
		col_names<-as.character(unlist(tabl[block_starts[i],]))
		col_names<-sapply(tabl[block_starts[i],],as.character)
		block<-tabl[(block_starts[i]+1):block_ends[i],]
		colnames(block)<-as.character(col_names)
		block[,"lower"]<-as.numeric(as.numeric(gsub(",","", as.character(block[,"lower"]))))
		block[,"upper"]<-as.numeric(as.numeric(gsub(",","", as.character(block[,"upper"]))))
		block<-block[which(!is.na(block[,"upper"])),]
		
		block<-data.frame(block,row.names="variable")
		
		baseBlock<-block[!(names(block) %in% row.names(block))]
		correlationMatrixBlock<-block[(names(block) %in% row.names(block))]
		# Replace '#' by the transposed numeric value
		correlationMatrixBlock[which(correlationMatrixBlock=="#", arr.ind=TRUE)]<-
			t(correlationMatrixBlock)[which(correlationMatrixBlock=="#", arr.ind=TRUE)]
		
		base<-rbind(base,baseBlock)
		if( is.null(correlationMatrix) ){
			correlationMatrix<-correlationMatrixBlock
		}
		else{
			correlationMatrix	<- merge(correlationMatrix, correlationMatrixBlock, by = "row.names", all = TRUE)
			correlationMatrix[is.na(correlationMatrix)] <- 0
			correlationMatrix	<- data.frame(correlationMatrix, row.names="Row.names")
		}
	}
	correlationMatrix<-data.matrix(correlationMatrix)[row.names(base), row.names(base)]
	# Return:
	estimate(base,correlation_matrix=correlationMatrix)
}
