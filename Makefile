#
# file: Makefile
#
# R package: decisionSupport
#
# Authors (ToDo order?):
#   Lutz Göhring <lutz.goehring@gmx.de>
#   Eike Luedeling (ICRAF) <E.Luedeling@cgiar.org>
#
# Affiliation: World Agroforestry Centre (ICRAF)
#
# License: ToDo
#
##############################################################################################
.PHONY: doc

doc: 
	R CMD Rd2pdf . --output=doc/decisionSupport.pdf --force

