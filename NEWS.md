---
title: "NEWS"
author: "Lutz Göhring"
date: "May 19, 2015"
output: html_document
---

# 1.101.1 release

## Not so minor changes:

1.  in class estimate:
    + in documentation of estimate_read_csv(): 
      + if correlation matrix is supplied: uncorrelated elements have to be explicitly defined as 0 (zero).
2. in class mcSimulation:
    1. in method mcSimulation(): 
      1. functionSyntax=”globalNames” 
        + changed to "plainNamesDeprecated" and remvoed from documentation;
        + made the use of varnames in the model function superfluous;
      2. functionSyntax=”plainNames” implemented, tested and examples implemented and documented
    2. in method summary.mcSimulation():
      + default for probs exteded to probs=c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
3. in class welfareDecisionAnalysis:
    1. method hist.welareDecsisionAnalysis() added
    2. in method and documentation of welfareDecisionAnalysis(): 
      + Expected Net Benefit (ENB and $enb, respectively) changed to the semantically correct ENB(PA) and $enbPa, respectively.
4. in class eviSimulation:
    + method hist.eviSimulation() added
5. in method individualEvpiSimulation(): 
    + BUG in default for perfectProspectiveValues for the 1-d case removed
6. in method decisionSupport(): for option write_table=TRUE writing full MC results to csv file implemented and tested 
7. in folder tests:
    1. test_individualEvpiSimulation: Example Hubbard, ch7 implemented
    2. test_welfareDecisionAnalysis.R implemented;  
8.   vignette for individualEvpiSimulation(): 
    + first draft in folder R/todo added  

## Minor Changes:

 + File: NEWS.md added
 + Locally deleted files commited
