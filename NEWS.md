---
title: "DecisionSupport NEWS"
author: "Eduardo Fernandez, Cory Whitney, Eike Luedeling"
date: "May, 2020"
output: html_document
---

# version 1.105.2 is a patch
- Rebuilt for R 4.0
- Split the mcSimulation function into seperate R script files (as.data.frame.mcSimulation, hist.mcSimulation, print.mcSimulation, summary.mcSimulation, print.summary.mcSimulation), each for its own special purpose
- Updated documentation for the mcSimulation function
- Added notes to mcSimulation function about naming conventions 

# Updated plotting functions
- Add basic function `plot_cashflow` for plotting cashflow 
- Add general function `plot_distributions` for distribution plots 
