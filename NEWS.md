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
- Now use results of mcSimulation for plotting functions

# Updated plotting functions
- Add basic function `plot_cashflow` for plotting cashflow 
- Add general function `plot_distributions` for distribution plots 

# Work in progress
- Use linear regression model to generate values for a correction of the boxplot width (may be replaced with a exponential model, this might be a better fit for the kinds of differences that we generally see in NPVs and other variables)
- Started to work on the cashflow plot by adding the mcSimulation object (will generate a panel comparison cf. boxplot in plot_distribiutions)
