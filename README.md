
<!-- README.md is generated from README.Rmd. Please edit that file -->

# decisionSupport <img src="https://raw.githubusercontent.com/eikeluedeling/decisionSupport/master/vignettes/decisionSupport.png" align="right" height="120"/>

<!-- badges: start -->
[![CRAN Status](https://www.r-pkg.org/badges/version/decisionSupport)](https://cran.r-project.org/package=decisionSupport)
[![R-CMD-check](https://github.com/eikeluedeling/decisionSupport/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eikeluedeling/decisionSupport/actions/workflows/R-CMD-check.yaml)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/decisionSupport)](https://cran.r-project.org/package=decisionSupport)
<!-- badges: end -->

**Quantitative Support of Decision Making under Uncertainty**

`decisionSupport` implements Monte Carlo simulation-based decision analysis for
development contexts. It allows users to:

- Define uncertain input variables as probability distributions
- Run Monte Carlo simulations of decision models
- Calculate the **Expected Value of Perfect Information (EVPI)** to prioritise research
- Visualise outcomes, sensitivities, and value of information

The approach is particularly useful for agricultural development, natural
resource management, and policy analysis where decisions must be made with
limited and uncertain data.

## Installation

``` r
# From CRAN
install.packages("decisionSupport")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("eikeluedeling/decisionSupport")
```

## Quick Example

``` r
library(decisionSupport)

# 1. Define input estimates (CSV or data.frame)
input_estimates <- estimate_read_csv("your_estimates.csv")

# 2. Write a model function
model_function <- function(x, varnames) {
  revenue <- x$yield * x$price * x$area
  cost    <- x$labour + x$inputs
  profit  <- revenue - cost
  return(list(profit = profit))
}

# 3. Run the Monte Carlo simulation
results <- mcSimulation(
  estimate = input_estimates,
  model_function = model_function,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)

# 4. Visualise results
plot_distributions(results, vars = "profit")
compound_figure(results, input_table = input_estimates,
                decision_var_name = "profit")
```

## Key Functions

| Function | Purpose |
|----------|---------|
| `mcSimulation()` | Run Monte Carlo simulation |
| `estimate_read_csv()` | Read input estimates from CSV |
| `plot_distributions()` | Visualise outcome distributions |
| `plot_cashflow()` | Plot time-series cashflows |
| `plot_pls()` | PLS-based sensitivity analysis |
| `plot_evpi()` | Visualise Value of Information |
| `compound_figure()` | Multi-panel decision summary |
| `multi_EVPI()` | Multi-variable EVPI analysis |
| `empirical_EVPI()` | Empirical EVPI calculation |
| `vv()` | Add temporal variability |
| `chance_event()` | Model probabilistic events |
| `discount()` | NPV discounting |
| `gompertz_yield()` | Gompertz growth curves |

## Learning Resources

- CRAN vignettes: `browseVignettes("decisionSupport")`
- [Teaching materials](https://cory-whitney.shinyapps.io/decisionSupport_course/) by Cory Whitney (University of Bonn)
- Lanzanova et al. (2019). [Improving development efficiency through decision analysis](https://doi.org/10.1016/j.worlddev.2019.06.005). *World Development*
- Luedeling & Shepherd (2016). [Decision-focused agricultural research](https://doi.org/10.1007/s11540-016-9318-7). *Solutions*

## Citation

``` r
citation("decisionSupport")
```

## Contributing

Contributions are welcome! Please open an issue or pull request on
[GitHub](https://github.com/eikeluedeling/decisionSupport/issues).

## License

GPL-3
