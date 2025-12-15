# repfactoR

`repfactoR` is an R package for analyzing **replicated two-level factorial (2^k) designs** using linear models. It is intended for experiments where each factor has two levels (e.g., low/high or -1/+1) and some design points are replicated, enabling estimation of pure error and inference on both **main effects** and **interactions**.

## Rationale
Replicated 2^k factorial designs are efficient for studying multiple experimental factors and their interactions. In practice, it can be tedious to (1) ensure consistent -1/+1 coding, (2) report results on the factorial “effect” scale, and (3) produce standard plots for interpretation. `repfactoR` provides a simple workflow to fit the full factorial model and generate effect summaries and visuals in a reproducible way.

## What the package does
Core functions:

- `fit_rep_factorial()`: Fits a full factorial linear model (main effects + all interactions) and standardizes factor coding to -1/+1 when possible.
- `effects_table()`: Returns **Yates-scaled effect estimates** (effect = 2 × coefficient under -1/+1 coding) with confidence intervals.
- `anova_table()`: Returns an ANOVA-style coefficient summary (SE, t, p-values) plus effect-scaled estimates.
- `main_effects_plot()`: Main-effects plot (mean response at -1 vs +1 for each factor).
- `interaction_plots()`: Pairwise interaction plots for all factor pairs.
- `cube_plot3()`: Cube plot of fitted means for exactly **3** factors.

## Sample dataset
The package ships with a small coffee-preference dataset stored at `inst/extdata/coffee.csv`. It includes an outcome (`Overall_liking`) and three two-level factors (`Milk`, `Packaging`, `Species`) to demonstrate a replicated 2^3 factorial workflow.

## Example
```{r}
library(repfactoR)

dat <- read.csv(system.file("extdata", "coffee.csv", package = "repfactoR"))

fit <- fit_rep_factorial(
  dat,
  response = "Overall_liking",
  factors  = c("Milk", "Packaging", "Species")
)

effects_table(fit)
anova_table(fit)
main_effects_plot(fit)
interaction_plots(fit)
cube_plot3(fit)  # only works when there are exactly 3 factors
```

## How to install repfactoR in R
```{r}
install.packages("devtools")
library(devtools)
devtools::install_github("Sheen-T/repfactoR")
library(repfactoR)
```

