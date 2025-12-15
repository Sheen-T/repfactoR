# repfactoR

`repfactoR` provides small, teaching-oriented helpers to analyze replicated 2^k factorial designs
(two-level factors, with replication). It focuses on a simple workflow:

1. Fit a full factorial linear model (all interactions) using {-1, +1} coding.
2. Report Yates-scaled effects (effect = 2 × coefficient).
3. Provide an ANOVA-style coefficient table.
4. Draw a cube plot (for exactly 3 factors) based on model-predicted means.

## Install (local)

```r
# install.packages("devtools")
devtools::install_local("repfactoR_v5")
```

## Example

```r
library(repfactoR)

fit <- fit_rep_factorial(
  toydata,
  response = "Overall_liking",
  factors  = c("Milk", "Packaging", "Species")
)

effects_table(fit)
anova_table(fit)
cube_plot3(fit)
```
