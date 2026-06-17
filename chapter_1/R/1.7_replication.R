# =============================================================================
# Hayashi (2000) Econometrics -- Chapter 1
# Replication of Section 1.7: Nerlove (1963)
# Returns to Scale in Electricity Supply
# =============================================================================
# Data: hatashir::nerlove (145 U.S. electricity firms, 1955)
# Goal: Replicate equations 1.7.7, 1.7.8, 1.7.9 and Figure 1.7

# install.packages("remotes")
# remotes::install_github("lachlandeer/hayashir")
# install for the first time

library(hayashir)
library(tidyverse)
library(broom)

data("nerlove")

# -- 1. Inspect ---------------------------------------------------------------
glimpse(nerlove)
summary(nerlove)

# -- 2. Log transforms --------------------------------------------------------
nerlove <- nerlove |>
  mutate(
    # Primary log variables
    lTC = log(total_cost),                   # log total cost    -dependent variable
    lQ  = log(output),               # log output
    lPL = log(price_labor),                   # log wage rate      (p_i1)
    lPK = log(price_capital),                   # log capital price  (p_i2)
    lPF = log(price_fuel),                   # log fuel price     (p_i3)
    # For restricted model [1.7.6]: divide TC and prices by PF
    lTC_PF = lTC - lPF,
    lPL_PF = lPL - lPF,
    lPK_PF = lPK - lPF,
    # For R^2 detour [1.7.4']: average cost as dependent variable
    lAC = lTC - lQ
  )
