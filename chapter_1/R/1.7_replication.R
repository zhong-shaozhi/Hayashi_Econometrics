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

# -- 3. Unrestricted OLS: equation [1.7.4] ------------------------------------
ols_u <- lm(lTC ~ lQ + lPL + lPK + lPF, data = nerlove)

# Read the results as a clean table
tidy(ols_u) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

# Check model-level statistics
glance(ols_u) |> select(r.squared, sigma, nobs)

# Extract SSR manually
SSR_U <- deviance(ols_u)
SSR_U

# -- 4. Test the Homogeneity Restriction --------------------------------------

# -- 4a. Restricted OLS: equation [1.7.6] -------------------------------------
ols_r <- lm(lTC_PF ~ lQ + lPL_PF + lPK_PF, data = nerlove)
tidy(ols_r) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

SSR_R <- deviance(ols_r)

# -- 4b. F-test of homogeneity ------------------------------------------------
n <- nrow(nerlove)
K <- length(coef(ols_u))
J <- 1
F_stat <- ((SSR_R - SSR_U) / J) / (SSR_U / (n-K))
F_crit <- qf(0.95, df1 = J, df2 = n - K)         # 5% critical value
p_val  <- pf(F_stat, df1 = J, df2 = n - K, lower.tail = FALSE)

tibble(F_stat, F_crit, p_val) |>
  mutate(across(everything(), \(x) round(x, 4)))

# -- 5. R^2 detour: equation [1.7.4'] -----------------------------------------
# Substract log(Q) from both sides of [1.7.4] to get average cost as LHS
ols_ac <- lm(lAC ~ lQ + lPL + lPK + lPF, data = nerlove)

# Compare fit statistics
bind_rows(
  glance(ols_u)  %>% mutate(model = "TC model [1.7.4]"),
  glance(ols_ac) %>% mutate(model = "AC model [1.7.4']")
) %>% 
  select(model, r.squared, sigma, df.residual) %>% 
  mutate(across(where(is.numeric), \(x) round(x, 3)))

# -- 6. t-test: constant returns to scale -------------------------------------
tidy(ols_r) %>% 
  filter(term == "lQ") %>% 
  mutate(
    # t-ratio for H0: beta2 = 1 (not the default H0: beta2 = 0)
    t_crs  = (estimate - 1) / std.error,
    t_crit = qt(0.975, df = n - length(coef(ols_r))),
    rts    = 1 / estimate
  ) %>% 
  select(estimate, std.error, t_crs, t_crit, rts) %>% 
  mutate(across(everything(), \(x) round(x, 3)))
