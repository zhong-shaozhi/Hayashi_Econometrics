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

# -- 7. Residual plot: Figure 1.7 ---------------------------------------------
# augment() appends .fitted and .resid columns to the original data
augment(ols_r) %>% 
  ggplot(aes(x = lQ, y = .resid)) +
  geom_point(size = 1.8, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_smooth(method = "loess", se = FALSE, colour = "steelblue", 
              linewidth = 0.8) +
  labs(
    x       = "log output",
    y       = "residual",
    title   = "Figure 1.7: Residuals vs. Log Output (Restricted Model)",
    caption = "Smooth line added to reveal pattern; not in Hayashi original"
  ) +
  theme_bw()

# Save to output/
ggsave("output/fig_1_7_residuals.png", width = 7, height = 5, dpi = 150)


# -- 8. Five-group analysis --------------------------------------------------

# Assign groups: sort by output, 5 groups of 29
nerlove_grouped <- nerlove %>% 
  arrange(output) %>% 
  mutate(group = rep(1:5, each = 29))

# Estimate restricted model separately for each group
# Pattern: nest -> map -> tidy -> unnest
group_results <- nerlove_grouped %>% 
  group_by(group) %>% 
  nest() %>% 
  mutate(
    fit    = map(data, \(df)
                 lm(lTC_PF ~ lQ + lPL_PF + lPK_PF, data = df)),
    coefs  = map(fit, tidy)
  ) %>% 
  unnest(coefs) %>% 
  filter(term == "lQ") %>% 
  transmute(
    group, 
    beta2  = estimate,
    se     = std.error,
    rts    = 1 / estimate,
    rts_lo = 1 / (estimate + 1.96 * std.error),
    rts_hi = 1 / (estimate - 1.96 * std.error)
  )

print(group_results)

group_results |>
  ggplot(aes(x = group, y = rts)) +
  geom_point(size = 3.5) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(aes(ymin = rts_lo, ymax = rts_hi), width = 0.15) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("Smallest\n(Group 1)", "2", "3", "4", "Largest\n(Group 5)")
  ) +
  labs(
    x     = "Output group (sorted ascending)",
    y     = "Returns to scale  r = 1/β₂",
    title = "Nerlove (1963): Scale economies diminish with firm size",
    subtitle = "Dashed line = constant returns to scale (r = 1)"
  ) +
  theme_bw()

ggsave("output/fig_nerlove_rts_by_group.png", width = 7, height = 5, dpi = 150)
