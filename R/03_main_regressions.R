# --------------------------------------------------
# Project: UKHLS Unemployment and Life Satisfaction
# Author: Michael Huber
# Script: 03_main_regressions.R
# Purpose: Regression models and causality checks
# --------------------------------------------------

rm(list = ls())

required_packages <- c(
  "stargazer",
  "caret",
  "tidyverse",
  "plm",
  "lmtest",
  "sandwich",
  "panelView",
  "here"
)

invisible(
  lapply(required_packages, require, character.only = TRUE)
)

derived_path <- here("data_derived")
output_path <- here("outputs")

# 1. Load cleaned analysis dataset -----------------------------------

ukhls_df <- readRDS(
  file.path(derived_path, "analysis_panel.rds")
)

# 2. Prepare regression dataset --------------------------------------

ukhls_total <- ukhls_df %>%
  select(
    pidp,
    wave,
    intdaty,
    gor,
    sex,
    nchild,
    urban,
    age,
    scghq1,
    ln_hhincome_adj,
    dep_anx_likert,
    diagnosis_indicator,
    sclfsato,
    hiqual,
    jbstat,
    marstat,
    sf12pcs,
    ls_stnd
  )

dummies_model <- dummyVars(
  ~ sex + urban + jbstat + hiqual + marstat,
  data = ukhls_total
)

dummified_df <- as.data.frame(
  predict(dummies_model, newdata = ukhls_total)
)

ukhls_total <- ukhls_total %>%
  cbind(dummified_df)

main_reg <- ukhls_total %>%
  filter(
    jbstat.Paid_employment == 1 |
      jbstat.Self_employed == 1 |
      jbstat.Unemployed == 1
  )

main_reg <- pdata.frame(
  main_reg,
  index = c("pidp", "wave")
)

reg_formula <- ls_stnd ~
  jbstat.Unemployed +
    diagnosis_indicator +
    age +
    I(age^2) +
    marstat +
    sex +
    sf12pcs +
    urban +
    nchild +
    hiqual +
    ln_hhincome_adj +
    wave

# 3. Main regression models ------------------------------------------

pooled_model_nc <- plm(
  ls_stnd ~ jbstat.Unemployed + wave,
  data = main_reg,
  model = "pooling",
  index = c("pidp", "wave")
)

ols_nc <- summary(pooled_model_nc)
ols_nc$coefficients <- round(ols_nc$coefficients, digits = 3)
print(ols_nc)

pooled_model <- plm(
  reg_formula,
  data = main_reg,
  model = "pooling",
  index = c("pidp", "wave")
)

ols <- summary(pooled_model)
ols$coefficients <- round(ols$coefficients, digits = 3)
print(ols)

re_model <- plm(
  reg_formula,
  data = main_reg,
  model = "random",
  index = c("pidp", "wave")
)

re_results <- summary(re_model)
re_results$coefficients <- round(re_results$coefficients, digits = 3)
print(re_results)

fe_model_nc <- plm(
  ls_stnd ~ jbstat.Unemployed + wave,
  data = main_reg,
  model = "within",
  index = c("pidp", "wave")
)

fe_nc <- summary(fe_model_nc)
fe_nc$coefficients <- round(fe_nc$coefficients, digits = 3)
print(fe_nc)

fe_model <- plm(
  reg_formula,
  data = main_reg,
  model = "within",
  index = c("pidp", "wave")
)

fe_results <- summary(fe_model)
fe_results$coefficients <- round(fe_results$coefficients, digits = 3)
print(fe_results)

# 4. Regression table: main specification ----------------------------

reg_table_main <- stargazer(
  pooled_model_nc,
  pooled_model,
  fe_model_nc,
  fe_model,
  re_model,
  type = "latex",
  style = "aer",
  title = "Regression Results - Dummy Variable Estimation",
  model.names = FALSE,
  column.labels = c("OLS", "OLS", "FE", "FE", "RE"),
  omit = c("wave", "Constant", "I(age^2)"),
  single.row = FALSE,
  star.cutoffs = c(0.1, 0.05, 0.01),
  omit.stat = c("f", "ser", "adj.rsq"),
  digits = 3,
  notes.align = "l"
)

capture.output(
  reg_table_main,
  file = file.path(output_path, "table_main_regressions.tex")
)

# 5. Appendix: alternative specification -----------------------------

reg_formula_no_mh <- ls_stnd ~
  jbstat.Unemployed +
    age +
    I(age^2) +
    marstat +
    sex +
    sf12pcs +
    urban +
    nchild +
    hiqual +
    ln_hhincome_adj +
    wave

pooled_model_nc_alt <- plm(
  ls_stnd ~ jbstat.Unemployed + wave,
  data = main_reg,
  model = "pooling",
  index = c("pidp", "wave")
)

ols_nc_alt <- summary(pooled_model_nc_alt)
ols_nc_alt$coefficients <- round(ols_nc_alt$coefficients, digits = 3)
print(ols_nc_alt)

pooled_model_alt <- plm(
  reg_formula_no_mh,
  data = main_reg,
  model = "pooling",
  index = c("pidp", "wave")
)

ols_alt <- summary(pooled_model_alt)
ols_alt$coefficients <- round(ols_alt$coefficients, digits = 3)
print(ols_alt)

fe_model_nc_alt <- plm(
  ls_stnd ~ jbstat.Unemployed + wave,
  data = main_reg,
  model = "within",
  index = c("pidp", "wave")
)

fe_nc_alt <- summary(fe_model_nc_alt)
fe_nc_alt$coefficients <- round(fe_nc_alt$coefficients, digits = 3)
print(fe_nc_alt)

fe_model_alt <- plm(
  reg_formula_no_mh,
  data = main_reg,
  model = "within",
  index = c("pidp", "wave")
)

fe_alt <- summary(fe_model_alt)
fe_alt$coefficients <- round(fe_alt$coefficients, digits = 3)
print(fe_alt)

reg_table_appendix <- stargazer(
  pooled_model_nc_alt,
  pooled_model_alt,
  fe_model_nc_alt,
  fe_model_alt,
  type = "latex",
  style = "aer",
  title = "Regression Results - No Mental Health Control",
  model.names = FALSE,
  column.labels = c("OLS", "OLS", "FE", "FE"),
  omit = c("wave", "Constant", "I(age^2)"),
  single.row = FALSE,
  star.cutoffs = c(0.1, 0.05, 0.01),
  omit.stat = c("f", "ser", "adj.rsq"),
  digits = 3,
  notes.align = "l"
)

capture.output(
  reg_table_appendix,
  file = file.path(output_path, "table_appendix_no_mh.tex")
)

# 6. Heterogeneous treatment effects ---------------------------------

male_df <- main_reg %>%
  filter(sex == 1)

female_df <- main_reg %>%
  filter(sex == 2)

reg_formula_sex <- ls_stnd ~
  jbstat.Unemployed +
    diagnosis_indicator +
    age +
    I(age^2) +
    marstat +
    sf12pcs +
    urban +
    nchild +
    hiqual +
    ln_hhincome_adj +
    wave

fe_male <- plm(
  reg_formula_sex,
  data = male_df,
  model = "within",
  index = c("pidp", "wave")
)

fe_female <- plm(
  reg_formula_sex,
  data = female_df,
  model = "within",
  index = c("pidp", "wave")
)

reg_formula_educ <- ls_stnd ~
  jbstat.Unemployed +
    diagnosis_indicator +
    age +
    I(age^2) +
    marstat +
    sf12pcs +
    urban +
    nchild +
    sex +
    ln_hhincome_adj +
    wave

educ <- main_reg %>%
  group_by(pidp) %>%
  filter(
    all(hiqual.Diploma == 1) |
      all(hiqual.High_school == 1) |
      all(hiqual.Undetermined_educ == 1) |
      all(hiqual.University_qual == 1)
  ) %>%
  ungroup()

diploma <- educ %>%
  filter(hiqual.Diploma == 1)

fe_diploma <- plm(
  reg_formula_educ,
  data = diploma,
  model = "within",
  index = c("pidp", "wave")
)

high_school <- educ %>%
  filter(hiqual.High_school == 1)

fe_high_school <- plm(
  reg_formula_educ,
  data = high_school,
  model = "within",
  index = c("pidp", "wave")
)

university <- educ %>%
  filter(hiqual.University_qual == 1)

fe_university <- plm(
  reg_formula_educ,
  data = university,
  model = "within",
  index = c("pidp", "wave")
)

heterogeneity_table <- stargazer(
  fe_diploma,
  fe_high_school,
  fe_university,
  fe_male,
  fe_female,
  type = "latex",
  style = "aer",
  model.names = FALSE,
  title = "Heterogeneity Test",
  single.row = FALSE,
  align = TRUE,
  keep = c("jbstat.Unemployed"),
  star.cutoffs = c(0.1, 0.05, 0.01),
  digits = 3,
  omit.stat = c("f", "ser", "adj.rsq")
)

capture.output(
  heterogeneity_table,
  file = file.path(output_path, "table_heterogeneity.tex")
)

# 7. Manual coefficient difference check -----------------------------

diff_coef <- 0.252 - 0.169
se_diff <- sqrt(0.015^2 + 0.016^2)
diff_test_stat <- diff_coef / se_diff

print(diff_test_stat)

# 8. Figure: treatment timing and histories --------------------------

timing <- main_reg %>%
  mutate(
    jbstat.Unemployed = as.numeric(
      as.character(jbstat.Unemployed)
    ),
    wave = as.numeric(as.character(wave)),
    pidp = as.numeric(as.character(pidp))
  )

panelview(
  1 ~ jbstat.Unemployed,
  data = timing,
  index = c("pidp", "wave"),
  axis.lab.angle = 90,
  by.timing = TRUE,
  display.all = TRUE,
  ylab = "Individuals"
)