# --------------------------------------------------
# Project: UKHLS Unemployment and Life Satisfaction
# Author: Michael Huber
# Script: 02_descriptive_analysis.R
# Purpose: Descriptive statistics and exploratory data analysis
# --------------------------------------------------

rm(list = ls())

required_packages <- c(
  "tidyverse",
  "caret",
  "gtsummary",
  "kableExtra",
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

# 2. Select variables for descriptive analysis -----------------------

main_vars <- ukhls_df %>%
  select(
    pidp,
    wave,
    gor,
    intdaty,
    nchild,
    age,
    scghq1,
    marstat,
    sclfsato,
    sex,
    urban,
    diagnosis_indicator,
    jbstat,
    hiqual,
    hhincome_adj,
    sf12pcs
  ) %>%
  names()

ukhls_total <- ukhls_df %>%
  select(all_of(main_vars))

# 3. Create dummy variables ------------------------------------------

dummies_model <- dummyVars(
  ~ sex + urban + jbstat + hiqual + marstat,
  data = ukhls_total
)

dummified_df <- as.data.frame(
  predict(dummies_model, newdata = ukhls_total)
)

# 4. Build descriptive dataset ---------------------------------------

ukhls_desc <- ukhls_total %>%
  cbind(dummified_df) %>%
  filter(
    jbstat.Paid_employment == 1 |
      jbstat.Self_employed == 1 |
      jbstat.Unemployed == 1
  ) %>%
  select(-c(sex, urban, hiqual, jbstat, marstat)) %>%
  mutate(number_of_observations = 1) %>%
  rename(
    "Number of children" = "nchild",
    "Age" = "age",
    "GHQ score" = "scghq1",
    "Life satisfaction" = "sclfsato",
    "Male" = "sex.1",
    "Female" = "sex.2",
    "Urban" = "urban.1",
    "Rural" = "urban.2",
    "Paid employment" = "jbstat.Paid_employment",
    "Self employed" = "jbstat.Self_employed",
    "Unemployed" = "jbstat.Unemployed",
    "Diploma" = "hiqual.Diploma",
    "University qualification" = "hiqual.University_qual",
    "High school" = "hiqual.High_school",
    "Undetermined education" = "hiqual.Undetermined_educ",
    "Household income" = "hhincome_adj",
    "Coupled" = "marstat.Coupled",
    "Single" = "marstat.Single",
    "Widowed" = "marstat.Widowed",
    "Diagnosed" = "diagnosis_indicator",
    "N" = "number_of_observations",
    "SF-12 PCS" = "sf12pcs"
  )

# 5. Summary table ---------------------------------------------------

summary_table <- ukhls_desc %>%
  tbl_summary(
    include = -c(
      "pidp",
      "wave",
      "gor",
      "intdaty",
      "Female",
      "Rural"
    ),
    by = "Unemployed",
    type = list(
      all_continuous() ~ "continuous",
      all_categorical() ~ "dichotomous",
      "Life satisfaction" ~ "continuous",
      "Number of children" ~ "continuous"
    ),
    statistic = list(
      all_categorical() ~ "{p}",
      all_continuous() ~ "{mean}\n({sd})",
      N ~ "{N_obs}"
    ),
    digits = list(
      everything() ~ c(1, 2),
      c(N, "Household income") ~ 0
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  modify_header(
    label = "Variables",
    stat_0 = "Total",
    stat_1 = "Other",
    stat_2 = "Unemployed"
  ) %>%
  as_kable_extra(format = "latex") %>%
  kable_styling(
    bootstrap_options = "condensed",
    full_width = FALSE
  )

print(summary_table)

# 6. Pairwise t-tests ------------------------------------------------

ttest_vars <- ukhls_desc %>%
  select(where(is.numeric)) %>%
  names()

ttest_results <- list()

for (variable in ttest_vars) {
  ttest_results[[variable]] <- pairwise.t.test(
    ukhls_desc[[variable]],
    ukhls_desc$Unemployed,
    p.adjust.method = "BH"
  )
}

print(ttest_results)

# 7. Figure 1: Life satisfaction by age and employment status --------

ukhls_dummified <- ukhls_total %>%
  cbind(dummified_df) %>%
  select(-c(sex, urban, hiqual, marstat))

by_age <- ukhls_dummified %>%
  mutate(jbstat = as.factor(jbstat)) %>%
  group_by(age, jbstat) %>%
  summarise(
    mean_sclfsato = mean(sclfsato, na.rm = TRUE),
    sd_sclfsato = sd(sclfsato, na.rm = TRUE),
    n = n(),
    se = sd_sclfsato / sqrt(n),
    .groups = "drop"
  ) %>%
  filter(jbstat != "Retired")

figure_1 <- by_age %>%
  ggplot(
    aes(
      x = age,
      y = mean_sclfsato,
      group = jbstat,
      color = jbstat
    )
  ) +
  geom_line() +
  geom_ribbon(
    aes(
      ymin = mean_sclfsato - 1.96 * se,
      ymax = mean_sclfsato + 1.96 * se,
      fill = jbstat
    ),
    alpha = 0.2,
    color = NA
  ) +
  theme_minimal() +
  labs(
    x = "Age",
    y = "Life Satisfaction Score",
    color = "Employment Status",
    fill = "Employment Status"
  ) +
  scale_color_manual(
    values = c("darkblue", "black", "grey", "red"),
    labels = c(
      "Out of Labour Force",
      "Paid Employment",
      "Self-employed",
      "Unemployed"
    )
  ) +
  scale_fill_manual(
    values = c("darkblue", "black", "grey", "red"),
    labels = c(
      "Out of Labour Force",
      "Paid Employment",
      "Self-employed",
      "Unemployed"
    )
  )

print(figure_1)

ggsave(
  filename = file.path(output_path, "figure_1_age_employment_ls.png"),
  plot = figure_1,
  width = 8,
  height = 5,
  dpi = 300
)