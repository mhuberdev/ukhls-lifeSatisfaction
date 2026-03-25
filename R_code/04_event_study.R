# --------------------------------------------------
# Project: UKHLS Unemployment and Life Satisfaction
# Author: Michael Huber
# Script: 04_event_study.R
# Purpose: Event-study analysis to confirm treatment effects of static model estimates
# --------------------------------------------------

rm(list = ls())

required_packages <- c(
  "sandwich",
  "tidyverse",
  "stargazer",
  "caret",
  "lmtest",
  "ggplot2",
  "plm",
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

# 2. Prepare unemployment indicator ----------------------------------

dummified_reg <- dummyVars(~ jbstat, data = ukhls_df)

dummified_df <- as.data.frame(
  predict(dummified_reg, newdata = ukhls_df)
)

unem <- ukhls_df %>%
  cbind(dummified_df) %>%
  filter(
    jbstat.Paid_employment == 1 |
      jbstat.Self_employed == 1 |
      jbstat.Unemployed == 1
  ) %>%
  rename(unemployed = jbstat.Unemployed)

# 3. Compute first unemployment event --------------------------------

unem <- unem %>%
  arrange(pidp, wave) %>%
  group_by(pidp) %>%
  mutate(
    first_unem = if (
      any(unemployed == 1, na.rm = TRUE)
    ) {
      first(
        wave[
          unemployed == 1 &
            dplyr::lag(unemployed, default = 0) == 0
        ],
        na_rm = TRUE
      )
    } else {
      NA_real_
    }
  ) %>%
  ungroup()

# 4. Create event time ------------------------------------------------

unem <- unem %>%
  mutate(relative_t = wave - first_unem) %>%
  mutate(relative_t = as.factor(relative_t))

dummified_reg <- dummyVars(~ relative_t, data = unem)

dummified_df <- as.data.frame(
  predict(dummified_reg, newdata = unem)
)

unem <- unem %>%
  cbind(dummified_df) %>%
  rename(
    lag1 = `relative_t.-1`,
    lag2 = `relative_t.-2`,
    lag3 = `relative_t.-3`,
    lag4 = `relative_t.-4`,
    t = `relative_t.0`,
    lead1 = `relative_t.1`,
    lead2 = `relative_t.2`,
    lead3 = `relative_t.3`,
    lead4 = `relative_t.4`
  )

event_formula <- ls_stnd ~ lag4 +
  lag3 +
  lag2 +
  t +
  post1 +
  post2 +
  post3 +
  wave

# 5. Define post-event indicators ------------------------------------

event <- unem %>%
  mutate(relative_t = as.numeric(as.character(relative_t))) %>%
  group_by(pidp) %>%
  mutate(
    post1 = if_else(
      unemployed == 1 & t == 0 & lead1 == 1,
      1,
      0
    ),
    post2 = if_else(
      unemployed == 1 &
        t == 0 &
        lead1 == 0 &
        lead2 == 1 &
        !any(lead1 == 1 & unemployed == 0),
      1,
      0
    ),
    post3 = if_else(
      unemployed == 1 &
        t == 0 &
        lead1 == 0 &
        lead2 == 0 &
        lead3 == 1 &
        !any(lead1 == 1 & unemployed == 0) &
        !any(lead2 == 1 & unemployed == 0),
      1,
      0
    )
  ) %>%
  ungroup() %>%
  pdata.frame(index = c("pidp", "wave"))

# 6. Estimate event-study model --------------------------------------

event_model <- plm(
  event_formula,
  data = event,
  model = "within",
  index = c("pidp", "wave")
)

quicklook <- summary(event_model)
print(quicklook)

# 7. Export regression table -----------------------------------------

event_table <- stargazer(
  event_model,
  type = "latex",
  style = "aer",
  title = "Regression Results - Event Study",
  model.names = FALSE,
  omit = c("wave", "Constant"),
  single.row = FALSE,
  star.cutoffs = c(0.1, 0.05, 0.01),
  omit.stat = c("f", "ser", "adj.rsq"),
  digits = 3,
  notes.align = "l"
)

capture.output(
  event_table,
  file = file.path(output_path, "table_event_study.tex")
)

# 8. Create coefficient data for plotting ----------------------------

coefs <- tibble(
  term = c(-4, -3, -2, -1, 0, 1, 2, 3),
  estimate = c(
    0.0254406,
    0.0191974,
    -0.0040505,
    0,
    -0.2227395,
    -0.3027803,
    -0.3113487,
    -0.1394384
  ),
  stder = c(
    0.0298380,
    0.0259774,
    0.0222515,
    0,
    0.0178254,
    0.0390340,
    0.0589345,
    0.0867299
  )
) %>%
  mutate(
    ci_low = estimate - 1.96 * stder,
    ci_high = estimate + 1.96 * stder
  )

# 9. Plot event-study coefficients -----------------------------------

figure_2 <- ggplot(
  coefs,
  aes(
    x = factor(term),
    y = estimate,
    ymin = ci_low,
    ymax = ci_high
  )
) +
  geom_point(size = 2, color = "darkblue") +
  geom_errorbar(width = 0.5, color = "darkblue") +
  geom_segment(
    aes(
      x = 4.5,
      xend = 4.5,
      y = -0.5,
      yend = 0.1
    ),
    linetype = "dashed",
    color = "red"
  ) +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    x = "Time Period",
    y = "Estimate"
  ) +
  theme_minimal(base_size = 12) +
  scale_x_discrete(
    labels = c(
      "-4" = "-4",
      "-3" = "-3",
      "-2" = "-2",
      "-1" = "-1",
      "0" = "0",
      "1" = "+1",
      "2" = "+2",
      "3" = "+3"
    )
  ) +
  scale_y_continuous(
    limits = c(-0.5, 0.1),
    breaks = seq(-0.5, 0.1, by = 0.1)
  )

print(figure_2)

ggsave(
  filename = file.path(output_path, "figure_2_event_study.png"),
  plot = figure_2,
  width = 8,
  height = 5,
  dpi = 300
)