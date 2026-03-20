# --------------------------------------------------
# Project: UKHLS Unemployment and Life Satisfaction
# Author: Michael Huber
# Script: 01_prepare_data.R
# Purpose: Clean and merge UKHLS panel data
# --------------------------------------------------

rm(list = ls())

required_packages <- c(
  "tidyverse",
  "stringr",
  "haven",
  "purrr",
  "here"
)

invisible(
  lapply(required_packages, require, character.only = TRUE)
)

raw_path <- here("data_raw")
derived_path <- here("data_derived")
output_path <- here("outputs")

# 1. Load individual-level UKHLS files -------------------------------

indresp_path <- file.path(raw_path, "UKHLS_indresp")

ukhls_ind_waves_list <- list.files(
  indresp_path,
  pattern = "\\.dta$",
  full.names = TRUE
)

xwdat <- read_dta(
  file.path(raw_path, "xwavedat.dta")
) %>%
  select(pidp, xwdat_dv)

ukhls_id_waves <- lapply(ukhls_ind_waves_list, read_dta)

ukhls_id_waves <- lapply(ukhls_id_waves, function(df) {
  df %>%
    select(
      ends_with(
        c(
          "pidp",
          "xwdat_dv",
          "gor_dv",
          "intdaty_dv",
          "_sex",
          "urban_dv",
          "ppid",
          "aidhh",
          "marstat_dv",
          "hidp",
          "apid",
          "fimngrs_dv",
          "age_dv",
          "nchild_dv",
          "hiqual_dv",
          "_jbstat",
          "hhsize"
        )
      ),
      contains(
        c(
          "scghq1",
          "sf12",
          "sclfsat",
          "hcondn17",
          "hconds17",
          "hcond17",
          "hconda17"
        )
      )
    )
})

names(ukhls_id_waves) <- gsub(
  ".*/(.*)_.*",
  "\\1",
  ukhls_ind_waves_list
)

df_list <- ukhls_id_waves %>%
  reduce(full_join, by = "pidp")

names(df_list) <- str_replace(names(df_list), "_(dv)$", "")
names(df_list) <- str_replace(
  names(df_list),
  "^([a-z])_([a-z]+)$",
  "\\2_\\1"
)
names(df_list) <- str_replace(
  names(df_list),
  "^([a-z])_([a-z]+)([0-9]+)([a-z]+)$",
  "\\2\\3\\4_\\1"
)
names(df_list) <- str_replace(
  names(df_list),
  "^([a-z])_([a-z]+)([0-9]+)$",
  "\\2\\3_\\1"
)

ukhls_long <- df_list %>%
  pivot_longer(
    -pidp,
    names_to = c(".value", "wave"),
    names_sep = "_"
  ) %>%
  filter(intdaty > 0) %>%
  left_join(xwdat, by = "pidp") %>%
  mutate(
    across(
      where(is.labelled),
      ~ as.numeric(as.character(.))
    )
  )

saveRDS(
  ukhls_long,
  file = file.path(
    derived_path,
    "ukhls_allobs_1_9.rds"
  )
)

# 2. Construct diagnosis-related variables ---------------------------

ukhls_long <- ukhls_long %>%
  filter(
    hconda17 == -8 |
      hconda17 >= 0 |
      is.na(hconda17)
  )

ukhls_long <- ukhls_long %>%
  arrange(pidp, wave) %>%
  group_by(pidp) %>%
  mutate(
    year_of_dep_diagnosis = case_when(
      hcond17 > 0 & hconda17 > 0 ~
        (intdaty - age) + hconda17,
      hcondn17 > 0 &
        all(hcond17 <= 0 | is.na(hcond17)) ~
        intdaty,
      TRUE ~ NA_real_
    )
  ) %>%
  fill(year_of_dep_diagnosis, .direction = "updown") %>%
  ungroup()

ukhls_j <- read_dta(
  file.path(
    raw_path,
    "UKHLS_indresp",
    "j_indresp.dta"
  )
)

xwdat3 <- xwdat %>%
  filter(xwdat_dv == 3)

ukhls_j <- ukhls_j %>%
  semi_join(xwdat3, by = "pidp") %>%
  select(
    ends_with("pidp"),
    ends_with("intdaty_dv"),
    starts_with("j_hcondno"),
    starts_with("j_hcondna"),
    contains(c("hcondncode37", "hcondncode38")),
    ends_with("age_dv")
  ) %>%
  mutate(
    across(
      where(is.labelled),
      ~ as.numeric(as.character(.))
    )
  )

ukhls_dep_anx <- ukhls_j %>%
  filter(
    if_any(
      contains("hcondno"),
      ~ .x %in% 37:38
    )
  )

ukhls_diagn <- ukhls_dep_anx %>%
  pivot_longer(
    cols = matches("j_hcondn\\w{1}\\d{1,2}"),
    names_to = c(".value", "number"),
    names_pattern = "(j_hcondn\\w{1})(\\d{1,2})"
  ) %>%
  filter(j_hcondno %in% 37:38) %>%
  mutate(
    age_of_diagnosis38 = case_when(
      j_hcondno == 38 ~ j_hcondna
    ),
    age_of_diagnosis37 = case_when(
      j_hcondno == 37 ~ j_hcondna
    )
  ) %>%
  mutate(
    age_of_diagnosis38 = coalesce(age_of_diagnosis38, 0),
    age_of_diagnosis37 = coalesce(age_of_diagnosis37, 0)
  ) %>%
  filter(
    age_of_diagnosis38 >= 0 &
      age_of_diagnosis37 >= 0
  ) %>%
  distinct(
    pidp,
    age_of_diagnosis37,
    age_of_diagnosis38,
    j_age_dv,
    j_intdaty_dv
  )

ukhls_bhps_depression <- ukhls_diagn %>%
  filter(age_of_diagnosis38 > 0) %>%
  mutate(
    year_of_diagnosis38 = as.numeric(
      j_intdaty_dv - j_age_dv + age_of_diagnosis38
    )
  ) %>%
  select(
    pidp,
    age_of_diagnosis38,
    year_of_diagnosis38
  )

j_bhps_depression <- ukhls_j %>%
  filter(!j_hcondna1 %in% c(-7, -9)) %>%
  select(pidp) %>%
  left_join(ukhls_bhps_depression, by = "pidp") %>%
  select(pidp, year_of_diagnosis38)

merged_diagnosis <- ukhls_long %>%
  filter(
    year_of_dep_diagnosis < 2019 |
      is.na(year_of_dep_diagnosis)
  ) %>%
  left_join(j_bhps_depression, by = "pidp")

merged_diagnosis <- merged_diagnosis %>%
  mutate(
    year_of_dep_diagn = case_when(
      year_of_dep_diagnosis > 0 &
        year_of_diagnosis38 > 0 ~
        year_of_dep_diagnosis,
      is.na(year_of_diagnosis38) ~
        year_of_dep_diagnosis,
      is.na(year_of_dep_diagnosis) ~
        year_of_diagnosis38,
      TRUE ~ NA_real_
    )
  )

ukhls_long <- merged_diagnosis %>%
  select(
    -c(
      year_of_dep_diagnosis,
      year_of_diagnosis38
    )
  ) %>%
  group_by(pidp) %>%
  filter(
    !any(
      hconda17 >= -1 & hconda17 <= 12,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

saveRDS(
  ukhls_long,
  file = file.path(
    derived_path,
    "ukhls_long_dd.rds"
  )
)

# 3. Clean and relabel person-level variables ------------------------

ukhls_long_label <- ukhls_long %>%
  mutate(
    jbstat = case_when(
      jbstat %in% 1 ~ "Self_employed",
      jbstat %in% 2 ~ "Paid_employment",
      jbstat %in% 3 ~ "Unemployed",
      jbstat %in% 4 ~ "Retired",
      TRUE ~ "Out_of_lf"
    ),
    marstat = case_when(
      marstat %in% c(1, 2) ~ "Coupled",
      marstat %in% 3 ~ "Widowed",
      marstat %in% c(4, 5, 6) ~ "Single"
    ),
    hiqual = case_when(
      hiqual %in% c(1, 2) ~ "University_qual",
      hiqual == 3 ~ "Diploma",
      hiqual == 4 ~ "High_school",
      TRUE ~ "Undetermined_educ"
    )
  )

ukhls_df <- ukhls_long_label %>%
  select(-c(hcond17, hconda17, hcondn17)) %>%
  filter(
    intdaty > 0,
    gor > 0,
    scghq1 >= 0,
    sclfsato >= 0,
    urban > 0,
    sex >= 0,
    hiqual > 0,
    nchild >= 0,
    marstat > 0
  )

saveRDS(
  ukhls_df,
  file = file.path(
    derived_path,
    "ukhls_ind.rds"
  )
)

# 4. Merge household-level income data -------------------------------

hh_path <- file.path(raw_path, "UKHLS_HH")

ukhls_hh_waves_list <- list.files(
  hh_path,
  pattern = "\\.dta$",
  full.names = TRUE
)

ukhls_hh_waves <- lapply(
  ukhls_hh_waves_list,
  read_dta
)

ukhls_hh_waves <- lapply(ukhls_hh_waves, function(df) {
  df %>%
    select(
      ends_with("hidp"),
      ends_with(
        c("fihhmnnet1_dv", "ieqmoecd_dv")
      )
    )
})

names(ukhls_hh_waves) <- gsub(
  ".*/(.*)_.*",
  "\\1",
  ukhls_hh_waves_list
)

ukhls_hh_waves <- map(
  ukhls_hh_waves,
  ~ .x %>%
    rename_with(~ "hidp", contains("hidp"))
)

hh <- ukhls_hh_waves %>%
  reduce(full_join, by = "hidp")

names(hh) <- str_replace(names(hh), "_(dv)$", "")
names(hh) <- str_replace(
  names(hh),
  "^([a-z])_([a-z]+)$",
  "\\2_\\1"
)
names(hh) <- str_replace(
  names(hh),
  "^([a-z])_([a-z]+)([0-9]+)$",
  "\\2\\3_\\1"
)

hh_long <- hh %>%
  pivot_longer(
    -hidp,
    names_to = c(".value", "wave"),
    names_sep = "_"
  ) %>%
  mutate(
    across(
      where(is.labelled),
      ~ as.numeric(as.character(.))
    )
  )

wave_mapping <- setNames(1:10, letters[1:10])

ukhls_df <- ukhls_df %>%
  mutate(
    wave = as.integer(wave_mapping[wave])
  )

hh_long <- hh_long %>%
  mutate(
    wave = as.integer(wave_mapping[wave])
  )

ukhls_df <- ukhls_df %>%
  left_join(
    hh_long,
    by = c("hidp", "wave")
  )

# 5. Create final analysis dataset ----------------------------------

ukhls_df <- ukhls_df %>%
  filter(
    ieqmoecd >= 0,
    !is.na(fihhmnnet1),
    !is.na(ieqmoecd),
    fihhmnnet1 >= 0
  ) %>%
  filter(
    intdaty > 0,
    gor > 0,
    scghq1 >= 0,
    sclfsato >= 0,
    urban > 0,
    sex >= 0,
    age >= 18,
    age < 66,
    nchild >= 0,
    sf12pcs >= 0
  ) %>%
  arrange(pidp, wave) %>%
  group_by(pidp) %>%
  mutate(
    diagnosis_indicator = case_when(
      year_of_dep_diagn <= intdaty ~ 1,
      TRUE ~ 0
    ),
    dep_anx_likert = case_when(
      scghq1 >= 12 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>%
  mutate(
    hhincome_adj = fihhmnnet1 / ieqmoecd,
    ln_hhincome_adj = log1p(hhincome_adj)
  )

factor_vars <- c(
  "sex",
  "urban",
  "dep_anx_likert",
  "diagnosis_indicator",
  "jbstat",
  "hiqual",
  "aidhh",
  "marstat"
)

ukhls_df <- ukhls_df %>%
  mutate(
    across(all_of(factor_vars), as.factor)
  )

max_pcs <- max(ukhls_df$sf12pcs, na.rm = TRUE)

ukhls_df$sf12pcs <- max_pcs + 1 - ukhls_df$sf12pcs

ukhls_df <- ukhls_df %>%
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
    hhincome_adj,
    dep_anx_likert,
    diagnosis_indicator,
    sclfsato,
    hiqual,
    jbstat,
    marstat,
    sf12pcs,
    fihhmnnet1,
    sclfsat1
  ) %>%
  mutate(
    ls_stnd = as.numeric(scale(sclfsato)),
    ghq_stnd = as.numeric(scale(scghq1))
  )

saveRDS(
  ukhls_df,
  file = file.path(
    derived_path,
    "analysis_panel.rds"
  )
)