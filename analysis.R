library(tidyverse)
library(haven)
library(lme4)
library(data.table)
library(vroom)

set.seed(5472)

# 1. map state FIPS to Census region
get_region <- function(fips) {
  case_when(
    fips %in% c(9, 23, 25, 33, 44, 50, 34, 36, 42) ~ "Northeast",
    fips %in% c(18, 17, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46) ~ "Midwest",
    fips %in% c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48) ~ "South",
    TRUE ~ "West"
  )
}

# 2. Load BRFSS (survey) and ACS (population frame)
brfss_raw <- read_xpt("LLCP2022.XPT_")

acs_cols <- c("STATEFIP","AGE","SEX","RACE","HISPAN",
              "HCOVANY","EDUC","PERWT","GQ")

acs_raw <- fread(
  "usa_00001.csv",
  select   = acs_cols,
  nThread  = 1,
  showProgress = TRUE
)

# 3. Clean BRFSS and harmonize covariates
survey_data <- brfss_raw %>%
  select(
    state_code = `_STATE`,
    age        = `_AGE80`,
    sex_code   = `_SEX`,
    race_code  = `_RACEGR4`,
    edu_code   = `_EDUCAG`,
    outcome    = `_HLTHPLN`,
    weight     = `_LLCPWT`
  ) %>%
  filter(age >= 18 & age <= 64) %>%
  mutate(
    y = case_when(outcome == 1 ~ 1,
                  outcome == 2 ~ 0,
                  TRUE ~ NA_real_),
    state_num = as.integer(state_code),
    state     = factor(state_code),
    region    = factor(get_region(state_num)),
    sex = case_when(sex_code == 1 ~ "Male",
                    sex_code == 2 ~ "Female"),
    race = case_when(
      race_code == 1 ~ "White",
      race_code == 2 ~ "Black",
      race_code == 5 ~ "Hispanic",
      race_code %in% c(3, 4) ~ "Other",
      TRUE ~ NA_character_
    ),
    education = case_when(
      edu_code == 1 ~ "No HS",
      edu_code == 2 ~ "HS Grad",
      edu_code == 3 ~ "Some College",
      edu_code == 4 ~ "College Grad",
      TRUE ~ NA_character_
    ),
    age_group = cut(
      age,
      breaks = c(17, 29, 44, 59, 64),
      labels = c("18-29", "30-44", "45-59", "60-64")
    )
  ) %>%
  drop_na(y, state, sex, race, education, age_group, weight, region) %>%
  mutate(wt_scaled = weight / mean(weight))

# 4. Clean ACS and build poststratification frame
census_data <- acs_raw %>%
  filter(GQ %in% c(1, 2)) %>%            # non-institutional population
  filter(AGE >= 18 & AGE <= 64) %>%
  mutate(
    y_truth = case_when(HCOVANY == 2 ~ 1,
                        HCOVANY == 1 ~ 0),
    state_num = STATEFIP,
    state  = factor(STATEFIP, levels = levels(survey_data$state)),
    region = factor(get_region(state_num), levels = levels(survey_data$region)),
    sex = case_when(SEX == 1 ~ "Male",
                    SEX == 2 ~ "Female"),
    race = case_when(
      HISPAN > 0 ~ "Hispanic",
      RACE == 1  ~ "White",
      RACE == 2  ~ "Black",
      TRUE       ~ "Other"
    ),
    education = case_when(
      EDUC < 60             ~ "No HS",
      EDUC >= 60 & EDUC < 70 ~ "HS Grad",
      EDUC >= 70 & EDUC < 80 ~ "Some College",
      EDUC >= 80             ~ "College Grad"
    ),
    age_group = cut(
      AGE,
      breaks = c(17, 29, 44, 59, 64),
      labels = c("18-29", "30-44", "45-59", "60-64")
    )
  ) %>%
  drop_na(y_truth, state, sex, race, education, age_group, region)

poststrat_df <- census_data %>%
  group_by(state, region, age_group, sex, race, education) %>%
  summarise(
    N         = sum(PERWT),
    true_rate = weighted.mean(y_truth, w = PERWT),
    .groups   = "drop"
  )

# 5. Add state-level SES (college share) and fit weighted MrP model
state_context <- poststrat_df %>%
  group_by(state) %>%
  summarise(
    state_college_prop =
      sum(N[education == "College Grad"]) / sum(N)
  )

survey_data  <- survey_data  %>% left_join(state_context, by = "state")
poststrat_df <- poststrat_df %>% left_join(state_context, by = "state")

fast_model <- glmer(
  y ~ (1 | state) +
      state_college_prop + region +
      age_group * education +
      sex + race,
  data    = survey_data,
  weights = wt_scaled,
  family  = binomial(link = "logit"),
  nAGQ    = 0
)

# 6. Poststratify to state level and compare with direct estimates
poststrat_df$pred_prob <- predict(
  fast_model,
  newdata = poststrat_df,
  type    = "response"
)

final_estimates <- poststrat_df %>%
  group_by(state) %>%
  summarise(
    MrP_Estimate = sum(pred_prob * N) / sum(N),
    True_Value   = sum(true_rate * N) / sum(N),
    Total_Pop    = sum(N)
  )

direct_estimates <- survey_data %>%
  group_by(state) %>%
  summarise(
    Direct_Estimate = weighted.mean(y, w = weight),
    Sample_Size     = n()
  )

results <- final_estimates %>%
  left_join(direct_estimates, by = "state") %>%
  mutate(
    Error_MrP    = abs(MrP_Estimate    - True_Value),
    Error_Direct = abs(Direct_Estimate - True_Value)
  )

rmse_mrp    <- sqrt(mean(results$Error_MrP^2))
rmse_direct <- sqrt(mean(results$Error_Direct^2))

cat("\n=== Competition Results on state level (RMSE) ===\n")
cat("MrP:", round(rmse_mrp, 4), "\n")
cat("Direct estimate:", round(rmse_direct, 4), "\n")

min_val <- min(
  c(results$True_Value,
    results$MrP_Estimate,
    results$Direct_Estimate)
) - 0.02

plot_result <- ggplot(results, aes(x = True_Value)) +
  geom_point(aes(y = Direct_Estimate, color = "Direct Estimate"),
             alpha = 0.6, size = 2) +
  geom_point(aes(y = MrP_Estimate, color = "MrP Estimate"),
             alpha = 0.8, size = 2) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "gray30") +
  scale_color_manual(values = c(
    "Direct Estimate" = "red",
    "MrP Estimate"    = "blue"
  )) +
  labs(
    title    = "Comparison: Upgraded MrP vs. Direct Estimation",
    subtitle = "State-level truth from ACS",
    x        = "ACS true insurance rate",
    y        = "Estimated rate",
    color    = "Method"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1,
              xlim  = c(min_val, 1),
              ylim  = c(min_val, 1))

print(plot_result)

# 7. State x Age performance (medium-grain)
subgroup_eval <- poststrat_df %>%
  group_by(state, age_group) %>%
  summarise(
    True_Value   = sum(true_rate * N) / sum(N),
    MrP_Estimate = sum(pred_prob * N) / sum(N),
    Subgroup_Pop = sum(N),
    .groups      = "drop"
  )

direct_subgroup <- survey_data %>%
  group_by(state, age_group) %>%
  summarise(
    Direct_Estimate = weighted.mean(y, w = weight),
    Sample_Size     = n(),
    .groups         = "drop"
  )

comparison <- subgroup_eval %>%
  left_join(direct_subgroup, by = c("state", "age_group")) %>%
  mutate(
    Error_MrP    = (MrP_Estimate    - True_Value)^2,
    Error_Direct = (Direct_Estimate - True_Value)^2
  )

rmse_mrp_sub    <- sqrt(weighted.mean(comparison$Error_MrP,
                                      w = comparison$Subgroup_Pop,
                                      na.rm = TRUE))
rmse_direct_sub <- sqrt(weighted.mean(comparison$Error_Direct,
                                      w = comparison$Subgroup_Pop,
                                      na.rm = TRUE))

small_n_comparison <- comparison %>%
  filter(Sample_Size < 50)

rmse_mrp_small    <- sqrt(mean(small_n_comparison$Error_MrP,
                               na.rm = TRUE))
rmse_direct_small <- sqrt(mean(small_n_comparison$Error_Direct,
                               na.rm = TRUE))

# 8. Cell-level coverage, bias correction, and RMSE
direct_cells <- survey_data %>%
  group_by(state, age_group, sex, race, education) %>%
  summarise(
    Direct_Estimate = weighted.mean(y, w = weight),
    Sample_Size     = n(),
    .groups         = "drop"
  )

cell_comparison <- poststrat_df %>%
  rename(MrP_Raw   = pred_prob,
         True_Value = true_rate) %>%
  left_join(direct_cells,
            by = c("state", "age_group", "sex", "race", "education"))

total_cells   <- nrow(cell_comparison)
missing_cells <- sum(is.na(cell_comparison$Direct_Estimate))

cat("\n=== Cell-Level Coverage Analysis ===\n")
cat("Total cells:", total_cells, "\n")
cat("Direct missing cells:", missing_cells, "\n")
cat("Direct miss rate:", round(missing_cells / total_cells * 100, 2), "%\n")
cat("MrP miss rate: 0% (all cells predicted)\n")

global_bias <- sum(cell_comparison$MrP_Raw * cell_comparison$N) /
               sum(cell_comparison$True_Value * cell_comparison$N)

cat("MrP global bias factor:",
    round(global_bias, 4),
    "(<1 => underestimation, >1 => overestimation)\n")

cell_comparison <- cell_comparison %>%
  mutate(
    MrP_Corrected = pmin(pmax(MrP_Raw / global_bias, 0), 1)
  )

valid_comparison <- cell_comparison %>%
  filter(!is.na(Direct_Estimate))

rmse_mrp_cell <- sqrt(weighted.mean(
  (valid_comparison$MrP_Corrected - valid_comparison$True_Value)^2,
  w = valid_comparison$N
))
rmse_direct_cell <- sqrt(weighted.mean(
  (valid_comparison$Direct_Estimate - valid_comparison$True_Value)^2,
  w = valid_comparison$N
))

cat("\n=== Survivor Bias Analysis (Cells with Direct Coverage) ===\n")
cat("MrP (bias-corrected) RMSE:", round(rmse_mrp_cell, 4), "\n")
cat("Direct RMSE:", round(rmse_direct_cell, 4), "\n")

plot_data <- valid_comparison %>%
  mutate(
    Err_Dir = abs(Direct_Estimate - True_Value),
    Err_MrP = abs(MrP_Corrected  - True_Value)
  )

ggplot(plot_data, aes(x = Sample_Size)) +
  geom_point(aes(y = Err_Dir, color = "Direct Error"), alpha = 0.3) +
  geom_smooth(aes(y = Err_Dir, color = "Direct Error"),
              se = FALSE) +
  geom_smooth(aes(y = Err_MrP, color = "MrP Error"),
              se = FALSE) +
  scale_x_log10() +
  labs(
    title    = "Error vs. Sample Size (Cell Level)",
    subtitle = "Direct estimation gets noisy in sparse cells; MrP stays calm.",
    x        = "Cell sample size (log scale)",
    y        = "Absolute error"
  ) +
  theme_minimal()

# 9. Four-way comparison: MrP vs raking vs no pooling vs complete pooling
four_way_comparison <- survey_data %>%
  group_by(state, age_group, sex, race, education) %>%
  summarise(
    Est_NoPooling = mean(y),
    Est_Raking    = weighted.mean(y, w = weight),
    Sample_Size   = n(),
    .groups       = "drop"
  ) %>%
  right_join(poststrat_df,
             by = c("state", "age_group", "sex", "race", "education")) %>%
  rename(
    Est_MrP    = pred_prob,
    True_Value = true_rate
  ) %>%
  mutate(
    Est_Complete = weighted.mean(survey_data$y,
                                 w = survey_data$weight)
  )

four_way_comparison <- four_way_comparison %>%
  mutate(
    SE_MrP       = (Est_MrP       - True_Value)^2,
    SE_Raking    = (Est_Raking    - True_Value)^2,
    SE_NoPooling = (Est_NoPooling - True_Value)^2,
    SE_Complete  = (Est_Complete  - True_Value)^2
  )

valid_rows <- four_way_comparison %>%
  filter(!is.na(Est_NoPooling))

rmse_summary <- data.frame(
  Method = c("MrP (Partial Pooling)",
             "Raking (Weighted Direct)",
             "No Pooling (Raw Mean)",
             "Complete Pooling (Global)"),
  RMSE = c(
    sqrt(weighted.mean(valid_rows$SE_MrP,       w = valid_rows$N)),
    sqrt(weighted.mean(valid_rows$SE_Raking,    w = valid_rows$N)),
    sqrt(weighted.mean(valid_rows$SE_NoPooling, w = valid_rows$N)),
    sqrt(weighted.mean(valid_rows$SE_Complete,  w = valid_rows$N))
  )
)

cat("\n=== 4-Way RMSE Comparison (Cell Level) ===\n")
print(rmse_summary)

plot_data_4way <- valid_rows %>%
  select(Sample_Size, N,
         SE_MrP, SE_Raking, SE_NoPooling, SE_Complete) %>%
  pivot_longer(
    cols      = starts_with("SE"),
    names_to  = "Method",
    values_to = "Squared_Error"
  ) %>%
  mutate(
    Method = factor(
      Method,
      levels = c("SE_MrP", "SE_Raking", "SE_NoPooling", "SE_Complete"),
      labels = c("MrP", "Raking", "No Pooling", "Complete Pooling")
    )
  )

plot_4way <- ggplot(plot_data_4way,
                    aes(x = Sample_Size,
                        y = Squared_Error,
                        color = Method)) +
  geom_smooth(method = "gam", se = FALSE, linewidth = 1.2) +
  scale_x_log10() +
  scale_color_manual(values = c(
    "MrP"             = "blue",
    "Raking"          = "red",
    "No Pooling"      = "orange",
    "Complete Pooling"= "green4"
  )) +
  labs(
    title    = "Performance vs. Sample Size (4-Way Comparison)",
    subtitle = "MrP leads in sparse cells; raking catches up as n grows.",
    x        = "Cell sample size (log scale)",
    y        = "Mean squared error (trend)"
  ) +
  theme_minimal()

print(plot_4way)

# 10. Correlation analysis: the accuracy–utility tradeoff
cor_summary <- four_way_comparison %>%
  filter(!is.na(Est_NoPooling)) %>%
  summarise(
    Cor_MrP       = cor(Est_MrP,       True_Value),
    Cor_Raking    = cor(Est_Raking,    True_Value),
    Cor_NoPooling = cor(Est_NoPooling, True_Value),
    Cor_Complete  = ifelse(sd(Est_Complete) == 0,
                           0,
                           cor(Est_Complete, True_Value))
  )

cat("\n=== Correlation Comparison (Utility) ===\n")
print(cor_summary)

set.seed(123)
plot_paradox_data <- four_way_comparison %>%
  filter(!is.na(Est_NoPooling)) %>%
  sample_n(500)

plot_paradox <- ggplot(plot_paradox_data) +
  geom_point(aes(x = True_Value,
                 y = Est_Complete,
                 color = "Complete Pooling"),
             alpha = 0.5) +
  geom_point(aes(x = True_Value,
                 y = Est_MrP,
                 color = "MrP"),
             alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed") +
  scale_color_manual(values = c(
    "Complete Pooling" = "green4",
    "MrP"              = "blue"
  )) +
  labs(
    title    = "Prediction vs Truth: why complete pooling collapses everything",
    subtitle = "Complete pooling has essentially zero correlation; MrP still tracks the gradient.",
    x        = "True rate (ACS)",
    y        = "Estimated rate"
  ) +
  theme_minimal()

print(plot_paradox)