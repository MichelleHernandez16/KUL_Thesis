############################################################
# Survival analysis of footballer career length (Full EDA)
# - Data: premier_simple.csv (One row per player)
# - Note: Baseline characteristics & Intensity Rates are used 
#         for modeling to prevent data leakage.
############################################################

#-----------------------------------------------------------
# 0. Packages and Setup
#-----------------------------------------------------------
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(survival)
library(survminer)
library(gridExtra) # Useful for arranging plots if installed
library(lubridate)
library(patchwork)
library(tidyr)

set.seed(123)

#-----------------------------------------------------------
# 1. Load Data
#-----------------------------------------------------------
# Recommended: Use relative paths (e.g., "data/premier_simple.csv")
df <- read_csv("la_liga_stats_2010_2015_depured2.csv", show_col_types = FALSE)
injuries_df <- read_csv("liga_injuries_detailed.csv", show_col_types = FALSE)
career_df <- read_csv("liga_history_detailed.csv", show_col_types = FALSE)


#-----------------------------------------------------------
# 2. Data Engineering
#-----------------------------------------------------------
# We perform all data cleaning, factor creation, and event 
# flagging in one centralized block.
surv_df <- df %>%
  select(
    player_name = player_name,
    fbref_url = player_url_fbref,
    transfer_url = player_url_tra,
    nationality = citizenship_tra, 
    career_length,
    retired             = retired_tra,          # "Yes"/"No"
    retirement_age,
    age_current = `current age_fbref`,
    debut_age = debut_age_transfermarket,
    debut_year = debut_year_fbref,
    injury_count_total = injury_count_total_tra,
    days_missed_total = days_missed_total_tra,
    games_missed_total = games_missed_total_tra,
    total_matches_club = `CAREER Sum of matches_played`,
    total_matches_total = `total_matches_total`, #Needed for time-dependence (club+nationalteam)
    seasons_club = total_seasons_national,         # NEW: Added for rate calculations
    foot = foot_tra,        # NEW: Extracted preferred foot
    position_simple,
    height = height_tra
  ) %>%
  mutate(
    # Clean numeric variables
    debut_age_num = as.numeric(gsub("[^0-9.]", "", debut_age)),
    height_clean = as.numeric(height),
    debut_year_num = as.numeric(debut_year),
    
    # NEW: Calculate Intensity Rates (Prevents Data Leakage)
    # Using seasons_club to find average injuries per season
    injuries_per_season = ifelse(!is.na(seasons_club) & seasons_club > 0, 
                                 injury_count_total / seasons_club, 0),
    
    # NEW: Clean Foot Variable
    foot_clean = case_when(
      tolower(foot) %in% c("left", "right", "both") ~ tolower(foot),
      TRUE ~ "unknown"
    ),
    
    # Create Binary Event for Survival Analysis (1 = Retired, 0 = Active/Censored)
    event = case_when(
      retired == "Yes" ~ 1L,
      retired == "No" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Create Factors / Bands
    debut_band = cut(debut_age_num, breaks = c(15, 18, 21, 24, 30), include.lowest = TRUE, right = FALSE),
    height_group = cut(height_clean, breaks = quantile(height_clean, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), include.lowest = TRUE),
    inj_band = cut(injury_count_total, breaks = quantile(injury_count_total, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), include.lowest = TRUE),
    
    # Create Decade for cohort analysis
    debut_decade = paste0(floor(debut_year_num / 10) * 10, "s"),
    
    #Clean Nationality
    nationality_clean = nationality %>%
      str_replace_all("\u00A0", " ") %>%   # No non-breaking spaces
      str_squish(),            
    
    # Create Regions for Frailty Model
    region = case_when(
      str_detect(nationality_clean, "England|Scotland|Wales|Northern Ireland|Ireland") ~ "UK_Ireland",
      str_detect(nationality_clean, "France|Spain|Germany|Italy|Netherlands|Portugal|Belgium") ~ "EU_Major",
      str_detect(nationality_clean, "Brazil|Argentina|Uruguay") ~ "South_America",
      TRUE ~ "Rest_of_World"
    )
  ) %>%
  # Filter to complete cases to ensure Stepwise AIC works properly
  filter(
    !is.na(career_length), 
    !is.na(event), 
    !is.na(debut_age_num), 
    !is.na(position_simple), 
    !is.na(height_group),
    !is.na(region),
    !is.na(injuries_per_season)
  )

#save csv 
write.csv(surv_df, "check3.csv", row.names = FALSE)

# Convert character variables to factors for proper model baselines
surv_df$foot_clean <- as.factor(surv_df$foot_clean)
surv_df$debut_decade <- as.factor(surv_df$debut_decade)



#-----------------------------------------------------------
# 3. Univariate Distributions
#-----------------------------------------------------------
num_vars <- c("career_length", "debut_age_num", "injury_count_total", 
              "injuries_per_season", "height_clean")

for (v in num_vars) {
  if (v %in% names(surv_df)) {
    p <- ggplot(surv_df, aes(x = .data[[v]])) +
      geom_histogram(bins = 30, colour = "white", fill = "steelblue") +
      theme_minimal() +
      labs(title = paste("Distribution of", v))
    print(p)
  }
}

ggplot(surv_df, aes(x = position_simple)) +
  geom_bar(fill = "firebrick", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Count by Position")

# NEW: Plot Foot distribution
ggplot(surv_df, aes(x = foot_clean)) +
  geom_bar(fill = "forestgreen", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Count by Preferred Foot")

#-----------------------------------------------------------
# 4. Bivariate Relationships (Exploratory Only)
#-----------------------------------------------------------
ggplot(surv_df, aes(x = position_simple, y = career_length)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Career Length by Position")

ggplot(surv_df, aes(x = debut_age_num, y = career_length)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  theme_minimal() +
  labs(title = "Career Length vs Debut Age")

#-----------------------------------------------------------
# 5. Kaplan-Meier Curves (Univariate Survival)
#-----------------------------------------------------------
surv_obj <- Surv(time = surv_df$career_length, event = surv_df$event)

# 5.1 Overall
km_overall <- survfit(surv_obj ~ 1)
ggsurvplot(km_overall, data = surv_df, risk.table = TRUE, title = "Overall Survival")

# 5.2 By Position
km_pos <- survfit(surv_obj ~ position_simple, data = surv_df)
ggsurvplot(km_pos, data = surv_df, pval = TRUE, risk.table = TRUE, 
           title = "Survival by Position", legend.title = "Pos")

# 5.3 By Debut Age Band
km_debut <- survfit(surv_obj ~ debut_band, data = surv_df)
ggsurvplot(km_debut, data = surv_df, pval = TRUE, risk.table = TRUE, 
           title = "Survival by Debut Age", legend.title = "Age Band")

# 5.4 By Decade (Are modern players lasting longer?)
km_decade <- survfit(surv_obj ~ debut_decade, data = surv_df)
ggsurvplot(km_decade, data = surv_df, pval = TRUE, risk.table = TRUE,
           xlim = c(0, 15), # Limit to 15 years for comparability
           title = "Career Survival by Decade", legend.title = "Debut Decade",
           palette = "jco")


#-----------------------------------------------------------
# 6. Cox Proportional Hazards Models & AIC Selection
#-----------------------------------------------------------

# 6.1 Define the "Full" Model with all valid covariates
# NEW: Added foot_clean, debut_decade, and injuries_per_season
cox_full <- coxph(Surv(career_length, event) ~ position_simple + debut_band + 
                    height_group + region + foot_clean + debut_decade + 
                    injuries_per_season, data = surv_df)

# 6.2 Perform Stepwise Covariate Selection based on AIC
cat("\n--- Running Stepwise AIC Selection for Cox Model ---\n")
cox_aic <- step(cox_full, direction = "both", trace = 1)

# Print final AIC-selected model
cat("\n--- Final Selected Cox Model ---\n")
summary(cox_aic)

#-----------------------------------------------------------
# 7. Advanced Diagnostics & Visualization
#-----------------------------------------------------------

# 7.1 Forest Plot (Using the AIC-Selected Model)
cox_summary <- summary(cox_aic)
hr_data <- data.frame(
  term = rownames(cox_summary$conf.int),
  hr = cox_summary$conf.int[, "exp(coef)"],
  lower = cox_summary$conf.int[, "lower .95"],
  upper = cox_summary$conf.int[, "upper .95"]
)

# Forest Plot
ggplot(hr_data, aes(y = term, x = hr)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Hazard Ratios for Career Retirement Risk",
    subtitle = "Right of Red Line = Higher Risk | Left of Red Line = Protective",
    x = "Hazard Ratio (95% CI)",
    y = ""
  )

# 7.2 Time-Varying Effects Check
zph_result <- cox.zph(cox_aic)
print(zph_result)

#-----------------------------------------------------------
# 8. Accelerated Failure Time (AFT) Model & AIC Selection
#-----------------------------------------------------------

# 8.1 Define the "Full" AFT Model
aft_full <- survreg(Surv(career_length, event) ~ position_simple + debut_age_num + 
                      height_clean + region + foot_clean + debut_decade + 
                      injuries_per_season, data = surv_df, dist = "weibull")

# 8.2 Perform Stepwise Covariate Selection based on AIC
cat("\n--- Running Stepwise AIC Selection for AFT Model ---\n")
aft_aic <- step(aft_full, direction = "both", trace = 0) # trace = 0 hides console clutter

# Print final AIC-selected model summary
cat("\n--- Final Selected AFT Model ---\n")
summary(aft_aic)

# 8.3 Extract Time Ratios (TR) from final model
# TR > 1 means the variable EXTENDS career length
time_ratios <- exp(coef(aft_aic))
print(time_ratios)

#-----------------------------------------------------------
# 9. Frailty Model by Region
#-----------------------------------------------------------
# Treating Region as a random effect to catch unobserved traits
cox_frailty <- coxph(Surv(career_length, event) ~ position_simple + debut_band + 
                       injuries_per_season + frailty(region), data = surv_df)

summary(cox_frailty)






############################################################
# 10. Cox with Time Dependence by Performance 
############################################################

# Time-dependent covariates and arrange by player-season
career_long <- career_df %>%
  mutate(
    season_year = as.numeric(str_extract(season_simple, "^[0-9]{4}")),
    matches_played = as.numeric(matches_played),
    minutes = as.numeric(minutes),
    goals = as.numeric(goals),
    assists = as.numeric(assists),
    age = as.numeric(age)
  ) %>%
  arrange(player_url, season_year) %>%
  group_by(player_url) %>%
  mutate(
    # time in years since the debut
    tstart = row_number() - 1,
    tstop  = row_number(),
    
    # The event of interest (only if the player is retired already)
    retired_bin = ifelse(retired %in% c("Yes", "YES", "yes", 1, TRUE), 1, 0),
    event = ifelse(row_number() == n() & retired_bin == 1, 1, 0)
  ) %>%
  ungroup()



#Add the previous variables used before
eda_player <- surv_df %>%
  rename(
    player_url = fbref_url,
    event_static = event  # Needed to run the dynamic analysis
  ) %>%
  distinct(player_url, .keep_all = TRUE)

analysis_df <- career_long %>%
  left_join(eda_player, by = "player_url")


#Create time dependent covariates
analysis_final <- analysis_df %>%
  group_by(player_url) %>%
  arrange(season_year) %>%
  mutate(
    # 1. Contemporary variables per season
    matches_this_season = matches_played,
    minutes_this_season = minutes,
    goals_this_season = goals,
    
    # 2. Accumulated variables  (experience/wear and tear)
    # Using lag "until beggining of this season"
    cum_matches = lag(cumsum(matches_played), default = 0),
    cum_minutes = lag(cumsum(minutes), default = 0),
    cum_goals = lag(cumsum(goals), default = 0),
    
    # 3. Rollng windows (Recent load)
    matches_lag1 = lag(matches_played, 1, default = 0),
    matches_lag2 = lag(matches_played, 2, default = 0),
    matches_lag3 = lag(matches_played, 3, default = 0),
    
    matches_last_1y = matches_lag1,
    matches_last_2y = matches_lag1 + matches_lag2,
    matches_last_3y = matches_lag1 + matches_lag2 + matches_lag3,
    
    # 4. Load indicators
    high_load = matches_played > 40,
    low_load = matches_played < 10,
    
    # 5. Load changes
    load_change = matches_played - matches_lag1,
    pct_increase = ifelse(matches_lag1 > 0, 
                          (matches_played - matches_lag1) / matches_lag1 * 100,
                          0),
    sharp_increase = pct_increase > 50,
    
    # 6. Experience (since debut)
    experience = tstart,
    experience_sq = experience^2,
    
    # 7. age 
    age = age  
  ) %>%
  ungroup()

# Check for NAs
summary(analysis_final %>% select(matches_this_season:age))



#Fix Nas
# make missing category
analysis_clean <- analysis_final %>%
  mutate(
    inj_band = factor(ifelse(is.na(inj_band), "Missing", as.character(inj_band)))
  )




# Validate the sum of the seasons with total_matches_total
player_check_total <- analysis_final %>%
  group_by(player_url) %>%
  summarise(
    sum_season = sum(matches_played, na.rm = TRUE),
    total_total = first(total_matches_total),  
    diff_total = sum_season - first(total_matches_total)
  )

summary(player_check_total$diff_total) #perfect, only 5 NAs and sum is ok, no duplicates


#Prepare final dataset
model_ready <- analysis_clean %>%
  # Make factor
  mutate(
    position_simple = as.factor(position_simple),
    debut_band = as.factor(debut_band),
    height_group = as.factor(height_group),
    inj_band = as.factor(ifelse(is.na(inj_band), "Missing", as.character(inj_band))),
    
    # Make additional time dependent
    .by = player_url
  ) %>%
  arrange(player_url, season_year) %>%
  group_by(player_url) %>%
  mutate(

    # Cumulative (Begining of season)
    # LAGGED VARIABLES: values from previous seasons
    # Using lag() prevents look-ahead bias
    # CUMULATIVE VARIABLES: total career exposure BEFORE this season
    # Using lag() ensures we don't use future information
    cum_matches = lag(cumsum(matches_played), default = 0),
    cum_goals = lag(cumsum(goals), default = 0),
    cum_minutes = lag(cumsum(minutes), default = 0),
    # - cum_matches captures "total mileage" on the player's body
    # - Separates chronic wear from acute fatigue
    # - Can detect non-linear effects (e.g., thresholds)
    
    # ROLLING WINDOWS: sum of matches in recent seasons
    # These capture "recent workload" effects
    matches_lag1 = lag(matches_played, 1, default = 0),
    matches_lag2 = lag(matches_played, 2, default = 0),
    matches_lag3 = lag(matches_played, 3, default = 0),
    
    matches_last_2y = matches_lag1 + matches_lag2,
    matches_last_3y = matches_lag1 + matches_lag2 + matches_lag3,
    
    # Loads : thresholds for extreme exposure
    high_load = matches_played > 40,
    low_load = matches_played < 10,
    
    # Load changes
    # LOAD CHANGES: rapid increases in workload
    # These capture "spikes" that might increase injury risk
    load_change = matches_played - matches_lag1,
    pct_increase = ifelse(matches_lag1 > 0, load_change / matches_lag1 * 100, 0),
    sharp_increase = pct_increase > 50,
    #  Rapid workload increases might be injury risk factors
    #  High load might indicate peak performance OR overuse
    # Low load might indicate decline or persistent injury
    
    
    # Experience : time since debut
    experience = tstart,
    experience_sq = experience^2
  ) %>%
  ungroup()


cox_model <- coxph(
  Surv(tstart, tstop, event) ~
    # Static covariates
    position_simple +
    debut_band +
    height_group +
    region +
    inj_band +
    
    # Time dependent covariates
    matches_played +
    cum_matches +
    matches_last_2y +
    high_load +
    sharp_increase +
    
    # Experience
    experience +
    
    cluster(player_url),
  data = model_ready,
  id = player_url
)

# Results
summary(cox_model)

# Test PH
ph_test <- cox.zph(cox_model)
print(ph_test)




############################################################
# 11. Cox with Time Dependence by Injuries and AIC selection 

############################################################

names(injuries_df)

# Process injuries_df
injuries_processed <- injuries_df %>%
  mutate(
    # Dates
    from_date = dmy(from_date),
    until_date = dmy(until_date),
    
    # Extract season
    season_year = as.numeric(season_simple),
    
    # Construct categories
    severity_cat = case_when(
      days <= 7 ~ "Minor (≤1 week)",
      days <= 28 ~ "Moderate (1-4 weeks)",
      days <= 90 ~ "Serious (1-3 months)",
      days > 90 ~ "Severe (>3 months)"
    ),
    
    # Type of injury
    injury_group = case_when(
      grepl("muscle|hamstring|thigh|calf|groin", tolower(injury_type)) ~ "Muscular",
      grepl("knee|ankle|ligament|acl|meniscus", tolower(injury_type)) ~ "Joint/Ligament",
      grepl("fracture|bone|break", tolower(injury_type)) ~ "Bone",
      grepl("head|concussion", tolower(injury_type)) ~ "Head",
      grepl("back|spine", tolower(injury_type)) ~ "Back/Spine",
      TRUE ~ "Other"
    )
  )

# Check
glimpse(injuries_processed)




# Distribution
injury_type_summary <- injuries_processed %>%
  group_by(injury_group) %>%
  summarise(
    n_injuries = n(),
    avg_days = mean(days, na.rm = TRUE),
    median_days = median(days, na.rm = TRUE),
    total_days = sum(days, na.rm = TRUE),
    pct = n() / nrow(injuries_processed) * 100
  ) %>%
  arrange(desc(n_injuries))

print(injury_type_summary)

# Visual
p1 <- ggplot(injury_type_summary, aes(x = reorder(injury_group, n_injuries), y = n_injuries)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Distribution per type",
       x = "", y = "Number of injuries") +
  theme_minimal()

# 1.2 Distribution per severity
severity_summary <- injuries_processed %>%
  group_by(severity_cat) %>%
  summarise(
    n_injuries = n(),
    avg_days = mean(days, na.rm = TRUE),
    pct = n() / nrow(injuries_processed) * 100
  )

p2 <- ggplot(severity_summary, aes(x = severity_cat, y = n_injuries, fill = severity_cat)) +
  geom_col() +
  labs(title = "Injuries per severity",
       x = "", y = "Number") +
  theme_minimal() +
  theme(legend.position = "none")

# 1.3 Top Injuries
top_injuries <- injuries_processed %>%
  count(injury_type, sort = TRUE) %>%
  head(10)

p3 <- ggplot(top_injuries, aes(x = reorder(injury_type, n), y = n)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(title = "Top 10 Injuries",
       x = "", y = "Frequency") +
  theme_minimal()

# Show

(p1 | p2) / p3




# Create dataset by season and player
injuries_by_season <- injuries_processed %>%
  group_by(player_url, season_year) %>%
  summarise(
    # Conting
    n_injuries = n(),
    total_days_missed = sum(days, na.rm = TRUE),
    total_games_missed = sum(games_missed, na.rm = TRUE),
    
    # severity
    minor_injuries = sum(severity_cat == "Minor (≤1 week)"),
    moderate_injuries = sum(severity_cat == "Moderate (1-4 weeks)"),
    serious_injuries = sum(severity_cat == "Serious (1-3 months)"),
    severe_injuries = sum(severity_cat == "Severe (>3 months)"),
    
    # Per type
    muscular_injuries = sum(injury_group == "Muscular"),
    joint_injuries = sum(injury_group == "Joint/Ligament"),
    bone_injuries = sum(injury_group == "Bone"),
    head_injuries = sum(injury_group == "Head"),
    back_injuries = sum(injury_group == "Back/Spine"),
    
    # Days per type
    days_muscular = sum(days[injury_group == "Muscular"], na.rm = TRUE),
    days_joint = sum(days[injury_group == "Joint/Ligament"], na.rm = TRUE),
    
    # Advanced
    n_injury_types = n_distinct(injury_group),
    max_severity = max(days, na.rm = TRUE),
    
    # Multiple injuries
    multiple_injuries = n_injuries > 1,
    
    .groups = "drop"
  )


# Add all the seasons with no injuries from the previous dataset per player

all_seasons <- career_df %>%
  mutate(season_year = as.numeric(str_extract(season_simple, "^[0-9]{4}"))) %>%
  select(player_url, season_year) %>%
  distinct()

injuries_complete <- all_seasons %>%
  left_join(injuries_by_season, by = c("player_url", "season_year")) %>%
  mutate(across(c(n_injuries:max_severity), ~replace_na(., 0)))



### Create cumulatives

injuries_final <- injuries_complete %>%
  arrange(player_url, season_year) %>%
  group_by(player_url) %>%
  mutate(
    # Cumulative at the beginning of the season
    cum_injuries = lag(cumsum(n_injuries), default = 0),
    cum_days_missed = lag(cumsum(total_days_missed), default = 0),
    cum_games_missed = lag(cumsum(total_games_missed), default = 0),
    cum_serious = lag(cumsum(serious_injuries + severe_injuries), default = 0),
    
    # Rolling windows (last seasons))
    injuries_last_1y = lag(n_injuries, 1, default = 0),
    injuries_last_2y = lag(n_injuries, 1, default = 0) + lag(n_injuries, 2, default = 0),
    injuries_last_3y = lag(n_injuries, 1, default = 0) + lag(n_injuries, 2, default = 0) + 
      lag(n_injuries, 3, default = 0),
    
    days_last_2y = lag(total_days_missed, 1, default = 0) + 
      lag(total_days_missed, 2, default = 0),
    
    # Metrics
    had_serious_last_2y = (lag(serious_injuries, 1, default = 0) + 
                             lag(serious_injuries, 2, default = 0)) > 0,
    
    recurring = n_injuries > 0 & lag(n_injuries, 1, default = 0) > 0,
    
    # Ratio
    avg_severity_hist = ifelse(cum_injuries > 0, cum_days_missed / cum_injuries, 0)
  ) %>%
  ungroup()




### Add to previous model

# ADD to model_ready (previous)
model_with_injuries <- model_ready %>%
  left_join(injuries_final, by = c("player_url", "season_year"))

# check join
summary(model_with_injuries %>% 
          select(n_injuries, cum_injuries, total_days_missed))






#Cox model with injuries


# Basic model with injuries
cox_injuries <- coxph(
  Surv(tstart, tstop, event) ~
    # Stats
    # Static covariates
    position_simple +
    debut_band +
    height_group +
    region +
    foot_clean +
   
    
    # Performance
    matches_played + cum_matches + matches_last_2y +
    high_load + sharp_increase +
    
    # Injuries (NEW)
    n_injuries + cum_injuries + injuries_last_2y + had_serious_last_2y +
    
    # Experience
    experience +
    
    cluster(player_url),
  data = model_with_injuries,
  id = player_url
)

summary(cox_injuries)

# Test de PH
ph_test_injuries <- cox.zph(cox_injuries)
print(ph_test_injuries)



## Model Comparison
AIC(cox_model, cox_injuries)

# Forest plot para variables de lesiones
library(ggplot2)

inj_hr <- data.frame(
  variable = c("Injuries current season", "Accumulated injuries", 
               "Last 2 year Injuries", "Severe Injurie Recently"),
  hr = exp(coef(cox_injuries)[c("n_injuries", "cum_injuries", 
                                "injuries_last_2y", "had_serious_last_2yTRUE")]),
  lower = exp(confint(cox_injuries)[c("n_injuries", "cum_injuries", 
                                      "injuries_last_2y", "had_serious_last_2yTRUE"), 1]),
  upper = exp(confint(cox_injuries)[c("n_injuries", "cum_injuries", 
                                      "injuries_last_2y", "had_serious_last_2yTRUE"), 2])
)

ggplot(inj_hr, aes(x = hr, y = reorder(variable, hr))) +
  geom_point(size = 3, color = "darkred") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "darkred") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_log10() +
  labs(title = "Effect of Injuries in Hazard",
       x = "Hazard Ratio (log scale)", y = "") +
  theme_minimal()



#AIC selection
cox_full_aic <- coxph(
  Surv(tstart, tstop, event) ~
    position_simple +
    debut_band +
    height_group +
    region +
    foot_clean +
    matches_played +
    cum_matches +
    matches_last_2y +
    high_load +
    sharp_increase +
    n_injuries +
    cum_injuries +
    injuries_last_2y +
    had_serious_last_2y,
  data = model_with_injuries,
  id = player_url
)
#Selection
cat("\n--- Running Stepwise AIC Selection for Cox Model ---\n")

cox_aic <- step(
  cox_full_aic,
  direction = "both",
  trace = 0
)

summary(cox_aic)

AIC(cox_aic)
AIC(cox_full_aic, cox_aic)


cox_final <- coxph(
  Surv(tstart, tstop, event) ~
    position_simple +
    debut_band +
    region +
    foot_clean +
    matches_played +
    cum_matches +
    matches_last_2y +
    high_load +
    sharp_increase +
    n_injuries +
    cum_injuries +
    injuries_last_2y +
    cluster(player_url),
  data = model_with_injuries,
  id = player_url
)

# Print final AIC-selected model summary
summary(cox_final)






###MODEL COMPARISON
# Static models
cox_static_full <- cox_full
cox_static_aic <- cox_aic
cox_frailty_region <- cox_frailty


# AFT models
aft_weibull_full <- aft_full
aft_weibull_aic <- aft_aic

# Time-dependent models
cox_td_perf <- cox_model
cox_td_inj <- cox_injuries
cox_td_aic <- cox_aic
cox_td_final <- cox_final


library(dplyr)
library(knitr)

model_comparison <- data.frame(
  Model = c(
    "KM Overall",
    "KM by Position",
    "KM by Debut Age",
    "KM by Decade",
    "Cox Static Full",
    "Cox Static AIC",
    "Cox Frailty (Region)",
    "Cox Time-Dependent Performance",
    "Cox Time-Dependent + Injuries",
    "Cox Time-Dependent AIC Selected",
    "Cox Time-Dependent Final (Clustered)",
    "AFT Weibull Full",
    "AFT Weibull AIC"
  ),
  
  Type = c(
    "Kaplan-Meier",
    "Kaplan-Meier",
    "Kaplan-Meier",
    "Kaplan-Meier",
    "Cox PH",
    "Cox PH",
    "Cox PH Frailty",
    "Cox PH Time-Dependent",
    "Cox PH Time-Dependent",
    "Cox PH Time-Dependent",
    "Cox PH Time-Dependent",
    "AFT",
    "AFT"
  ),
  
  Parameters = c(
    NA, NA, NA, NA,
    length(coef(cox_static_full)),
    length(coef(cox_static_aic)),
    length(coef(cox_frailty_region)),
    length(coef(cox_td_perf)),
    length(coef(cox_td_inj)),
    length(coef(cox_td_aic)),
    length(coef(cox_td_final)),
    length(coef(aft_weibull_full)),
    length(coef(aft_weibull_aic))
  ),
  
  AIC = c(
    NA, NA, NA, NA,
    AIC(cox_static_full),
    AIC(cox_static_aic),
    AIC(cox_frailty_region),
    AIC(cox_td_perf),
    NA,                 # clustered models not ideal for AIC comparison
    AIC(cox_td_aic),
    NA,
    AIC(aft_weibull_full),
    AIC(aft_weibull_aic)
  ),
  
  Concordance = c(
    NA, NA, NA, NA,
    summary(cox_static_full)$concordance[1],
    summary(cox_static_aic)$concordance[1],
    summary(cox_frailty_region)$concordance[1],
    summary(cox_td_perf)$concordance[1],
    summary(cox_td_inj)$concordance[1],
    summary(cox_td_aic)$concordance[1],
    summary(cox_td_final)$concordance[1],
    NA,
    NA
  ),
  
  Observations = c(
    nrow(surv_df),
    nrow(surv_df),
    nrow(surv_df),
    nrow(surv_df),
    summary(cox_static_full)$n,
    summary(cox_static_aic)$n,
    summary(cox_frailty_region)$n,
    summary(cox_td_perf)$n,
    summary(cox_td_inj)$n,
    summary(cox_td_aic)$n,
    summary(cox_td_final)$n,
    aft_weibull_full$n,
    aft_weibull_aic$n
  ),
  
  Events = c(
    sum(surv_df$event == 1),
    sum(surv_df$event == 1),
    sum(surv_df$event == 1),
    sum(surv_df$event == 1),
    summary(cox_static_full)$nevent,
    summary(cox_static_aic)$nevent,
    summary(cox_frailty_region)$nevent,
    summary(cox_td_perf)$nevent,
    summary(cox_td_inj)$nevent,
    summary(cox_td_aic)$nevent,
    summary(cox_td_final)$nevent,
    sum(surv_df$event == 1),
    sum(surv_df$event == 1)
  )
)

kable(model_comparison, digits = 3,
      caption = "Comparison of Survival Models for Player Career Duration")



#Now all the other types of models
model_family_table <- data.frame(
  Model = c(
    "Kaplan-Meier (Overall)",
    "Kaplan-Meier (by Position)",
    "Cox PH Static",
    "Cox PH with Frailty (Region)",
    "AFT Weibull",
    "Cox Time-Dependent (AIC)"
  ),
  
  Model_Type = c(
    "Non-parametric",
    "Non-parametric",
    "Semi-parametric",
    "Semi-parametric",
    "Parametric",
    "Semi-parametric"
  ),
  
  Parameters = c(
    NA,
    NA,
    length(coef(cox_full)),
    length(coef(cox_frailty)),
    length(coef(aft_full)),
    length(coef(cox_td_final))
  ),
  
  AIC = c(
    NA,
    NA,
    AIC(cox_full),
    AIC(cox_frailty),
    AIC(aft_full),
    AIC(cox_td_final)
  ),
  
  Concordance = c(
    NA,
    NA,
    summary(cox_full)$concordance[1],
    summary(cox_frailty)$concordance[1],
    NA,
    summary(cox_td_final)$concordance[1]
  )
)

kable(model_family_table,
      digits = 3,
      caption = "Comparison of Survival Modeling Approaches")

