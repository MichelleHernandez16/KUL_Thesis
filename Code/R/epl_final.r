################################################################################
# ADVANCED SURVIVAL ANALYSIS: ENGLISH PREMIER LEAGUE CAREER LENGTH
# Description: Static and dynamic survival analysis of player career length,
# Datasets: premier_simple.csv, players_injuries_detailed.csv,
#           premier_career_history_full.csv
################################################################################

# ==============================================================================
# SECTION 1: Environment Setup & Package Loading
# ==============================================================================
# Load necessary libraries
suppressPackageStartupMessages({
  library(dplyr)      # Data manipulation
  library(tidyr)      # Reshaping data (replace_na, etc.)
  library(stringr)    # String parsing and cleaning
  library(lubridate)  # Date parsing for injury tracking
  library(readr)      # Fast file reading
  library(ggplot2)    # Core visualization
  library(patchwork)  # Combining ggplot panels (p1 | p2 / p3)
  library(survival)   # Core survival models (Cox, AFT, cluster, strata, tt, pspline)
  library(survminer)  # Enhanced survival model visualizations
  library(flexsurv)   # AFT
})

set.seed(123)

# Optional: uncomment and edit if needed
setwd("C:/git/Thesis/Data")

# ==============================================================================
# SECTION 2: Helper Functions
# ==============================================================================
require_columns <- function(data, cols, data_name) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Missing columns in ", data_name, ": ",
      paste(missing_cols, collapse = ", ")
    ))
  }
}

extract_start_year <- function(x) {
  out <- stringr::str_extract(as.character(x), "^[0-9]{4}")
  as.numeric(out)
}

clean_numeric <- function(x) {
  as.numeric(gsub("[^0-9.]", "", as.character(x)))
}

# ==============================================================================
# SECTION 3: Data Ingestion
# ==============================================================================
cat("\n--- Loading Datasets ---\n")

df_static_premier   <- read_csv("premier_simple.csv", show_col_types = FALSE)
df_injuries_premier <- read_csv("players_injuries_detailed.csv", show_col_types = FALSE)
df_history_premier  <- read_csv("premier_career_history_full.csv", show_col_types = FALSE)

require_columns(
  df_static_premier,
  c("player_name.x", "citizenship.x", "fbref_url", "career_length", "retired",
    "debut_age", "debut_year", "injury_count_total", "seasons_club",
    "foot.x", "position_simple", "height.y"),
  "premier_simple.csv"
)

require_columns(
  df_injuries_premier,
  c("player_url", "season_year", "injury_type", "days"),
  "players_injuries_detailed.csv"
)

require_columns(
  df_history_premier,
  c("player_url", "season_simple", "matches_played"),
  "premier_career_history_full.csv"
)

# ==============================================================================
# SECTION 4: Static Data Engineering (Baseline Profile)
# ==============================================================================
cat("\n--- Processing Static Data ---\n")

surv_df_premier <- df_static_premier %>%
  transmute(
    player_name = player_name.x,
    player_url = fbref_url,
    nationality = citizenship.x,
    career_length = as.numeric(career_length),
    retired = retired,
    debut_age = debut_age,
    debut_year = debut_year,
    days_missed_total,
    games_missed_total,
    National_team,
    injury_count_total = as.numeric(injury_count_total),
    seasons_club = as.numeric(seasons_club),
    foot = foot.x,
    position_simple = position_simple,
    height = height.y
  ) %>%
  mutate(
    debut_age_num = clean_numeric(debut_age),
    debut_year_num = as.numeric(debut_year),
    height_clean = as.numeric(height),
    injuries_per_season = ifelse(!is.na(seasons_club) & seasons_club > 0,
                                 injury_count_total / seasons_club, 0),
    foot_clean = factor(case_when(
      str_to_lower(foot) %in% c("left", "right", "both") ~ str_to_lower(foot),
      TRUE ~ "unknown"
    )),
    event = case_when(
      retired %in% c("Yes", "YES", "yes", 1, TRUE) ~ 1L,
      retired %in% c("No", "NO", "no", 0, FALSE) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    nationality_clean = str_squish(str_replace_all(as.character(nationality), "\u00A0", " ")),
    region = factor(case_when(
      str_detect(nationality_clean, "England|Scotland|Wales|Northern Ireland|Ireland") ~ "UK_Ireland",
      str_detect(nationality_clean, "France|Spain|Germany|Italy|Netherlands|Portugal|Belgium") ~ "EU_Major",
      str_detect(nationality_clean, "Brazil|Argentina|Uruguay") ~ "South_America",
      TRUE ~ "Rest_of_World"
    ))
  ) %>%
  filter(complete.cases(career_length, event, debut_age_num, debut_year_num,
                        position_simple, height_clean, region, injuries_per_season))

# ==============================================================================
# SECTION 5: Exploratory Data Analysis (EDA)
# ==============================================================================
cat("\n--- Generating Exploratory Plots ---\n")

# 5.1 Static Dashboard
p_pos <- ggplot(surv_df_premier, aes(x = position_simple)) +
  geom_bar(fill = "firebrick", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Positions", x = "", y = "Count")

p_age <- ggplot(surv_df_premier, aes(x = debut_age_num, y = career_length)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "red") +
  theme_minimal() +
  labs(title = "Debut Age vs Career Length", x = "Debut Age", y = "Years")

print(p_pos | p_age)

# 5.2 Injury EDA (single cleaned object; avoids duplicated objects/sections)
eda_injuries <- df_injuries_premier %>%
  mutate(
    season_year = as.numeric(season_year),
    days = as.numeric(days),
    injury_type = as.character(injury_type),
    severity_cat = case_when(
      days <= 7 ~ "Minor (≤1 week)",
      days <= 28 ~ "Moderate (1-4 weeks)",
      days <= 90 ~ "Serious (1-3 months)",
      TRUE ~ "Severe (>3 months)"
    ),
    injury_group = case_when(
      # Muscular
      grepl("muscle|hamstring|thigh|calf|groin|adductor|abdominal muscle|muscular|muscle fiber|muscle tear|muscle strain|muscle contusion|sore muscles|contracture", 
            tolower(injury_type)) ~ "Muscular",
      # Joint/Ligament
      grepl("knee|ankle|ligament|acl|meniscus|cruciate|collateral|syndesmosis|joint|capsular|cartilage|sprain", 
            tolower(injury_type)) ~ "Joint/Ligament",
      # Hands
      grepl("finger|thumb|hand|wrist|metacarpal", 
            tolower(injury_type)) ~ "Hand/Finger",
      # Bone 
      grepl("fracture|bone|break|fissure|hairline crack|stress reaction|bruise.*bone|bone.*bruise|bone edema", 
            tolower(injury_type)) ~ "Bone",
      # Tendon
      grepl("tendon|achilles|patellar tendon|tendonitis|tendon rupture|tendon tear|tendon irritation", 
            tolower(injury_type)) ~ "Tendon",
      # Contusion
      grepl("bruise|contusion|hematoma|dead leg|knock", 
            tolower(injury_type)) ~ "Contusion",
      # Illness/Infection
      grepl("virus|flu|influenza|quarantine|cold|bronchitis|infection|abscess|tonsillitis|chickenpox|corona|malaria|tuberculosis|mononucleation|stomach flu|intestinal virus", 
            tolower(injury_type)) ~ "Illness/Infection",
      # Internal/Abdominal
      grepl("appendicitis|Appendectomy|appendix|kidney|liver|hernia|inguinal|testicular|testicle|abdominal problems|stomach problems|intestinal", 
            tolower(injury_type)) ~ "Internal/Abdominal",
      # concussion/Neurological
      grepl("concussion|head|cerebral|skull|brain|vertebral|spine|cervical|herniated disc|sciatica|lumbago", 
            tolower(injury_type)) ~ "Head/Spine/Neurological",
      
      # Other
      TRUE ~ "Other"
    )
  )

p_inj_type <- ggplot(eda_injuries %>% count(injury_group),
                     aes(x = reorder(injury_group, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Injury Type Distribution", x = "", y = "Count")

p_inj_sev <- ggplot(eda_injuries %>% count(severity_cat),
                    aes(x = severity_cat, y = n, fill = severity_cat)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Injury Severity Distribution", x = "", y = "Count")

print(p_inj_type | p_inj_sev)




# ------------------------------------------------------------------------------
# 5.2 Univariate Analysis (Distributions of Individual Variables)
# ------------------------------------------------------------------------------
shared_theme <- theme_minimal() +
  theme(
    plot.title    = element_text(size = 20, face = "bold",   hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 15, face = "italic", hjust = 0.5, color = "gray40", margin = margin(b = 12)),
    axis.title.x  = element_text(size = 18, face = "bold", margin = margin(t = 12)),
    axis.title.y  = element_text(size = 18, face = "bold", margin = margin(r = 12)),
    axis.text.x   = element_text(size = 15),
    axis.text.y   = element_text(size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    plot.margin   = margin(25, 25, 25, 25)
  )


# A. Histograms - Regular scale
regular_vars <- list(
  career_length = "Career Length",
  debut_age_num = "Debut Age",
  height_clean = "Height",
  injury_count_total = "Total Injuries"
)

for (v in names(regular_vars)) {
  if (!(v %in% names(surv_df_premier))) next
  pretty_name <- regular_vars[[v]]
  x           <- surv_df_premier[[v]]
  x_min       <- min(x, na.rm = TRUE)
  x_max       <- max(x, na.rm = TRUE)
  breaks_seq  <- seq(x_min, x_max, length.out = 31)
  
  x_label <- ifelse(v == "height_clean", paste(pretty_name, "(m)"), pretty_name)
  
  print(
    ggplot(surv_df_premier, aes(x = .data[[v]])) +
      geom_histogram(breaks = breaks_seq, colour = "white", fill = "steelblue", alpha = 0.7) +
      shared_theme +
      labs(title = paste("Distribution of", pretty_name), subtitle = "Premier League",
           x = x_label, y = "Count")
  )
}

# B. Log scale histograms for skewed vars
log_vars <- list(
  injuries_per_season = "Injuries Per Season"
#  days_missed = "Days Missed"
)

for (v in names(log_vars)) {
  if (!(v %in% names(surv_df_premier))) {
    message("Skipping ", v, " — not found in surv_df_premier")
    next
  }
  pretty_name <- log_vars[[v]]
  
  print(
    ggplot(surv_df_premier, aes(x = .data[[v]])) +
      geom_histogram(colour = "white", fill = "steelblue", alpha = 0.7, bins = 30) +
      scale_x_continuous(trans = "log1p", labels = scales::number_format(accuracy = 0.1)) +
      shared_theme +
      labs(title = paste("Distribution of", pretty_name), subtitle = "Premier League (log scale)",
           x = paste(pretty_name, "(log scale)"), y = "Count")
  )
}

# C. Bar charts - categorical
print(
  ggplot(surv_df_premier, aes(x = position_simple)) +
    geom_bar(fill = "firebrick", alpha = 0.8) +
    shared_theme +
    labs(title = "Count by Position", subtitle = "Premier League", x = "Position", y = "Count")
)

print(
  ggplot(surv_df_premier, aes(x = foot_clean)) +
    geom_bar(fill = "forestgreen", alpha = 0.8) +
    shared_theme +
    labs(title = "Count by Preferred Foot", subtitle = "Premier League", x = "Preferred Foot", y = "Count")
)

print(
  ggplot(surv_df_premier, aes(x = region)) +
    geom_bar(fill = "purple", alpha = 0.8) +
    shared_theme +
    labs(title = "Count by Region", subtitle = "Premier League", x = "Region", y = "Count")
)





# ------------------------------------------------------------------------------
# 5.3 Bivariate Analysis (Predictors vs. Target Variable: Career Length)
# ------------------------------------------------------------------------------
# Range for career length
y_min_cl <- min(surv_df_premier$career_length, na.rm = TRUE)
y_max_cl <- max(surv_df_premier$career_length, na.rm = TRUE)

# Boxplots - categorical vs career length
print(
  ggplot(surv_df_premier, aes(x = position_simple, y = career_length, fill = position_simple)) +
    geom_boxplot(alpha = 0.7, linewidth = 0.7, outlier.alpha = 0.4) +
    coord_cartesian(ylim = c(y_min_cl, y_max_cl)) +
    shared_theme + theme(legend.position = "none") +
    labs(title = "Career Length by Position", subtitle = "Premier League",
         x = "Position", y = "Career Length (Years)")
)

print(
  ggplot(surv_df_premier, aes(x = foot_clean, y = career_length, fill = foot_clean)) +
    geom_boxplot(alpha = 0.7, linewidth = 0.7, outlier.alpha = 0.4) +
    coord_cartesian(ylim = c(y_min_cl, y_max_cl)) +
    shared_theme + theme(legend.position = "none") +
    labs(title = "Career Length by Preferred Foot", subtitle = "Premier League",
         x = "Preferred Foot", y = "Career Length (Years)")
)

print(
  ggplot(surv_df_premier, aes(x = National_team, y = career_length, fill = National_team)) +
    geom_boxplot(alpha = 0.7, linewidth = 0.7, outlier.alpha = 0.4) +
    coord_cartesian(ylim = c(y_min_cl, y_max_cl)) +
    shared_theme + theme(legend.position = "none") +
    labs(title = "Career Length by National Team", subtitle = "Premier League",
         x = "National Team", y = "Career Length (Years)")
)

# Scatterplots
x_min_age <- min(surv_df_premier$debut_age_num, na.rm = TRUE)
x_max_age <- max(surv_df_premier$debut_age_num, na.rm = TRUE)
x_min_inj <- min(surv_df_premier$injuries_per_season, na.rm = TRUE)
x_max_inj <- max(surv_df_premier$injuries_per_season, na.rm = TRUE)
x_min_h <- min(surv_df_premier$height_clean, na.rm = TRUE)
x_max_h <- max(surv_df_premier$height_clean, na.rm = TRUE)

print(
  ggplot(surv_df_premier, aes(x = debut_age_num, y = career_length)) +
    geom_point(alpha = 0.3, size = 2, colour = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, colour = "firebrick", linewidth = 1.2) +
    coord_cartesian(xlim = c(x_min_age, x_max_age), ylim = c(y_min_cl, y_max_cl)) +
    shared_theme +
    labs(title = "Career Length vs Debut Age", subtitle = "Premier League",
         x = "Debut Age (Years)", y = "Career Length (Years)")
)

print(
  ggplot(surv_df_premier, aes(x = height_clean, y = career_length)) +
    geom_point(alpha = 0.3, size = 2, colour = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, colour = "blue", linewidth = 1.2) +
    coord_cartesian(xlim = c(x_min_h, x_max_h), ylim = c(y_min_cl, y_max_cl)) +
    shared_theme +
    labs(title = "Career Length vs Height", subtitle = "Premier League",
         x = "Height (m)", y = "Career Length (Years)")
)

print(
  ggplot(surv_df_premier, aes(x = injuries_per_season, y = career_length)) +
    geom_point(alpha = 0.3, size = 2, colour = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, colour = "darkgreen", linewidth = 1.2) +
    coord_cartesian(xlim = c(x_min_inj, x_max_inj), ylim = c(y_min_cl, y_max_cl)) +
    shared_theme +
    labs(title = "Career Length vs Injury Rate", subtitle = "Premier League",
         x = "Injuries per Season", y = "Career Length (Years)")
)


# ==============================================================================
# SECTION 6: Kaplan-Meier Survival Estimates
# ==============================================================================
cat("\n--- Kaplan-Meier Survival Estimates ---\n")

surv_static <- Surv(time = surv_df_premier$career_length, event = surv_df_premier$event)
summary(surv_static)

km_df <- surv_df_premier %>%
  mutate(
    debut_band = cut(debut_age_num, breaks = c(15, 18, 21, 24, 30), right = FALSE),
    debut_decade = factor(paste0(floor(debut_year_num / 10) * 10, "s"))
  )
summary(km_df)



fit_pos <- survfit(surv_static ~ position_simple, data = surv_df_premier)

fit_decade <- survfit(surv_static ~ debut_decade, data = km_df)

fit_age <- survfit(surv_static ~ debut_band, data = km_df)

fit_nat <- survfit(surv_static ~ National_team, data = km_df)





summary(fit_pos)
summary(fit_nat)



print(ggsurvplot(survfit(surv_static ~ 1, data = surv_df_premier), data = surv_df_premier,
                 risk.table = TRUE, title = "Overall Career Survival"))



ggsurvplot(
  fit_pos,
  data = km_df,
  pval = TRUE,
  risk.table = TRUE,
  risk.table.fontsize = 4,
  title = "Survival by Position",
  legend.labs = c("Attack", "Defender", "Goalkeeper", "Midfielder")
)


 ggsurvplot(
  fit_decade,
  data = km_df,
  pval = TRUE,
  risk.table = TRUE,
  risk.table.fontsize = 4,
  title = "Survival by Debut Decade",
  legend.labs = c("1900s", "2000s", "2010s")
)


ggsurvplot(
  fit_age,
  data = km_df,
  pval = TRUE,
  risk.table = TRUE,
  risk.table.fontsize = 4,
  title = "Survival by Debut Age",
  legend.labs = c("[15-18]", "[18-21]", "[21-24]", "[24-30]")
)


ggsurvplot(
  fit_nat,
  data = km_df,
  pval = TRUE,
  risk.table = TRUE,
  risk.table.fontsize = 4,
  title = "Survival by National Team",
  legend.labs = c("No", "Yes")
)

# ==============================================================================
# SECTION 7: Static Baseline Models (Cox & AFT)
# ==============================================================================
cat("\n--- Running Static Models ---\n")

# Improved over old EPL model: continuous debut age and height retained via splines
cox_static_full <- coxph(
  Surv(career_length, event) ~ position_simple + pspline(debut_age_num) +
    pspline(height_clean) + region + National_team + foot_clean + injuries_per_season,
  data = surv_df_premier
)


print(summary(cox_static_full))

cox_static_aic <- step(cox_static_full, direction = "both", trace = 0)
cat("\n--- Final Selected Static Cox Model (via AIC) ---\n")
print(summary(cox_static_aic))

zph_static <- cox.zph(cox_static_aic)
print(zph_static)

aft_weibull <- flexsurvreg(
  Surv(career_length, event) ~ position_simple + debut_age_num +National_team +
    height_clean + region + foot_clean + injuries_per_season,
  data = surv_df_premier,
  dist = "weibull"
)
cat("\n--- Weibull AFT Model ---\n")
print(aft_weibull)

# Forest plot for static AIC-selected model
cox_summary <- summary(cox_static_aic)
hr_data <- data.frame(
  term = rownames(cox_summary$conf.int),
  hr = cox_summary$conf.int[, "exp(coef)"],
  lower = cox_summary$conf.int[, "lower .95"],
  upper = cox_summary$conf.int[, "upper .95"]
)

print(
  ggplot(hr_data, aes(y = reorder(term, hr), x = hr)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "Hazard Ratios: Static Cox Model",
         x = "Hazard Ratio (95% CI)", y = "")
)



# 2. Test non-monotonic alternatives
aft_lognormal <- flexsurvreg(
  Surv(career_length, event) ~ position_simple + debut_age_num +National_team +
    height_clean + region + foot_clean + injuries_per_season,
  data = surv_df_premier,
  dist = "lognormal"
)
cat("\n--- Lognormal AFT Model ---\n")
print(aft_lognormal)



aft_loglogistic <- flexsurvreg(
  Surv(career_length, event) ~ position_simple + debut_age_num +National_team +
    height_clean + region + foot_clean + injuries_per_season,
  data = surv_df_premier,
  dist = "llogis"
)
cat("\n--- Loglogistic AFT Model ---\n")
print(aft_loglogistic)

# Generalized Gamma (flexsurv package)
aft_gengamma <- flexsurvreg(
  Surv(career_length, event) ~ position_simple + debut_age_num + National_team +
    height_clean + region + foot_clean + injuries_per_season,
  data = surv_df_premier,
  dist = "gengamma"
)


# 3. Compare AIC 
aic_table <- data.frame(
  Weibull = AIC(aft_weibull),
  LogNormal = AIC(aft_lognormal),
  LogLogistic = AIC(aft_loglogistic),
  GenGamma = AIC(aft_gengamma)
  
)
print(aic_table)




# 4. Validate fit with Cox-Snell residuals
#using flexsurv function for residuals
cs_weibull  <- coxsnell_flexsurvreg(aft_weibull)
cs_lnorm    <- coxsnell_flexsurvreg(aft_lognormal)
cs_llogis   <- coxsnell_flexsurvreg(aft_loglogistic)
cs_gengamma <- coxsnell_flexsurvreg(aft_gengamma)


names(cs_weibull)


plot_coxsnell <- function(cs, title){
  
  fit <- survfit(
    Surv(cs$est, cs$status) ~ 1
  )
  
  plot(
    fit,
    fun = "cumhaz",
    main = title,
    xlab = "Cox-Snell residuals",
    ylab = "Estimated cumulative hazard"
  )
  
  abline(0, 1, lty = 2, lwd = 2)
}

par(mfrow = c(2,2))

plot_coxsnell(cs_weibull,  "Weibull")
plot_coxsnell(cs_lnorm,    "Lognormal")
plot_coxsnell(cs_llogis,   "Log-logistic")
plot_coxsnell(cs_gengamma, "Generalized Gamma")
par(mfrow = c(1,1))

# ==============================================================================
# SECTION 8: Dynamic Time-Dependent Engineering (Counting Process)
# ==============================================================================
cat("\n--- Engineering Time-Dependent Covariates ---\n")

# 8.1 Process Dynamic Injury Data by Season
injuries_td <- eda_injuries %>%
  mutate(season_year = as.numeric(season_year)) %>%
  group_by(player_url, season_year) %>%
  summarise(
    n_injuries = n(),
    total_days_missed = sum(days, na.rm = TRUE),
    serious_injuries = sum(severity_cat %in% c("Serious (1-3 months)", "Severe (>3 months)")),
    .groups = "drop"
  )



# 8.2 Process Performance Workload by Season
performance_td <- df_history_premier %>%
  mutate(season_year = as.numeric(str_extract(season_simple, "^[0-9]{4}"))) %>%
  group_by(player_url, season_year) %>%
  summarise(matches_played = sum(matches_played, na.rm = TRUE), .groups = "drop")

# 8.3 Merge Base History, Apply Age-Based Time Scale (Fixes Left Truncation)
td_df_full <- df_history_premier %>%
  mutate(season_year = as.numeric(str_extract(season_simple, "^[0-9]{4}"))) %>%
  distinct(player_url, season_year) %>%
  
  # Join metrics
  left_join(injuries_td, by = c("player_url", "season_year")) %>%
  left_join(performance_td, by = c("player_url", "season_year")) %>%
  mutate(across(c(n_injuries, total_days_missed, serious_injuries, matches_played), ~replace_na(., 0))) %>%
  
  # Join static data to calculate Age
  left_join(surv_df_premier %>% select(player_url, debut_year_num, debut_age_num, event_static = event, 
                               position_simple, height_clean, region, foot_clean), by = "player_url") %>%
  filter(!is.na(debut_age_num), !is.na(position_simple)) %>% # Remove unmatched players
  
  arrange(player_url, season_year) %>%
  group_by(player_url) %>%
  mutate(
    # Rolling Workload and Injury Metrics
    cum_injuries = lag(cumsum(n_injuries), default = 0),
    had_serious_last_2y = (lag(serious_injuries, 1, default = 0) + lag(serious_injuries, 2, default = 0)) > 0,
    cum_matches = lag(cumsum(matches_played), default = 0),
    high_load = lag(matches_played, 1, default = 0) > 30,
    
    # METHODOLOGY FIX: Age as the Time Scale (Solves Left Truncation)
    age_start = debut_age_num + (season_year - debut_year_num),
    age_stop  = age_start + 1,
    
    # Event only triggers on the final row if the player officially retired
    is_last_row = row_number() == n(),
    event_dynamic = ifelse(is_last_row & event_static == 1, 1, 0)
  ) %>%
  ungroup() %>%
  # Ensure no logical errors in time computation
  filter(age_stop > age_start)




# ==============================================================================
# SECTION 9: Time-Dependent Cox Model (Methodology Corrected)
# ==============================================================================
cat("\n--- Running Final Time-Dependent Cox Model ---\n")

# This final model uses:
# 1. Age as the underlying time scale (Surv(age_start, age_stop)).
# 2. Continuous height with penalized splines to handle U-shaped physical relationships.
# 3. Stratification by region to correct spatial PH violations.
# 4. Time-Transform tt() functions to correct temporal PH violations for workload metrics.

td_df_full <- df_history_premier %>%
  mutate(season_year = as.numeric(str_extract(season_simple, "^[0-9]{4}"))) %>%
  
  distinct(player_url, season_year) %>%
  
  # Join metrics
  left_join(injuries_td, by = c("player_url", "season_year")) %>%
  left_join(performance_td, by = c("player_url", "season_year")) %>%
  mutate(across(c(n_injuries, total_days_missed, serious_injuries,
                  matches_played), ~replace_na(., 0))) %>%
  
  # JOIN STATIC DATA — 
  left_join(surv_df_premier %>% select(player_url,
                               debut_year_num,
                               debut_age_num,
                               event_static = event,
                               position_simple,
                               height_clean,
                               region,
                               foot_clean,
                               National_team),  # <-- add this line
            by = "player_url") %>%
  
  filter(!is.na(debut_age_num), !is.na(position_simple)) %>%
  
  arrange(player_url, season_year) %>%
  group_by(player_url) %>%
  mutate(
    # Rolling metrics
    cum_injuries        = lag(cumsum(n_injuries), default = 0),
    had_serious_last_2y = (lag(serious_injuries, 1, default = 0) +
                             lag(serious_injuries, 2, default = 0)) > 0,
    cum_matches         = lag(cumsum(matches_played), default = 0),
    high_load           = lag(matches_played, 1, default = 0) > 30,
    
    sharp_increase = (lag(matches_played, 1, default = 0) - 
      lag(matches_played, 2, default = 0)) > 15,
    
    # Age time scale
    age_start = debut_age_num + (season_year - debut_year_num),
    age_stop  = age_start + 1,
    
    #logs
    log_days_missed  = log1p(total_days_missed),  # skewness 21 → needs fixing
    log_cum_injuries  = log1p(cum_injuries),  # skewness 21 → needs fixing
    log_n_injuries   = log1p(n_injuries)   ,        # optional, modest improvement
    
    # Event
    is_last_row    = row_number() == n(),
    event_dynamic  = ifelse(is_last_row & event_static == 1, 1, 0)
  ) %>%
  ungroup() %>%
  filter(age_stop > age_start)


#model with national
cox_td_national <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~
    position_simple +
    strata(region) +
    National_team +
    had_serious_last_2y +
    matches_played +
    high_load+
    log_days_missed  +
    log_n_injuries+
    # log_cum_injuries+ #non significant, days missed and n injuries already capture severity
    tt(high_load )+
    tt(matches_played) +
    cluster(player_url),
  data = td_df_full,
  id = player_url,
  tt = function(x, t, ...) x * log(t + 1)
)

summary(cox_td_national)



# Without tt() terms for Schoenfeld testing
cox_td_ph_check <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~
    position_simple +
    strata(region) +
    National_team +
    high_load+
    matches_played+
    had_serious_last_2y +
    log_days_missed  +
    log_n_injuries+
    # log_cum_injuries+ #non significant, days missed and n injuries already capture severity
    cluster(player_url),
  data = td_df_full,
  id = player_url
)

ph_test_td <- cox.zph(cox_td_ph_check)
print(ph_test_td)

par(mfrow = c(3, 3))
plot(ph_test_td)


# 8.1 INTERACTIONS

# Interaction 1 — Position × had_serious_last_2y


cox_int_1 <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~ 
    position_simple * had_serious_last_2y +  # interaction
    strata(region) +
    National_team +
    n_injuries + 
    cum_injuries + 
    matches_played + 
    high_load +
    tt(matches_played) +
    cluster(player_url),
  data = td_df_full,
  id = player_url,
  tt = function(x, t, ...) x * log(t + 1)
)

summary(cox_int_1)
# 
# Interaction 2 — National_team × had_serious_last_2y


cox_int_2 <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~ 
    position_simple +
    strata(region) +
    National_team * had_serious_last_2y +  # interaction
    n_injuries + 
    cum_injuries + 
    matches_played + 
    high_load +
    tt(matches_played) +
    cluster(player_url),
  data = td_df_full,
  id = player_url,
  tt = function(x, t, ...) x * log(t + 1)
)

summary(cox_int_2)


# Interaction 3 — Position × high_load

cox_int_3 <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~ 
    position_simple * high_load +  # interaction
    strata(region) +
    National_team +
    n_injuries + 
    cum_injuries + 
    had_serious_last_2y +
    matches_played + 
    tt(matches_played) +
    cluster(player_url),
  data = td_df_full,
  id = player_url,
  tt = function(x, t, ...) x * log(t + 1)
)

summary(cox_int_3)



# Interaction 4 — National_team × matches_played

cox_int_4 <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~ 
    position_simple +
    strata(region) +
    National_team * matches_played +  # interaction
    n_injuries + 
    cum_injuries + 
    had_serious_last_2y +
    high_load +
    tt(matches_played) +
    cluster(player_url),
  data = td_df_full,
  id = player_url,
  tt = function(x, t, ...) x * log(t + 1)
)

summary(cox_int_4) #Significant as in La liga


#Interaction 5 sharp increase X position

cox_int_5 <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~ 
    position_simple + 
    strata(region) + 
    National_team + 
    had_serious_last_2y + 
    matches_played + 
    high_load + 
    sharp_increase + 
    tt(matches_played) + 
    National_team:matches_played + 
    position_simple:sharp_increase + # New interaction
    cluster(player_url),
  data = td_df_full,
  id = player_url,
  tt = function(x, t, ...) x * log(t + 1)
)

summary(cox_int_5)

#Interaction 6 Injury group x position
injuries_by_group_td <- eda_injuries %>%
  mutate(season_year = as.numeric(season_year)) %>%
  group_by(player_url, season_year) %>%
  summarise(
    had_joint_injury = any(injury_group == "Joint/Ligament"),
    had_muscular_injury = any(injury_group == "Muscular"),
    had_acl = any(grepl("cruciate ligament", tolower(injury_type))),
    .groups = "drop"
  )

#join to td df
td_df_full <- td_df_full %>%
  left_join(injuries_by_group_td, by = c("player_url", "season_year")) %>%
  mutate(across(c(had_joint_injury, had_muscular_injury, had_acl), ~replace_na(., FALSE)))


cox_int_6 <- coxph(
  Surv(age_start, age_stop, event_dynamic) ~ 
    position_simple + strata(region) + National_team + 
    had_muscular_injury + had_joint_injury +
    matches_played + tt(matches_played) + 
    National_team:matches_played + 
    position_simple : had_joint_injury +
    cluster(player_url), 
  data = td_df_full, 
  tt = function(x, t, ...) x * log(t + 1)
)

summary(cox_int_6)




# Compare all interaction models
interaction_comparison <- data.frame(
  Model = c("Base (no interactions)",
            "Interaction 1",
            "Interaction 2", 
            "Interaction 3",
            "Interaction 4",
            "Interaction 5",
            "Interaction 6"),
  AIC = c(
    AIC(cox_td_national),
    AIC(cox_int_1),
    AIC(cox_int_2),
    AIC(cox_int_3),
    AIC(cox_int_4),
    AIC(cox_int_5),
    AIC(cox_int_6)
  ),
  Concordance = c(
    0.780,  
    NA, NA, NA, NA, NA , NA   
  )
)

interaction_comparison <- interaction_comparison %>%
  mutate(delta_AIC = AIC - min(AIC)) %>%
  arrange(AIC)

print(interaction_comparison) #Again interaction 4 is the best option

# Decision rule:
# delta_AIC < 2 → strong support for that model
# delta_AIC 2-7 → some support
# delta_AIC > 10 → no support, drop interaction

# NOTE: only National X matches improves the model
# Early career: playing matches is VERY protective
# Late career:  playing matches is LESS protective
# Which is what we expect biologically


# ==============================================================================
# SECTION 10: Final Time-Dependent Cox Model (Methodology Corrected)
# ==============================================================================
# CONSIDERING INTERACTION AND STRATA
cox_td_final <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~ 
    
    # STATIC BASELINE CHARACTERISTICS 
    position_simple +          # Captures the baseline risk differences between roles
    strata(region) +           # Fixes Proportional Hazards (PH) violation for geographic origin 
    National_team +            # Marker for elite quality and selection status
    #foot_clean +              #Non significant
    
    # SPLINES
    pspline(height_clean) +  # To capture non linear effects 
    pspline(debut_age_num)+ # To capture non linear effects 
    
    # DYNAMIC INJURY BURDEN 
    # Based on AIC support and previous discussion, we use the 2-year serious window
    had_serious_last_2y +      # Injuries of more than 90 days in the previous 2 seasons 
    log_days_missed +          # Logged total days missed to handle outliers
    log_n_injuries +           # Logged injury frequency per season ### VERY highly correlated with days missed, so we keep only the severity effect
    cum_injuries +             #Non significant and highly correlated with had serious injury 2
    
    # DYNAMIC WORKLOAD 
    matches_played +   
   cum_matches +       #non significant         
    high_load +  
    
    # TIME-VARYING COEFFICIENTS (tt)
    # These terms fix PH violations by allowing the hazard to change with age
    # Log-time transformation: Effect = base_coef + tt_coef × log(age + 1)
    tt(matches_played) +       # The protective effect of playing diminishes as players age 
    tt(high_load) +            # Analyzes if high match volume (>30) changes its impact over time 
    tt(cum_matches) + 
    
    # INTERACTION: THE "ELITE INSURANCE" EFFECT
    # Tests if match participation is more protective for nationally selected players
    National_team:matches_played + 
    
    # STANDARD ERROR CORRECTION
    cluster(player_url),       # Adjusts SEs for multiple observations per player [10, 13]
  
  data    = td_df_full,
  id      = player_url,
  tt      = function(x, t, ...) x * log(t + 1)  # Standard log-time transformation [3, 10]
)

# Output the results
summary(cox_td_final)


AIC(cox_td_final)# Best result so far as well







# Cox model WITHOUT tt() terms
cox_ph_check <- coxph(
  Surv(age_start, age_stop, event_dynamic) ~
    position_simple +
    strata(region) +
    National_team +
    foot_clean +
    pspline(height_clean) +
    pspline(debut_age_num) +
    had_serious_last_2y +
    log_days_missed +
    log_n_injuries +
    cum_injuries +
    matches_played +
    high_load +
    cum_matches +
    National_team:matches_played,
  
  data = td_df_full,
  id = player_url,
  cluster = player_url
)

# PH assumption test
ph_test <- cox.zph(cox_ph_check)

# Results
print(ph_test)

# Global test
ph_test$table







# ==============================================================================
# 11. MULTI-STATE MODEL FOR TRANSITIONS BETWEEN STATES

# ==============================================================================



# STEP 1: MAKE SURE ever_had_acl IS JOINED TO td_df_full FIRST


# Create ACL career flag if not already in td_df_full
if(!"ever_had_acl" %in% names(td_df_full)) {
  acl_career <- eda_injuries %>%
    mutate(is_acl = grepl("ACL|cruciate|ligament", tolower(injury_type))) %>%
    group_by(player_url) %>%
    summarise(
      ever_had_acl = any(is_acl, na.rm = TRUE),
      .groups = "drop"
    )
  
  td_df_full <- td_df_full %>%
    left_join(acl_career, by = "player_url") %>%
    mutate(ever_had_acl = replace_na(ever_had_acl, FALSE))
}


# STEP 2: CREATE STATE VARIABLES


ms_data <- td_df_full %>%
  arrange(player_url, season_year) %>%
  group_by(player_url) %>%
  mutate(
    # Current state (0=Active, 1=Seriously Injured, 2=Retired)
    state = case_when(
      event_dynamic == 1 & is_last_row ~ 2,  # Retired
      serious_injuries > 0 ~ 1,              # Seriously injured this season
      TRUE ~ 0                                # Active
    ),
    
    # Previous state
    prev_state = lag(state, default = 0),
    
    # Time increment
    time_years = age_stop - age_start,
    
    # Valid transition flag (skip first obs per player)
    valid = !is.na(lag(state))
  ) %>%
  ungroup()

# Check if we have enough events
cat("State distribution:\n")
table(ms_data$state)

cat("\nValid transitions per type:\n")
ms_data %>%
  filter(valid) %>%
  count(prev_state, state) %>%
  print()


# STEP 3: EXPAND TO ONE ROW PER POSSIBLE TRANSITION


ms_expanded <- bind_rows(
  # From Active: transitions 1 (→Injured) and 2 (→Retired)
  ms_data %>%
    filter(prev_state == 0, valid) %>%
    mutate(trans = 1L, status = as.integer(state == 1)),
  
  ms_data %>%
    filter(prev_state == 0, valid) %>%
    mutate(trans = 2L, status = as.integer(state == 2)),
  
  # From Injured: transitions 3 (→Active) and 4 (→Retired)
  ms_data %>%
    filter(prev_state == 1, valid) %>%
    mutate(trans = 3L, status = as.integer(state == 0)),
  
  ms_data %>%
    filter(prev_state == 1, valid) %>%
    mutate(trans = 4L, status = as.integer(state == 2))
) %>%
  arrange(player_url, season_year, trans)

# Check event counts per transition
cat("\nEvents per transition type:\n")
ms_expanded %>%
  group_by(trans) %>%
  summarise(
    n_rows = n(),
    n_events = sum(status),
    event_rate = mean(status)
  ) %>%
  mutate(
    transition = case_when(
      trans == 1 ~ "Active → Injured",
      trans == 2 ~ "Active → Retired",
      trans == 3 ~ "Injured → Active",
      trans == 4 ~ "Injured → Retired"
    )
  ) %>%
  print()


# STEP 4: MULTI-STATE COX MODELS (ONLY IF ENOUGH EVENTS)


# Model 1: Active → Seriously Injured
if(nrow(ms_expanded %>% filter(trans == 1, status == 1)) > 5) {
  cox_injury <- coxph(
    Surv(age_start, age_stop, status) ~
      position_simple + 
      National_team + 
      debut_age_num + 
      matches_played +
      high_load +
      cluster(player_url),
    data = ms_expanded %>% filter(trans == 1),
    method = "breslow"
  )
  cat("\n========== ACTIVE → SERIOUSLY INJURED ==========\n")
  print(summary(cox_injury)$coefficients)
} else {
  cat("\n========== ACTIVE → SERIOUSLY INJURED ==========\n")
  cat("Insufficient events for this transition\n")
}

# Model 2: Active → Retired
if(nrow(ms_expanded %>% filter(trans == 2, status == 1)) > 5) {
  cox_retire_direct <- coxph(
    Surv(age_start, age_stop, status) ~
      position_simple + 
      National_team + 
      debut_age_num + 
      matches_played +
      cluster(player_url),
    data = ms_expanded %>% filter(trans == 2),
    method = "breslow"
  )
  cat("\n========== ACTIVE → RETIRED ==========\n")
  print(summary(cox_retire_direct)$coefficients)
} else {
  cat("\n========== ACTIVE → RETIRED ==========\n")
  cat("Insufficient events for this transition\n")
}

# Model 3: Injured → Recovered (Active)
if(nrow(ms_expanded %>% filter(trans == 3, status == 1)) > 5) {
  cox_recovery <- coxph(
    Surv(age_start, age_stop, status) ~
      position_simple + 
      National_team + 
      debut_age_num + 
      cluster(player_url),
    data = ms_expanded %>% filter(trans == 3),
    method = "breslow"
  )
  cat("\n========== INJURED → RECOVERED ==========\n")
  print(summary(cox_recovery)$coefficients)
} else {
  cat("\n========== INJURED → RECOVERED ==========\n")
  cat("Insufficient events for this transition\n")
}

# Model 4: Injured → Retired
if(nrow(ms_expanded %>% filter(trans == 4, status == 1)) > 5) {
  cox_retire_injured <- coxph(
    Surv(age_start, age_stop, status) ~
      position_simple + 
      National_team + 
      debut_age_num + 
      ever_had_acl +
      cluster(player_url),
    data = ms_expanded %>% filter(trans == 4),
    method = "breslow"
  )
  cat("\n========== INJURED → RETIRED ==========\n")
  print(summary(cox_retire_injured)$coefficients)
} else {
  cat("\n========== INJURED → RETIRED ==========\n")
  cat("Insufficient events for this transition\n")
}


# STEP 5: TRANSITION COUNTS


transitions <- ms_data %>%
  filter(valid) %>%
  count(prev_state, state) %>%
  mutate(
    transition = case_when(
      prev_state == 0 & state == 0 ~ "Active → Active",
      prev_state == 0 & state == 1 ~ "Active → Injured",
      prev_state == 0 & state == 2 ~ "Active → Retired",
      prev_state == 1 & state == 0 ~ "Injured → Recovered",
      prev_state == 1 & state == 1 ~ "Injured → Still Injured",
      prev_state == 1 & state == 2 ~ "Injured → Retired",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(desc(n))

cat("\n========== TRANSITION COUNTS ==========\n")
print(transitions)





install.packages("randomForest")
# ==============================================================================
# 12: Baseline ML Models & Robustness Checks (EPL)
#        Penalized Cox + Random Forest on Static Baseline Data
# ==============================================================================

cat("\n--- SECTION 11: Baseline ML Models & Robustness (EPL) ---\n")

suppressPackageStartupMessages({
  library(glmnet)
  library(randomForest)
  library(survival)
  library(dplyr)
})

set.seed(123)

# ----------------------------
# 11.1 Single train/test split (player-level)
# ----------------------------

cat("\n11.1 Player-level train/test split (static baseline)\n")

player_ids <- surv_df_premier$player_url
unique_ids <- unique(player_ids)

train_ids <- sample(unique_ids, size = floor(0.7 * length(unique_ids)))
train_idx <- player_ids %in% train_ids
test_idx  <- !train_idx

cat("  Training players:", length(unique(player_ids[train_idx])), "\n")
cat("  Test players:",     length(unique(player_ids[test_idx])),  "\n")

# Survival outcome and design matrix (static baseline covariates)
y_surv <- with(surv_df_premier, Surv(time = career_length, event = event))

x_vars <- surv_df_premier %>%
  dplyr::select(
    position_simple,
    debut_age_num,
    height_clean,
    region,
    National_team,
    foot_clean,
    injuries_per_season
  )

x_mat <- model.matrix(~ ., data = x_vars)[, -1, drop = FALSE]

x_train <- x_mat[train_idx, , drop = FALSE]
x_test  <- x_mat[test_idx,  , drop = FALSE]
y_train <- y_surv[train_idx]
y_test  <- y_surv[test_idx]

# ----------------------------
# 11.2 Penalized Cox regression (glmnet)
# ----------------------------

cat("\n11.2 Penalized Cox (elastic net) on static EPL data\n")

cv_cox_en_epl <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  family = "cox",
  alpha  = 0.5,
  nfolds = 5
)

# Optional plot of cross-validated deviance
plot(cv_cox_en_epl)

lp_test_glmnet <- as.numeric(
  predict(cv_cox_en_epl, newx = x_test, s = "lambda.min", type = "link")
)

# Harrell-style concordance on test set
conc_obj_epl <- survConcordance(y_test ~ lp_test_glmnet)

cat("\n--- EPL Penalized Cox Results (Section 11) ---\n")
cat("Best lambda (lambda.min):", cv_cox_en_epl$lambda.min, "\n")
cat("Test-set concordance:",     conc_obj_epl$concordance, "\n")

cox_coef_epl <- coef(cv_cox_en_epl, s = "lambda.min")
cat("\nNon-zero coefficients at lambda.min:\n")
print(cox_coef_epl)

# ----------------------------
# 12.3 Random forest regression for career length
# ----------------------------

cat("\n11.3 Random forest regression for EPL career length\n")

reg_data_epl <- surv_df_premier %>%
  dplyr::select(
    career_length,
    position_simple,
    debut_age_num,
    height_clean,
    region,
    National_team,
    foot_clean,
    injuries_per_season
  )

reg_train_epl <- reg_data_epl[train_idx, ]
reg_test_epl  <- reg_data_epl[test_idx, ]

rf_reg_epl <- randomForest(
  career_length ~ .,
  data       = reg_train_epl,
  ntree      = 500,
  mtry       = floor(sqrt(ncol(reg_train_epl) - 1)),
  importance = TRUE
)

pred_len_epl <- predict(rf_reg_epl, newdata = reg_test_epl)
rf_rmse_epl  <- sqrt(mean((pred_len_epl - reg_test_epl$career_length)^2))

cat("\n--- EPL Random Forest Regression Results (Section 12) ---\n")
cat("Test RMSE (years):",              rf_rmse_epl, "\n")
cat("Percent variance explained (%):", rf_reg_epl$rsq[rf_reg_epl$ntree] * 100, "\n")

# Variable importance plot
varImpPlot(rf_reg_epl, main = "EPL RF Variable Importance (Static Baseline)")

# ----------------------------
# 12.4 Robustness checks: repeated random splits
# ----------------------------

cat("\n11.4 Robustness checks (5 repeated player-level splits)\n")

n_reps <- 5
conc_vals <- rep(NA_real_, n_reps)
rmse_vals <- rep(NA_real_, n_reps)
var_exp   <- rep(NA_real_, n_reps)

set.seed(456)

for (r in seq_len(n_reps)) {
  cat("\nReplication", r, "of", n_reps, "\n")
  
  player_ids <- surv_df_premier$player_url
  unique_ids <- unique(player_ids)
  
  train_ids_r <- sample(unique_ids, size = floor(0.7 * length(unique_ids)))
  train_idx_r <- player_ids %in% train_ids_r
  test_idx_r  <- !train_idx_r
  
  if (!any(train_idx_r) || !any(test_idx_r)) {
    cat("  Warning: empty train or test set; skipping.\n")
    next
  }
  
  # Survival ML
  x_train_r <- x_mat[train_idx_r, , drop = FALSE]
  x_test_r  <- x_mat[test_idx_r,  , drop = FALSE]
  y_train_r <- y_surv[train_idx_r]
  y_test_r  <- y_surv[test_idx_r]
  
  conc_vals[r] <- tryCatch({
    cv_fit_r <- cv.glmnet(
      x      = x_train_r,
      y      = y_train_r,
      family = "cox",
      alpha  = 0.5,
      nfolds = 5
    )
    lp_test_r <- as.numeric(
      predict(cv_fit_r, newx = x_test_r, s = "lambda.min", type = "link")
    )
    cobj_r <- survConcordance(y_test_r ~ lp_test_r)
    cat("  Penalized Cox concordance:", cobj_r$concordance, "\n")
    cobj_r$concordance
  }, error = function(e) {
    cat("  Penalized Cox failed in this split:", conditionMessage(e), "\n")
    NA_real_
  })
  
  # Regression ML
  tryCatch({
    reg_train_r <- reg_data_epl[train_idx_r, ]
    reg_test_r  <- reg_data_epl[test_idx_r, ]
    
    rf_fit_r <- randomForest(
      career_length ~ .,
      data       = reg_train_r,
      ntree      = 500,
      mtry       = floor(sqrt(ncol(reg_train_r) - 1)),
      importance = FALSE
    )
    
    pred_r <- predict(rf_fit_r, newdata = reg_test_r)
    rmse_vals[r] <- sqrt(mean((pred_r - reg_test_r$career_length)^2))
    var_exp[r]   <- rf_fit_r$rsq[rf_fit_r$ntree] * 100
    
    cat("  RF RMSE:", rmse_vals[r],
        " | % Var explained:", var_exp[r], "\n")
  }, error = function(e) {
    cat("  RF regression failed in this split:", conditionMessage(e), "\n")
  })
}

# Summary
cat("\n--- Section 11 EPL robustness summary ---\n")

valid_conc <- conc_vals[!is.na(conc_vals)]
valid_rmse <- rmse_vals[!is.na(rmse_vals)]
valid_var  <- var_exp[!is.na(var_exp)]

cat("\nPenalized Cox concordance (valid splits):\n")
print(valid_conc)
if (length(valid_conc) > 0) {
  cat("Mean concordance:", mean(valid_conc), "\n")
  cat("SD concordance:",   sd(valid_conc),   "\n")
} else {
  cat("No valid concordance values (all splits failed).\n")
}

cat("\nRandom forest RMSE (valid splits):\n")
print(valid_rmse)
if (length(valid_rmse) > 0) {
  cat("Mean RMSE:", mean(valid_rmse), "\n")
  cat("SD RMSE:",   sd(valid_rmse),   "\n")
} else {
  cat("No valid RMSE values (all splits failed).\n")
}

cat("\nRandom forest % variance explained (valid splits):\n")
print(valid_var)
if (length(valid_var) > 0) {
  cat("Mean %Var explained:", mean(valid_var), "\n")
  cat("SD %Var explained:",   sd(valid_var),   "\n")
} else {
  cat("No valid %Var values (all splits failed).\n")
}

cat("\nNote: NA values indicate splits where the model failed or the split was empty.\n")


