################################################################################
# ADVANCED SURVIVAL ANALYSIS: LA LIGA CAREER LENGTH
# Description: Evaluates career span using both static baseline models and 
#              dynamic time-dependent covariates (counting process format).
# Datasets:    la_liga_stats...csv, liga_injuries...csv, liga_history...csv
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

# Set seed for reproducibility
set.seed(123)

# Define working directory (Update this path to where your CSVs are located)
setwd("C:/git/Thesis/Data")

# ==============================================================================
# SECTION 2: Data Ingestion
# ==============================================================================
cat("\n--- Loading Datasets ---\n")
df_static_liga    <- read_csv("la_liga_stats_2010_2015_depured2.csv", show_col_types = FALSE)
df_injuries_liga  <- read_csv("liga_injuries_detailed.csv", show_col_types = FALSE)
df_history_liga   <- read_csv("liga_history_detailed.csv", show_col_types = FALSE)


# ==============================================================================
# SECTION 3: Static Data Engineering (Baseline Profile)
# ==============================================================================
cat("\n--- Processing Static Data ---\n")
# Creates the dataset for standard Kaplan-Meier and Baseline Models.
# IMPORTANT: Keeps continuous variables intact and converts aggregates to rates.
# Calculate seasons_club from df_history_liga (excluding national team)
seasons_club_lookup <- df_history_liga %>%
  group_by(player_url) %>%
  summarise(seasons_club = n_distinct(season_simple, na.rm = TRUE), .groups = "drop")

surv_df_liga <- df_static_liga %>%
  select(
    player_name = player_name,
    player_url = player_url_fbref, # Unique ID
    nationality = citizenship_tra, 
    career_length,
    retired = retired_tra, 
    retirement_age, #MICH
    age_current = `current age_fbref`,
    debut_age = debut_age_transfermarket,
    debut_year = debut_year_fbref,
    National_team, 
    injury_count_total = injury_count_total_tra,
    seasons_national = total_seasons_national,
    foot = foot_tra,
    position_simple,
    height = height_tra
  ) %>%
  left_join(seasons_club_lookup, by = c("player_url" = "player_url")) %>%
  mutate(
    # Clean continuous numeric data (No arbitrary binning)
    seasons_club = coalesce(seasons_club, 0),
    injury_count_total = coalesce(injury_count_total, 0),
    debut_age_num  = as.numeric(gsub("[^0-9.]", "", debut_age)),
    height_clean = as.numeric(gsub("[^0-9.]", "", height)),
    debut_year_num = as.numeric(debut_year),
    
    # [ANTI-LEAKAGE] Calculate Intensity Rates for static models
    injuries_per_season = ifelse(!is.na(seasons_club) & seasons_club > 0, 
                                 injury_count_total / seasons_club, 0),
    
    # Clean categorical covariates
    foot_clean = as.factor(case_when(
      tolower(foot) %in% c("left", "right", "both") ~ tolower(foot),
      TRUE ~ "unknown"
    )),
    
    # Binarize Event
    event = case_when(
      retired == "Yes" ~ 1L,
      retired == "No"  ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Group Nationalities for Frailty/Strata Models
    nationality_clean = str_squish(str_replace_all(nationality, "\u00A0", " ")), 
    region = as.factor(case_when(
      str_detect(nationality_clean, "England|Scotland|Wales|Northern Ireland|Ireland") ~ "UK_Ireland",
      str_detect(nationality_clean, "France|Spain|Germany|Italy|Netherlands|Portugal|Belgium") ~ "EU_Major",
      str_detect(nationality_clean, "Brazil|Argentina|Uruguay") ~ "South_America",
      TRUE ~ "Rest_of_World"
    ))
  ) %>%
  # Filter only complete records for accurate model comparisons
  filter(complete.cases(career_length, event, debut_age_num, debut_year_num, 
                        position_simple, height_clean, region, injuries_per_season))


# ==============================================================================
# SECTION 4: Exploratory Data Analysis (EDA)
# ==============================================================================
cat("\n--- Generating Exploratory Plots ---\n")

# 4.1 Static Variables Dashboard
p_pos <- ggplot(surv_df_liga, aes(x = position_simple)) +
  geom_bar(fill = "firebrick", alpha = 0.8) +
  theme_minimal() + labs(title = "Positions", x = "", y = "Count")

p_age <- ggplot(surv_df_liga, aes(x = debut_age_num, y = career_length)) +
  geom_point(alpha = 0.3) + geom_smooth(method = "loess", se = FALSE, colour = "red") +
  theme_minimal() + labs(title = "Debut Age vs Career Length", x = "Debut Age", y = "Years")

print(p_pos | p_age)

# 4.2 Injury Dashboard
eda_injuries <- df_injuries_liga %>%
  mutate(
    severity_cat = case_when(
      days <= 7 ~ "Minor (≤1w)", days <= 28 ~ "Moderate (1-4w)",
      days <= 90 ~ "Serious (1-3m)", TRUE ~ "Severe (>3m)"
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


p_inj1 <- ggplot(eda_injuries %>% count(injury_group), aes(x = reorder(injury_group, n), y = n)) +
  geom_col(fill = "steelblue") + coord_flip() + theme_minimal() + labs(title = "Type", x = "", y = "Count")
p_inj2 <- ggplot(eda_injuries %>% count(severity_cat), aes(x = severity_cat, y = n, fill = severity_cat)) +
  geom_col() + theme_minimal() + theme(legend.position = "none") + labs(title = "Severity", x = "", y = "")

print(p_inj1 | p_inj2)


# ==============================================================================
# SECTION 4: Exploratory Data Analysis (EDA) - Univariate & Bivariate
# ==============================================================================
cat("\n--- Generating Univariate & Bivariate Exploratory Plots ---\n")

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

# ------------------------------------------------------------------------------
# 4.1 Univariate Analysis (Distributions of Individual Variables)
# ------------------------------------------------------------------------------
# A. Histograms - Regular scale
regular_vars <- list(
  career_length = "Career Length",
  debut_age_num = "Debut Age",
  height_clean = "Height",
  injury_count_total = "Total Injuries"
)

for (v in names(regular_vars)) {
  if (!(v %in% names(surv_df_liga))) next
  pretty_name <- regular_vars[[v]]
  x           <- surv_df_liga[[v]]
  x_min       <- min(x, na.rm = TRUE)
  x_max       <- max(x, na.rm = TRUE)
  breaks_seq  <- seq(x_min, x_max, length.out = 31)
  
  # Height label
  x_label <- ifelse(v == "height_clean", paste(pretty_name, "(m)"), pretty_name)
  
  print(
    ggplot(surv_df_liga, aes(x = .data[[v]])) +
      geom_histogram(breaks = breaks_seq, colour = "white", fill = "steelblue", alpha = 0.7) +
      shared_theme +
      labs(title = paste("Distribution of", pretty_name), subtitle = "La Liga",
           x = x_label, y = "Count")
  )
}


# B. Log scale histograms for skewed vars (only injuries_per_season and days_missed)
log_vars <- list(
  injuries_per_season = "Injuries Per Season"
#  days_missed = "Days Missed"
)

for (v in names(log_vars)) {
  if (!(v %in% names(surv_df_liga))) {
    message("Skipping ", v, " — not found in surv_df_liga")
    next
  }
  pretty_name <- log_vars[[v]]
  
  print(
    ggplot(surv_df_liga, aes(x = .data[[v]])) +
      geom_histogram(colour = "white", fill = "steelblue", alpha = 0.7, bins = 30) +
      scale_x_continuous(trans = "log1p", labels = scales::number_format(accuracy = 0.1)) +
      shared_theme +
      labs(title = paste("Distribution of", pretty_name), subtitle = "La Liga (log scale)",
           x = paste(pretty_name, "(log scale)"), y = "Count")
  )
}

# C. Bar charts - categorical
print(
  ggplot(surv_df_liga, aes(x = position_simple)) +
    geom_bar(fill = "firebrick", alpha = 0.8) +
    shared_theme +
    labs(title = "Count by Position", subtitle = "La Liga", x = "Position", y = "Count")
)

print(
  ggplot(surv_df_liga, aes(x = foot_clean)) +
    geom_bar(fill = "forestgreen", alpha = 0.8) +
    shared_theme +
    labs(title = "Count by Preferred Foot", subtitle = "La Liga", x = "Preferred Foot", y = "Count")
)

print(
  ggplot(surv_df_liga, aes(x = region)) +
    geom_bar(fill = "purple", alpha = 0.8) +
    shared_theme +
    labs(title = "Count by Region", subtitle = "La Liga", x = "Region", y = "Count")
)


# ------------------------------------------------------------------------------
# 4.2 Bivariate Analysis (Predictors vs. Target Variable: Career Length)
# ------------------------------------------------------------------------------

# Range for career length (used in all boxplots and scatterplots)
y_min_cl <- min(surv_df_liga$career_length, na.rm = TRUE)
y_max_cl <- max(surv_df_liga$career_length, na.rm = TRUE)

# A. Boxplots - categorical vs career length
print(
  ggplot(surv_df_liga, aes(x = position_simple, y = career_length, fill = position_simple)) +
    geom_boxplot(alpha = 0.7, linewidth = 0.7, outlier.alpha = 0.4) +
    coord_cartesian(ylim = c(y_min_cl, y_max_cl)) +
    shared_theme +
    theme(legend.position = "none") +
    labs(title = "Career Length by Position", subtitle = "La Liga",
         x = "Position", y = "Career Length (Years)")
)

print(
  ggplot(surv_df_liga, aes(x = foot_clean, y = career_length, fill = foot_clean)) +
    geom_boxplot(alpha = 0.7, linewidth = 0.7, outlier.alpha = 0.4) +
    coord_cartesian(ylim = c(y_min_cl, y_max_cl)) +
    shared_theme +
    theme(legend.position = "none") +
    labs(title = "Career Length by Preferred Foot", subtitle = "La Liga",
         x = "Preferred Foot", y = "Career Length (Years)")
)

print(
  ggplot(surv_df_liga, aes(x = National_team, y = career_length, fill = National_team)) +
    geom_boxplot(alpha = 0.7, linewidth = 0.7, outlier.alpha = 0.4) +
    coord_cartesian(ylim = c(y_min_cl, y_max_cl)) +
    shared_theme +
    theme(legend.position = "none") +
    labs(title = "Career Length by National Team", subtitle = "La Liga",
         x = "National Team", y = "Career Length (Years)")
)

# B. Scatterplots - continuous vs career length
# Calculate ranges for each predictor
x_min_age <- min(surv_df_liga$debut_age_num, na.rm = TRUE)
x_max_age <- max(surv_df_liga$debut_age_num, na.rm = TRUE)

x_min_inj <- min(surv_df_liga$injuries_per_season, na.rm = TRUE)
x_max_inj <- max(surv_df_liga$injuries_per_season, na.rm = TRUE)

# Height range (in meters)
x_min_h <- min(surv_df_liga$height_clean, na.rm = TRUE)
x_max_h <- max(surv_df_liga$height_clean, na.rm = TRUE)

print(
  ggplot(surv_df_liga, aes(x = debut_age_num, y = career_length)) +
    geom_point(alpha = 0.3, size = 2, colour = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, colour = "firebrick", linewidth = 1.2) +
    coord_cartesian(xlim = c(x_min_age, x_max_age), ylim = c(y_min_cl, y_max_cl)) +
    shared_theme +
    labs(title = "Career Length vs Debut Age", subtitle = "La Liga",
         x = "Debut Age (Years)", y = "Career Length (Years)")
)

print(
  ggplot(surv_df_liga, aes(x = height_clean, y = career_length)) +
    geom_point(alpha = 0.3, size = 2, colour = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, colour = "blue", linewidth = 1.2) +
    coord_cartesian(xlim = c(x_min_h, x_max_h), ylim = c(y_min_cl, y_max_cl)) +
    shared_theme +
    labs(title = "Career Length vs Height", subtitle = "La Liga",
         x = "Height (m)", y = "Career Length (Years)")
)

print(
  ggplot(surv_df_liga, aes(x = injuries_per_season, y = career_length)) +
    geom_point(alpha = 0.3, size = 2, colour = "steelblue") +
    geom_smooth(method = "loess", se = TRUE, colour = "darkgreen", linewidth = 1.2) +
    coord_cartesian(xlim = c(x_min_inj, x_max_inj), ylim = c(y_min_cl, y_max_cl)) +
    shared_theme +
    labs(title = "Career Length vs Injury Rate", subtitle = "La Liga",
         x = "Injuries per Season", y = "Career Length (Years)")
)



# ------------------------------------------------------------------------------
# 4.3 Injury-Specific EDA
# ------------------------------------------------------------------------------
# Create a temporary dataframe to visualize the raw injury types and severity
eda_injuries <- df_injuries_liga %>%
  mutate(
    severity_cat = case_when(
      days <= 7 ~ "Minor (≤1w)", days <= 28 ~ "Moderate (1-4w)",
      days <= 90 ~ "Serious (1-3m)", TRUE ~ "Severe (>3m)"
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

# Plot Distribution of Injury Types
p_inj_type <- ggplot(eda_injuries %>% count(injury_group), aes(x = reorder(injury_group, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total Count of Injury Types", x = "Injury Category", y = "Frequency")
print(p_inj_type)

# Plot Distribution of Injury Severity
p_inj_sev <- ggplot(eda_injuries %>% count(severity_cat), aes(x = severity_cat, y = n, fill = severity_cat)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Count of Injuries by Severity", x = "Severity", y = "Frequency")
print(p_inj_sev)


# ==============================================================================
# SECTION 5: Kaplan-Meier Survival Estimates
# ==============================================================================

cat("\n--- Kaplan-Meier Survival Estimates ---\n")

surv_static <- Surv(time = surv_df_liga$career_length, event = surv_df_liga$event)
summary(surv_static)

km_df <- surv_df_liga %>%
  mutate(
    debut_band = cut(debut_age_num, breaks = c(15, 18, 21, 24, 30), right = FALSE),
    debut_decade = factor(paste0(floor(debut_year_num / 10) * 10, "s"))
  )
summary(km_df)



fit_pos <- survfit(surv_static ~ position_simple, data = surv_df_liga)

fit_decade <- survfit(surv_static ~ debut_decade, data = km_df)

fit_age <- survfit(surv_static ~ debut_band, data = km_df)

fit_nat <- survfit(surv_static ~ National_team, data = km_df)





summary(fit_pos)
summary(fit_nat)



print(ggsurvplot(survfit(surv_static ~ 1, data = surv_df_liga), data = surv_df_liga,
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
# SECTION 6: Static Baseline Models (Cox & AFT)
# ==============================================================================
cat("\n--- Running Static Models ---\n")

# 6.1 Cox Proportional Hazards (Using Splines for continuous height and age)
cox_static <- coxph(Surv(career_length, event) ~ position_simple + pspline(debut_age_num) + 
                      pspline(height_clean) + region + National_team + foot_clean + injuries_per_season, 
                    data = surv_df_liga)
print(cox_static)

# 6.2 Accelerated Failure Time (Weibull) - Robustness Check for PH violations
aft_weibull <- flexsurvreg(Surv(career_length, event) ~ position_simple + debut_age_num +  National_team +
                         height_clean + region + injuries_per_season, 
                       data = surv_df_liga, dist = "weibull")
print(aft_weibull)


# 2. Test non-monotonic alternatives
aft_lognormal <- flexsurvreg(
  Surv(career_length, event) ~ position_simple + debut_age_num +National_team +
    height_clean + region + foot_clean + injuries_per_season,
  data = surv_df_liga,
  dist = "lognormal"
)
cat("\n--- Lognormal AFT Model ---\n")
print(aft_lognormal)



aft_loglogistic <- flexsurvreg(
  Surv(career_length, event) ~ position_simple + debut_age_num +National_team +
    height_clean + region + foot_clean + injuries_per_season,
  data = surv_df_liga,
  dist = "llogis"
)
cat("\n--- Loglogistic AFT Model ---\n")
print(aft_loglogistic)




# Generalized Gamma (flexsurv package)
aft_gengamma <- flexsurvreg(
  Surv(career_length, event) ~ position_simple + debut_age_num + National_team +
    height_clean + region + foot_clean + injuries_per_season,
  data = surv_df_liga,
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
# SECTION 6.1: Static Cox Model & Stepwise AIC Selection
# ==============================================================================
cat("\n--- Running Static Cox Model & AIC Selection ---\n")

# Define the "Full" Model with all potential covariates
cox_static_full <- coxph(Surv(career_length, event) ~ position_simple + pspline(debut_age_num) + 
                           pspline(height_clean) + region + National_team + foot_clean + injuries_per_season, 
                         data = surv_df_liga)

# Perform Stepwise Covariate Selection based on AIC (as requested by committee)
cox_static_aic <- step(cox_static_full, direction = "both", trace = 0)

cat("\n--- Final Selected Static Cox Model (via AIC) ---\n")
print(summary(cox_static_aic))


ph_test <- cox.zph(cox_static_aic)
print(ph_test)

# Visual inspection
par(mfrow = c(2, 2))
plot(ph_test, main = c("Position", "Debut Age", "Region"))


# ==============================================================================
# SECTION 7: Dynamic Time-Dependent Engineering (Counting Process)
# ==============================================================================
cat("\n--- Engineering Time-Dependent Covariates ---\n")

# 7.1 Process Dynamic Injury Data by Season
injuries_td <- eda_injuries %>%
  mutate(season_year = as.numeric(season_simple)) %>%
  group_by(player_url, season_year) %>%
  summarise(
    n_injuries = n(),
    total_days_missed = sum(days, na.rm = TRUE),
    serious_injuries = sum(severity_cat %in% c("Serious (1-3m)", "Severe (>3m)")),
    .groups = "drop"
  )

# 7.2 Process Performance Workload by Season
performance_td <- df_history_liga %>%
  mutate(season_year = as.numeric(str_extract(season_simple, "^[0-9]{4}"))) %>%
  group_by(player_url, season_year) %>%
  summarise(matches_played = sum(matches_played, na.rm = TRUE), .groups = "drop")

# 7.3 Merge Base History, Apply Age-Based Time Scale (Fixes Left Truncation)
td_df_full <- df_history_liga %>%
  mutate(season_year = as.numeric(str_extract(season_simple, "^[0-9]{4}"))) %>%
  distinct(player_url, season_year) %>%
  
  # Join metrics
  left_join(injuries_td, by = c("player_url", "season_year")) %>%
  left_join(performance_td, by = c("player_url", "season_year")) %>%
  mutate(across(c(n_injuries, total_days_missed, serious_injuries, matches_played), ~replace_na(., 0))) %>%
  
  # Join static data to calculate Age
  left_join(surv_df_liga %>% select(player_url, debut_year_num, debut_age_num, event_static = event, 
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
# SECTION 8: Time-Dependent Cox Model (Methodology Corrected)
# ==============================================================================
cat("\n--- Running Final Time-Dependent Cox Model ---\n")

# This final model uses:
# 1. Age as the underlying time scale (Surv(age_start, age_stop)).
# 2. Continuous height with penalized splines to handle U-shaped physical relationships.
# 3. Stratification by region to correct spatial PH violations.
# 4. Time-Transform tt() functions to correct temporal PH violations for workload metrics.

td_df_full <- df_history_liga %>%
  mutate(season_year = as.numeric(str_extract(season_simple, "^[0-9]{4}"))) %>%
  
  distinct(player_url, season_year) %>%

  # Join metrics
  left_join(injuries_td, by = c("player_url", "season_year")) %>%
  left_join(performance_td, by = c("player_url", "season_year")) %>%
  mutate(across(c(n_injuries, total_days_missed, serious_injuries,
                  matches_played), ~replace_na(., 0))) %>%

  # JOIN STATIC DATA — add National_team here
  left_join(surv_df_liga %>% select(player_url,
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

summary(cox_int_4)


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
  mutate(season_year = as.numeric(season_simple)) %>%
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

print(interaction_comparison)

# Decision rule:
# delta_AIC < 2 → strong support for that model
# delta_AIC 2-7 → some support
# delta_AIC > 10 → no support, drop interaction

#MICH: NOTE: only National X matches improves the model
# Early career: playing matches is VERY protective
# Late career:  playing matches is LESS protective
# Which is what we expect biologically


# ==============================================================================
# SECTION 9: Final Time-Dependent Cox Model (Methodology Corrected)
# ==============================================================================
# CONSIDERING INTERACTION AND STRATA
cox_td_final <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~ 
    
    # STATIC BASELINE CHARACTERISTICS 
    position_simple +          # Captures the baseline risk differences between roles
    strata(region) +           # Fixes Proportional Hazards (PH) violation for geographic origin 
    National_team +            # Marker for elite quality and selection status
   # foot_clean +              #Non significant
    
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
    cum_matches +#non significant 
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
    cluster(player_url),       # Adjusts SEs for multiple observations per player 
  
  data    = td_df_full,
  id      = player_url,
  tt      = function(x, t, ...) x * log(t + 1)  # Standard log-time transformation
)

# Output the results
summary(cox_td_final)


AIC(cox_td_final)# Best result so far

#Why we have used splines




# 1) Linear effect model
cox_linear_age <- coxph(
  Surv(career_length, event) ~ 
    position_simple + debut_age_num + height_clean +
    region + National_team + foot_clean + injuries_per_season,
  data = surv_df_liga
)

# 2) Spline effect model 
cox_spline_age <- coxph(
  Surv(career_length, event) ~ 
    position_simple + pspline(debut_age_num) + height_clean +
    region + National_team + foot_clean + injuries_per_season,
  data = surv_df_liga
)

# 3) Compare fit (AIC and likelihood ratio test)
AIC(cox_linear_age, cox_spline_age)

anova(cox_linear_age, cox_spline_age, test = "LRT")

# 4) Optional: plot the spline effect of debut age
termplot(cox_spline_age, term = "pspline(debut_age_num)",
         se = TRUE, col.term = "blue", col.se = "lightblue")





cox_td_final <- coxph(
  Surv(time = age_start, time2 = age_stop, event = event_dynamic) ~ 
    
    # STATIC BASELINE
    position_simple +
    strata(region) +
    National_team +
    pspline(debut_age_num) +   # Add this — fixes Martingale issue
    
    # DYNAMIC INJURY
    had_serious_last_2y +      # Keep — AIC and theory support
    
    # DYNAMIC WORKLOAD
    matches_played +
    
    # TIME-VARYING CORRECTIONS
    tt(matches_played) +
    tt(high_load) +
    
    # INTERACTION
    National_team:matches_played +
    
    cluster(player_url),
  
  data = td_df_full,
  id   = player_url,
  tt   = function(x, t, ...) x * log(t + 1)
)


# Recheck Martingale residuals — does age plot flatten now?
cox_null <- coxph(
  Surv(time = age_start, time2 = age_stop,
       event = event_dynamic) ~
    strata(region) + cluster(player_url),
  data = td_df_full, id = player_url
)

mart_resid <- residuals(cox_null, type = "martingale")

par(mfrow = c(1, 2))
scatter.smooth(td_df_full$matches_played, mart_resid,
               xlab = "Matches Played",
               ylab = "Martingale Residual",
               main = "Linearity: Matches Played",
               col = "grey70",
               lpars = list(col = "red", lwd = 2))
abline(h = 0, lty = 2)

scatter.smooth(td_df_full$age_start, mart_resid,
               xlab = "Age",
               ylab = "Martingale Residual",
               main = "Linearity: Age",
               col = "grey70",
               lpars = list(col = "red", lwd = 2))
abline(h = 0, lty = 2)






# ==============================================================================
# SECTION 10:MULTI-STATE MODEL FOR TRANSITIONS BETWEEN STATES

# ==============================================================================

library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)

  
  # STEP 1: MAKE SURE ever_had_acl IS JOINED TO td_df_full FIRST
  
  
  # Create ACL career in td_df_full
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
  

  # STEP 4: MULTI-STATE COX MODELS 
 
  
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
  
  
  
  exp(confint(cox_injury))
  exp(confint(cox_retire_direct))
  exp(confint(cox_recovery))
  exp(confint(cox_retire_injured))
