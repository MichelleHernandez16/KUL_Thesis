########################################################
###########    EDA   ###################################
########################################################

####LOAD PACKAGES AND DATASET
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(visdat)
library(naniar)


setwd("C:/git/Thesis/Data")
#df <- read.csv("la_liga_stats_2010_2015_final.csv")
df <- read.csv("la_liga_stats_2010_2015_depured.csv")
str(df)
glimpse(df)



####ASPECTS TO ANALYSE
##  1) NUMERICAL VALUES
##  2) MISSING VALUES
##  3) OUTLIERS



########################################################
#1) NUMERICAL VALUES
#Convert to numeric

df <- df %>%
mutate(
current.age_fbref  = as.numeric(current.age_fbref),
debut_year_fbref = as.numeric(debut_year_fbref),
debut_age_fbref = as.numeric(debut_age_fbref),
YEAR_tra = as.numeric(YEAR_tra),
height_tra = as.numeric(height_tra),
first_tier_debut_age = as.numeric(first_tier_debut_age),
second_tier_debut_age = as.numeric(second_tier_debut_age),
debut_age_transfermarket = as.numeric(debut_age_transfermarket),
injury_count_total_tra = as.numeric(injury_count_total_tra),
days_missed_total_tra = as.numeric(days_missed_total_tra),
games_missed_total_tra = as.numeric(games_missed_total_tra),
injury_seasons_tra = as.numeric(injury_seasons_tra),
death_age = as.numeric(death_age),
year_retirement = as.numeric(year_retirement),
career_lenght = as.numeric(career_lenght),
career_lenght_w_actives_and_dead  = as.numeric(career_lenght_w_actives_and_dead ),
shirt_number_tra = as.numeric(shirt_number_tra),
total_seasons_national = as.numeric(total_seasons_national),
matches_national = as.numeric(matches_national),
minutes_national = as.numeric(minutes_national),
goals_national = as.numeric(goals_national),
height_fbref = as.numeric(gsub("[^0-9\\.]", "", height_fbref)),  # removes "m" or commas
CAREER.Sum.of.matches_played = as.numeric(CAREER.Sum.of.matches_played),
CAREER.Sum.of.minutes = as.numeric(CAREER.Sum.of.minutes),
CAREER.Sum.of.goals = as.numeric(CAREER.Sum.of.goals)
)

# Convert binary variables
bin_columns <- c("retired_tra", "Deceased_tra", "National_team")
df[bin_columns] <- lapply(df[bin_columns], function(x) ifelse(x == "Yes", 1, 0))


#########################################################
## 2) Missing values


### MISSING DATA CHECK
missing_summary <- df %>%
  summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Percentage") %>%
  mutate(Missing_Percentage = round(Missing_Percentage, 1)) %>%
  arrange(desc(Missing_Percentage))

print(missing_summary, n=15) ## I THInk most of the important variables dont have many missing values








### DETECT CENSORING

# Check the retirement variables
table(df$retired_tra, useNA = "ifany") #Add a NA row


# No Yes 
# 0   1 
# 579 730 


df <- df %>%
  mutate(retirement_date_tra = as.Date(retirement_date_tra)) ##data format


# Look at a few rows
df %>%
  select(player_name, retired_tra, retirement_date_tra) %>%
  head(10)

# df <- df %>%
#   mutate(event = ifelse(retired_tra == "Yes" | retired_tra == 1, 1, 0))

# Summary counts
df %>%
  count(retired_tra) %>%
  mutate(percent = n / sum(n) * 100)

# Do all retired players have retired date? are they active with a date?
df %>%
  summarise(
    missing_retired = sum(is.na(retired_tra)), #Players with no info about retirement
    missing_retirement_date = sum(is.na(retirement_date_tra)), #players with no retirement data (must equal active players (575))
    retired_but_no_date = sum(retired_tra == 1 & is.na(retirement_date_tra)), #players with retired=yes=1 but no retirement date (must be zero)
    active_with_date = sum(retired_tra == 0 & !is.na(retirement_date_tra)) #player still active but with retirement date(must be zero)
  )




ggplot(df, aes(x = factor(retired_tra, labels = c("Active (Censored)", "Retired")))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Censored Players",
       x = "Career Status", y = "Count") +
  theme_minimal()




censor_summary <- df %>%
  count(retired_tra) %>%
  mutate(
    status = ifelse(retired_tra == 1, "Retired", "Active (Censored)"),
    percent = round(100 * n / sum(n), 1),
    label = paste0(status, " — ", percent, "%")
  )

censor_summary



ggplot(censor_summary, aes(x = "", y = percent, fill = status)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(percent, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = " Active vs Retired Players",
       fill = " Status") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )


##Look for the distribution of career lenght (Iam using the lenght considering actives, retired, and dead)
v <- df$career_lenght_w_actives_and_dead

is.numeric(v)                         # TRUE
sum(is.na(v))                         # how many NA
sum(!is.finite(v), na.rm = TRUE)      # Inf/NaN
summary(v)


#Plot

ggplot(df, aes(x = career_lenght_w_actives_and_dead)) +
  geom_histogram(
    binwidth = 1,         # 1 year
    boundary = 0,         # 
    closed = "left",
    fill = "steelblue",
    color = "black",
    na.rm = TRUE
  ) +
  labs(title = "Distribution of Career Length considering retired, active and death players",
       x = "Career Length (Years)", y = "Count Players") +
  theme_minimal()

#I think it doesnt look skewed

#Now check for outliers
#Boxplot 
ggplot(df, aes(y = career_lenght_w_actives_and_dead)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "gray40", na.rm = TRUE) +
  labs(title = "Career Length (Boxplot)", y = "Career Length (Years)", x = NULL) +
  theme_minimal()

## I depured the data as I had some players that did not played in the Liga but were in the lists (they were in selection B and had not debuted yet)
## Now I am considering only players who actually played during the league

#Are the careers of retired players different than of active players?
df %>%
  summarise(
    mean = mean(career_lenght_w_actives_and_dead, na.rm = TRUE),
    median = median(career_lenght_w_actives_and_dead, na.rm = TRUE),
    sd = sd(career_lenght_w_actives_and_dead, na.rm = TRUE),
    min = min(career_lenght_w_actives_and_dead, na.rm = TRUE),
    max = max(career_lenght_w_actives_and_dead, na.rm = TRUE),
    n = sum(!is.na(career_lenght_w_actives_and_dead))
  )

df %>%
  group_by(retired_tra) %>%
  summarise(
    count = n(),
    mean = mean(career_lenght_w_actives_and_dead, na.rm = TRUE),
    median = median(career_lenght_w_actives_and_dead, na.rm = TRUE),
    sd = sd(career_lenght_w_actives_and_dead, na.rm = TRUE),
    min = min(career_lenght_w_actives_and_dead, na.rm = TRUE),
    max = max(career_lenght_w_actives_and_dead, na.rm = TRUE)
  )

##looks the same

####Basic eda
# statistics for numerical variables
numerical_vars <- df %>% 
  select(where(is.numeric)) %>%
  names()

df %>% 
  select(all_of(numerical_vars)) %>%
  summary()

### Key variables 
#

#DHeight
class(df$height_tra)

ggplot(df, aes(x = height_tra)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Height", x = "Height", y = "Count")
# I dont see anything




#Debut age
class(df$debut_age_transfermarket)


ggplot(df, aes(x = debut_age_transfermarket)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of debut Ages", x = "Age", y = "Count")
#I Think the ones that debuted >25 have played in third or forth leages before (not professional

library(dplyr)

stats_debut <- df %>%
  summarise(
    mean = mean(debut_age_transfermarket, na.rm = TRUE),
    median = median(debut_age_transfermarket, na.rm = TRUE),
    p90 = quantile(debut_age_transfermarket, 0.90, na.rm = TRUE),
    min = min(debut_age_transfermarket, na.rm = TRUE),
    max = max(debut_age_transfermarket, na.rm = TRUE)
  )

stats_debut

#Scatterplot
ggplot(df, aes(x = debut_age_transfermarket,
               y = career_lenght_w_actives_and_dead)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Career Length vs Debut Age",
       x = "Debut Age",
       y = "Career Length (Years)") +
  theme_minimal()
##I THInk there is a clear negative relationship

#I Will check by different debut ages (bins)
df <- df %>%
  mutate(debut_group = cut(
    debut_age_transfermarket,
    breaks = c(0, 17, 19, 21, 23, 25, Inf),
    labels = c("<17", "17–18", "19–20", "21–22", "23–24", "25+")
  ))

df %>%
  group_by(debut_group) %>%
  summarise(
    mean_career = round(mean(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    median_career = round(median(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    n = n()
  )


ggplot(df, aes(x = debut_group,
               y = career_lenght_w_actives_and_dead,
               fill = debut_group)) +
  geom_boxplot() +
  labs(title = "Career Length by Debut Age Group",
       x = "Debut Age Group",
       y = "Career Length (Years)") +
  theme_minimal() +
  theme(legend.position = "none")

#Regression? or anova maybe
model <- lm(career_lenght_w_actives_and_dead ~ debut_age_transfermarket, data = df)
summary(model)

#P is highly sig
# Each year later a player debuts, their career is 0.46 years shorter
# Only 9.9% of career length variance is explained by debut age alone??? ( low R2) check interpretations



# Position analysis

position_career_summary <- df %>%
  group_by(position_tra) %>%
  summarise(
    n = n(),
    mean_career = round(mean(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    median_career = round(median(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    sd_career = round(sd(career_lenght_w_actives_and_dead, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_career))

position_career_summary


#I want to simplify the positions
# First, let's see all unique positions data
unique_positions <- unique(df$position_tra)
print(unique_positions)

# Create a dataframe 
position_lookup <- data.frame(
  position_tra = c(
    "Attack - Centre-Forward", "Defender - Right-Back", "Defender - Left-Back",
    "Defender - Centre-Back", "Attack - Left Winger", "Goalkeeper",
    "Midfield - Attacking Midfield", "Midfield - Defensive Midfield", 
    "Midfield - Central Midfield", "Attack - Right Winger", 
    "Midfield - Right Midfield", "Midfield - Left Midfield",
    "Attack - Second Striker", "Centre-Back", "Right Winger",
    "Defender - Centre-Back", "Defender", "Defensive Midfield",
    "	
Defender - Right-Back"
  ),
  position_simple = c(
    "Attack", "Defender", "Defender", "Defender", "Attack", "Goalkeeper",
    "Midfield", "Midfield", "Midfield", "Attack", "Midfield", "Midfield",
    "Attack", "Defender", "Attack", "Defender", "Defender", "Midfield",
    "Defender"
  )
)

# Merge with main dataframe
df <- df %>%
  left_join(position_lookup, by = "position_tra")

#
position_career_summary2 <- df %>%
  group_by(position_simple) %>%
  summarise(
    n = n(),
    mean_career = round(mean(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    median_career = round(median(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    sd_career = round(sd(career_lenght_w_actives_and_dead, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_career))

position_career_summary2


#Plot
ggplot(position_career_summary2,
       aes(x = reorder(position_simple, mean_career),
           y = mean_career)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_career - sd_career,
                    ymax = mean_career + sd_career),
                width = 0.2) +
  labs(title = "Mean Career Length by Position",
       x = "Position",
       y = "Mean Career Length (years)") +
  coord_flip() +               #
  theme_minimal()


ggplot(df, aes(x = position_simple,
               y = career_lenght_w_actives_and_dead,
               fill = position_simple)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40", na.rm = TRUE) +
  labs(title = "Career Length by Position",
       x = "Position",
       y = "Career Length (years)") +
  theme_minimal() +
  theme(legend.position = "none")

##I think I have to check more as defenders shouldnt be the ones with shorter careers
##seems there is a relationship 

# Basic ANOVA to test if position affects career length
anova_result <- aov(career_lenght_w_actives_and_dead ~ position_simple, 
                    data = df %>% filter(!is.na(position_simple)))
summary(anova_result)

# F-value = 4.839 with p = 0.00233 seems there differences between lenght and positions, but R2 is small (151)



#Maybe foot?
foot_summary <- df %>%
  group_by(foot_tra) %>%
  summarise(
    n = n(),
    mean_career = round(mean(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    median_career = round(median(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    sd_career = round(sd(career_lenght_w_actives_and_dead, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_career))

foot_summary

#Plot
ggplot(foot_summary,
       aes(x = reorder(foot_tra, mean_career),
           y = mean_career)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_career - sd_career,
                    ymax = mean_career + sd_career),
                width = 0.2) +
  labs(title = "Mean Career Length by Foot",
       x = "Foot",
       y = "Mean Career Length (years)") +
  coord_flip() +               #
  theme_minimal()


# Basic ANOVA to test if position affects career length
anova_result2 <- aov(career_lenght_w_actives_and_dead ~ foot_tra, 
                    data = df %>% filter(!is.na(position_simple)))
summary(anova_result2)
#Signigicant but weak effect


#Nationality

# Origin analysis
Country_summ <- df %>%
  count(nation) %>%
  arrange(desc(n)) %>%
  head(15)  # Top 20 clubs



country_summary <- df %>%
  group_by(nation) %>%
  summarise(
    n = n(),
    mean_career = round(mean(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    median_career = round(median(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    sd_career = round(sd(career_lenght_w_actives_and_dead, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_career))

country_summary

#Plot
ggplot(Country_summ,
       aes(x = reorder(nation, mean_career),
           y = mean_career)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_career - sd_career,
                    ymax = mean_career + sd_career),
                width = 0.2) +
  labs(title = "Mean Career Length by Nation",
       x = "Foot",
       y = "Mean Career Length (years)") +
  coord_flip() +               #
  theme_minimal()


# Basic ANOVA to test if position affects career length
anova_result3 <- aov(career_lenght_w_actives_and_dead ~ nation, 
                     data = df %>% filter(!is.na(position_simple)))
summary(anova_result3)
#Signigicant but strogn effect

# Summary by nation
nation_summary <- df %>%
  filter(!is.na(nation)) %>%
  group_by(nation) %>%
  summarise(
    n_players = n(),
    mean_career = mean(career_lenght_w_actives_and_dead),
    sd_career = sd(career_lenght_w_actives_and_dead),
    median_career = median(career_lenght_w_actives_and_dead),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_career))

# View top and bottom nations
head(nation_summary, 10)  # Top 10 nations
tail(nation_summary, 10)  # Bottom 10 nations


# Top 20 nations by career length
top_nations <- nation_summary %>%
  filter(n_players >= 5) %>%  # Only nations with reasonable sample size
  slice_max(mean_career, n = 20)

ggplot(top_nations, aes(x = reorder(nation, mean_career), y = mean_career)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_career - sd_career/sqrt(n_players),
                    ymax = mean_career + sd_career/sqrt(n_players)),
                width = 0.2) +
  coord_flip() +
  labs(title = "Top 20 Nations by Average Career Length",
       subtitle = "Only nations with 5+ players included",
       x = "Nation",
       y = "Mean Career Length (Years)") +
  theme_minimal()





#Players from national selection
national_summary <- df %>%
  group_by(National_team) %>%
  summarise(
    n = n(),
    mean_career = round(mean(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    median_career = round(median(career_lenght_w_actives_and_dead, na.rm = TRUE), 2),
    sd_career = round(sd(career_lenght_w_actives_and_dead, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_career))
national_summary

#Plot
ggplot(national_summary,
       aes(x = reorder(National_team, mean_career),
           y = mean_career)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_career - sd_career,
                    ymax = mean_career + sd_career),
                width = 0.2) +
  labs(title = "Mean Career Length by players in national team",
       x = "Selectionates?",
       y = "Mean Career Length (years)") +
  coord_flip() +               #
  theme_minimal()

#players playing national team seems to have longer careers

# Basic ANOVA to test if NATIONAL affects career length
anova_result4 <- aov(career_lenght_w_actives_and_dead ~ National_team, 
                     data = df %>% filter(!is.na(position_simple)))
summary(anova_result4)
##Very significant! also explains 942 of 21,421 total sum of squares (strongest so far), explains a lot of the variance



## INjuries
df %>%
  summarise(
    mean_inj = mean(injury_count_total_tra, na.rm = TRUE),
    median_inj = median(injury_count_total_tra, na.rm = TRUE),
    sd_inj = sd(injury_count_total_tra, na.rm = TRUE),
    min_inj = min(injury_count_total_tra, na.rm = TRUE),
    max_inj = max(injury_count_total_tra, na.rm = TRUE),
    total_players = n()
  )



df %>% count(injury_count_total_tra)

df %>%
  count(injury_count_total_tra) %>%
  mutate(percent = round(100 * n / sum(n), 1))





ggplot(df, aes(x = injury_count_total_tra)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Number of Injuries",
       x = "Number of Injuries", y = "Count") +
  theme_minimal()


ggplot(df, aes(x = position_simple, y = injury_count_total_tra, fill = position_simple)) +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  labs(title = "Injuries by Position",
       x = "Position", y = "Number of Injuries") +
  theme_minimal() +
  theme(legend.position = "none")

#scatter
ggplot(df, aes(x = injury_count_total_tra,
               y = career_lenght_w_actives_and_dead)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Career Length vs Number of Injuries",
       x = "Injuries", y = "Career Length (Years)") +
  theme_minimal()
#I thought the trend would be negative

# Basic ANOVA 
# LM
model_injury <- lm(career_lenght_w_actives_and_dead ~ injury_count_total_tra, 
                   data = df)
summary(model_injury)
#Highly significant but positive effect! we should investigate


#Games missed

df %>%
  group_by(retired_tra) %>%
  summarise(
    mean_injuries = mean(games_missed_total_tra, na.rm = TRUE),
    median_injuries = median(games_missed_total_tra, na.rm = TRUE)
  )



ggplot(df, aes(x = factor(retired_tra), y = games_missed_total_tra)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Injuries: Active (0) vs Retired (1)",
       x = "Status", y = "Number of Games missed") +
  theme_minimal()



#Games missed
df %>%
  summarise(
    mean_inj = mean(games_missed_total_tra, na.rm = TRUE),
    median_inj = median(games_missed_total_tra, na.rm = TRUE),
    sd_inj = sd(games_missed_total_tra, na.rm = TRUE),
    min_inj = min(games_missed_total_tra, na.rm = TRUE),
    max_inj = max(games_missed_total_tra, na.rm = TRUE),
    total_players = n()
  )



df %>% count(games_missed_total_tra)

df %>%
  count(games_missed_total_tra) %>%
  mutate(percent = round(100 * n / sum(n), 1))





ggplot(df, aes(x = games_missed_total_tra)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Number of Games missed",
       x = "Number of games", y = "Count") +
  theme_minimal()


ggplot(df, aes(x = position_simple, y = games_missed_total_tra, fill = position_simple)) +
  geom_boxplot(alpha = 0.7, na.rm = TRUE) +
  labs(title = "Injuries by Position",
       x = "Position", y = "Number of Injuries") +
  theme_minimal() +
  theme(legend.position = "none")

#scatter
ggplot(df, aes(x = games_missed_total_tra,
               y = career_lenght_w_actives_and_dead)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Career Length vs Number of Games missed",
       x = "Games", y = "Career Length (Years)") +
  theme_minimal()
#I thought the trend would be negative

# Basic ANOVA 
# LM
model_injury2 <- lm(career_lenght_w_actives_and_dead ~ games_missed_total_tra, 
                   data = df)
summary(model_injury2)
#Highly significant Each additional game missed → 0.01 years longer career 
#Survivorship Bias?


# Create injury severity categories based on games missed
df <- df %>%
  mutate(
    games_missed_group = case_when(
      is.na(games_missed_total_tra) ~ "Missing",
      games_missed_total_tra == 0 ~ "No Games Missed",
      games_missed_total_tra <= 10 ~ "Few Games Missed (1-10)",
      games_missed_total_tra <= 30 ~ "Moderate Games Missed (11-30)", 
      games_missed_total_tra > 30 ~ "Many Games Missed (30+)"
    )
  )

# Summary by games missed groups
games_missed_summary <- df %>%
  filter(!is.na(games_missed_total_tra)) %>%
  group_by(games_missed_group) %>%
  summarise(
    n = n(),
    mean_career = mean(career_lenght_w_actives_and_dead),
    median_career = median(career_lenght_w_actives_and_dead),
    mean_injuries = mean(injury_count_total_tra, na.rm = TRUE),
    .groups = 'drop'
  )

print(games_missed_summary)

ggplot(df %>% filter(!is.na(games_missed_total_tra)), 
       aes(x = games_missed_group, y = career_lenght_w_actives_and_dead, fill = games_missed_group)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Career Length by Games Missed Due to Injury",
       subtitle = "More games missed → slightly longer careers??",
       x = "Games Missed Due to Injury",
       y = "Career Length (Years)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

##CHeck the bins, not a good separation I think






# # Club analysis
# club_summary <- df %>%
#   count(current_club_tra) %>%
#   arrange(desc(n)) %>%
#   head(20)  # Top 20 clubs
# 
# ggplot(club_summary, aes(x = reorder(current_club_tra, n), y = n)) +
#   geom_col(fill = "darkred") +
#   coord_flip() +
#   labs(title = "Top 20 CUrrent Clubs by Player Count", x = "Club", y = "Count")
# 
# 
# club_summary_active <- df %>%
#   filter(retired_tra != "Yes") %>%        # keep only active players
#   count(current_club_tra) %>%
#   arrange(desc(n)) %>%
#   head(20)
# 
# ggplot(club_summary_active, aes(x = reorder(current_club_tra, n), y = n)) +
#   geom_col(fill = "darkred") +
#   coord_flip() +
#   labs(
#     title = "Top 20 Current Clubs by Active Player Count",
#     x = "Club",
#     y = "Number of Active Players"
#   ) +
#   theme_minimal()
# ###Are those without club already retired?



# # Club they were playing at the liga in timescope analysis
# club_liga <- df %>%
#   count(squad) %>%
#   arrange(desc(n)) %>%
#   head(20)  # Top 20 clubs
# 
# 
# ggplot(club_liga, aes(x = reorder(squad, n), y = n)) +
#   geom_col(fill = "darkred") +
#   coord_flip() +
#   labs(
#     title = "Top 20 Liga clubs",
#     x = "Club",
#     y = "Number of Active Players"
#   ) +
#   theme_minimal()
# 
# 
# 
# # Club debut analysis
# club_debut <- df %>%
#   count(first_club_fbref) %>%
#   arrange(desc(n)) %>%
#   head(20)  # Top 20 clubs
# 
# 
# ggplot(club_debut, aes(x = reorder(first_club_fbref, n), y = n)) +
#   geom_col(fill = "darkred") +
#   coord_flip() +
#   labs(
#     title = "Top Debut clubs",
#     x = "Club",
#     y = "Number of Active Players"
#   ) +
#   theme_minimal()
# 
# 



#Retirement age
class(df$Retirement_age)

ggplot(df, aes(x = Retirement_age)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of retirement Ages", x = "Age", y = "Count")

mean(df$Retirement_age, na.rm = TRUE)


# df <- df %>%
#   mutate(career_length = Retirement_age - debut_age)

#Lenght
class(df$Years_career)

ggplot(df, aes(x = Years_career)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of lenght of the careers", x = "Age", y = "Count")

mean(df$Years_career, na.rm = TRUE)


df %>%
  group_by(position) %>%
  summarise(
    mean_career = round(mean(Years_career, na.rm = TRUE), 2),
    median_career = round(median(Years_career, na.rm = TRUE), 2),
    sd_career = round(sd(Years_career, na.rm = TRUE), 2),
    n_players = n()
  ) %>%
  arrange(desc(mean_career))



ggplot(df, aes(x = position, y = Years_career, fill = position)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40") +
  labs(title = "Career Length by Player Position",
       x = "Position", y = "Career Length (years)") +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(df, aes(x = position, y = Years_career, fill = position)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  labs(title = "Career Length Distribution by Position",
       x = "Position", y = "Career Length (years)") +
  theme_minimal() +
  theme(legend.position = "none")



#Lenght
class(df$Years_career)

ggplot(df, aes(x = Years_career)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of retirement Ages", x = "Age", y = "Count")

mean(df$Years_career, na.rm = TRUE)


df %>%
  group_by(position_tra) %>%
  summarise(
    mean_career = round(mean(Years_career, na.rm = TRUE), 2),
    median_career = round(median(Years_career, na.rm = TRUE), 2),
    sd_career = round(sd(Years_career, na.rm = TRUE), 2),
    n_players = n()
  ) %>%
  arrange(desc(mean_career))



ggplot(df, aes(x = position_tra, y = Years_career, fill = position)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40") +
  labs(title = "Career Length by Player Position Transfermkt",
       x = "Position", y = "Career Length (years)") +
  theme_minimal() +
  theme(legend.position = "none")




#Age



df %>%
  group_by(position) %>%
  summarise(
    mean_career = round(mean(Retirement_age, na.rm = TRUE), 2),
    median_career = round(median(Retirement_age, na.rm = TRUE), 2),
    sd_career = round(sd(Retirement_age, na.rm = TRUE), 2),
    n_players = n()
  ) %>%
  arrange(desc(mean_career))



ggplot(df, aes(x = position, y = Retirement_age, fill = position)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40") +
  labs(title = "Career Length by Player Position Transfermkt",
       x = "Position", y = "Career Length (years)") +
  theme_minimal() +
  theme(legend.position = "none")





#Debut Age



df %>%
  group_by(position) %>%
  summarise(
    mean_career = round(mean(debut_age_fbref, na.rm = TRUE), 2),
    median_career = round(median(debut_age_fbref, na.rm = TRUE), 2),
    sd_career = round(sd(debut_age_fbref, na.rm = TRUE), 2),
    n_players = n()
  ) %>%
  arrange(desc(mean_career))



ggplot(df, aes(x = position, y = debut_age_fbref, fill = position)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40") +
  labs(title = "Career Length by Player Position Transfermkt",
       x = "Position", y = "Career Length (years)") +
  theme_minimal() +
  theme(legend.position = "none")



#Minutes played



df %>%
  group_by(position) %>%
  summarise(
    mean_career = round(mean(CAREER.Sum.of.minutes, na.rm = TRUE), 2),
    median_career = round(median(CAREER.Sum.of.minutes, na.rm = TRUE), 2),
    sd_career = round(sd(CAREER.Sum.of.minutes, na.rm = TRUE), 2),
    n_players = n()
  ) %>%
  arrange(desc(mean_career))



ggplot(df, aes(x = position, y = CAREER.Sum.of.minutes, fill = position)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40") +
  labs(title = "Career Length by Player Position Transfermkt",
       x = "Position", y = "Career Length (years)") +
  theme_minimal() +
  theme(legend.position = "none")



#Games played



df %>%
  group_by(position) %>%
  summarise(
    mean_career = round(mean(CAREER.Sum.of.matches_played, na.rm = TRUE), 2),
    median_career = round(median(CAREER.Sum.of.matches_played, na.rm = TRUE), 2),
    sd_career = round(sd(CAREER.Sum.of.matches_played, na.rm = TRUE), 2),
    n_players = n()
  ) %>%
  arrange(desc(mean_career))



ggplot(df, aes(x = position, y = CAREER.Sum.of.matches_played, fill = position)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray40") +
  labs(title = "Career Length by Player Position Transfermkt",
       x = "Position", y = "Career Length (years)") +
  theme_minimal() +
  theme(legend.position = "none")




# Function  EDA report
generate_eda_report <- function(data) {
  cat("=== FOOTBALL CAREER SURVIVAL ANALYSIS EDA REPORT ===\n\n")
  
  cat("1. DATA DIMENSIONS:\n")
  cat("   Observations:", nrow(data), "\n")
  cat("   Variables:", ncol(data), "\n\n")
  
  cat("2. MISSING VALUES SUMMARY:\n")
  print(missing_summary %>% arrange(desc(Missing_Percentage)))
  
  cat("\n3. KEY VARIABLE DISTRIBUTIONS:\n")
  cat("   Age range:", range(data$Retirement_age, na.rm = TRUE), "\n")
  cat("   Debut age range:", range(data$debut_age_fbref, na.rm = TRUE), "\n")
  cat("   Career length range:", range(data$Years_career, na.rm = TRUE), "\n")
  
  cat("\n4. POSITION DISTRIBUTION:\n")
  print(position_summary)
}

generate_eda_report(df)
