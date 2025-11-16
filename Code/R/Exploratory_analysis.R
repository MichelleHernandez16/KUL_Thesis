########################################################
###########    EDA   ###################################
########################################################

####LOAD PACKAGES AND DATASET
library(dplyr)
library(lubridate)
library(ggplot2)
install.packages("tidyverse")
install.packages("visdat")
install.packages("naniar")
library(tidyverse)
library(visdat)
library(naniar)



df <- read.csv("la_liga_stats_2010_2015_final.csv")
str(df)
glimpse(df)

#Convert to numeric
df <- df %>%
  mutate(
    debut_year_fbref = as.numeric(debut_year_fbref),
    debut_age_fbref = as.numeric(debut_age_fbref),
    Years_career = as.numeric(Years_career),
    Retirement_age = as.numeric(Retirement_age),
    height_fbref = as.numeric(gsub("[^0-9\\.]", "", height_fbref)),  # removes "m" or commas
    CAREER.Sum.of.matches_played = as.numeric(CAREER.Sum.of.matches_played),
    CAREER.Sum.of.minutes = as.numeric(CAREER.Sum.of.minutes),
    CAREER.Sum.of.goals = as.numeric(CAREER.Sum.of.goals)
  )

#did any valyes converted to NA?
df %>%
  filter(is.na(as.numeric(debut_age_fbref)) & !is.na(debut_age_fbref)) %>%
  select(debut_age_fbref)


### DETECT CENSORING

# Check the retirement variables
table(df$retired_tra, useNA = "ifany") #Add a NA row

# (blank)      No     Yes 
# 1    1081    1701  ### 61% are retired


df <- df %>%
  mutate(retirement_date_tra = as.Date(retirement_date_tra)) ##data format


# Look at a few rows
df %>%
  select(player_name, retired_tra, retirement_date_tra) %>%
  head(10)

df <- df %>%
  mutate(event = ifelse(retired_tra == "Yes" | retired_tra == 1, 1, 0))

# Summary counts
df %>%
  count(event) %>%
  mutate(percent = n / sum(n) * 100)

# Do all retired players have retired date? are they active with a date?
df %>%
  summarise(
    missing_retired_flag = sum(is.na(retired_tra)),
    missing_retirement_date = sum(is.na(retirement_date_tra)),
    retired_but_no_date = sum(event == 1 & is.na(retirement_date_tra)),
    active_with_date = sum(event == 0 & !is.na(retirement_date_tra))
  )




ggplot(df, aes(x = factor(event, labels = c("Active (Censored)", "Retired")))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Censored Players",
       x = "Career Status", y = "Count") +
  theme_minimal()




censor_summary <- df %>%
  count(event) %>%
  mutate(
    status = ifelse(event == 1, "Retired", "Active (Censored)"),
    percent = round(100 * n / sum(n), 1),
    label = paste0(status, " — ", percent, "%")
  )

censor_summary


library(ggplot2)

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


pie(censor_summary$percent,
    labels = censor_summary$label,
    col = c("skyblue", "tan"),
    main = "Censoring of Players")




### MISSING DATA CHECK
missing_summary <- df %>%
  summarise(across(everything(), ~sum(is.na(.))/n()*100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Percentage")
missing_summary

print(missing_summary) ## I THInk most of the important variables dont have many missing values


####Basic eda
# statistics for numerical variables
numerical_vars <- df %>% 
  select(where(is.numeric)) %>%
  names()

df %>% 
  select(all_of(numerical_vars)) %>%
  summary()

# Distribution of key variables 

#Debut age
class(df$debut_age_fbref)
df$debut_age_fbref <- as.numeric(df$debut_age_fbref)
class(df$debut_age_fbref)


ggplot(df, aes(x = debut_age_fbref)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of debut Ages", x = "Age", y = "Count")
#I Think the ones that debuted >25 have played in third or forth leages (not professional



ggplot(df, aes(x = debut_year_fbref)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Debut Ages", x = "Debut Age", y = "Count")
#I should check why most debuted in 2010

# Position analysis
position_summary <- df %>%
  count(position) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(position_summary, aes(x = reorder(position, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Player Distribution by Position", x = "Position", y = "Count")

# Club analysis
club_summary <- df %>%
  count(current_club_tra) %>%
  arrange(desc(n)) %>%
  head(20)  # Top 20 clubs

ggplot(club_summary, aes(x = reorder(current_club_tra, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top 20 CUrrent Clubs by Player Count", x = "Club", y = "Count")


club_summary_active <- df %>%
  filter(retired_tra != "Yes") %>%        # keep only active players
  count(current_club_tra) %>%
  arrange(desc(n)) %>%
  head(20)

ggplot(club_summary_active, aes(x = reorder(current_club_tra, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "Top 20 Current Clubs by Active Player Count",
    x = "Club",
    y = "Number of Active Players"
  ) +
  theme_minimal()
###Are those without club already retired?



# Club they were playing at the liga in timescope analysis
club_liga <- df %>%
  count(squad) %>%
  arrange(desc(n)) %>%
  head(20)  # Top 20 clubs


ggplot(club_liga, aes(x = reorder(squad, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "Top 20 Liga clubs",
    x = "Club",
    y = "Number of Active Players"
  ) +
  theme_minimal()



# Club debut analysis
club_debut <- df %>%
  count(first_club_fbref) %>%
  arrange(desc(n)) %>%
  head(20)  # Top 20 clubs


ggplot(club_debut, aes(x = reorder(first_club_fbref, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "Top Debut clubs",
    x = "Club",
    y = "Number of Active Players"
  ) +
  theme_minimal()


# Origin analysis
Country_summ <- df %>%
  count(nation) %>%
  arrange(desc(n)) %>%
  head(10)  # Top 20 clubs


ggplot(Country_summ, aes(x = reorder(nation, n), y = n)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(
    title = "Top Origin countries",
    x = "Club",
    y = "Number of Active Players"
  ) +
  theme_minimal()


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
