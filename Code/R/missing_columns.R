########################################################
###########    Join CSVS   ###################################
########################################################

##FILE TO JOIN THE LIST OF PLAYERS WITH THE DEBUT CSV AND THE GENERAL DATA CSV FROM TRANSFERMARKT




#Load libraries
library(dplyr)
library(stringr)
library(lubridate)
library(readr)

### Load data
-

# Adjust path if needed; here assumed in working directory
  setwd("C:/git/Thesis/Data")
df<- read_csv("la_liga_stats_2010_2015_depured.csv",
                show_col_types = FALSE)
names(df)

###NOW ADD THE PERFORMANCE SUMMARIES
### CONSTRUCTED COLUMNS
str(df2$total_seasons_national)
str(df$`CAREER Sum of goals`)
str(df2$matches_national)
str(df2$`CAREER Sum of matches_played`)



#I need to change my national team variables as they are not numeric
national_cols <- c("matches_national", "minutes_national", "goals_national", "total_seasons_national")

df2 <- df %>%
  mutate(
    across(
      all_of(national_cols),
      ~ parse_number(replace(as.character(.), . == "Not_national_team", "0")),
      .names = "{.col}_num"
    )
  )

#replace back
df2 <- df %>%
  mutate(
    across(
      all_of(national_cols),
      ~ parse_number(replace(as.character(.), . == "Not_national_team", "0"))
    )
  )


#Sum of national team performance and all competitions performance metrics to have a total 
df2 <- df2 %>%
  mutate(
    total_matches_total = coalesce(matches_national, 0) + coalesce(`CAREER Sum of matches_played`, 0),
    total_minutes_total = coalesce(minutes_national, 0) + coalesce(`CAREER Sum of minutes`, 0),
    total_goals_total   = coalesce(goals_national,   0) + coalesce(`CAREER Sum of goals`,   0)
  ) # need to indicate the column with backticks because of spaces in the names




#Simplify the positions 
df2 <- df2 %>%
  mutate(
    position_clean = str_squish(position_tra),  # clean
    position_simple = case_when(
      str_detect(position_clean, "Goalkeeper") ~ "Goalkeeper",
      str_detect(position_clean, "Attack|Winger|Second Striker|Centre-Forward") ~ "Attack",
      str_detect(position_clean, "Midfield") ~ "Midfield",
      str_detect(position_clean, "Defender|Centre-Back|Right-Back|Left-Back") ~ "Defender",
      TRUE ~ "Other"
    )
  )



#current age and retirement age

df2 <- df2 %>%
  mutate(
    dob = dmy(dob_age_tra)   # change to date
  )


df2 <- df2 %>%
  mutate(
    age_current = floor(interval(dob, today()) / years(1))
  )


#calculate agecurrent considering cases where dob age is NA
df2 <- df2 %>%
  mutate(
    year_birth = suppressWarnings(as.integer(parse_number(as.character(YEAR_tra)))),
    age_current = case_when(
      !is.na(dob) ~ floor(interval(dob, today()) / years(1)),
      is.na(dob) & !is.na(year_birth) ~ year(today()) - year_birth,
      TRUE ~ NA_real_
    )
  )

df2 %>% summarise(
  na_dob = sum(is.na(dob)),
  na_age_current = sum(is.na(age_current))
)


df2 <- df2 %>%
  mutate(
    retirement_date = na_if(trimws(retirement_date_tra), ""),
    retirement_date_clean = dmy(retirement_date_tra, quiet = TRUE)
  )

#rename retirement age column
df2 <- df2 %>%
  rename(retirement_age = `retirement age`)


#Now career_lenght
df2 <- df2 %>%
  mutate(
    debut_age_transfermarket = as.numeric(debut_age_transfermarket),
    death_age = as.numeric(death_age)   
  )

unique(df2$debut_age_transfermarket[is.na(as.numeric(df2$debut_age_transfermarket))])



df2 <- df2 %>%
  mutate(
    career_length = case_when(
      
      # 1. Retired players
      retired_tra == "Yes" & !is.na(retirement_age) ~
        retirement_age - debut_age_transfermarket,
      
      # 2. Not retired but dead
      Deceased_tra == "Yes" & !is.na(death_age) ~
        death_age - debut_age_transfermarket,
      
      # 3. Still playing
      retired_tra != "Yes" & Deceased_tra != "Yes" & !is.na(age_current) ~
        age_current - debut_age_transfermarket,
      
      # 4. Others
      TRUE ~ NA_real_
    )
  )



#save csv
write.csv(df2,
          "C:/git/Thesis/Data/la_liga_stats_2010_2015_depured2.csv",
          row.names = FALSE)
file.exists( "C:/git/Thesis/Data/la_liga_stats_2010_2015_depured2.csv")












