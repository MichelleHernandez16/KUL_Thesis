########################################################
###########    Join CSVS   ###################################
########################################################

##FILE TO JOIN THE LIST OF PLAYERS WITH THE DEBUT CSV AND THE GENERAL DATA CSV FROM TRANSFERMARKT


#setwd("C:/git/Thesis/Data/Premier")
#As some transfermarket Ids were incorrect, this one is the revised 
setwd("C:/git/Thesis/Data/Premier/Premier_revised")

#Load libraries
library(dplyr)
library(stringr)
library(lubridate)

#Load the csvs
3# debut_df <- read.csv("premier_players_debut.csv")
# transfer_df <- read.csv("players_transfermarkt.csv")
# fbref_df <- read.csv("premier_fbref.csv")


#Load the csvs (THIS PART IS FOR VALIDATED PLAYERS AFTER CHECKING TRANSFER ID)
premier_df <- read.csv("premier_league_player_stats_cleaned_revised.csv")
debut_df <- read.csv("premier_players_debut_2.csv")
transfer_df <- read.csv("players_transfermarkt_2.csv")
fbref_df <- read.csv("premier_fbref2.csv")



# #Join the csvs
# premier_all <- premier_df %>%
#   left_join(debut_df, by = "transfermarkt_id", relationship = "many-to-many") %>%
#   left_join(transfer_df, by = "transfermarkt_id", relationship = "many-to-many")%>%
#   left_join(fbref_df, by = "transfermarkt_id", relationship = "many-to-many")


#JOINT THE CVS BUT NOW BY THE transfermarkt_url
premier_all <- premier_df %>%
  left_join(debut_df, by = "transfermarkt_url", relationship = "many-to-many") %>%
  left_join(transfer_df, by = "transfermarkt_url", relationship = "many-to-many") %>%
  left_join(fbref_df, by = "fbref_url", relationship = "many-to-many")



head(premier_all)
names(premier_all)



#save csv
#write.csv(premier_all,
          "C:/git/Thesis/Data/Premier/Premier_revised/premier_all2.csv",
          row.names = FALSE)
file.exists("C:/git/Thesis/Data/Premier/Premier_revised/premier_all2.csv")





###NOW ADD THE PERFORMANCE SUMMARIES

all_almost_df <- read.csv("premier_all2.csv")

 national_df <- read.csv("premier_national_team.csv")
 
 

 #Make summary by player
 career_summary <- national_df %>%
   group_by(player_url, player_name) %>%
   summarise(
    matches_national = sum(matches_played, na.rm = TRUE),
    minutes_national = sum(minutes, na.rm = TRUE),
    goals_national = sum(goals, na.rm = TRUE),
    seasons_national       = n_distinct(season),   
     .groups = "drop"
   )
 

 
 
 #join to my main dataframe
 premier_all2 <- all_almost_df %>%
   left_join(career_summary,
             by = c("fbref_url" = "player_url"),
             relationship = "many-to-many")
 
 
 ##All competitions history
history_df <- read.csv("premier_career_history.csv")



 
history_summary <- history_df %>%
  group_by(player_url) %>%
  summarise(
    total_matches_club = sum(matches_played, na.rm = TRUE),
    total_minutes_club = sum(minutes, na.rm = TRUE),
    total_goals_club   = sum(goals, na.rm = TRUE),
    seasons_club       = n_distinct(season),   # seasons
    clubs_club         = n_distinct(squad),    # disctinct clubs
    .groups = "drop"
  )



names(premier_all2)
names(history_summary)




premier_all3 <- premier_all2 %>%
  left_join(history_summary,
            by = c("fbref_url" = "player_url"),
            relationship = "many-to-many")



###NOW Add if they are deceased
deceased_df <- read.csv("players_with_deceased_status.csv")



premier_all3 <- premier_all3 %>%
  left_join(deceased_df, by = "transfermarkt_url", relationship = "many-to-many")







#save csv
write.csv(premier_all3,
          "C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_all3.csv",
          row.names = FALSE)
file.exists("C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_all3.csv")




### CONSTRUCTED COLUMNS


#Sum of national team performance and all competitions performance metrics to have a total 
premier_all4 <- premier_all3 %>%
  mutate(
    total_matches_total = coalesce(matches_national, 0) + coalesce(total_matches_club, 0),
    total_minutes_total = coalesce(minutes_national, 0) + coalesce(total_minutes_club, 0),
    total_goals_total   = coalesce(goals_national,   0) + coalesce(total_goals_club,   0)
  )



#Simplify the positions 
premier_all4 <- premier_all4 %>%
  mutate(
    position_clean = str_squish(position.x.x),  # clean
    position_simple = case_when(
      str_detect(position_clean, "Goalkeeper") ~ "Goalkeeper",
      str_detect(position_clean, "Attack|Winger|Second Striker|Centre-Forward") ~ "Attack",
      str_detect(position_clean, "Midfield") ~ "Midfield",
      str_detect(position_clean, "Defender|Centre-Back|Right-Back|Left-Back") ~ "Defender",
      TRUE ~ "Other"
    )
  )



#current age and retirement age

premier_all4 <- premier_all4 %>%
  mutate(
    dob = dmy(dob_age.x)   # change to date
  )


premier_all4 <- premier_all4 %>%
  mutate(
    age_current = floor(interval(dob, today()) / years(1))
  )


premier_all4 <- premier_all4 %>%
  mutate(
    retirement_date = na_if(trimws(retirement_date), ""),
    retirement_date_clean = dmy(retirement_date, quiet = TRUE)
  )


premier_all4 <- premier_all4 %>%
  mutate(
    retirement_age = case_when(
      retired == "Yes" & !is.na(dob) & !is.na(retirement_date_clean) ~
        floor(interval(dob, retirement_date_clean) / years(1)),
      TRUE ~ NA_real_
    )
  )


#Now career_lenght
premier_all4 <- premier_all4 %>%
  mutate(
    debut_age_transfermarket = as.numeric(debut_age_transfermarket),
    death_age = as.numeric(death_age)   
  )

unique(premier_all4$debut_age_transfermarket[is.na(as.numeric(premier_all4$debut_age_transfermarket))])



premier_all4 <- premier_all4 %>%
  mutate(
    career_length = case_when(
      
      # 1. Retired players
      retired == "Yes" & !is.na(retirement_age) ~
        retirement_age - debut_age_transfermarket,
      
      # 2. Not retired but dead
      deceased == "Yes" & !is.na(death_age) ~
        death_age - debut_age_transfermarket,
      
      # 3. Still playing
      retired != "Yes" & deceased != "Yes" & !is.na(age_current) ~
        age_current - debut_age_transfermarket,
      
      # 4. Others
      TRUE ~ NA_real_
    )
  )



#save csv
write.csv(premier_all4,
          "C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_all4.csv",
          row.names = FALSE)
file.exists("C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_all4.csv")










##OK NOW FOR NOT DETAILED DATA



#Load the csvs (THIS PART IS FOR VALIDATED PLAYERS AFTER CHECKING TRANSFER ID)
#premier_df_2 <- read.csv("premier_league_player_stats_cleaned_revised.csv")
debut_df_2 <- read.csv("premier_players_debut_2.csv")
transfer_df_2 <- read.csv("players_transfermarkt_2.csv")
fbref_df_2 <- read.csv("premier_fbref2.csv")

id_lookup <- premier_df %>% 
  select(transfermarkt_url, fbref_url) %>%
  distinct()



#JOINT THE CVS BUT NOW BY THE transfermarkt_url
premier_all <- premier_df %>%
  left_join(debut_df, by = "transfermarkt_url", relationship = "many-to-many") %>%
  left_join(transfer_df, by = "transfermarkt_url", relationship = "many-to-many") %>%
  left_join(fbref_df, by = "fbref_url", relationship = "many-to-many")

premier_simple <- debut_df %>%
  left_join(transfer_df, by = "transfermarkt_url", relationship = "many-to-many") %>%
  left_join(id_lookup,   by = "transfermarkt_url") %>%  # adds fbref_url
  left_join(fbref_df,    by = "fbref_url", relationship = "many-to-many")





#save csv
#write.csv(premier_all,
"C:/git/Thesis/Data/Premier/Premier_revised/premier_all2.csv",
row.names = FALSE)
file.exists("C:/git/Thesis/Data/Premier/Premier_revised/premier_all2.csv")





###NOW ADD THE PERFORMANCE SUMMARIES



national_df <- read.csv("premier_national_team.csv")




career_summary <- national_df %>%
  group_by(player_url, player_name) %>%
  summarise(
    matches_national = sum(matches_played, na.rm = TRUE),
    minutes_national = sum(minutes, na.rm = TRUE),
    goals_national = sum(goals, na.rm = TRUE),
    seasons_national       = n_distinct(season),   
    .groups = "drop"
  )




#join to my main dataframe
premier_simple2 <- premier_simple %>%
  left_join(career_summary,
            by = c("fbref_url" = "player_url"),
            relationship = "many-to-many")


##All competitions history
history_df <- read.csv("premier_career_history.csv")




history_summary <- history_df %>%
  group_by(player_url) %>%
  summarise(
    total_matches_club = sum(matches_played, na.rm = TRUE),
    total_minutes_club = sum(minutes, na.rm = TRUE),
    total_goals_club   = sum(goals, na.rm = TRUE),
    seasons_club       = n_distinct(season),   # seasons
    clubs_club         = n_distinct(squad),    # disctinct clubs
    .groups = "drop"
  )



names(premier_all2)
names(history_summary)




premier_simple2 <- premier_simple2 %>%
  left_join(history_summary,
            by = c("fbref_url" = "player_url"),
            relationship = "many-to-many")



###NOW Add if they are deceased
deceased_df <- read.csv("players_with_deceased_status.csv")



premier_simple2 <- premier_simple2 %>%
  left_join(deceased_df, by = "transfermarkt_url", relationship = "many-to-many")







#save csv
write.csv(premier_simple2,
          "C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_simple.csv",
          row.names = FALSE)
#file.exists("C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_all3.csv")




### CONSTRUCTED COLUMNS


#Sum of national team performance and all competitions performance metrics to have a total 
premier_simple3 <- read.csv("premier_simple.csv")

premier_simple3 <- premier_simple3 %>%
  mutate(
    total_matches_total = coalesce(matches_national, 0) + coalesce(total_matches_club, 0),
    total_minutes_total = coalesce(minutes_national, 0) + coalesce(total_minutes_club, 0),
    total_goals_total   = coalesce(goals_national,   0) + coalesce(total_goals_club,   0)
  )



#Simplify the positions 
premier_simple3 <- premier_simple3 %>%
  mutate(
    position_clean = str_squish(position.x),  # clean
    position_simple = case_when(
      str_detect(position_clean, "Goalkeeper") ~ "Goalkeeper",
      str_detect(position_clean, "Attack|Winger|Second Striker|Centre-Forward") ~ "Attack",
      str_detect(position_clean, "Midfield") ~ "Midfield",
      str_detect(position_clean, "Defender|Centre-Back|Right-Back|Left-Back") ~ "Defender",
      TRUE ~ "Other"
    )
  )



#current age and retirement age
unique(premier_simple3$dob_age.x[1:50])

premier_simple3 <- premier_simple3 %>%
  mutate(
    dob = dmy(dob_age.x)   # change to date
  )


premier_simple3 <- premier_simple3 %>%
  mutate(
    age_current = floor(interval(dob, today()) / years(1))
  )


premier_simple3 <- premier_simple3 %>%
  mutate(
    retirement_date = na_if(trimws(retirement_date), ""),
    retirement_date_clean = dmy(retirement_date, quiet = TRUE)
  )


premier_simple3 <- premier_simple3 %>%
  mutate(
    retirement_age = case_when(
      retired == "Yes" & !is.na(dob) & !is.na(retirement_date_clean) ~
        floor(interval(dob, retirement_date_clean) / years(1)),
      TRUE ~ NA_real_
    )
  )


#Now career_lenght
premier_simple3 <- premier_simple3 %>%
  mutate(
    debut_age_transfermarket = as.numeric(debut_age_transfermarket),
    death_age = as.numeric(death_age)   
  )

unique(premier_all4$debut_age_transfermarket[is.na(as.numeric(premier_all4$debut_age_transfermarket))])



premier_simple3 <- premier_simple3 %>%
  mutate(
    career_length = case_when(
      
      # 1. Retired players
      retired == "Yes" & !is.na(retirement_age) ~
        retirement_age - debut_age_transfermarket,
      
      # 2. Not retired but dead
      deceased == "Yes" & !is.na(death_age) ~
        death_age - debut_age_transfermarket,
      
      # 3. Still playing
      retired != "Yes" & deceased != "Yes" & !is.na(age_current) ~
        age_current - debut_age_transfermarket,
      
      # 4. Others
      TRUE ~ NA_real_
    )
  )



#save csv
write.csv(premier_simple3,
          "C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_simple.csv",
          row.names = FALSE)
file.exists("C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_simple.csv")






