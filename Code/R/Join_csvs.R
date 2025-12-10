########################################################
###########    Join CSVS   ###################################
########################################################

##FILE TO JOIN THE LIST OF PLAYERS WITH THE DEBUT CSV AND THE GENERAL DATA CSV FROM TRANSFERMARKT


#setwd("C:/git/Thesis/Data/Premier")
#As some transfermarket Ids were incorrect, this one is the revised 
setwd("C:/git/Thesis/Data/Premier/Premier_revised")

#Load libraries
library(dplyr)

#Load the csvs
# premier_df <- read.csv("premier_league_player_stats_cleaned.csv")
# debut_df <- read.csv("premier_players_debut.csv")
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
names(premier_)



#save csv
#write.csv(premier_all,
          "C:/git/Thesis/Data/Premier/Premier_revised/premier_all2.csv",
          row.names = FALSE)
file.exists("C:/git/Thesis/Data/Premier/Premier_revised/premier_all2.csv")





###NOW ADD THE PERFORMANCE SUMMARIES

all_almost_df <- read.csv("premier_all2.csv")

 national_df <- read.csv("premier_national_team.csv")
 
 

 
 career_summary <- national_df %>%
   group_by(player_url, player_name) %>%
   summarise(
     total_matches = sum(matches_played, na.rm = TRUE),
     total_minutes = sum(minutes, na.rm = TRUE),
     total_goals = sum(goals, na.rm = TRUE),
     total_assists = sum(assists, na.rm = TRUE),
     seasons_played = n(),  #
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
  group_by(player_name) %>%
  summarise(
    total_matches_club = sum(matches_played, na.rm = TRUE),
    total_minutes_club = sum(minutes, na.rm = TRUE),
    total_goals_club   = sum(goals, na.rm = TRUE),
    total_assists_club = sum(assists, na.rm = TRUE),
    seasons_club       = n(),         
    .groups = "drop"
  )

 




names(premier_all2)
names(history_summary)


premier_all2 <- premier_all2 %>%
  rename(player_name = player_name.x)


premier_all3 <- premier_all2 %>%
  left_join(history_summary,
            by = "player_name",
            relationship = "many-to-many")


#save csv
write.csv(premier_all3,
          "C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_all3.csv",
          row.names = FALSE)
file.exists("C:/git/Thesis/Data/Premier/Premier_revised/final_dataset/premier_all3.csv")
