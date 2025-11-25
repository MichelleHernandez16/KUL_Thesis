########################################################
###########    Join CSVS   ###################################
########################################################

##FILE TO JOIN THE LIST OF PLAYERS WITH THE DEBUT CSV AND THE GENERAL DATA CSV FROM TRANSFERMARKT


setwd("C:/git/Thesis/Data/Premier")


#Load libraries
library(dplyr)

#Load the csvs
premier_df <- read.csv("premier_league_player_stats_cleaned.csv")
debut_df <- read.csv("premier_players_debut.csv")
transfer_df <- read.csv("players_transfermarkt.csv")






#Join the csvs
premier_all <- premier_df %>%
  left_join(debut_df, by = "transfermarkt_id", relationship = "many-to-many") %>%
  left_join(transfer_df, by = "transfermarkt_id", relationship = "many-to-many")


head(premier_joined)
names(premier_joined)
