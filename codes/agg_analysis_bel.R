

##############################################
#### ANALYSIS OF MEANS FROM BEL SESSIONS ####
##############################################
#create group means datatset
#one graph: 1) bars with all means-of-means

####################################
#### PRELIMINARIES ####
####################################
rm(list=ls())
library(tidyverse)
library(forcats)
##############################################
##############################################

####################################
#### CREATE GROUP MEANS DATASET ####
####################################
data_firstsession<-read_csv("/Users/UseNetID/Dropbox/MaIn/data/LINEEX_december2018/2018RJAMA0102_RawData_bel_03122018/AcceptanceCurse_2018-12-03.csv")
data_secondsession<-read_csv("/Users/UseNetID/Dropbox/MaIn/data/LINEEX_december2018/2018RJAMA0105_RawData_bel_04122018/AcceptanceCurse_2018-12-04.csv")
data_thirdsession<-read_csv("/Users/UseNetID/Dropbox/MaIn/data/LINEEX_december2018/2018RJAMA0108_RawData_bel_05122018/AcceptanceCurse_2018-12-05.csv")
data_all_bel <- data_firstsession %>%
  bind_rows(data_secondsession) %>%
  bind_rows(data_thirdsession)
rm(data_firstsession)
rm(data_secondsession)
rm(data_thirdsession)
min_round <- 20
max_round <- 60
data_all_bel <- mutate(data_all_bel,round_ind = subsession.round_number>=min_round & subsession.round_number<=max_round)
data_all_bel <- data_all_bel %>%
  mutate(player.type=factor(player.type)) %>%
  mutate(player.type=fct_recode(player.type,
                                "H"="1",
                                "M"="2",
                                "L"="3"))
data_all_bel <- data_all_bel %>%
  mutate(player.signal=factor(player.signal)) %>%
  mutate(player.signal=fct_recode(player.signal,
                                  "h"="1",
                                  "m"="2",
                                  "l"="3"))
data_all_bel <- data_all_bel %>%
  mutate(subsession.game_name=factor(subsession.game_name))

data_groups <- data_all_bel %>%
  group_by(group.id_in_subsession,
           session.code,
           player.type,
           player.signal,
           subsession.game_name
  ) %>%
  summarise(mean_choice = mean(player.choice[round_ind&player.status==0]))

data_groups <- data_groups %>%
  ungroup() %>%
  filter(player.type=="M") %>%
  select(session.code,
         group.id_in_subsession,
         player.signal,
         subsession.game_name,
         mean_choice)

data_group_means <- data_groups %>%
  group_by(player.signal,
           subsession.game_name
  ) %>%
  summarise(mean = mean(mean_choice))
##############################################
##############################################


####################################
#### FIGURE: ALL BARS ####
####################################
plot_data <-  filter(data_group_means,player.signal=="m" )
f <- ggplot(data=plot_data, aes(x=subsession.game_name, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  # scale_fill_manual(name  ="Signal",
  #                   values=c( "h"="red", "m"="yellow","l"="blue")) +
  ylab("Proposal rate") +
  xlab("Game") +
  theme(axis.title.y = element_text(size=20)) +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.text.x  = element_text(size=20)) +
  theme(axis.text.y  = element_text(size=18)) +
  theme(legend.text = element_text(size = 18),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size=20)) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_line(colour = "grey") # get rid of major grid
    , panel.grid.minor = element_line(colour = "grey") # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
    , legend.key = element_rect(fill = "transparent", color = NA)
  )

ggsave(f, filename = "bars_bel.png",  bg = "transparent", path="/Users/UseNetID/Dropbox/MaIn/paper/figures")

##############################################
##############################################