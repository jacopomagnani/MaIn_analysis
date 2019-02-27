

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
data_firstsession<-read_csv("/Users/UseNetID/Dropbox/MaIn/data/LINEEX_december2018/2018RJAMA0101_RawData_base_03122018/AcceptanceCurse_2018-12-03.csv")
data_secondsession<-read_csv("/Users/UseNetID/Dropbox/MaIn/data/LINEEX_december2018/2018RJAMA0104_RawData_base_04122018/AcceptanceCurse_2018-12-04.csv")
data_thirdsession<-read_csv("/Users/UseNetID/Dropbox/MaIn/data/LINEEX_december2018/2018RJAMA0107_RawData_base_04122018/AcceptanceCurse_2018-12-04.csv")
data_all_base <- data_firstsession %>%
  bind_rows(data_secondsession) %>%
  bind_rows(data_thirdsession)
data_all_base <- data_thirdsession
rm(data_firstsession)
rm(data_secondsession)
rm(data_thirdsession)
min_round <- 20
max_round <- 60
data_all_base <- mutate(data_all_base,round_ind = subsession.round_number>=min_round & subsession.round_number<=max_round)
data_all_base <- data_all_base %>%
  mutate(player.type=factor(player.type)) %>%
  mutate(player.type=fct_recode(player.type,
                                "H"="1",
                                "M"="2",
                                "L"="3"))
data_all_base <- data_all_base %>%
  mutate(player.partner_type=factor(player.partner_type)) %>%
  mutate(player.partner_type=fct_recode(player.partner_type,
                                "H"="1",
                                "M"="2",
                                "L"="3"))
data_all_base <- data_all_base %>%
  mutate(player.signal=factor(player.signal)) %>%
  mutate(player.signal=fct_recode(player.signal,
                                  "h"="1",
                                  "m"="2",
                                  "l"="3"))
data_all_base <- data_all_base %>%
  mutate(subsession.game_name=factor(subsession.game_name))


data_all_base %>%
  filter(player.type=="M" & player.signal=="m" & player.match==1) %>%
  filter(subsession.round_number>=30) %>%
  group_by(subsession.game_name) %>%
  summarise(mean_points=mean(player.points),
            share_H=mean(player.partner_type=="H"),
            share_M=mean(player.partner_type=="M"),
            share_L=mean(player.partner_type=="L")
            )