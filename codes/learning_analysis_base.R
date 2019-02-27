############################################################
#### ANALYSIS OF LEARNING BEHAVIOR FROM BASE SESSIONS ####
############################################################

####################################
#### PRELIMINARIES ####
####################################
rm(list=ls())
library(tidyverse)
library(forcats)
##############################################
##############################################


####################################
#### CREATE DATASET ####
####################################

####### ISSUE !!!! THE THIRD DATASET ALREADY CONTAINS THE PREVIOUS TWO !!! #############


data_treatment <- c()
num_sessions = 3
list_session_number=c("01", "04", "07")
list_session_day=c("03", "04", "04")
root = "/Users/UseNetID/Dropbox/MaIn/data/LINEEX_december2018/2018RJAMA01"
i=3
session_number=list_session_number[i]
session_day=list_session_day[i]
path_data_game = paste(root,
                       session_number,
                       "_RawData_base_",
                       session_day,
                       "122018/AcceptanceCurse_2018-12-",
                       session_day,
                       ".csv",
                       sep="")
path_data_mpl = paste(root,
                       session_number,
                       "_RawData_base_",
                       session_day,
                       "122018/mpl_2018-12-",
                       session_day,
                       ".csv",
                       sep="")
path_data_crt = paste(root,
                      session_number,
                      "_RawData_base_",
                      session_day,
                      "122018/crt_2018-12-",
                      session_day,
                      ".csv",
                      sep="")
path_data_survey = paste(root,
                      session_number,
                      "_RawData_base_",
                      session_day,
                      "122018/survey_2018-12-",
                      session_day,
                      ".csv",
                      sep="")
### EDIT GAME DATA ###
data_game<-read_csv(path_data_game)
data_game <- data_game %>%
  mutate(player.type=factor(player.type)) %>%
  mutate(player.type=fct_recode(player.type,
                                        "H"="1",
                                        "M"="2",
                                        "L"="3")) %>%
  mutate(player.signal=factor(player.signal)) %>%
  mutate(player.signal=fct_recode(player.signal,
                                "h"="1",
                                "m"="2",
                                "l"="3")) %>%
  mutate(player.partner_type=factor(player.partner_type)) %>%
  mutate(player.partner_type=fct_recode(player.partner_type,
                                "H"="1",
                                "M"="2",
                                "L"="3")) %>%
  mutate(subsession.game_name=factor(subsession.game_name))  %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  mutate(session.code=factor(session.code)) %>%
  select(session.code, participant.id_in_treatment, player.choice,subsession.round_number,subsession.game_name,
         player.match, player.partner_type, player.type, player.signal) 
  

#add variables about past types
data_game <- data_game %>%
  group_by(participant.id_in_treatment) %>%
  mutate(matched_H_AMm=cumsum(player.match & player.partner_type=="H" & subsession.game_name=="A" & player.type=="M" & player.signal=="m")-
           (player.match & player.partner_type=="H" & subsession.game_name=="A" & player.type=="M" & player.signal=="m") ) %>%
  mutate(matched_M_AMm=cumsum(player.match & player.partner_type=="M" & subsession.game_name=="A" & player.type=="M" & player.signal=="m")-
           (player.match & player.partner_type=="M" & subsession.game_name=="A" & player.type=="M" & player.signal=="m") ) %>%
  mutate(matched_L_AMm=cumsum(player.match & player.partner_type=="L" & subsession.game_name=="A" & player.type=="M" & player.signal=="m")-
           (player.match & player.partner_type=="L" & subsession.game_name=="A" & player.type=="M" & player.signal=="m") ) %>%
  mutate(matched_H_BMm=cumsum(player.match & player.partner_type=="H" & subsession.game_name=="B" & player.type=="M" & player.signal=="m")-
           (player.match & player.partner_type=="H" & subsession.game_name=="B" & player.type=="M" & player.signal=="m") ) %>%
  mutate(matched_M_BMm=cumsum(player.match & player.partner_type=="M" & subsession.game_name=="B" & player.type=="M" & player.signal=="m")-
           (player.match & player.partner_type=="M" & subsession.game_name=="B" & player.type=="M" & player.signal=="m") ) %>%
  mutate(matched_L_BMm=cumsum(player.match & player.partner_type=="L" & subsession.game_name=="B" & player.type=="M" & player.signal=="m")-
           (player.match & player.partner_type=="L" & subsession.game_name=="B" & player.type=="M" & player.signal=="m") ) %>%
  mutate(observed_H_m=cumsum(player.partner_type=="H"& player.signal=="m")-(player.partner_type=="H"& player.signal=="m")) %>%
  mutate(observed_M_m=cumsum(player.partner_type=="M"& player.signal=="m")-(player.partner_type=="M"& player.signal=="m")) %>%
  mutate(observed_L_m=cumsum(player.partner_type=="L"& player.signal=="m")-(player.partner_type=="L"& player.signal=="m"))

#keep only Mm observations
data_game <- data_game %>%
  filter(player.type=="M") %>%
  filter(player.signal=="m")

######################
### EDIT MPL DATA ###
data_mpl<-read_csv(path_data_mpl)
data_mpl <- data_mpl %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.switching_row)
######################
### EDIT CRT DATA ###
data_crt<-read_csv(path_data_crt)
data_crt <- data_crt %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.num_correct)
######################
### EDIT SURVEY DATA ###
data_survey<-read_csv(path_data_survey)
data_survey <- data_survey %>%
  mutate(player.sex=factor(player.sex)) %>%
  mutate(player.major=factor(player.major)) %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment,player.sex,player.major)
######################
### MERGE DATA ACROSS APPS ###
data_treatment <- data_game %>%
  left_join(data_mpl, by="participant.id_in_treatment") %>%
  left_join(data_crt, by="participant.id_in_treatment") %>%
  left_join(data_survey, by="participant.id_in_treatment") %>%
  rename(player.risk_aversion = "player.switching_row") %>%
  rename(player.crt_score = "player.num_correct") %>%
  rename(subsession.game = "subsession.game_name")

rm(list=c("data_game", "data_mpl", "data_crt", "data_survey"))
##############################

##############################################
##############################################
  
  
  


####################################
#### WITHIN REGRESSION ####
####################################

data_reg <- data_treatment %>%
  filter(subsession.round_number>0 & subsession.round_number<60) %>%
  mutate(subsession.late=(subsession.round_number>30)) %>%
  mutate(player.stem=(player.major=="Science, technology, engineering and mathematics")) %>%
  mutate(matched_H_Mm=matched_H_AMm+matched_H_BMm) %>%
  mutate(matched_M_Mm=matched_M_AMm+matched_M_BMm) %>%
  mutate(matched_L_Mm=matched_L_AMm+matched_L_BMm)
  

summary(glm(formula = player.choice ~ 
              + subsession.round_number
              # + matched_H_Mm #* player.crt_score
              # + matched_M_Mm #* player.crt_score
              # + matched_L_Mm #* player.crt_score
              + matched_H_AMm * subsession.game
              + matched_M_AMm * subsession.game
              + matched_L_AMm * subsession.game
              + matched_H_BMm * subsession.game
              + matched_M_BMm * subsession.game
              + matched_L_BMm * subsession.game
              + observed_H_m #* player.crt_score
              # + observed_M_m #* subsession.game
              # + observed_L_m #* subsession.game
              + participant.id_in_treatment
            ,data=data_reg
            ,family = "gaussian"
  )
)




##############################################
##############################################


