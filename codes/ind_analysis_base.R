####################################
#### PRELIMINARIES ####
####################################
library(here)
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
data_game_raw<-read_csv(here("data","MaIn_data_base_game.csv"))
data_game_raw <- data_game_raw %>%
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
data_game <- data_game_raw %>%
  filter(subsession.round_number>20) %>%
  filter(player.type!="L") %>%
  filter(player.signal=="m")
######################
### EDIT MPL DATA ###
data_mpl<-read_csv(here("data","MaIn_data_base_mpl.csv"))
data_mpl <- data_mpl %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.switching_row)
######################
### EDIT CRT DATA ###
data_crt<-read_csv(here("data","MaIn_data_base_crt.csv"))
data_crt <- data_crt %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.num_correct)
######################
### EDIT SURVEY DATA ###
data_survey<-read_csv(here("data","MaIn_data_base_survey.csv"))
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
  rename(player.crt_score = "player.num_correct")
rm(list=c("data_game", "data_mpl", "data_crt", "data_survey"))
##############################
##############################################
##############################################


####################################
#### BETWEEN REGRESSION ####
####################################
data_reg <- data_treatment
reg1 <- glm(formula = player.choice ~
              + (player.type=="H")
            + subsession.game_name
            + (player.type=="H") * subsession.game_name
            + (player.type=="H") * subsession.game_name * player.crt_score
            + (player.type=="H") * subsession.game_name * subsession.round_number
            #+ player.risk_aversion
            #+ player.crt_score
            #+ player.sex
            #+ player.major
            #+ subsession.round_number
            ,data=data_reg
            ,family = "gaussian"
)
summary(reg1)
##############################################
##############################################

####################################
#### BAR PLOTS ####
####################################
plot_data <- data_treatment %>%
  #filter(player.crt_score >=2) %>%
  filter(subsession.round_number >=45) %>%
  #filter(player.sex=="Female") %>%
  group_by(player.type,
           subsession.game_name
  ) %>%
  summarise(mean = mean(player.choice))
scale=1.2
f <- ggplot(data=plot_data, aes(x=player.type, y=mean, fill=subsession.game_name)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black", width = 0.5) +
  scale_fill_manual(name  ="Game    ",
                    values=c( "A"="white", "B"="grey"),
                    labels=c(" A  ", " B  ")) +
  ylab("Proposal rate") +
  xlab("Quality") +
  ylim(0,1) +
  theme(axis.title.y = element_text(size=16*scale)) +
  theme(axis.title.x = element_text(size=16*scale)) +
  theme(axis.text.x  = element_text(size=16*scale)) +
  theme(axis.text.y  = element_text(size=16*scale)) +
  theme(legend.text = element_text(size = 16*scale),
        legend.title = element_text(size=16*scale)) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_line(colour = "grey") # get rid of major grid
    , panel.grid.minor = element_line(colour = "grey") # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
    , legend.key = element_rect(fill = "transparent", color = NA)
    , legend.position = "bottom"
    , legend.margin = margin(20,0.5,0.5,0.5)
  )
#ggsave(f, filename = "bars_AvsB_highCRT.png",  bg = "transparent", path="/Users/UseNetID/Dropbox/MaIn/paper/figures")
ggsave(f, filename = "bars_AvsB_learnt.png",  bg = "transparent", path="/Users/UseNetID/Dropbox/MaIn/paper/figures")