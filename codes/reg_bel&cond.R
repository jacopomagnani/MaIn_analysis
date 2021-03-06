###################################################################
#### ANALYSIS OF INDIVIDUAL PROPOSAL DECISIONS FROM BEL & COND ####
###################################################################

####################################
#### PRELIMINARIES ####
####################################
library(here)
library(tidyverse)
library(forcats)
library(sandwich)
library(lmtest)
library(stargazer)
library(latex2exp)
##############################################
##############################################



####################################
#### CREATE BEL DATASET ####
####################################
### EDIT GAME DATA ###
data_game_raw<-read_csv(here("data","MaIn_data_bel_game.csv"))
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
  mutate(subsession.p= 1.00 * (subsession.game_name=="A") + 
           0.75 * (subsession.game_name=="B") + 
           0.50 * (subsession.game_name=="C") + 
           0.25 * (subsession.game_name=="D") + 
           0 * (subsession.game_name=="E")) %>%
  mutate(subsession.adverse = 1 - subsession.p) %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  mutate(session.code=factor(session.code)) %>%
  select(session.code, participant.id_in_treatment, player.choice,subsession.round_number,subsession.game_name,
         player.match, player.partner_type, player.type, player.signal,group.id_in_subsession, player.status,
         subsession.p, subsession.adverse)
data_game <- data_game_raw %>%
  filter(subsession.round_number>20) %>%
  filter(player.status==0 & player.signal=="m")
######################
### EDIT MPL DATA ###
data_mpl <-read_csv(here("data","MaIn_data_bel_mpl.csv"))
data_mpl <- data_mpl %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.switching_row)
######################
### EDIT CRT DATA ###
data_crt <-read_csv(here("data","MaIn_data_bel_crt.csv"))
data_crt <- data_crt %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.num_correct)
######################
### EDIT SURVEY DATA ###
data_survey <-read_csv(here("data","MaIn_data_bel_survey.csv"))
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
#### BEL REGRESSION ####
####################################
data_reg <- data_treatment
reg_bel_lpm <- glm(formula = player.choice ~
                        + subsession.adverse
                      + player.crt_score 
                      + player.sex
                      +  player.risk_aversion
                      +  player.major
                      +  subsession.round_number
                      ,data=data_reg
                      ,family = "gaussian"
)
results_bel_lpm<-coeftest(reg_bel_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession)

reg_bel_log <- glm(formula = player.choice ~
                     + subsession.adverse
                   + player.crt_score 
                   + player.sex
                   +  player.risk_aversion
                   +  player.major
                   +  subsession.round_number
                   ,data=data_reg
                   ,family = "binomial"
)
results_bel_log<-coeftest(reg_bel_log, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession)
##############################################
##############################################

##################################################################################################
##################################################################################################

####################################
#### CREATE COND DATASET ####
####################################
### EDIT GAME DATA ###
data_game_raw<-read_csv(here("data","MaIn_data_cond_game.csv"))
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
  mutate(subsession.p= 1.00 * (subsession.game_name=="A") + 
           0.75 * (subsession.game_name=="B") + 
           0.50 * (subsession.game_name=="C") + 
           0.25 * (subsession.game_name=="D") + 
           0 * (subsession.game_name=="E")) %>%
  mutate(subsession.adverse = 1 - subsession.p) %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  mutate(session.code=factor(session.code)) %>%
  select(session.code, participant.id_in_treatment, player.choice,subsession.round_number,subsession.game_name,
         player.match, player.partner_type, player.type, player.signal,group.id_in_subsession, player.status,
         subsession.p, subsession.adverse)
data_game <- data_game_raw %>%
  filter(subsession.round_number>20) %>%
  filter(player.status==0 & player.signal=="m")
######################
### EDIT MPL DATA ###
data_mpl <-read_csv(here("data","MaIn_data_cond_mpl.csv"))
data_mpl <- data_mpl %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.switching_row)
######################
### EDIT CRT DATA ###
data_crt <-read_csv(here("data","MaIn_data_cond_crt.csv"))
data_crt <- data_crt %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.num_correct)
######################
### EDIT SURVEY DATA ###
data_survey <-read_csv(here("data","MaIn_data_cond_survey.csv"))
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
#### COND REGRESSION ####
####################################
data_cond <- data_treatment
reg_cond_lpm <- glm(formula = player.choice ~
                     + subsession.adverse
                   + player.crt_score 
                   + player.sex
                   +  player.risk_aversion
                   +  player.major
                   +  subsession.round_number
                   ,data=data_cond
                   ,family = "gaussian"
)
results_cond_lpm<-coeftest(reg_cond_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession)

reg_cond_log <- glm(formula = player.choice ~
                     + subsession.adverse
                   + player.crt_score 
                   + player.sex
                   +  player.risk_aversion
                   +  player.major
                   +  subsession.round_number
                   ,data=data_cond
                   ,family = "binomial"
)
results_cond_log<-coeftest(reg_cond_log, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession)
##############################################
##############################################

##################################################################################################
##################################################################################################

####################################
#### CREATE TABLE ####
####################################
stargazer(reg_bel_lpm, 
          reg_bel_log,
          reg_cond_lpm, 
          reg_cond_log,
          se = list(results_bel_lpm[,"Std. Error"],
                    results_bel_log[,"Std. Error"],
                    results_cond_lpm[,"Std. Error"],
                    results_cond_log[,"Std. Error"]),
          p = list(results_bel_lpm[,"Pr(>|z|)"],
                   results_bel_log[,"Pr(>|z|)"],
                   results_cond_lpm[,"Pr(>|z|)"],
                   results_cond_log[,"Pr(>|z|)"]),
          covariate.labels = c("Adverse selection $(1-p)$", NULL, NULL, NULL, NULL, NULL),
          dep.var.labels   = c("$propose$"),
          #column.labels = c("Linear", "Logit"),
          column.labels   = c("BEL", "COND"),
          column.separate = c(2, 2),
          model.names = FALSE,
          #model.numbers = FALSE,
          omit = c("player.crt_score","player.sex","player.risk_aversion","player.major","subsession.round_number"),
          add.lines = list(c("Regression", "Linear", "Logit","Linear", "Logit"),
                             c("Individual Characteristics", "Yes", "Yes","Yes", "Yes"),
                             c("Clustering", "Yes", "Yes","Yes", "Yes")),
          out = here("output/tables","table_reg_B+C.tex"), 
          float=FALSE)
##############################################
##############################################
