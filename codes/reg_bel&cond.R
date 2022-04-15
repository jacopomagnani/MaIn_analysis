### ANALYSIS OF INDIVIDUAL PROPOSAL DECISIONS FROM BEL & COND ###

# preliminaries -----------------------------------------------------------

library(here)
library(tidyverse)
library(forcats)
library(sandwich)
library(lmtest)
library(stargazer)
library(latex2exp)




# create BEL data -------------------------------------------------------------

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
min_round=0
data_game <- data_game_raw %>%
  filter(subsession.round_number>=min_round) %>%
  filter(player.status==0 & player.signal=="m")


# BEL regression ----------------------------------------------------------

data_reg <- data_game
reg_bel_lpm <- glm(formula = player.choice ~
                     + subsession.adverse
                   ,data=data_reg
                   ,family = "gaussian"
)
results_bel_lpm<-coeftest(reg_bel_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession,multi0=TRUE)

reg_bel_log <- glm(formula = player.choice ~
                     + subsession.adverse
                   ,data=data_reg
                   ,family = "binomial"
)
results_bel_log<-coeftest(reg_bel_log, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession,multi0=TRUE)


# create COND data --------------------------------------------------------

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

min_round=20
data_game <- data_game_raw %>%
  filter(subsession.round_number>=min_round) %>%
  filter(player.status==0 & player.signal=="m")

# COND regressions --------------------------------------------------------

data_cond <- data_game
reg_cond_lpm <- glm(formula = player.choice ~
                     + subsession.adverse
                   ,data=data_cond
                   ,family = "gaussian"
)
results_cond_lpm<-coeftest(reg_cond_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession,multi0=TRUE)

reg_cond_log <- glm(formula = player.choice ~
                     + subsession.adverse
                   ,data=data_cond
                   ,family = "binomial"
)
results_cond_log<-coeftest(reg_cond_log, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession,multi0=TRUE)


# create table ------------------------------------------------------------

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
                             c("Clustering", "Yes", "Yes","Yes", "Yes")),
          out = here("output/tables","table_reg_B+C.tex"), 
          float=FALSE)
