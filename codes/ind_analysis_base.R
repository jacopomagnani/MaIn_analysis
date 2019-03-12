###################################################################
#### ANALYSIS OF INDIVIDUAL CHARACTERISTICS FROM BASE SESSIONS ####
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
#### CREATE DATASET ####
####################################
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
         player.match, player.partner_type, player.type, player.signal,group.id_in_subsession)
data_game <- data_game_raw %>%
  filter(subsession.round_number>20) %>%
  filter(player.type!="L") %>%
  filter(player.signal=="m")
######################
### EDIT MPL DATA ###
data_mpl <-read_csv(here("data","MaIn_data_base_mpl.csv"))
data_mpl <- data_mpl %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.switching_row)
######################
### EDIT CRT DATA ###
data_crt <-read_csv(here("data","MaIn_data_base_crt.csv"))
data_crt <- data_crt %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  select(participant.id_in_treatment, player.num_correct)
######################
### EDIT SURVEY DATA ###
data_survey <-read_csv(here("data","MaIn_data_base_survey.csv"))
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
#### BASELINE REGRESSION ####
####################################
data_reg <- data_treatment %>%
  mutate(subsession.game_A=(subsession.game_name=="A"))
reg_simple_lpm <- glm(formula = player.choice ~
              + (player.type=="H")
            + subsession.game_A
            + (player.type=="H") * subsession.game_A
            ,data=data_reg
            ,family = "gaussian"
)
results_simple_lp<-coeftest(reg_simple_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession)

reg_simple_logit <- glm(formula = player.choice ~
                        + (player.type=="H")
                      + subsession.game_A
                      + (player.type=="H") * subsession.game_A
                      ,data=data_reg
                      ,family = "binomial"
)
results_simple_logit<-coeftest(reg_simple_logit, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession)
stargazer(reg_simple_lpm, 
          reg_simple_logit,
          se = list(results_simple_lp[,"Std. Error"], results_simple_logit[,"Std. Error"]),
          p = list(results_simple_lp[,"Pr(>|z|)"], results_simple_logit[,"Pr(>|z|)"]),
          covariate.labels = c("$H$", "$A$", "$H\\times A$"),
          dep.var.labels   = c("$propose$"),
          model.names = FALSE,
          add.lines = list(c("Regression", "Linear", "Logit"),
                           c("Clustering", "Yes", "Yes")),
          out = here("output/tables","table_reg_BASE_simple.tex"), 
          float=FALSE)

##############################################
##############################################

####################################
#### AUGMENTED REGRESSION ####
####################################
data_reg <- data_treatment %>%
  mutate(player.risk_aversion=(player.risk_aversion-mean(player.risk_aversion))/sd(player.risk_aversion)) %>%
  mutate(player.crt_score=(player.crt_score-mean(player.crt_score))/sd(player.crt_score)) %>%
  mutate(player.female=(player.sex=="Female")) %>%
  mutate(player.nonstem=(player.major=="Science, technology, engineering and mathematics")) %>%
  mutate(subsession.round_number=subsession.round_number-30) %>%
  mutate(subsession.game_A=(subsession.game_name=="A"))
reg_more_lpm <- glm(formula = player.choice ~
              + (player.type=="H")
            + subsession.game_A
            + (player.type=="H") * subsession.game_A
            + (player.type=="H") * subsession.game_A * player.crt_score
            + (player.type=="H") * subsession.game_A * subsession.round_number
            + (player.type=="H") * subsession.game_A * player.risk_aversion
            + (player.type=="H") * subsession.game_A * player.female
            + (player.type=="H") * subsession.game_A * player.nonstem
            ,data=data_reg
            ,family = "gaussian"
)
results_more_lpm<-coeftest(reg_more_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession)
reg_more_logit <- glm(formula = player.choice ~
                      + (player.type=="H")
                    + subsession.game_A
                    + (player.type=="H") * subsession.game_A
                    + (player.type=="H") * subsession.game_A * player.crt_score
                    + (player.type=="H") * subsession.game_A * subsession.round_number
                    + (player.type=="H") * subsession.game_A * player.risk_aversion
                    + (player.type=="H") * subsession.game_A * player.female
                    + (player.type=="H") * subsession.game_A * player.nonstem
                    ,data=data_reg
                    ,family = "binomial"
)
results_more_logit<-coeftest(reg_more_logit, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession)
stargazer(reg_more_lpm, 
          reg_more_logit,
          se = list(results_more_lpm[,"Std. Error"], results_more_logit[,"Std. Error"]),
          p = list(results_more_lpm[,"Pr(>|z|)"], results_more_logit[,"Pr(>|z|)"]),
          covariate.labels = c("$H$", "$A$", "CRT","Round", "Risk Aversion", "Female", "STEM",
                               "$H\\times A$",
                               "$H\\times$ CRT","$A\\times$ CRT",
                               "$H\\times$ Round","$A\\times$ Round",
                               "$H\\times$ Risk Aversion","$A\\times$ Risk Aversion",
                               "$H\\times$ Female","$A\\times$ Female",
                               "$H\\times$ STEM","$A\\times$ STEM",
                               "$H\\times A\\times$ CRT",
                               "$H\\times A\\times$ Round",
                               "$H\\times A\\times$ Risk Aversion",
                               "$H\\times A\\times$ Female",
                               "$H\\times A\\times$ STEM",
                               NULL),
          dep.var.labels   = c("$propose$"),
          model.names = FALSE,
          add.lines = list(c("Regression", "Linear", "Logit"),
                           c("Clustering", "Yes", "Yes")),
          single.row = TRUE,
          out = here("output/tables","table_reg_BASE_big.tex"), 
          float=FALSE)
##############################################
##############################################

####################################
#### BAR PLOTS FOR SUBSAMPLES ####
####################################
data_treatment <- data_treatment %>%
  mutate(subsession.late=(subsession.round_number>=45)) %>%
  mutate(player.high_crt=(player.crt_score >=2)) %>%
  mutate(player.female=(player.sex=="Female")) %>%
  mutate(player.high_ra=(player.risk_aversion >=6)) %>%
  mutate(player.stem_major=(player.major=="Science, technology, engineering and mathematics"))
var_subsample <- c("player.high_crt", "player.female", "player.high_ra", "player.stem_major", "subsession.late")
plot_names1 <- c("bars_AvsB_highCRT.png","bars_AvsB_female.png","bars_AvsB_highRA.png","bars_AvsB_stem.png","bars_AvsB_late.png")
plot_names2 <- c("bars_AvsB_lowCRT.png","bars_AvsB_male.png","bars_AvsB_lowRA.png","bars_AvsB_nonstem.png","bars_AvsB_early.png")
scale <- 1.2
for(i in seq_along(var_subsample)){
  for(k in c(0,1)){
    varname <- var_subsample[i]
    if(k==0){plot_name <- plot_names2[i]} else{plot_name <- plot_names1[i]}
    plot_data <- data_treatment %>% 
      filter(get(varname)==k) %>%
      group_by(player.type,
               subsession.game_name
      ) %>%
      summarise(mean = mean(player.choice))
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
    ggsave(f, filename = plot_name,  bg = "transparent", path=here("output/figures"))
  }
}

##############################################
##############################################