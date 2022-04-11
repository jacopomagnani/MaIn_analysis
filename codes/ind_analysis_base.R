#### ANALYSIS OF INDIVIDUAL BEHAVIOR FROM BASE SESSIONS ####


# preliminaries -----------------------------------------------------------

library(here)
library(tidyverse)
library(forcats)
library(sandwich)
library(lmtest)
library(stargazer)
library(latex2exp)


# EDIT GAME DATA  ---------------------------------------------------------

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
  mutate(group.id_in_treatment=interaction(factor(session.code),factor(group.id_in_subsession))) %>%
  mutate(session.code=factor(session.code)) %>%
  select(session.code, participant.id_in_treatment, player.choice,subsession.round_number,subsession.game_name,
         player.match, player.partner_type, player.type, player.signal,group.id_in_treatment)
data_game <- data_game_raw %>%
  filter(subsession.round_number>20) 

# BASELINE REGRESSION -----------------------------------------------------

data_reg <- data_game %>%
  filter(player.type!="L") %>%
  filter(player.signal=="m") %>%
  mutate(subsession.game_A=(subsession.game_name=="A"))

reg_simple_lpm <- glm(formula = player.choice ~
              + (player.type=="H")
            + subsession.game_A
            + (player.type=="H") * subsession.game_A
            ,data=data_reg
            ,family = "gaussian"
)
results_simple_lp<-coeftest(reg_simple_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_treatment, multi0=TRUE)
coeftest(reg_simple_lpm, vcov = vcovCL, cluster = ~ group.id_in_treatment, multi0=TRUE)
coeftest(reg_simple_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment, multi0=TRUE)


reg_simple_logit <- glm(formula = player.choice ~
                        + (player.type=="H")
                      + subsession.game_A
                      + (player.type=="H") * subsession.game_A
                      ,data=data_reg
                      ,family = "binomial"
)
results_simple_logit<-coeftest(reg_simple_logit, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_treatment, multi0=TRUE)
coeftest(reg_simple_logit, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_treatment, multi0=TRUE)
coeftest(reg_simple_logit, vcov = vcovCL, cluster = ~ participant.id_in_treatment, multi0=TRUE)
coeftest(reg_simple_logit, vcov = vcovCL, cluster = ~group.id_in_treatment, multi0=TRUE)

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






# INDIVIDUAL CHANGES IN PROPOSAL RATES -------------------------------------------
#create data
data_frequencies <- data_game_raw %>%
  group_by(participant.id_in_treatment, player.type, player.signal, subsession.game_name) %>%
  summarise(frequency = mean(player.choice))
data_frequencies_s <- data_frequencies %>%
  unite(case,player.type,player.signal,subsession.game_name, sep="") %>%
  spread(case,frequency)
#parameters for plotting distributions on two panels
bin_width=0.1
minbin=-1.05
maxbin=+1.05
ceiling=0.45
scale=1.5
#plot with Mm change distribution
f <- ggplot(data_frequencies_s, aes(x=-MmB+MmA)) +
  geom_histogram(aes(y =(..count..)/48),
                 color="black",
                 fill="grey",
                 breaks=seq(minbin, maxbin, by=bin_width)) +
  ylim(c(0,ceiling)) +
  ylab("Frequency") +
  xlab("Change in proposal rates (A-B)") +
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
ggsave(f, filename = "hist_Mm_change.png",  bg = "transparent", path= here("output/figures"))

#plot with Hm change distribution
f <- ggplot(data_frequencies_s, aes(x=-HmB+HmA)) +
  geom_histogram(aes(y =(..count..)/48),
                 color="black",
                 fill="grey",
                 breaks=seq(minbin, maxbin, by=bin_width)) +
  ylim(c(0,ceiling))+
  ylab("Frequency") +
  xlab("Change in proposal rates (A-B)") +
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
ggsave(f, filename = "hist_Hm_change.png",  bg = "transparent", path= here("output/figures"))


#hypothesis test of difference in distributions
test_data <- data_frequencies_s %>%
  mutate(change_H=-HmB+HmA, change_M = -MmB+MmA)
diff = test_data$change_H-test_data$change_M
test=wilcox.test(diff, alternative = "less")
p=test$p.value
reject_null=(p<=0.05)
#hypothesis test: change=0 for M types
test_data <- data_frequencies_s %>%
  mutate(change_H=-HmB+HmA, change_M = -MmB+MmA)
diff = test_data$change_M-0
test=wilcox.test(diff, alternative = "two.sided")
p=test$p.value
reject_null=(p<=0.05)
#hypothesis test: change=0 for H types
test_data <- data_frequencies_s %>%
  mutate(change_H=-HmB+HmA, change_M = -MmB+MmA)
diff = test_data$change_H-0
test=wilcox.test(diff, alternative = "less")
p=test$p.value
reject_null=(p<=0.05)


