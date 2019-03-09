
#########################################################################
#### ANALYSIS OF AGGREGATE PROPOSAL RATES FROM BEL and COND SESSIONS ####
#########################################################################


####################################
#### PRELIMINARIES ####
####################################
library(here)
library(tidyverse)
library(forcats)
library(latex2exp)

##############################################
##############################################

#########################################
#### CREATE GROUP AND MEANS DATASETS ####
#########################################
min_round <- 20
max_round <- 60
data_bel <- read_csv(here("data","MaIn_data_bel_game.csv")) %>%
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
#  mutate(game=factor(subsession.game_name)) %>%
  filter(player.status==0 & player.signal=="m") %>%
  filter(subsession.round_number>=min_round & subsession.round_number<=max_round)

data_groups_bel <- data_bel %>%
  group_by(group.id_in_subsession,
           session.code,
           #player.type,
           #player.signal,
           subsession.game_name
  ) %>%
  summarise(mean_choice = mean(player.choice))

data_means_bel <- data_groups_bel %>%
  group_by(#player.type,
           #player.signal,
           subsession.game_name
  ) %>%
  summarise(mean = mean(mean_choice))


data_con <- read_csv(here("data","MaIn_data_cond_game.csv")) %>%
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
  #  mutate(game=factor(subsession.game_name)) %>%
  filter(player.status==0 & player.signal=="m") %>%
  filter(subsession.round_number>=min_round & subsession.round_number<=max_round)

data_groups_cond <- data_bel %>%
  group_by(group.id_in_subsession,
           session.code,
           #player.type,
           #player.signal,
           subsession.game_name
  ) %>%
  summarise(mean_choice = mean(player.choice))

data_means_cond <- data_groups_bel %>%
  group_by(#player.type,
    #player.signal,
    subsession.game_name
  ) %>%
  summarise(mean = mean(mean_choice))



##############################################
##############################################

###################################################
#### FIGURE: Mm PROPOSAL RATES IN BEL AND COND ####
###################################################

data_means_bel <- data_means_bel %>%
  mutate(treatment=rep("BEL",length(mean)))
data_means_cond <- data_means_cond %>%
  mutate(treatment=rep("COND",length(mean)))
data_plot <- data_bel %>%
  bind_rows(data_cond)

scale <- 1.2

f <- ggplot(data = data_plot, aes(x=subsession.game_name, 
                             y=mean, 
                             colour=treatment, 
                             group=treatment, 
                             linetype=treatment,
                             shape=treatment
                            )) +
  geom_point(size=3)+
  geom_line(size=1) +
  scale_color_discrete(name="Treatment") +
  scale_linetype_discrete(name="Treatment") +
  scale_shape_discrete(name="Treatment") +
  theme(legend.text = element_text(size = 16*scale), 
        legend.title = element_text(size = 16*scale)) +
  ylab("Proposal rate") +
  xlab(TeX("$\\gamma$")) +
  theme(axis.title.x = element_text(size=16*scale)) +
  theme(axis.title.y = element_text(size=16*scale)) +
  theme(axis.text.x  = element_text(size=16*scale)) +
  theme(axis.text.y  = element_text(size=16*scale)) +
  ylim(0,1) +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_line(colour = "grey") # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
    , legend.key = element_rect(fill = "transparent", color = NA)
    , legend.key.size = unit(1, "cm")
    , legend.position = "bottom"
  ) +
  scale_x_discrete(breaks=c("A", "B", "C","D","E"),
                     labels=c("1", "0.75", "0.5","0.25","0"))+
  geom_hline(aes(yintercept=0.66), colour="black", linetype="dashed")
ggsave(f, filename = "Bel_Cond.png",  bg = "transparent", path=here("output/figures"))

##############################################
##############################################

###################################################################
#### TEST DIFFERENCE IN Mm PROPOSAL RATES BETWEEN BEL and COND ####
###################################################################

#diff = test_data$mean_choice[test_data$subsession.game_name=="B"]-test_data$mean_choice[test_data$subsession.game_name=="A"]
#test=wilcox.test(diff, alternative = "two.sided")
p=test$p.value
pass=(p<=0.05)
##############################################
##############################################
