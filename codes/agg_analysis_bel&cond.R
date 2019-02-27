
#########################################################################
#### ANALYSIS OF AGGREGATE PROPOSAL RATES FROM BEL and COND SESSIONS ####
#########################################################################


####################################
#### PRELIMINARIES ####
####################################
rm(list=ls())
library(tidyverse)
library(forcats)
library(latex2exp)

##############################################
##############################################

####################################
#### CREATE GROUP MEANS DATASET ####
####################################
data_raw<-read_csv("/Users/UseNetID/Dropbox/MaIn/data/LINEEX_december2018/2018RJAMA0108_RawData_bel_05122018/AcceptanceCurse_2018-12-05.csv")
min_round <- 20
max_round <- 60
data_bel <- data_raw %>%
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
  mutate(game=factor(subsession.game_name)) %>%
  filter(player.status==0 & player.signal=="m") %>%
  filter(subsession.round_number>=min_round & subsession.round_number<=max_round) %>%
  group_by(game) %>%
  summarise(mean = mean(player.choice))

data_raw<-read_csv("/Users/UseNetID/Dropbox/MaIn/data/LINEEX_december2018/2018RJAMA0109_RawData_comp_05122018/AcceptanceCurse_2018-12-05.csv")
min_round <- 20
max_round <- 60
data_cond <- data_raw %>%
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
  mutate(game=factor(subsession.game_name)) %>%
  filter(player.status==0 & player.signal=="m") %>%
  filter(subsession.round_number>=min_round & subsession.round_number<=max_round) %>%
  group_by(game) %>%
  summarise(mean = mean(player.choice))

data_bel <- data_bel %>%
  mutate(treatment=rep("BEL",length(mean)))
data_cond <- data_cond %>%
  mutate(treatment=rep("COND",length(mean)))
data_plot <- data_bel %>%
  bind_rows(data_cond)

scale=1.2

f<-ggplot(data = data_plot, aes(x=game, 
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
ggsave(f, filename = "Bel_Cond.png",  bg = "transparent", path="/Users/UseNetID/Dropbox/MaIn/paper/figures")

