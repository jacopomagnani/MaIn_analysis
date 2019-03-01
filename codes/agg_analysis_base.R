##############################################
#### ANALYSIS OF MEANS FROM BASE SESSIONS ####
##############################################

####################################
#### PRELIMINARIES ####
####################################
library(here)
library(tidyverse)
library(forcats)
##############################################
##############################################

####################################
#### CREATE GROUP MEANS DATASET ####
####################################
data_all_base <- read_csv(here("data","MaIn_data_base_game.csv"))
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
  mutate(player.signal=factor(player.signal)) %>%
  mutate(player.signal=fct_recode(player.signal,
                                "h"="1",
                                "m"="2",
                                "l"="3"))
data_all_base <- data_all_base %>%
  mutate(subsession.game_name=factor(subsession.game_name))

data_groups <- data_all_base %>%
  group_by(group.id_in_subsession,
           session.code,
           player.type,
           player.signal,
           subsession.game_name
           ) %>%
  summarise(mean_choice = mean(player.choice[round_ind]))

data_group_means <- data_groups %>%
  group_by(player.type,
           player.signal,
           subsession.game_name
  ) %>%
  summarise(mean = mean(mean_choice))
##############################################
##############################################

####################################
#### FIGURE: ALL BARS ####
####################################

scale <- 1.2
games <- c("A","B")
plot_names=c("bars_A.png","bars_B.png")
for(i in c(1,2)){
  game <- games[i]
  plot_name <- plot_names[i]
  plot_data <- filter(data_group_means,subsession.game_name==game )
  f <- ggplot(data=plot_data, aes(x=player.type, y=mean, fill=player.signal)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black") +
    scale_fill_manual(name  ="Signal    ",
                      values=c( "h"="red", "m"="yellow","l"="blue"),
                      labels=c(" h  ", " m  ", " l ")) +
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
  ggsave(f, filename = plot_name,  bg = "transparent", path= here("output"))
}
##############################################
##############################################

#####################################
#### FIGURE: ONLY Hm and Mm BARS ####
#####################################
scale <- 1.2
plot_data <- data_group_means %>% filter(player.type!="L" & player.signal=="m")
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
ggsave(f, filename = "bars_AvsB.png",  bg = "transparent", path = here("output"))

##############################################
##############################################

##############################################################
#### TEST DIFFERENCE IN Mm PROPOSAL RATES BETWEEN A and B ####
##############################################################
test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="M"  & player.type!="L") 
  
diff = test_data$mean_choice[test_data$subsession.game_name=="A"]-test_data$mean_choice[test_data$subsession.game_name=="B"]
test=wilcox.test(diff, alternative = "two.sided")
p=test$p.value
pass=(p<=0.05)
##############################################
##############################################

#######################################################################
#### COMPUTE MEAN POINTS AND PARTNER TYPES FOR Mm REALIZED MATCHES ####
#######################################################################
data_all_base <- data_all_base %>%
  mutate(player.partner_type=factor(player.partner_type)) %>%
  mutate(player.partner_type=fct_recode(player.partner_type,
                                        "H"="1",
                                        "M"="2",
                                        "L"="3"))
data_all_base %>%
  filter(player.type=="M" & player.signal=="m" & player.match==1) %>%
  group_by(subsession.game_name) %>%
  summarise(mean_points=mean(player.points),
            share_H=mean(player.partner_type=="H"),
            share_M=mean(player.partner_type=="M"),
            share_L=mean(player.partner_type=="L")
  )
##############################################
##############################################
