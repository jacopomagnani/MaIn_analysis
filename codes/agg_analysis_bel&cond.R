
### ANALYSIS OF AGGREGATE PROPOSAL RATES FROM BEL and COND SESSIONS ###


# preliminaries -----------------------------------------------------------

library(here)
library(tidyverse)
library(forcats)
library(latex2exp)



# CREATE GROUP AND MEANS DATASETS -----------------------------------------


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
  mutate(subsession.p= 1.00 * (subsession.game_name=="A") + 
           0.75 * (subsession.game_name=="B") + 
           0.50 * (subsession.game_name=="C") + 
           0.25 * (subsession.game_name=="D") + 
           0 * (subsession.game_name=="E")) %>%
  mutate(subsession.adverse = 1 - subsession.p) %>%
  filter(player.status==0 & player.signal=="m") %>%
  filter(subsession.round_number>=min_round & subsession.round_number<=max_round)

data_groups_bel <- data_bel %>%
  group_by(group.id_in_subsession,
           session.code,
           subsession.game_name
  ) %>%
  summarise(mean_choice = mean(player.choice), adverse = mean(subsession.adverse))

data_means_bel <- data_groups_bel %>%
  group_by(subsession.game_name) %>%
  summarise(mean = mean(mean_choice), adverse = mean(adverse), sd = sd(mean_choice))


data_cond <- read_csv(here("data","MaIn_data_cond_game.csv")) %>%
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
  mutate(subsession.p= 1.00 * (subsession.game_name=="A") + 
           0.75 * (subsession.game_name=="B") + 
           0.50 * (subsession.game_name=="C") + 
           0.25 * (subsession.game_name=="D") + 
           0 * (subsession.game_name=="E")) %>%
  mutate(subsession.adverse = 1 - subsession.p) %>%
  filter(player.status==0 & player.signal=="m") %>%
  filter(subsession.round_number>=min_round & subsession.round_number<=max_round)

data_groups_cond <- data_cond %>%
  group_by(group.id_in_subsession,
           session.code,
           subsession.game_name
  ) %>%
  summarise(mean_choice = mean(player.choice), adverse = mean(subsession.adverse))

data_means_cond <- data_groups_cond %>%
  group_by(subsession.game_name) %>%
  summarise(mean = mean(mean_choice), adverse = mean(adverse), sd = sd(mean_choice))



# FIGURE: Mm PROPOSAL RATES IN BEL AND COND -------------------------------


data_means_bel <- data_means_bel %>%
  mutate(treatment=rep("BEL",length(mean)))
data_means_cond <- data_means_cond %>%
  mutate(treatment=rep("COND",length(mean)))
data_plot <- data_means_bel %>%
  bind_rows(data_means_cond)

scale <- 1.2

f <- ggplot(data = data_plot, aes(x=adverse, 
                             y=mean, 
                             fill=treatment,
                             group=treatment
                            )) +
  geom_bar(stat="identity", color="black", width=0.2,
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1,
                position=position_dodge(0.2)) +
  scale_fill_manual(name="Treatment",
                     values=c( "BEL"="grey", "COND"="white")) +
  scale_x_continuous(name =TeX("Adverse selection $(1-p)$"), 
                   breaks=c(0,0.25,0.5,0.75,1)) +
  theme(legend.text = element_text(size = 16*scale), 
        legend.title = element_text(size = 16*scale)) +
  ylab("Proposal rate") +
  xlab(TeX("Adverse selection $(1-p)$")) +
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
  )
ggsave(f, filename = "Bel_Cond.png",  bg = "transparent", path=here("output/figures"))



# TEST DIFFERENCE IN Mm PROPOSAL RATES BETWEEN BEL and COND ---------------

game_set=c("A", "B", "C", "D", "E")
p=rep(0,length(game_set))
diff=rep(0,length(game_set))
i <- 0
for(game in game_set){
  groupmeans_bel <- data_groups_bel %>% filter(subsession.game_name==game) %>% pull(mean_choice)
  groupmeans_cond <- data_groups_cond %>% filter(subsession.game_name==game) %>% pull(mean_choice)
  test=wilcox.test(groupmeans_bel, groupmeans_cond, alternative = "greater")
  i <- i+1
  p[i]=test$p.value
  diff[i]=mean(groupmeans_bel)-mean(groupmeans_cond)
}
diff
p

