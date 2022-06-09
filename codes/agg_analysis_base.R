### ANALYSIS OF MEANS FROM BASE SESSIONS ###

# preliminaries -----------------------------------------------------------

library(here)
library(tidyverse)
library(forcats)

# create group and means datasets -----------------------------------------

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

data_means <- data_groups %>%
  group_by(player.type,
           player.signal,
           subsession.game_name
  ) %>%
  summarise(mean = mean(mean_choice), sd = sd(mean_choice))

# figure: all bars --------------------------------------------------------

scale <- 1.2
games <- c("A","B")
plot_names=c("bars_A.png","bars_B.png")
for(i in c(1,2)){
  game <- games[i]
  plot_name <- plot_names[i]
  plot_data <- filter(data_means,subsession.game_name==game )
  f <- ggplot(data=plot_data, aes(x=player.type, y=mean, fill=player.signal)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black") +
    geom_errorbar(aes(ymin=mean-2*sd, ymax=mean+2*sd), width=.2,
                  position=position_dodge(0.9)) +
    scale_fill_manual(name  ="Signal    ",
                      values=c( "h"="red", "m"="yellow","l"="blue"),
                      labels=c(" h  ", " m  ", " l ")) +
    ylab("Proposal rate") +
    xlab("Quality") +
    ylim(-0.25,1.25) +
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
  ggsave(f, filename = plot_name,  bg = "transparent", path= here("output/figures"))
}



# compute expected gains from proposing -----------------------------------
source(here("codes","exp_match_pay.R"))
source(here("codes","prob_accept.R"))
D=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
M=matrix(rep(c(160,80,40),3),nrow = 3,ncol = 3,byrow = TRUE)
sigma_A=data_means %>% filter(subsession.game_name=="A") %>% pull(mean)
S_A=matrix(sigma_A,nrow = 3,ncol = 3,byrow = TRUE)
R_A=matrix(rep(c(100,75,25),3),nrow = 3,ncol = 3,byrow = FALSE)
sigma_B=data_means %>% filter(subsession.game_name=="B") %>% pull(mean)
S_B=matrix(sigma_B,nrow = 3,ncol = 3,byrow = TRUE)
R_B=matrix(rep(c(80,75,25),3),nrow = 3,ncol = 3,byrow = FALSE)
delta_A  <-  prob_accept(S_A,D) * (exp_match_pay(S_A,D,M,0) - R_A)
delta_B  <-  prob_accept(S_B,D) * (exp_match_pay(S_B,D,M,0) - R_B)


# figure: only Mm and Hm bars ---------------------------------------------

scale <- 1.2
plot_data <- data_means %>% filter(player.type!="L" & player.signal=="m")
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
ggsave(f, filename = "bars_AvsB.png",  bg = "transparent", path = here("output/figures"))


# test if proposal rates are > or < than 50% ------------------------------
#dominant strategies

#HhA
test_data <- data_groups %>%
  filter(player.signal=="h" & player.type=="H" & subsession.game_name=="A") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)
#HhB
test_data <- data_groups %>%
  filter(player.signal=="h" & player.type=="H" & subsession.game_name=="B") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)

#MhA
test_data <- data_groups %>%
  filter(player.signal=="h" & player.type=="M" & subsession.game_name=="A") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)
#MhB
test_data <- data_groups %>%
  filter(player.signal=="h" & player.type=="M" & subsession.game_name=="B") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)

#LhA
test_data <- data_groups %>%
  filter(player.signal=="h" & player.type=="L" & subsession.game_name=="A") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)
#LhB
test_data <- data_groups %>%
  filter(player.signal=="h" & player.type=="L" & subsession.game_name=="B") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)

#LmA
test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="L" & subsession.game_name=="A") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)
#LmB
test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="L" & subsession.game_name=="B") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)

#LlA
test_data <- data_groups %>%
  filter(player.signal=="l" & player.type=="L" & subsession.game_name=="A") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)
#LlB
test_data <- data_groups %>%
  filter(player.signal=="l" & player.type=="L" & subsession.game_name=="B") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)

#dominated strategies
#HlA
test_data <- data_groups %>%
  filter(player.signal=="l" & player.type=="H" & subsession.game_name=="A") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "less" )
p=test$p.value
reject=(p<=0.05)
#HlB
test_data <- data_groups %>%
  filter(player.signal=="l" & player.type=="H" & subsession.game_name=="B") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "less" )
p=test$p.value
reject=(p<=0.05)

#MlA
test_data <- data_groups %>%
  filter(player.signal=="l" & player.type=="M" & subsession.game_name=="A") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "less" )
p=test$p.value
reject=(p<=0.05)
#MlB
test_data <- data_groups %>%
  filter(player.signal=="l" & player.type=="M" & subsession.game_name=="B") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "less" )
p=test$p.value
reject=(p<=0.05)

#other strategies
#HmA
test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="H" & subsession.game_name=="A") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "less" )
p=test$p.value
reject=(p<=0.05)

#HmA different from 0?
test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="H" & subsession.game_name=="A") 
diff = test_data$mean_choice-0
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)

#HmB
test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="H" & subsession.game_name=="B") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)

#MmA=0.5 or greater?
test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="M" & subsession.game_name=="A") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)

#MmB
test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="M" & subsession.game_name=="B") 
diff = test_data$mean_choice-0.5
test=wilcox.test(diff, alternative = "greater" )
p=test$p.value
reject=(p<=0.05)

# TEST DIFFERENCE IN Mm PROPOSAL RATES BETWEEN A and B --------------------

test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="M"  & player.type!="L") 
diff = test_data$mean_choice[test_data$subsession.game_name=="B"]-test_data$mean_choice[test_data$subsession.game_name=="A"]
test=wilcox.test(diff, alternative = "greater")
p=test$p.value
reject=(p<=0.05)

test_data <- data_groups %>%
  filter(player.signal=="m" & player.type=="H"  & player.type!="L") 
diff = test_data$mean_choice[test_data$subsession.game_name=="B"]-test_data$mean_choice[test_data$subsession.game_name=="A"]
test=wilcox.test(diff, alternative = "greater")
p=test$p.value
reject=(p<=0.05)


# dynamics ----------------------------------------------------------------


data_all_base <- read_csv(here("data","MaIn_data_base_game.csv"))
min_round <- 0
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

means <- data_all_base %>%
  filter(player.type=="M", player.signal=="m", subsession.game_name=="A") %>%
  group_by(subsession.round_number) %>%
  summarise(mean_choice = mean(player.choice))

means %>% mutate(late=(subsession.round_number>58)) %>%
  group_by(late) %>%
  summarise(block_mean=mean(mean_choice))
