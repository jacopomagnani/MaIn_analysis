
# preliminaries -----------------------------------------------------------

library(here)
library(tidyverse)
library(forcats)
library(sandwich)
library(lmtest)

# create dataset ----------------------------------------------------------

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


# create individual frequencies -------------------------------------------

data_frequencies <- data_game_raw %>%
  group_by(participant.id_in_treatment, player.type, player.signal, subsession.game_name) %>%
  summarise(frequency = mean(player.choice))

data_frequencies_s <- data_frequencies %>%
  unite(case,player.type,player.signal,subsession.game_name, sep="") %>%
  spread(case,frequency)


# distribution of proposal frequencies -------------------------

plot_data <- data_frequencies %>% filter(player.type=="M", player.signal=="m")

ggplot(plot_data, aes(x=frequency, color=subsession.game_name)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")


plot_data <- data_frequencies %>% filter(player.type=="H", player.signal=="m")

ggplot(plot_data, aes(x=frequency, color=subsession.game_name)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")


# distribution of changes in proposal frequency ---------------------------

#parameters for plotting distributions on two panels
bin_width=0.1
minbin=-1.05
maxbin=+1.05
ceiling=0.45
scale=1
#plot with Mm change distribution
ggplot(data_frequencies_s, aes(x=-MmB+MmA)) +
  geom_histogram(aes(y =(..count..)/48),
                 color="black",
                 fill="white",
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
#plot with Mm change distribution
ggplot(data_frequencies_s, aes(x=-HmB+HmA)) +
  geom_histogram(aes(y =(..count..)/48),
                 color="black",
                 fill="white",
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

#plot with two distributions
plot_data <- data_frequencies_s %>%
  mutate(change_H=-HmB+HmA, change_M = -MmB+MmA) %>%
  select(participant.id_in_treatment,change_H, change_M) %>%
  gather(key, value, -participant.id_in_treatment)
ggplot(plot_data, aes(x=value, color=key)) +
  geom_histogram(aes(y =(..count..)/48),fill="white", alpha=0.5, position="identity" )

#hypothesis test of difference in distributions
test_data <- data_frequencies_s %>%
  mutate(change_H=-HmB+HmA, change_M = -MmB+MmA)
diff = test_data$change_H-test_data$change_M
test=wilcox.test(diff, alternative = "two.sided")
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
test=wilcox.test(diff, alternative = "two.sided")
p=test$p.value
reject_null=(p<=0.05)

# individual types --------------------------------------------------------

#create measure of weak-dominance violation
data_frequencies_s$HhA[is.na(data_frequencies_s$HhA)] = 1
data_frequencies_s$HhB[is.na(data_frequencies_s$HhB)] = 1
data_frequencies_s$HlA[is.na(data_frequencies_s$HlA)] = 0
data_frequencies_s$HlB[is.na(data_frequencies_s$HlB)] = 0
data_frequencies_s$MhA[is.na(data_frequencies_s$MhA)] = 1
data_frequencies_s$MhB[is.na(data_frequencies_s$MhB)] = 1
data_frequencies_s$MlA[is.na(data_frequencies_s$MlA)] = 0
data_frequencies_s$MlB[is.na(data_frequencies_s$MlB)] = 0
data_frequencies_s$LhA[is.na(data_frequencies_s$LhA)] = 1
data_frequencies_s$LhB[is.na(data_frequencies_s$LhB)] = 1
data_frequencies_s$LmA[is.na(data_frequencies_s$LmA)] = 1
data_frequencies_s$LmB[is.na(data_frequencies_s$LmB)] = 1
data_frequencies_s <- data_frequencies_s %>%
  mutate(WD_mistake = 1*(HhA==0|HhB==0|HlA!=0|HlB!=0|MhA==0|MhB==0|MlA!=0|MlB!=0|LhA==0|LhB==0|LmA==0|LmB==0))

    
# create measure of individual proposal rates
data_frequencies_s <- data_frequencies_s %>%
  mutate(HmA_propose = 1*(HmA>=0.5),
         HmB_propose = 1*(HmB>=0.5),
         MmA_propose = 1*(MmA>=0.5),
         MmB_propose = 1*(MmB>=0.5))

#check how many people propose/make mistakes
sum(data_frequencies_s$HmA_propose)
sum(data_frequencies_s$HmB_propose)
sum(data_frequencies_s$MmA_propose)
sum(data_frequencies_s$MmB_propose)
sum(data_frequencies_s$WD_mistake)

# Mm players who propose>50% in A are more likely to propose>50% as Mm players in B
sum(data_frequencies_s$MmA_propose*data_frequencies_s$MmB_propose)/sum(data_frequencies_s$MmA_propose)
sum((1-data_frequencies_s$MmA_propose)*data_frequencies_s$MmB_propose)/sum(1-data_frequencies_s$MmA_propose)


# Mm players who propose>50% in A are not more likely to propose>50% as Hm players in B
sum(data_frequencies_s$MmA_propose*data_frequencies_s$HmB_propose)/sum(data_frequencies_s$MmA_propose)
sum((1-data_frequencies_s$MmA_propose)*data_frequencies_s$HmB_propose)/sum(1-data_frequencies_s$MmA_propose)


# Mm players who propose>50% in A are more likely to propose>50% as Hm players in A
sum(data_frequencies_s$MmA_propose*data_frequencies_s$HmA_propose)/sum(data_frequencies_s$MmA_propose)
sum((1-data_frequencies_s$MmA_propose)*data_frequencies_s$HmA_propose)/sum(1-data_frequencies_s$MmA_propose)


# Mm players who propose>50% in A are slightly more likely to choose weakly dominated strategies
sum(data_frequencies_s$MmA_propose*data_frequencies_s$WD_mistake)/sum(data_frequencies_s$MmA_propose)
sum((1-data_frequencies_s$MmA_propose)*data_frequencies_s$WD_mistake)/sum(1-data_frequencies_s$MmA_propose)


#full table
data_freq=cbind(data_frequencies_s$MmA_propose,
                1-data_frequencies_s$MmA_propose,
                data_frequencies_s$MmB_propose,
                1-data_frequencies_s$MmB_propose,
                data_frequencies_s$HmA_propose,
                1-data_frequencies_s$HmA_propose,
                data_frequencies_s$HmB_propose,
                1-data_frequencies_s$HmB_propose,
                data_frequencies_s$WD_mistake,
                1-data_frequencies_s$WD_mistake)
num_variables = dim(data_freq)[2]
output_matrix = matrix(nrow=num_variables, ncol=num_variables)
for(i in seq(1:num_variables)){
  for(j in seq(1:num_variables)){
    output_matrix[i,j]=sum(data_freq[,i]*data_freq[,j])
    if((i+1)%/%2>(j+1)%/%2){output_matrix[i,j]="-"}
  }
}
library(xtable)
x <- xtable(output_matrix)
print(x, floating=FALSE, tabular.environment="bmatrix", hline.after=NULL, include.rownames=FALSE, include.colnames=FALSE)

#number of subjects who make no mistakes: 3
sum((1-data_frequencies_s$MmA_propose)*data_frequencies_s$MmB_propose*(1-data_frequencies_s$HmA_propose)*data_frequencies_s$MmB_propose*(1-data_frequencies_s$WD_mistake))
