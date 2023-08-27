
# Install and load packages --------------------------------------------
# Package names
packages <- c("here",
              "tidyverse",
              "forcats",
              "sandwich",
              "lmtest",
              "stargazer",
              "latex2exp",
              "bbmle",
              "xtable",
              "ggplot2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Table A1 ---------------------------------------------------

#set min and max rounds
min_round <- 20
max_round <- 60

#create datasets for BASE aggregate analysis
data_base_raw <- read_csv(here("data","MaIn_data_base_game.csv")) %>%
  filter(subsession.round_number>=min_round) %>%
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
  mutate(subsession.game_name=factor(subsession.game_name))

data_base_groups <- data_base_raw %>%
  group_by(group.id_in_subsession,
           session.code,
           player.type,
           player.signal,
           subsession.game_name
  ) %>%
  summarise(mean_choice = mean(player.choice))

num_groups=length(unique(paste0(data_base_groups$group.id_in_subsession, data_base_groups$session.code)))

data_base_means <- data_base_groups %>%
  group_by(player.type,
           player.signal,
           subsession.game_name
  ) %>%
  summarise(mean = mean(mean_choice), sd = sd(mean_choice)/sqrt(num_groups))

#source auxiliary codes
source(here("codes","exp_match_pay.R"))
source(here("codes","prob_accept.R"))

#compute expected gains from proposing
D=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
M=matrix(rep(c(160,80,40),3),nrow = 3,ncol = 3,byrow = TRUE)
sigma_A=data_base_means %>% filter(subsession.game_name=="A") %>% pull(mean)
S_A=matrix(sigma_A,nrow = 3,ncol = 3,byrow = TRUE)
R_A=matrix(rep(c(100,75,25),3),nrow = 3,ncol = 3,byrow = FALSE)
sigma_B=data_base_means %>% filter(subsession.game_name=="B") %>% pull(mean)
S_B=matrix(sigma_B,nrow = 3,ncol = 3,byrow = TRUE)
R_B=matrix(rep(c(80,75,25),3),nrow = 3,ncol = 3,byrow = FALSE)
delta_A  <-  prob_accept(S_A,D) * (exp_match_pay(S_A,D,M,0) - R_A)
delta_B  <-  prob_accept(S_B,D) * (exp_match_pay(S_B,D,M,0) - R_B)

delta_A = format(round(delta_A,0),nsmall=0)
colnames(delta_A)=c("sh","sm","sl")
rownames(delta_A)=c("qH","qM","qL")
delta_B = format(round(delta_B,0),nsmall=0)
colnames(delta_B)=c("sh","sm","sl")
rownames(delta_B)=c("qH","qM","qL")

write(print(xtable(delta_A,caption = c("Table A1, Game A")),
            include.colnames = TRUE,
            include.rownames = TRUE,),
      file=here("output/tables","tableA1a.tex"))
write(print(xtable(delta_B,caption = c("Table A1, Game B")),
            include.colnames = TRUE,
            include.rownames = TRUE), 
      file=here("output/tables","tableA1b.tex"))


# Table A2 and Table A3 ----------------------------------------------------------------


#set min and max rounds
min_round <- 0
max_round <- 60

#create datasets for BASE aggregate analysis
data_base_raw <- read_csv(here("data","MaIn_data_base_game.csv")) %>%
  filter(subsession.round_number>=min_round) %>%
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
  mutate(subsession.game_name=factor(subsession.game_name))

data_base_ind <- data_base_raw %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  mutate(group.id_in_treatment=interaction(factor(session.code),factor(group.id_in_subsession))) %>%
  mutate(session.code=factor(session.code)) %>%
  select(session.code, participant.id_in_treatment, player.choice,subsession.round_number,subsession.game_name,
         player.match, player.partner_type, player.type, player.signal,group.id_in_treatment)

data_frequencies <- data_base_ind %>%
  group_by(participant.id_in_treatment, player.type, player.signal, subsession.game_name) %>%
  summarise(frequency = mean(player.choice))

data_frequencies_s <- data_frequencies %>%
  unite(case,player.type,player.signal,subsession.game_name, sep="") %>%
  spread(case,frequency)

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
    #if((i+1)%/%2>(j+1)%/%2){output_matrix[i,j]="-"}
  }
}

output_matrix= format(round(output_matrix,0),nsmall=0)
colnames(output_matrix)=c("PMmA","NMmA","PMmB","NMmB","PHmA","NHmA","PHmB","NHmB","WD","NoWD")
rownames(output_matrix)=c("PMmA","NMmA","PMmB","NMmB","PHmA","NHmA","PHmB","NHmB","WD","NoWD")

print(xtable(output_matrix,caption = c("Table A2")),
      include.rownames=TRUE, 
      include.colnames=TRUE,
      file=here("output/tables","tableA2.tex"))

freq_martix = matrix(rep(0,16),nrow = 4,ncol = 4)
freq_martix[1,1]=sum((data_frequencies_s$HmA_propose)*(data_frequencies_s$HmB_propose)*(data_frequencies_s$MmA_propose)*(data_frequencies_s$MmB_propose))
freq_martix[1,2]=sum((data_frequencies_s$HmA_propose)*(data_frequencies_s$HmB_propose)*(data_frequencies_s$MmA_propose)*(1-data_frequencies_s$MmB_propose))
freq_martix[1,3]=sum((data_frequencies_s$HmA_propose)*(data_frequencies_s$HmB_propose)*(1-data_frequencies_s$MmA_propose)*(data_frequencies_s$MmB_propose))
freq_martix[1,4]=sum((data_frequencies_s$HmA_propose)*(data_frequencies_s$HmB_propose)*(1-data_frequencies_s$MmA_propose)*(1-data_frequencies_s$MmB_propose))
freq_martix[2,1]=sum((data_frequencies_s$HmA_propose)*(1-data_frequencies_s$HmB_propose)*(data_frequencies_s$MmA_propose)*(data_frequencies_s$MmB_propose))
freq_martix[2,2]=sum((data_frequencies_s$HmA_propose)*(1-data_frequencies_s$HmB_propose)*(data_frequencies_s$MmA_propose)*(1-data_frequencies_s$MmB_propose))
freq_martix[2,3]=sum((data_frequencies_s$HmA_propose)*(1-data_frequencies_s$HmB_propose)*(1-data_frequencies_s$MmA_propose)*(data_frequencies_s$MmB_propose))
freq_martix[2,4]=sum((data_frequencies_s$HmA_propose)*(1-data_frequencies_s$HmB_propose)*(1-data_frequencies_s$MmA_propose)*(1-data_frequencies_s$MmB_propose))
freq_martix[3,1]=sum((1-data_frequencies_s$HmA_propose)*(data_frequencies_s$HmB_propose)*(data_frequencies_s$MmA_propose)*(data_frequencies_s$MmB_propose))
freq_martix[3,2]=sum((1-data_frequencies_s$HmA_propose)*(data_frequencies_s$HmB_propose)*(data_frequencies_s$MmA_propose)*(1-data_frequencies_s$MmB_propose))
freq_martix[3,3]=sum((1-data_frequencies_s$HmA_propose)*(data_frequencies_s$HmB_propose)*(1-data_frequencies_s$MmA_propose)*(data_frequencies_s$MmB_propose))
freq_martix[3,4]=sum((1-data_frequencies_s$HmA_propose)*(data_frequencies_s$HmB_propose)*(1-data_frequencies_s$MmA_propose)*(1-data_frequencies_s$MmB_propose))
freq_martix[4,1]=sum((1-data_frequencies_s$HmA_propose)*(1-data_frequencies_s$HmB_propose)*(data_frequencies_s$MmA_propose)*(data_frequencies_s$MmB_propose))
freq_martix[4,2]=sum((1-data_frequencies_s$HmA_propose)*(1-data_frequencies_s$HmB_propose)*(data_frequencies_s$MmA_propose)*(1-data_frequencies_s$MmB_propose))
freq_martix[4,3]=sum((1-data_frequencies_s$HmA_propose)*(1-data_frequencies_s$HmB_propose)*(1-data_frequencies_s$MmA_propose)*(data_frequencies_s$MmB_propose))
freq_martix[4,4]=sum((1-data_frequencies_s$HmA_propose)*(1-data_frequencies_s$HmB_propose)*(1-data_frequencies_s$MmA_propose)*(1-data_frequencies_s$MmB_propose))

freq_martix=format(round(freq_martix,0),nsmall=0)
colnames(freq_martix)=c("PMmA&PMmB","PMmA&NMmB","NMmA&PMmB","NMmA&NMmB")
rownames(freq_martix)=c("PHmA&PHmB","PHmA&NHmB","NHmA&PHmB","NHmA&NHmB")

print(xtable(freq_martix,caption = c("Table A3")),
      include.rownames=TRUE, 
      include.colnames=TRUE,
      file=here("output/tables","tableA3.tex"))


# Figure A1 and Figure A2 -------------------------------------------------

source(here("codes","solve_qre.R"))
source(here("codes","qre_distance.R"))
source(here("codes","exp_match_pay.R"))
source(here("codes","prob_accept.R"))
source(here("codes","qre_probabilities.R"))

m <- c(160,80,40)
r <- c(NA,75,25) #first entry will be set depending on game
M <- matrix(rep(m,3),nrow = 3,ncol = 3,byrow = TRUE)
D <- matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
type_labels <- c("Hh","Hm","Hl","Mh","Mm","Ml","Lh","Lm","Ll")
game_set <- c("A","B")
lambda_set <- c(0.05,0.15,0.25)
chi_set <- seq(0,1,0.02)
N <- length(game_set)*length(lambda_set)*length(chi_set)

for(i in seq(1,length(type_labels))){assign(type_labels[i],rep(0,N))}
GAME=rep(0,N)
LAMBDA=rep(0,N)
CHI=rep(0,N)
i=0

for(game in game_set){
  if(game=="A"){r[1] <- 100}
  if(game=="B"){r[1] <- 80}
  R <- matrix(rep(r,3),nrow = 3,ncol = 3,byrow = FALSE)
  for(lambda in lambda_set){
    for(chi in chi_set){
      i <- i+1
      VALUES <- solve_qre(D,R,M,lambda,chi)
      Hh[i] <- VALUES[1]
      Hm[i] <- VALUES[2]
      Hl[i] <- VALUES[3]
      Mh[i] <- VALUES[4]
      Mm[i] <- VALUES[5]
      Ml[i] <- VALUES[6]
      Lh[i] <- VALUES[7]
      Lm[i] <- VALUES[8]
      Ll[i] <- VALUES[9]
      GAME[i] <- game
      LAMBDA[i] <- lambda
      CHI[i] <- chi
    }
  }
}
results <- data.frame(Hh, Hm, Hl, Mh, Mm, Ml, Lh, Lm, Ll, GAME, LAMBDA, CHI)
results_tidy <- results %>% gather("TYPE","SIGMA",-GAME, -LAMBDA, -CHI)

selected_types <- c("Hm", "Mm")
plot_data_high <- results_tidy %>%
  filter(LAMBDA==0.25) %>%
  filter(TYPE %in% selected_types)
plot_data_low <- results_tidy %>%
  filter(LAMBDA==0.05) %>%
  filter(TYPE %in% selected_types)
plot_data_medium <- results_tidy %>%
  filter(LAMBDA==0.15) %>%
  filter(TYPE %in% selected_types)

scale=1.2
plot_data <- plot_data_medium
plot_name <- "figureA1.png"
f <- ggplot(plot_data, aes(x=CHI, y=SIGMA, colour=GAME, linetype=TYPE)) +
  geom_line(size=1) +
  theme(legend.text = element_text(size = 16*scale), 
        legend.title = element_text(size = 16*scale)) +
  scale_color_discrete(name="Game") +
  scale_linetype_discrete(name="Quality",
                          breaks=c("Hm", "Mm"),
                          labels=c("H", "M")) +
  ylab(TeX("Proposal rate")) +
  xlab(TeX("$\\chi$")) +
  theme(axis.title.x = element_text(size=20*scale)) +
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
    , legend.key.size = unit(1*scale, "cm")
    , legend.position = "bottom"
  )
ggsave(f, filename = plot_name,  bg = "transparent", path= here("output/figures"))

scale=1.2
plot_data <- results_tidy %>%
  filter(LAMBDA==0.05|LAMBDA==0.25) %>%
  filter(TYPE %in% selected_types)
plot_name <- "figureA2.png"
f <- ggplot(plot_data, aes(x=CHI, y=SIGMA, colour=GAME, linetype=TYPE)) +
  geom_line(size=1) +
  facet_grid(.~ LAMBDA, labeller = label_bquote(cols = lambda == .(LAMBDA))) +
  theme(legend.text = element_text(size = 16*scale), 
        legend.title = element_text(size = 16*scale)) +
  scale_color_discrete(name="Game") +
  scale_linetype_discrete(name="Quality",
                          breaks=c("Hm", "Mm"),
                          labels=c("H", "M")) +
  ylab(TeX("Proposal rate")) +
  xlab(TeX("$\\chi$")) +
  theme() +
  theme(axis.title.x = element_text(size=20*scale)) +
  theme(axis.title.y = element_text(size=16*scale)) +
  theme(axis.text.x  = element_text(size=12*scale)) +
  theme(axis.text.y  = element_text(size=16*scale)) +
  ylim(0,1) +
  theme(
    strip.text = element_text(size=16*scale)
    ,panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_line(colour = "grey") # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , panel.spacing = unit(1*scale, "cm")
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent", color = NA) # get rid of legend panel bg
    , legend.key = element_rect(fill = "transparent", color = NA)
    , legend.key.size = unit(1*scale, "cm")
    , legend.position = "bottom"
  )
ggsave(f, filename = plot_name,  bg = "transparent", path= here("output/figures"))


# Table A4 and Table A5 ---------------------------------------------------


#set min and max rounds
min_round <- 0
max_round <- 60

#create datasets for BASE aggregate analysis
data_base_raw <- read_csv(here("data","MaIn_data_base_game.csv")) %>%
  filter(subsession.round_number>=min_round) %>%
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
  mutate(subsession.game_name=factor(subsession.game_name))

data_base_groups <- data_base_raw %>%
  group_by(group.id_in_subsession,
           session.code,
           player.type,
           player.signal,
           subsession.game_name
  ) %>%
  summarise(mean_choice = mean(player.choice))

data_base_means <- data_base_groups %>%
  group_by(player.type,
           player.signal,
           subsession.game_name
  ) %>%
  summarise(mean = mean(mean_choice))


q_set=c("H","M","L")
s_set=c("h","m","l")
game_set=c("A","B")

means_A=matrix(rep(0,9),nrow = 3,ncol = 3)
pvals_A=matrix(rep(0,9),nrow = 3,ncol = 3)
this_game="A"
for(i in seq_along(q_set)){
  for(j in seq_along(s_set)){
    means_A[i,j]=data_base_means %>% 
      filter(player.type==q_set[i] & player.signal==s_set[j] & subsession.game_name==this_game) %>%
      pull(mean)
    if(means_A[i,j]>=0.5){
      direction="greater"
    } else {direction="less"}
    test_data <- data_base_groups %>%
      filter(player.type==q_set[i] & player.signal==s_set[j] & subsession.game_name==this_game)
    diff = test_data$mean_choice-0.5
    test=wilcox.test(diff, alternative = direction )
    pvals_A[i,j]=test$p.value
  }
}

means_B=matrix(rep(0,9),nrow = 3,ncol = 3)
pvals_B=matrix(rep(0,9),nrow = 3,ncol = 3)
this_game="B"
for(i in seq_along(q_set)){
  for(j in seq_along(s_set)){
    means_B[i,j]=data_base_means %>% 
      filter(player.type==q_set[i] & player.signal==s_set[j] & subsession.game_name==this_game) %>%
      pull(mean)
    if(means_B[i,j]>=0.5){
      direction="greater"
    } else {direction="less"}
    test_data <- data_base_groups %>%
      filter(player.type==q_set[i] & player.signal==s_set[j] & subsession.game_name==this_game)
    diff = test_data$mean_choice-0.5
    test=wilcox.test(diff, alternative = direction )
    pvals_B[i,j]=test$p.value
  }
}

means_A=format(round(means_A,2),nsmall=2)
pvals_A=format(round(pvals_A,3),nsmall=3)
colnames(means_A)=c("sh","sm","sl")
rownames(means_A)=c("qH","qM","qL")
colnames(pvals_A)=c("sh","sm","sl")
rownames(pvals_A)=c("qH","qM","qL")
means_B=format(round(means_B,2),nsmall=2)
pvals_B=format(round(pvals_B,3),nsmall=3)
colnames(means_B)=c("sh","sm","sl")
rownames(means_B)=c("qH","qM","qL")
colnames(pvals_B)=c("sh","sm","sl")
rownames(pvals_B)=c("qH","qM","qL")


write(print(xtable(means_A,caption = c("Table A4, Game A")),
            include.colnames = TRUE,
            include.rownames = TRUE,),
      file=here("output/tables","tableA4a.tex"))
write(print(xtable(means_B,caption = c("Table A4, Game B")),
            include.colnames = TRUE,
            include.rownames = TRUE), 
      file=here("output/tables","tableA4b.tex"))

write(print(xtable(pvals_A,caption = c("Table A5, Game A")),
            include.colnames = TRUE,
            include.rownames = TRUE,),
      file=here("output/tables","tableA5a.tex"))
write(print(xtable(pvals_B,caption = c("Table A5, Game B")),
            include.colnames = TRUE,
            include.rownames = TRUE), 
      file=here("output/tables","tableA5b.tex"))


# Table A6 ----------------------------------------------------------------


#set min and max rounds
min_round <- 0
max_round <- 60

data_base_raw <- read_csv(here("data","MaIn_data_base_game.csv")) %>%
  filter(subsession.round_number>=min_round) %>%
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
  mutate(subsession.game_name=factor(subsession.game_name))

data_base_ind <- data_base_raw %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  mutate(group.id_in_treatment=interaction(factor(session.code),factor(group.id_in_subsession))) %>%
  mutate(session.code=factor(session.code)) %>%
  select(session.code, participant.id_in_treatment, player.choice,subsession.round_number,subsession.game_name,
         player.match, player.partner_type, player.type, player.signal,group.id_in_treatment)

data_reg1 <- data_base_ind %>%
  filter(player.type!="L") %>%
  filter(player.signal=="m") %>%
  mutate(subsession.game_A=(subsession.game_name=="A"))

reg1_lpm <- glm(formula = player.choice ~
                  + (player.type=="H")
                + subsession.game_A
                + (player.type=="H") * subsession.game_A
                ,data=data_reg1
                ,family = "gaussian"
)
results_reg1_lp<-coeftest(reg1_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_treatment, multi0=TRUE)
reg1_logit <- glm(formula = player.choice ~
                    + (player.type=="H")
                  + subsession.game_A
                  + (player.type=="H") * subsession.game_A
                  ,data=data_reg1
                  ,family = "binomial"
)
results_reg1_logit<-coeftest(reg1_logit, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_treatment, multi0=TRUE)

stargazer(reg1_lpm, 
          reg1_logit,
          se = list(results_reg1_lp[,"Std. Error"], results_reg1_logit[,"Std. Error"]),
          p = list(results_reg1_lp[,"Pr(>|z|)"], results_reg1_logit[,"Pr(>|z|)"]),
          covariate.labels = c("$H$", "$A$", "$H\\times A$"),
          dep.var.labels   = c("$propose$"),
          model.names = FALSE,
          add.lines = list(c("Regression", "Linear", "Logit"),
                           c("Clustering", "Yes", "Yes")),
          out = here("output/tables","tableA6.tex"), 
          float=TRUE, title = "Table A6")



# Table A7 ----------------------------------------------------------------


#set min and max rounds
min_round <- 0
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

num_groups_bel=length(unique(paste0(data_groups_bel$group.id_in_subsession, data_groups_bel$session.code)))

data_means_bel <- data_groups_bel %>%
  group_by(subsession.game_name) %>%
  summarise(mean = mean(mean_choice), adverse = mean(adverse), sd = sd(mean_choice)/sqrt(num_groups_bel))


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

num_groups_cond=length(unique(paste0(data_groups_cond$group.id_in_subsession, data_groups_cond$session.code)))

data_means_cond <- data_groups_cond %>%
  group_by(subsession.game_name) %>%
  summarise(mean = mean(mean_choice), adverse = mean(adverse), sd = sd(mean_choice)/sqrt(num_groups_cond))


data_bel_reg <- data_bel %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  mutate(session.code=factor(session.code)) %>%
  select(session.code, participant.id_in_treatment, player.choice,subsession.round_number,subsession.game_name,
         player.match, player.partner_type, player.type, player.signal,group.id_in_subsession, player.status,
         subsession.p, subsession.adverse)

data_cond_reg <- data_cond %>%
  mutate(participant.id_in_treatment=interaction(factor(session.code),factor(participant.id_in_session))) %>%
  mutate(session.code=factor(session.code)) %>%
  select(session.code, participant.id_in_treatment, player.choice,subsession.round_number,subsession.game_name,
         player.match, player.partner_type, player.type, player.signal,group.id_in_subsession, player.status,
         subsession.p, subsession.adverse)


reg2_bel_lpm <- glm(formula = player.choice ~
                      + subsession.adverse
                    ,data=data_bel_reg
                    ,family = "gaussian"
)
results_reg2_bel_lpm<-coeftest(reg2_bel_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession,multi0=TRUE)
reg2_bel_log <- glm(formula = player.choice ~
                      + subsession.adverse
                    ,data=data_bel_reg
                    ,family = "binomial"
)
results_reg2_bel_log<-coeftest(reg2_bel_log, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession,multi0=TRUE)
reg2_cond_lpm <- glm(formula = player.choice ~
                       + subsession.adverse
                     ,data=data_cond_reg
                     ,family = "gaussian"
)
results_reg2_cond_lpm<-coeftest(reg2_cond_lpm, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession,multi0=TRUE)
reg2_cond_log <- glm(formula = player.choice ~
                       + subsession.adverse
                     ,data=data_cond_reg
                     ,family = "binomial"
)
results_reg2_cond_log<-coeftest(reg2_cond_log, vcov = vcovCL, cluster = ~ participant.id_in_treatment + group.id_in_subsession,multi0=TRUE)

stargazer(reg2_bel_lpm, 
          reg2_bel_log,
          reg2_cond_lpm, 
          reg2_cond_log,
          se = list(results_reg2_bel_lpm[,"Std. Error"],
                    results_reg2_bel_log[,"Std. Error"],
                    results_reg2_cond_lpm[,"Std. Error"],
                    results_reg2_cond_log[,"Std. Error"]),
          p = list(results_reg2_bel_lpm[,"Pr(>|z|)"],
                   results_reg2_bel_log[,"Pr(>|z|)"],
                   results_reg2_cond_lpm[,"Pr(>|z|)"],
                   results_reg2_cond_log[,"Pr(>|z|)"]),
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
          out = here("output/tables","tableA7.tex"), 
          float=TRUE, title = "Table A7")

# Table A8 ----------------------------------------------------------------


#set min and max rounds
min_round <- 0
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
adv = c("Averse selection parameter",0,0.25,0.5,0.75,1)
diff=c("Difference in proposal rates",round(diff,2))
p=c("Wilcoxon test p-value",round(p,2))
matrix(c(adv,diff,p),byrow = TRUE,nrow=3)
write(print(xtable(matrix(c(adv,diff,p),byrow = TRUE,nrow=3),caption = c("Table A8")),include.colnames = FALSE,include.rownames = FALSE),
      file=here("output/tables","tableA8.tex"))


# Table A9 ----------------------------------------------------------------

#set min and max rounds
min_round <- 0
max_round <- 60

#source auxiliary files
source(here("codes","solve_qre.R"))
source(here("codes","qre_distance.R"))
source(here("codes","exp_match_pay.R"))
source(here("codes","prob_accept.R"))
source(here("codes","qre_probabilities.R"))
source(here("codes","likelihood_BASE.R"))
source(here("codes","likelihood_BEL.R"))
source(here("codes","likelihood_COND.R"))
source(here("codes","likelihood_joint.R"))
source(here("codes","likelihood_joint_restricted.R"))
#prepare data
raw_data<-read_csv(here("data","MaIn_data_base_game.csv"))
mle_data_BASE <- raw_data %>% 
  filter(subsession.round_number>=min_round & subsession.round_number<=max_round) %>%
  mutate(treatment=rep("BASE",length(player.choice)),
         propose = player.choice, 
         type=player.type,
         signal=player.signal,
         game=(subsession.game_name=="A")*1+(subsession.game_name=="B")*2) %>%
  select(treatment,propose,type,signal,game)
rm(raw_data)
raw_data<-read_csv(here("data","MaIn_data_bel_game.csv"))
mle_data_BEL <- raw_data %>%
  filter(player.status==0) %>%
  filter(subsession.round_number>=min_round & subsession.round_number<=max_round) %>%
  mutate(treatment=rep("BEL",length(player.choice)),
         propose = player.choice,
         type=player.type,
         signal=player.signal,
         game=(subsession.game_name=="A")*1+
           (subsession.game_name=="B")*2+
           (subsession.game_name=="C")*3+
           (subsession.game_name=="D")*4+
           (subsession.game_name=="E")*5
  ) %>%
  select(treatment,propose,type,signal,game)
rm(raw_data)
raw_data<-read_csv(here("data","MaIn_data_cond_game.csv"))
mle_data_COND <- raw_data %>%
  filter(player.status==0) %>%
  filter(subsession.round_number>=min_round & subsession.round_number<=max_round) %>%
  mutate(treatment=rep("COND",length(player.choice)),
         propose = player.choice,
         type=player.type,
         signal=player.signal,
         game=(subsession.game_name=="A")*1+
           (subsession.game_name=="B")*2+
           (subsession.game_name=="C")*3+
           (subsession.game_name=="D")*4+
           (subsession.game_name=="E")*5
  ) %>%
  select(treatment,propose,type,signal,game)
rm(raw_data)
mle_data = mle_data_BASE %>%
  bind_rows(mle_data_BEL) %>%
  bind_rows(mle_data_COND)
#fixed parameters
M_BASE <- matrix(rep(c(160,80,40),3),nrow = 3,ncol = 3,byrow = TRUE)
D_BASE <- matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
R_BASE_A <- matrix(rep(c(100,75,25),3),nrow = 3,ncol = 3,byrow = FALSE)
R_BASE_B <- matrix(rep(c(80,75,25),3),nrow = 3,ncol = 3,byrow = FALSE)

M_BEL <- matrix(rep(c(160,80,40),3),nrow = 3,ncol = 3,byrow = TRUE)
D_BEL <- matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
R_BEL <- matrix(rep(c(100,75,25),3),nrow = 3,ncol = 3,byrow = FALSE)
S_BEL_A <- matrix(c(1,1.00,0,1,1,0,1,1,1),nrow = 3,ncol = 3,byrow = TRUE)
S_BEL_B <- matrix(c(1,0.75,0,1,1,0,1,1,1),nrow = 3,ncol = 3,byrow = TRUE)
S_BEL_C <- matrix(c(1,0.50,0,1,1,0,1,1,1),nrow = 3,ncol = 3,byrow = TRUE)
S_BEL_D <- matrix(c(1,0.25,0,1,1,0,1,1,1),nrow = 3,ncol = 3,byrow = TRUE)
S_BEL_E <- matrix(c(1,0.00,0,1,1,0,1,1,1),nrow = 3,ncol = 3,byrow = TRUE)

M_COND <- matrix(rep(c(160,80,40),3),nrow = 3,ncol = 3,byrow = TRUE)
R_COND <- matrix(rep(c(100,75,25),3),nrow = 3,ncol = 3,byrow = FALSE)
S_COND <- matrix(c(1,1,1,1,1,1,1,1,1),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_A <- matrix(c(0.500,0.500,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_B <- matrix(c(0.625,0.375,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_C <- matrix(c(0.750,0.250,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_D <- matrix(c(0.875,0.125,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_E <- matrix(c(1.000,0.000,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
#run mle
mle_joint <- mle2(minuslogl = likelihood_joint,
                  start = list(lambda=0.1, chi_BASE=0.5, chi_BEL=0.5),
                  optimizer="nlminb", #NOTE: R will crash with default "optim"
                  data = mle_data,
                  lower=c(lambda=0.001,chi_BASE=0.001, chi_BEL=0.001),
                  upper=c(lambda=0.4,chi_BASE=0.99, chi_BEL=0.99))
mle_results_joint <- summary(mle_joint)
NLL_joint <- mle_joint@details$objective
#test chi_BASE>chi_BEL
mle_results_joint_restricted <- mle2(minuslogl = likelihood_joint_restricted,
                                     start = list(lambda=0.1, chi=0.5),
                                     optimizer="nlminb", #NOTE: R will crash with default "optim"
                                     data = mle_data,
                                     lower=c(lambda=0.001,chi=0.001),
                                     upper=c(lambda=0.4,chi=0.99))
NLL_joint_restricted=mle_results_joint_restricted@details$objective
chistat <- -2*(NLL_joint-NLL_joint_restricted) #chi2 statistic
pvalue <- 1-pchisq(chistat,df=1)
#create table 9
dummydata <- data.frame(y=1,lambda=2,chi_BASE=3,chi_BEL=4)
dummy_reg <- lm(formula = y ~ lambda + chi_BASE + chi_BEL, data = dummydata)
stargazer(dummy_reg,
          coef = list(mle_results_joint@coef[,"Estimate"]),
          se = list(mle_results_joint@coef[,"Std. Error"]),
          p = list(mle_results_joint@coef[,"Pr(z)"]),
          covariate.labels = c("$\\lambda$", "$\\chi^{BASE}$", "$\\chi^{BEL}$"),
          dep.var.labels   = NULL,
          dep.var.labels.include = FALSE,
          dep.var.caption ="",
          model.names = FALSE,
          omit = c("Constant"),
          omit.stat = c("all"),
          add.lines = list(c("-2 log Lik.", as.character(format(mle_results_joint@m2logL))),
                           c("Observations", as.character(length(mle_joint@data$treatment)))),
          out = here("output/tables","tableA9.tex"), 
          float=TRUE, title = "Table A9")

