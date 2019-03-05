
##################################
###  CALLING PROGRAM FOR MLE #####
##################################

##################################
### PRELIMINARIES ################
##################################
library(tidyverse)
library(forcats)
library(bbmle)
library(here)
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
##################################
##################################

##################################
####### PREPARE DATA #############
##################################
min_round <- 1
max_round <- 60

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
##################################

##################################
####### FIXED PARAMETERS #########
##################################
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
##################################
##################################


############################################
####### RUN ESTIMATION FOR BASE ONLY #######
############################################
mle_results_BASE <- mle2(minuslogl = likelihood_BASE,
                    start = list(lambda=0.1, chi=0.5),
                    optimizer="nlminb", #NOTE: R will crash with default "optim"
                    data = mle_data,
                    lower=c(lambda=0.01,chi=0.01),
                    upper=c(lambda=0.4,chi=0.99))
summary(mle_results_BASE)
############################################
############################################



############################################
####### RUN JOINT ESTIMATION ###############
############################################
mle_results_joint <- mle2(minuslogl = likelihood_joint,
                         start = list(lambda=0.1, chi_BASE=0.5, chi_BEL=0.5),
                         optimizer="nlminb", #NOTE: R will crash with default "optim"
                         data = mle_data,
                         lower=c(lambda=0.001,chi_BASE=0.001, chi_BEL=0.001),
                         upper=c(lambda=0.4,chi_BASE=0.99, chi_BEL=0.99))
summary(mle_results_joint)
NLL_joint=mle_results_joint@details$objective
rm(mle_results_joint)

#test chi_BASE>chi_BEL
mle_results_joint_restricted <- mle2(minuslogl = likelihood_joint_restricted,
                          start = list(lambda=0.1, chi=0.5),
                          optimizer="nlminb", #NOTE: R will crash with default "optim"
                          data = mle_data,
                          lower=c(lambda=0.001,chi=0.001),
                          upper=c(lambda=0.4,chi=0.99))
NLL_joint_restricted=mle_results_joint_restricted@details$objective
rm(mle_results_joint_restricted)
chistat <- -2*(NLL_joint-NLL_joint_restricted) #chi2 statistic
pvalue <- 1-pchisq(chistat,df=1)
############################################
############################################

