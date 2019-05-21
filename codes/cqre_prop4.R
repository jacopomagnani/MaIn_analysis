#########################################
### CQRE ANALYSIS FOR PROPOSITION 4   ###
#########################################

##################################
### PRELIMINARIES ################
##################################
library(tidyverse)
library(forcats)
library(latex2exp)
library(here)
library(ggplot2)
source(here("codes","solve_qre.R"))
source(here("codes","qre_distance.R"))
source(here("codes","exp_match_pay.R"))
source(here("codes","prob_accept.R"))
source(here("codes","qre_probabilities.R"))
##################################
##################################

##################################
####### FIXED PARAMETERS #########
##################################
m <- c(160,80,40)
r <- c(NA,75,25) #first entry will be set depending on game
M <- matrix(rep(m,3),nrow = 3,ncol = 3,byrow = TRUE)
D <- matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
type_labels <- c("Hm", "Mm")
##################################
##################################

#####################################
####### SET PARAMETER SPACE #########
#####################################
game_set <- c("A","B")
lambda_set <- seq(0.05,0.15,0.002)
chi_set <- seq(0,1,0.02)
N <- length(game_set)*length(lambda_set)*length(chi_set)
##################################
##################################

##################################
####### INITIALIZE #########
##################################
for(i in seq(1,length(type_labels))){assign(type_labels[i],rep(0,N))}
GAME=rep(0,N)
LAMBDA=rep(0,N)
CHI=rep(0,N)
i=0
##################################
##################################

##################################
####### SOLVE MODEL #########
##################################
for(game in game_set){
  if(game=="A"){r[1] <- 100}
  if(game=="B"){r[1] <- 80}
  R <- matrix(rep(r,3),nrow = 3,ncol = 3,byrow = FALSE)
  for(lambda in lambda_set){
    for(chi in chi_set){
      i <- i+1
      VALUES <- solve_qre(D,R,M,lambda,chi)
      Hm[i] <- VALUES[2]
      Mm[i] <- VALUES[5]
      GAME[i] <- game
      LAMBDA[i] <- lambda
      CHI[i] <- chi
    }
  }
}
results <- data.frame(Hm, Mm, GAME, LAMBDA, CHI)
diffs <- results %>%
  gather(variable, value, -(GAME:CHI)) %>%
  unite(temp, GAME, variable) %>%
  spread(temp, value) %>%
  mutate(diffH=B_Hm-A_Hm,diffM=B_Mm-A_Mm, diff=(B_Hm-A_Hm)-(B_Mm-A_Mm))
##################################
##################################

##################################
####### ANALYZE RESULTS   ########
##################################

#find 
diffs_sub <- diffs %>% 
  filter(diffH>0.25, diffM<0.05)
min(diffs_sub$LAMBDA)
min(diffs_sub$CHI)



##################################
##################################


