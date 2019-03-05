#####################################################
### PLOTS COMPARATIVE STATICS OF CQRE IN COND TASK ###
#####################################################

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
M_COND <- matrix(rep(c(160,80,40),3),nrow = 3,ncol = 3,byrow = TRUE)
R_COND <- matrix(rep(c(100,75,25),3),nrow = 3,ncol = 3,byrow = FALSE)
S_COND <- matrix(c(1,1,1,1,1,1,1,1,1),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_A <- matrix(c(0.500,0.500,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_B <- matrix(c(0.625,0.375,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_C <- matrix(c(0.750,0.250,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_D <- matrix(c(0.875,0.125,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
D_COND_E <- matrix(c(1.000,0.000,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
type_labels <- c("Hh","Hm","Hl","Mh","Mm","Ml","Lh","Lm","Ll")
##################################
##################################

#####################################
####### SET PARAMETER SPACE #########
#####################################
game_set <- c("A","B","C","D","E")
lambda_set <- c(0.05,0.10,0.15)
chi_set <- 0
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
  if(game=="A"){D_COND <- D_COND_A}
  if(game=="B"){D_COND <- D_COND_B}
  if(game=="C"){D_COND <- D_COND_C}
  if(game=="D"){D_COND <- D_COND_D}
  if(game=="E"){D_COND <- D_COND_E}
  for(lambda in lambda_set){
    for(chi in chi_set){
      i <- i+1
      VALUES <- qre_probabilities(S_COND,D_COND,M_COND,R_COND,lambda,chi)
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
results_tidy <- results %>% gather("TYPE","SIGMA",-GAME, -LAMBDA, -CHI) %>%
  mutate(LAMBDA=factor(LAMBDA))
##################################
##################################

##################################
####### PREPARE PLOT DATA ########
##################################
selected_types <- c("Mm")
plot_data <- results_tidy %>%
  filter(TYPE %in% selected_types)
##################################
##################################

##################################
####### PLOTS #########
##################################
scale=1.2
plot_name <- "plot_cqre_COND.png"
f <- ggplot(plot_data, aes(x=GAME, y=SIGMA, group=LAMBDA, colour=LAMBDA, linetype=LAMBDA)) +
  geom_line(size=1) +
  theme(legend.text = element_text(size = 16*scale), 
        legend.title = element_text(size = 16*scale)) +
  # scale_color_discrete(name="Game") +
  # scale_linetype_discrete(name="Quality",
  #                         breaks=c("Hm", "Mm"),
  #                         labels=c("H", "M")) +
  ylab(TeX("Proposal rate")) +
  xlab(TeX("Game")) +
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
ggsave(f, filename = plot_name,  bg = "transparent", path= here("output"))

##################################
##################################