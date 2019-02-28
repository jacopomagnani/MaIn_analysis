#########################################
### PLOTS COMPARATIVE STATICS OF CQRE ###
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
##################################
##################################

##################################
####### FIXED PARAMETERS #########
##################################
m=c(160,80,40)
r=c(NA,75,25) #first entry will be set depending on game
M=matrix(rep(m,3),nrow = 3,ncol = 3,byrow = TRUE)
D=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
##################################
##################################

#####################################
####### SET PARAMETER SPACE #########
#####################################
game_set=c("A","B")
type_set=c("H","M")
lambda_set=c(0.05,0.10,0.15)
chi_set=seq(0,1,0.02)
N=length(game_set)*length(type_set)*length(lambda_set)*length(chi_set)
offset=length(game_set)*length(lambda_set)*length(chi_set)
##################################
##################################

##################################
####### INITIALIZE #########
##################################
SIGMA=rep(0,N)
TYPE=rep(0,N)
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
  if(game=="A"){r[1]=100}
  if(game=="B"){r[1]=80}
  R=matrix(rep(r,3),nrow = 3,ncol = 3,byrow = FALSE)
  for(lambda in lambda_set){
    for(chi in chi_set){
      i=i+1
      VALUES=solve_qre(D,R,M,lambda,chi)
      SIGMA[i]=VALUES[2] #type H
      TYPE[i]="H"
      GAME[i]=game
      LAMBDA[i]=lambda
      CHI[i]=chi
      j=i+offset
      SIGMA[j]=VALUES[5] #type M
      TYPE[j]="M"
      GAME[j]=game
      LAMBDA[j]=lambda
      CHI[j]=chi
    }
  }
}
##################################
##################################

##################################
####### STORE DATA #########
##################################
plot_data <- data.frame(SIGMA, TYPE, GAME, LAMBDA, CHI)
plot_data_high <- plot_data %>%
  filter(LAMBDA==0.15)
plot_data_low <- plot_data %>%
  filter(LAMBDA==0.05)
plot_data_medium <- plot_data %>%
  filter(LAMBDA==0.10)
##################################
##################################

##################################
####### PLOTS #########
##################################
scale=1.2
for(plot_id in seq(1,length(lambda_set))){
  if(plot_id==1){
    plot_data=plot_data_high
    plot_name="plot_cqre_high.png"
  }
  if(plot_id==2){
    plot_data=plot_data_medium
    plot_name="plot_cqre_medium.png"
  }
  if(plot_id==3){
    plot_data=plot_data_low
    plot_name="plot_cqre_low.png"
  }
  f <- ggplot(plot_data, aes(x=CHI, y=SIGMA, colour=GAME, linetype=TYPE)) +
    geom_line(size=1) +
    theme(legend.text = element_text(size = 16*scale), 
          legend.title = element_text(size = 16*scale)) +
    scale_color_discrete(name="Game") +
    scale_linetype_discrete(name="Quality") +
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
  ggsave(f, filename = plot_name,  bg = "transparent", path= here("output"))
}
##################################
##################################
