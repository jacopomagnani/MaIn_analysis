
### PLOTS QRE EQUILIBRIUM ###
rm(list=ls())
source('solve_qre.R')
source('qre_distance.R')
source('exp_match_pay.R')
library("ggplot2")
library(tidyr)

m=c(160,80,40)
M=matrix(rep(m,3),nrow = 3,ncol = 3,byrow = TRUE)
D=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)

lambda_set=seq(from = 0, to = 0.4, by = 0.05)
n=length(lambda_set)

chi=0.95

### GAME A ###

r=c(100,75,25) #GAME A
R=matrix(rep(r,3),nrow = 3,ncol = 3,byrow = FALSE)


VALUES=matrix(rep(0,n*9),nrow = 9,ncol = n)
for(i in seq.int(1,n)){
  VALUES[,i]=solve_qre(D,R,M,lambda_set[i],chi)
}


test_data <-
  data.frame(
    Hh = VALUES[1,],
    Hm = VALUES[2,],
    Hl = VALUES[3,],
    Mh = VALUES[4,],
    Mm = VALUES[5,],
    Ml = VALUES[6,],
    Lh = VALUES[7,],
    Lm = VALUES[8,],
    Ll = VALUES[9,],
    lambda = lambda_set
  )
test_data %>%
  gather(key,probability, Hh,Hm,Hl,Mh,Mm,Ml,Lh,Lm,Ll) %>%
  ggplot(aes(x=lambda, y=probability, colour=key)) +
  geom_line()



### GAME B ###

r=c(80,75,25)# GAME B
R=matrix(rep(r,3),nrow = 3,ncol = 3,byrow = FALSE)

VALUES=matrix(rep(0,n*9),nrow = 9,ncol = n)
for(i in seq.int(1,n)){
  VALUES[,i]=solve_qre(D,R,M,lambda_set[i],chi)
}

test_data <-
  data.frame(
    Hh = VALUES[1,],
    Hm = VALUES[2,],
    Hl = VALUES[3,],
    Mh = VALUES[4,],
    Mm = VALUES[5,],
    Ml = VALUES[6,],
    Lh = VALUES[7,],
    Lm = VALUES[8,],
    Ll = VALUES[9,],
    lambda = lambda_set
  )
test_data %>%
  gather(key,probability, Hh,Hm,Hl,Mh,Mm,Ml,Lh,Lm,Ll) %>%
  ggplot(aes(x=lambda, y=probability, colour=key)) +
  geom_line()





############################### RISK AVERSION ############################### 

####### GAME A ###

m=c(160,80,40)
M=matrix(rep(m,3),nrow = 3,ncol = 3,byrow = TRUE)
r=c(100,75,25) #GAME A
R=matrix(rep(r,3),nrow = 3,ncol = 3,byrow = FALSE)
P=matrix(c(0.5,0.5,0,0,1,0,0,0.5,0.5),nrow = 3,ncol = 3,byrow = TRUE)
#P=matrix(rep(colSums(p),3),nrow = 3,ncol = 3,byrow = TRUE)


al=-1 #CRRA
M=(M^(1-al)-1)/(1-al)
R=(R^(1-al)-1)/(1-al)


lambda_set=seq(from = 0, to = 0.01, by = 0.001)
n=length(lambda_set)
VALUES=matrix(rep(0,n*9),nrow = 9,ncol = n)
for(i in seq.int(1,n)){
  VALUES[,i]=solve_qre(P,R,M,lambda_set[i])
}


test_data <-
  data.frame(
    Hh = VALUES[1,],
    Hm = VALUES[2,],
    Hl = VALUES[3,],
    Mh = VALUES[4,],
    Mm = VALUES[5,],
    Ml = VALUES[6,],
    Lh = VALUES[7,],
    Lm = VALUES[8,],
    Ll = VALUES[9,],
    lambda = lambda_set
  )
test_data %>%
  gather(key,probability, Hh,Hm,Hl,Mh,Mm,Ml,Lh,Lm,Ll) %>%
  ggplot(aes(x=lambda, y=probability, colour=key)) +
  geom_line()
#######################


