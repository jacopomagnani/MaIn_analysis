### SIMULATES DATA GENERATING PROCESS ###
rm(list=ls())


#number of groups per treatment
num_groups_per_treat=6
#number of individuals per group
num_ind_per_group=8
#number of rounds
num_rounds=60*2/3
#number of treatments
T=3
#number of groups
G=num_groups_per_treat*T
# number of individuals
N=G*num_ind_per_group

num_sim=100

tot_pass=0
for(n in seq(1,num_sim)){
  
  #base treatment
  b=0.35
  gW=0.2
  iW=0.1
  irW=0.7
  Z_base=rep(0,num_groups_per_treat)
  for(g in seq(1,num_groups_per_treat)){
    z=0
    e_g=rnorm(1,0,1)
    for(i in seq(1,num_ind_per_group)){
      e_i=rnorm(1,0,1)
      for(r in seq(1,num_rounds)){
        e_ir=rnorm(1,0,1)
        y=b + gW*e_g + iW*e_i + irW*e_ir
        d=(y>=0)
        z=z+d
      }
    }
    Z_base[g]=z/(num_ind_per_group*num_rounds)
  }
  #print(Z_base)
  # print(mean(Z_base))
  # print(range(Z_base))
  
  ##cond treatment
  gW=0.1
  iW=0.1
  irW=0.8
  b=-0.45
  Z_cond=rep(0,num_groups_per_treat)
  for(g in seq(1,num_groups_per_treat)){
    z=0
    e_g=rnorm(1,0,1)
    for(i in seq(1,num_ind_per_group)){
      e_i=rnorm(1,0,1)
      for(r in seq(1,num_rounds)){
        e_ir=rnorm(1,0,1)
        y=b + gW*e_g + iW*e_i + irW*e_ir
        d=(y>=0)
        z=z+d
      }
    }
    Z_cond[g]=z/(num_ind_per_group*num_rounds)
  }
  #print(Z_cond)
  # print(mean(Z_cond))
  # print(range(Z_cond))
  
  
  ### test  ###
  test=wilcox.test(Z_base,Z_cond, alternative = "g")
  #test=t.test(Z_base,Z_cond, alternative = "g")
  p=test$p.value
  pass=(p<=0.05)
  tot_pass=tot_pass+pass
}
PWR=tot_pass/num_sim
print(PWR)


###############

###############

### WITHIN TEST ###

#base treatment
#vectors are like: HmB,HmA,MmB,MmA
b=c(0,-1,0.35,0.35)
gW=0.1
iW=0.1
irW=0.8


num_sim=100

tot_pass=0
for(n in seq(1,num_sim)){
  
  
  X=rep(0,num_groups_per_treat)
  for(g in seq(1,num_groups_per_treat)){
    z=c(0,0,0,0)
    e_g=rnorm(4,0,1)
    for(i in seq(1,num_ind_per_group)){
      e_i=rnorm(4,0,1)
      for(r in seq(1,num_rounds)){
        e_ir=rnorm(4,0,1)
        y=b + gW*e_g + iW*e_i + irW*e_ir
        d=(y>=0)
        z=z+d
      }
    }
    z=z/(num_ind_per_group*num_rounds)
    x=(z[1]-z[2])-(z[3]-z[4])
    X[g]=x
  }
  ### test  ###
  test=wilcox.test(X, alternative = "g",mu=0.22)
  #test=t.test(Z_base,Z_cond, alternative = "g")
  p=test$p.value
  pass=(p<=0.05)
  tot_pass=tot_pass+pass
}
PWR=tot_pass/num_sim
print(PWR)

