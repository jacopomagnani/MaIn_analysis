####### INDIVIDUAL ANALYSIS #########



rm(list=ls())
data=read.csv(file = "pilot_base.csv",header = TRUE)

id = data$participant.id_in_session
propose = data$player.choice
accepted = data$player.partner_choice
match = propose * accepted
type = factor(x = data$player.type,levels = c("1","2", "3"),labels = c("H","M","L"))
partner_type=factor(x = data$player.partner_type,levels = c("1","2", "3"),labels = c("H","M","L"))
signal = factor(x = data$player.signal,levels = c("1","2", "3"),labels = c("h","m","l"))
round =data$subsession.round_number
game = data$subsession.game_name
A = (game=="A")
B = (game=="B")




####### computing various backward looking variables ###########

cum_match_noL_A = rep(0,length(id))
cum_match_H_A = rep(0,length(id))
cum_match_M_A = rep(0,length(id))
cum_match_L_A = rep(0,length(id))
cum_nomatch_noL_A = rep(0,length(id))
cum_nomatch_H_A = rep(0,length(id))
cum_nomatch_M_A = rep(0,length(id))
cum_nomatch_L_A = rep(0,length(id))
cum_L_A = rep(0,length(id))
cum_noL_A = rep(0,length(id))
cum_H_A = rep(0,length(id))
cum_M_A = rep(0,length(id))

cum_match_noL_B = rep(0,length(id))
cum_match_H_B = rep(0,length(id))
cum_match_M_B = rep(0,length(id))
cum_match_L_B = rep(0,length(id))
cum_nomatch_noL_B = rep(0,length(id))
cum_nomatch_H_B = rep(0,length(id))
cum_nomatch_M_B = rep(0,length(id))
cum_nomatch_L_B = rep(0,length(id))
cum_L_B = rep(0,length(id))
cum_noL_B = rep(0,length(id))
cum_H_B = rep(0,length(id))
cum_M_B = rep(0,length(id))

cum_match_noL = rep(0,length(id))
cum_match_H = rep(0,length(id))
cum_match_M = rep(0,length(id))
cum_match_L = rep(0,length(id))
cum_nomatch_noL = rep(0,length(id))
cum_nomatch_H = rep(0,length(id))
cum_nomatch_M = rep(0,length(id))
cum_nomatch_L = rep(0,length(id))
cum_L= rep(0,length(id))
cum_noL = rep(0,length(id))
cum_H = rep(0,length(id))
cum_M = rep(0,length(id))

cum_match = rep(0,length(id))


mem=60
for(i in unique(id)){
  for(t in seq(2,max(round))){
    ind=(id==i&round<t)
    ind_new= (ind & game=="A" & type=="M" & signal=="m")
    cum_match_noL_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" | partner_type[ind_new]=="M") * (match[ind_new]==1),mem))
    cum_match_H_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="H") * (match[ind_new]==1),mem))
    cum_match_M_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="M") * (match[ind_new]==1),mem))
    cum_match_L_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="L") * (match[ind_new]==1),mem))
    cum_nomatch_noL_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" | partner_type[ind_new]=="M") * (match[ind_new]==0),mem))
    cum_nomatch_H_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" ) * (match[ind_new]==0),mem))
    cum_nomatch_M_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="M" ) * (match[ind_new]==0),mem))
    cum_nomatch_L_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="L") * (match[ind_new]==0),mem))
    cum_L_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="L"),mem))
    cum_noL_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" | partner_type[ind_new]=="M"),mem))
    cum_H_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="H"),mem))
    cum_M_A[id==i&round==t] = sum(tail((partner_type[ind_new]=="M"),mem))
    
    ind_new= (ind & game=="B" & type=="M" & signal=="m")
    cum_match_noL_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" | partner_type[ind_new]=="M") * (match[ind_new]==1),mem))
    cum_match_H_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="H") * (match[ind_new]==1),mem))
    cum_match_M_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="M") * (match[ind_new]==1),mem))
    cum_match_L_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="L") * (match[ind_new]==1),mem))
    cum_nomatch_noL_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" | partner_type[ind_new]=="M") * (match[ind_new]==0),mem))
    cum_nomatch_H_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" ) * (match[ind_new]==0),mem))
    cum_nomatch_M_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="M" ) * (match[ind_new]==0),mem))
    cum_nomatch_L_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="L") * (match[ind_new]==0),mem))
    cum_L_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="L"),mem))
    cum_noL_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" | partner_type[ind_new]=="M"),mem))
    cum_H_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="H"),mem))
    cum_M_B[id==i&round==t] = sum(tail((partner_type[ind_new]=="M"),mem))
    
    ind_new= (ind & game=="B" & type=="M" & signal=="m")
    cum_match_noL[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" | partner_type[ind_new]=="M") * (match[ind_new]==1),mem))
    cum_match_H[id==i&round==t] = sum(tail((partner_type[ind_new]=="H") * (match[ind_new]==1),mem))
    cum_match_M[id==i&round==t] = sum(tail((partner_type[ind_new]=="M") * (match[ind_new]==1),mem))
    cum_match_L[id==i&round==t] = sum(tail((partner_type[ind_new]=="L") * (match[ind_new]==1),mem))
    cum_nomatch_noL[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" | partner_type[ind_new]=="M") * (match[ind_new]==0),mem))
    cum_nomatch_H[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" ) * (match[ind_new]==0),mem))
    cum_nomatch_M[id==i&round==t] = sum(tail((partner_type[ind_new]=="M" ) * (match[ind_new]==0),mem))
    cum_nomatch_L[id==i&round==t] = sum(tail((partner_type[ind_new]=="L") * (match[ind_new]==0),mem))
    cum_L[id==i&round==t] = sum(tail((partner_type[ind_new]=="L"),mem))
    cum_noL[id==i&round==t] = sum(tail((partner_type[ind_new]=="H" | partner_type[ind_new]=="M"),mem))
    cum_H[id==i&round==t] = sum(tail((partner_type[ind_new]=="H"),mem))
    cum_M[id==i&round==t] = sum(tail((partner_type[ind_new]=="M"),mem))
    
    cum_match[id==i&round==t] = sum(tail((match[ind_new]==1),mem))
    
  }
}


regdata = data.frame(id,
                     propose,
                     type,
                     signal,
                     round,
                     game,
                     A,
                     B,
                     cum_match_noL_A,
                     cum_match_L_A,
                     cum_nomatch_noL_A,
                     cum_nomatch_L_A,
                     cum_L_A,
                     cum_noL_A,
                     cum_match_H_A,
                     cum_match_M_A,
                     cum_nomatch_H_A,
                     cum_nomatch_M_A,
                     cum_H_A,
                     cum_M_A,
                     cum_match_noL_B,
                     cum_match_L_B,
                     cum_nomatch_noL_B,
                     cum_nomatch_L_B,
                     cum_L_B,
                     cum_noL_B,
                     cum_match_H_B,
                     cum_match_M_B,
                     cum_nomatch_H_B,
                     cum_nomatch_M_B,
                     cum_H_B,
                     cum_M_B,
                     cum_match_noL,
                     cum_match_L,
                     cum_nomatch_noL,
                     cum_nomatch_L,
                     cum_L,
                     cum_noL,
                     cum_match_H,
                     cum_match_M,
                     cum_nomatch_H,
                     cum_nomatch_M,
                     cum_H,
                     cum_M,
                     cum_match
                     )

reg1=lm(propose ~ 
          round
          +A
          # +A*round
          # +cum_match_noL_A
          # +cum_match_H_A
          # +cum_match_M_A
          # +cum_match_L_A
          # +cum_nomatch_noL_A
          # +cum_nomatch_H_A
          # +cum_nomatch_M_A
          # +cum_nomatch_L_A
          # +cum_noL_A
          # +cum_H_A
          # +cum_M_A
          # +cum_L_A
          # +cum_nomatch_H
          # +cum_nomatch_M
          # +cum_nomatch_L
          +cum_match_H
          # +cum_match_M
          +cum_match_L
          # +cum_H
          # +cum_M
          # +cum_L
          +cum_match
        , 
        data=subset(regdata, (type=="M" & signal=="m" )))

summary(reg1)

# library(plm)
# 
# freg=plm(propose ~ 
#           # round
#         # +A
#         # +A*round
#         # +cum_match_noL_A
#         +cum_match_H_A
#         +cum_match_M_A
#         +cum_match_L_A
#         # +cum_nomatch_noL_A
#         # +cum_nomatch_H_A
#         # +cum_nomatch_M_A
#         # +cum_nomatch_L_A
#         # +cum_noL_A
#         # +cum_H_A
#         # +cum_M_A
#         # +cum_L_A
#         # +cum_nomatch_H
#         # +cum_nomatch_M
#         # +cum_nomatch_L
#         # +cum_match_H
#         # +cum_match_M
#         # +cum_match_L
#         # +cum_H
#         # +cum_M
#         # +cum_L
#         # +cum_match
#         +cum_match_H_B
#         +cum_match_M_B
#         +cum_match_L_B
#         , 
#         data=subset(regdata, (type=="M" & signal=="m" )),
#         index=c("id","round"),
#         model="within"
#         )
# 
# summary(freg)

