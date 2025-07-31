############
## Human Monogamy in Mammalian Context'
## Modelling full sibling proportions
############

## Set working directory to folder containing human.data and nonhuman.data files

#setwd("C:/Users/md479/OneDrive - University of Cambridge/RESEARCH/Sibling Props paper")
setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/RESEARCH/Sibling Props paper")


## Figure 2a

set.seed(1) # set seed
EPPs = rep((0:100/100),each=100); runs = length(EPPs)  # EPP values to test (this is 0 to 1, intervals of 0.01, 100 times each and take the mean)
meta_epp = meta_full = meta_patH = meta_matH = rep(NA,times=runs) # somewhere to save the results 

for(run in 1:runs){ # Loop through runs 
EPP = meta_epp[run] = EPPs[run] # set the EPP for the run
Nf = Nm = 24 # Set group size
per_female = 4; Off = Nf*per_female  # Number of offspring per female
Moth = rep(1:Nf,each=per_female) ## assign all offspring a mother
Fath = Moth + Nf ## begin with exclusive monogamy 
rewire = which(sample(c(0,1),Off,prob = c((1-EPP),EPP),replace = T)==1) # randomly sample offspring-father links with probability EPP
Fath[rewire] = sample((Nf+1):(Nf+Nm),length(rewire),replace = T) # resample those fathers

# Now cycle through an estimate the number of siblings by type
Full = MatH = PatH = 0 # empty counters
for(i in 1:Off){
  for(j in 1:Off){
    if(i!=j){
    if(Moth[i] == Moth[j] & Fath[i] == Fath[j]) Full = Full+1
    if(Moth[i] == Moth[j] & Fath[i] != Fath[j]) MatH = MatH+1
    if(Moth[i] != Moth[j] & Fath[i] == Fath[j]) PatH = PatH+1
    }
  }
}
meta_full[run] = Full/2 # all dyads get counted twice, so half the values
meta_patH[run] = PatH/2
meta_matH[run] = MatH/2
}

## Summarise results
prob_full = meta_full/(meta_full+meta_matH+meta_patH)*100
prob_matH = meta_matH/(meta_full+meta_matH+meta_patH)*100
prob_patH = meta_patH/(meta_full+meta_matH+meta_patH)*100
res = cbind.data.frame(meta_epp,meta_full,meta_matH,meta_patH,prob_full,prob_matH,prob_patH)

res_summary = aggregate(res, list(res$meta_epp), mean)
res_summary


## plot results from simulations
par(mfrow = c(1, 2))
plot(res$meta_epp*100,res$prob_full,ylim=c(0,100),pch=16,cex=0.5,col="grey80",frame=F,ylab="% full siblings",xlab="% extra-pair paternity")
points(res_summary$meta_epp*100,res_summary$prob_full,type='l',lwd=2)


## ## ## 
####  Figure 2b Estimate EPP in datasets
## ## ## 

Human.data <- read.csv("Human data.csv")
Nonhumandata <- read.csv("nonhuman data.csv")

# take observed sibling values and match them up with sibling values from the simulation
# then take the average of the EPP values that produced those sibling proportions and take this
# as our estimated EPP for that population

# 1) do this for humans 
obs_full_prop = Human.data$Full/(Human.data$MaternalHalf+Human.data$PaternalHalf+Human.data$Full)
EPP_est_human = rep(NA,times=length(obs_full_prop))
for(i in 1:length(obs_full_prop)){
  upper = (round(obs_full_prop[i],2)+0.02)*100 # this is taking values plus or minus 2 percentage points around the observed valye for that population
  lower = (round(obs_full_prop[i],2)-0.02)*100
  EPP_est_human[i] = mean(res$meta_epp[
    which(res$prob_full<=upper & res$prob_full>=lower)
  ])
}


## 2) do for nonhuman monogamous species
Mono_full_prop = Nonhumandata$PropFull[which(Nonhumandata$SocialMonogamy=="Y")]
EPP_est_mono = rep(NA,times=length(Mono_full_prop))
for(i in 1:length(Mono_full_prop)){
  upper = (round(Mono_full_prop[i],2)+2)
  lower = (round(Mono_full_prop[i],2)-2)
  EPP_est_mono[i] = mean(res$meta_epp[which(res$prob_full<=upper & res$prob_full>=lower)])
}


## 3) do for nonhuman non-monogamous species
NonMono_full_prop = Nonhumandata$PropFull[which(Nonhumandata$SocialMonogamy=="N")]
EPP_est_nonmono = rep(NA,times=length(NonMono_full_prop))
for(i in 1:length(NonMono_full_prop)){
  upper = (round(NonMono_full_prop[i],2)+2)
  lower = (round(NonMono_full_prop[i],2)-2)
  EPP_est_nonmono[i] = mean(res$meta_epp[which(res$prob_full<=upper & res$prob_full>=lower)])
}


## plot estimated EPP rates
boxplot(EPP_est*100,EPP_est_mono*100,EPP_est_nonmono*100,
        whisklty=1,
        ylab="extra-pair paternity (%)",
        ylim=c(0,100),
        names = c("Human","Monog.","Non-monog."),
        boxwex=.7,
        frame=F,
        col=c("grey90","orange","pink")
        )
text(0.3, -30, expression(italic("n =")), xpd = TRUE,cex=.9)
text(1, -30, length(EPP_est), xpd = TRUE,cex=.9)
text(2, -30, length(EPP_est_mono), xpd = TRUE,cex=.9)
text(3, -30, length(EPP_est_nonmono), xpd = TRUE,cex=.9)

## Save plot as 8.2 x 4.5inches


