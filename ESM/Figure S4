############
## 'Human Monogamy in Mammalian Context'
## Parameter exploration for the EPP vs full sibling simulations
############

set.seed(1) # set seed
depth = 5 # how many runs per simulations (100 in the paper, 5 here for speed)

### ### ### 
### 1) explore effect of offspring number on results 
### ### ### 

parity = c(2,4,16) # List of the number of offspring to be tested
EPPs = rep((0:50/50),each=depth); runs = length(EPPs)  ## EPP values to test (this is 0 to 1, intervals of 0.02, 100 times each and take the mean)
meta_res1 = as.data.frame(matrix(NA,51,length(parity))) # somewhere to save the results 

for(p in 1:length(parity)){ # loop through N offspring 

  meta_epp = meta_full = meta_matH = meta_patH = rep(NA,times=runs) # somewhere to save the data 
  
  for(run in 1:runs){
    
    EPP = meta_epp[run] = EPPs[run]
    Nf = Nm = 20 # Assume an equal sex ratio overall
    Off = Nf*parity[p] # How many offspring in total
    
    Moth = rep(1:Nf,each=parity[p]) ## assign all offspring a mother
    Fath = Moth + Nf ## begin with excludive monogamy 
    
    rewire = which(sample(c(0,1),Off,prob = c((1-EPP),EPP),replace = T)==1) # randomly sample offspring-father links with probability EPP
    Fath[rewire] = sample((Nf+1):(Nf+Nm),length(rewire),replace = T) # resample those fathers
    
    # Now cycle through an estimate the number of siblings by type
    Full = MatH = PatH = 0
    for(i in 1:Off){
      for(j in 1:Off){
        if(i!=j){
          if(Moth[i] == Moth[j] & Fath[i] == Fath[j]) Full = Full+1
          if(Moth[i] == Moth[j] & Fath[i] != Fath[j]) MatH = MatH+1
          if(Moth[i] != Moth[j] & Fath[i] == Fath[j]) PatH = PatH+1
        }
      }
    }
    meta_full[run] = Full/2;    meta_patH[run] = PatH/2; meta_matH[run] = MatH/2
  }
  
  ## Summarise and record the results
  prob_full = meta_full/(meta_full+meta_matH+meta_patH)*100
  prob_matH = meta_matH/(meta_full+meta_matH+meta_patH)*100
  prob_patH = meta_patH/(meta_full+meta_matH+meta_patH)*100
  res = cbind.data.frame(meta_epp,meta_full,meta_matH,meta_patH,prob_full,prob_matH,prob_patH)
  res_summary = aggregate(res$prob_full, list(res$meta_epp), mean)
  meta_res1[,p] = res_summary$x
  
}





### ### ### 
### 2) explore effect of female number on results 
### ### ### 

Nfs = c(5,10,20) # List of the number of females to be tested
EPPs = rep((0:50/50),each=depth); runs = length(EPPs)  ## EPP values to test (this is 0 to 1, intervals of 0.02, 100 times each and take the mean)
meta_res2 = as.data.frame(matrix(NA,51,length(Nfs))) # somewhere to save the results 

for(p in 1:length(Nfs)){ # loop through N offspring 
  
  meta_epp = meta_full = meta_matH = meta_patH = rep(NA,times=runs) # somewhere to save the data 
  
  for(run in 1:runs){
    
    k = 4
    EPP = meta_epp[run] = EPPs[run]
    Nf = Nm = Nfs[p] # Assume an equal sex ratio overall
    Off = Nf*4 # How many offspring in total
    Moth = rep(1:Nf,each=k) ## assign all offspring a mother
    Fath = Moth + Nf ## begin with exclusive monogamy 
    
    
    
    rewire = which(sample(c(0,1),Off,prob = c((1-EPP),EPP),replace = T)==1) # randomly sample offspring-father links with probability EPP
    Fath[rewire] = sample((Nf+1):(Nf+Nm),length(rewire),replace = T) # resample those fathers
    
    # Now cycle through an estimate the number of siblings by type
    Full = MatH = PatH = 0
    for(i in 1:Off){
      for(j in 1:Off){
        if(i!=j){
          if(Moth[i] == Moth[j] & Fath[i] == Fath[j]) Full = Full+1
          if(Moth[i] == Moth[j] & Fath[i] != Fath[j]) MatH = MatH+1
          if(Moth[i] != Moth[j] & Fath[i] == Fath[j]) PatH = PatH+1
        }
      }
    }
    meta_full[run] = Full/2;    meta_patH[run] = PatH/2; meta_matH[run] = MatH/2
  }
  
  ## Summarise and record the results
  prob_full = meta_full/(meta_full+meta_matH+meta_patH)*100
  prob_matH = meta_matH/(meta_full+meta_matH+meta_patH)*100
  prob_patH = meta_patH/(meta_full+meta_matH+meta_patH)*100
  res = cbind.data.frame(meta_epp,meta_full,meta_matH,meta_patH,prob_full,prob_matH,prob_patH)
  res_summary = aggregate(res$prob_full, list(res$meta_epp), mean)
  meta_res2[,p] = res_summary$x
  
}






### ### ### 
### 3) explore effect of extra-group paternity
### ### ### 

extragroup = c(0,0.5,1)
EPPs = rep((0:50/50),each=depth)
meta_res3 = as.data.frame(matrix(NA,51,length(extragroup)))

for(p in 1:length(extragroup)){ # loop through N offspring 
  
  meta_epp = meta_full = meta_matH = meta_patH = rep(NA,times=runs) # somewhere to save the data 
  
  for(run in 1:runs){
    
    out_group = extragroup[p]
    k = 4
    EPP = meta_epp[run] = EPPs[run]
    Nf = Nm = 20 # Assume an equal sex ratio overall
    Off = Nf*4 # How many offspring in total
    Moth = rep(1:Nf,each=k) ## assign all offspring a mother
    Fath = Moth + Nf ## begin with exclusive monogamy 
    
    rewire = which(sample(c(0,1),Off,prob = c((1-EPP),EPP),replace = T)==1) # randomly sample offspring-father links with probability EPP
    Fath[rewire] = sample((Nf+1):(Nf+Nm),length(rewire),replace = T) # resample those fathers
    
    ## some of these are then overwritten with random out group male
    replace_out = rewire[which(sample(c(0,1),length(rewire),prob = c((1-out_group),out_group),replace = T)==1)]
    Fath[replace_out] = sample(1000:(1000+length(replace_out)),length(replace_out),replace = F)
    
    # Now cycle through an estimate the number of siblings by type
    Full = MatH = PatH = 0
    for(i in 1:Off){
      for(j in 1:Off){
        if(i!=j){
          if(Moth[i] == Moth[j] & Fath[i] == Fath[j]) Full = Full+1
          if(Moth[i] == Moth[j] & Fath[i] != Fath[j]) MatH = MatH+1
          if(Moth[i] != Moth[j] & Fath[i] == Fath[j]) PatH = PatH+1
        }
      }
    }
    meta_full[run] = Full/2;    meta_patH[run] = PatH/2; meta_matH[run] = MatH/2
  }
  
  ## Summarise and record the results
  prob_full = meta_full/(meta_full+meta_matH+meta_patH)*100
  prob_matH = meta_matH/(meta_full+meta_matH+meta_patH)*100
  prob_patH = meta_patH/(meta_full+meta_matH+meta_patH)*100
  res = cbind.data.frame(meta_epp,meta_full,meta_matH,meta_patH,prob_full,prob_matH,prob_patH)
  res_summary = aggregate(res$prob_full, list(res$meta_epp), mean)
  meta_res3[,p] = res_summary$x
  
}



### Plot results

par(mfrow = c(1, 3)) # To save three plots

plot(res$meta_epp*100,res$prob_full,ylim=c(0,100),pch="",main="vary parity",cex=0.5,col="grey80",frame=F,ylab="% full siblings",xlab="% extra-pair paternity")
points((0:50/50)*100,meta_res1[,1],type='l',lwd=2,col="red")
points((0:50/50)*100,meta_res1[,2],type='l',lwd=2,col="orange")
points((0:50/50)*100,meta_res1[,3],type='l',lwd=2,col="forestgreen")

plot(res$meta_epp*100,res$prob_full,ylim=c(0,100),pch="",main="vary Nf",cex=0.5,col="grey80",frame=F,ylab="% full siblings",xlab="% extra-pair paternity")
points((0:50/50)*100,meta_res2[,1],type='l',lwd=2,col="red")
points((0:50/50)*100,meta_res2[,2],type='l',lwd=2,col="orange")
points((0:50/50)*100,meta_res2[,3],type='l',lwd=2,col="forestgreen")

plot(res$meta_epp*100,res$prob_full,ylim=c(0,100),pch="",cex=0.5,main="extra-group paternity",col="grey80",frame=F,ylab="% full siblings",xlab="% extra-pair paternity")
points((0:50/50)*100,meta_res3[,1],type='l',lwd=2,col="red")
points((0:50/50)*100,meta_res3[,2],type='l',lwd=2,col="orange")
points((0:50/50)*100,meta_res3[,3],type='l',lwd=2,col="forestgreen")






