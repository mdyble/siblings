############
### Geographic distribution
############

## Import Human.data CSV

## ESM figure 

Ethno_data = Human.data[which(Human.data$Cat!="aDNA"),]
Humans_prop = mean((Ethno_data$Full/Ethno_data$Total)*100)

Africa = Ethno_data$Full[which(Ethno_data$Continent=="Africa")]/Ethno_data$Total[which(Ethno_data$Continent=="Africa")]*100
Americas = Ethno_data$Full[which(Ethno_data$Continent=="Americas")]/Ethno_data$Total[which(Ethno_data$Continent=="Americas")]*100
Eurasia =Ethno_data$Full[which(Ethno_data$Continent=="Eurasia")]/Ethno_data$Total[which(Ethno_data$Continent=="Eurasia")]*100
Oceania =Ethno_data$Full[which(Ethno_data$Continent=="Oceania")]/Ethno_data$Total[which(Ethno_data$Continent=="Oceania")]*100

mean(c(Americas,Africa,Eurasia,Oceania))
mean(Americas)
mean(c(Africa,Eurasia,Oceania))

Ns = c(length(Africa),length(Americas),length(Eurasia),length(Oceania))
labs = c("Africa","Americas","Eurasia","Oceania")

gaps = c(1,2,3,4)
boxplot(Africa,Americas,Eurasia,Oceania,
        frame=F,
        at = gaps,
        ylab="Proportion full sibs",
        xaxt="n",
        whisklty=1,
        boxwex=.7,
        ylim=c(0,100),col=c("grey90","grey90","grey90","grey90"))

axis(1, at = gaps, labels = FALSE)
text(gaps, -10, labs, srt = 0, xpd = TRUE,cex=.9)
text(gaps, -18, Ns, xpd = TRUE,cex=.9)
text(0.3, -18, expression(italic("n =")), xpd = TRUE,cex=.9)
overall_mean = mean(c(Africa,Americas,Eurasia,Oceania))
abline(h = mean(overall_mean), col = "red", lwd = 2,lty=2)



### Adjusted mean
set.seed(1)
sims = rep(NA,times=10000)
for(i in 1:10000){
  sims[i] = mean(c(Eurasia,(sample(Africa,10)),(sample(Eurasia,10)),(sample(Oceania,10))))
}

mean(sims)
sims[order(sims)][250]
sims[order(sims)][9750]

#### end ####



