## ## ## ## ## ## ## ## ## ## ##
## Human monogamy in mammalian context
## Code for Figure 1
## ## ## ## ## ## ## ## ## ## ##

require(Ternary) # Package for the plots

## Import Human.data and nonhuman.data

#### FIGURE 1a
par(mfrow = c(1, 1))
TernaryPlot(alab = "Full sib fraction\u2192", blab = "Paternal sib fraction \u2192", clab = "\u2190 Maternal sib fraction",
            main = "", point = "up", lab.cex = 0.8, grid.minor.lines = 0, grid.lty = "solid", col = "white", grid.col = "grey80", axis.col = "black", ticks.col = "black",axis.rotate = F,padding = 0.1)

## Add hunter-gatherer data point
HG = Human.data[which(Human.data$Cat=="HG"),]
data_points <- list(xx = c(HG$Full[1], HG$PaternalHalf[1], HG$MaternalHalf[1])) 
for(i in 2:nrow(HG)){data_points[[i]] = c(HG$Full[i], HG$PaternalHalf[i], HG$MaternalHalf[i])}
AddToTernary(graphics::points, data_points, pch = 21, cex = 1.2, bg = "grey90")

## Add horticulturalist data points
Horti = Human.data[which(Human.data$Cat=="Horticultural"),]
data_points <- list(xx = c(Horti$Full[1], Horti$PaternalHalf[1], Horti$MaternalHalf[1])) 
for(i in 2:nrow(Horti)){data_points[[i]] = c(Horti$Full[i], Horti$PaternalHalf[i], Horti$MaternalHalf[i])}
AddToTernary(graphics::points, data_points, pch = 23, cex = 1.2, bg = "grey90")

## Add agriculturalist data points
Agri = Human.data[which(Human.data$Cat=="Agricultural"),]
data_points <- list(xx = c(Agri$Full[1], Agri$PaternalHalf[1], Agri$MaternalHalf[1])) 
for(i in 2:nrow(Agri)){data_points[[i]] = c(Agri$Full[i], Agri$PaternalHalf[i], Agri$MaternalHalf[i])}
AddToTernary(graphics::points, data_points, pch = 22, cex = 1.2, bg = "grey90")

# Add pastoralist data points
Pasto = Human.data[which(Human.data$Cat=="Pastoral"),]
data_points <- list(xx = c(Pasto$Full[1], Pasto$PaternalHalf[1], Pasto$MaternalHalf[1])) 
for(i in 2:nrow(Pasto)){data_points[[i]] = c(Pasto$Full[i], Pasto$PaternalHalf[i], Pasto$MaternalHalf[i])}
AddToTernary(graphics::points, data_points, pch = 24, cex = 1.2, bg = "grey90")

## Add non-monogamous mammals
Nonmon = nonhuman.data[which(nonhuman.data$SocialMonogamy=="N"),]
data_points <- list(xx = c(Nonmon[1,4], Nonmon[1,5], Nonmon[1,6])) 
for(i in 2:nrow(Nonmon)){data_points[[i]] = c(Nonmon[i,4], Nonmon[i,5], Nonmon[i,6])}
AddToTernary(graphics::points, data_points, pch = 21, cex = 2, bg ="orange")
AddToTernary(text, data_points, (Nonmon$Code), cex = 0.5, font = 2,col="black")

## Add monogamous species
Monog = nonhuman.data[which(nonhuman.data$SocialMonogamy=="Y"),]
data_points <- list(xx = c(Monog[1,4], Monog[1,5], Monog[1,6])) 
for(i in 2:nrow(Monog)){data_points[[i]] = c(Monog[i,4], Monog[i,5], Monog[i,6])}
AddToTernary(graphics::points, data_points, pch = 21, cex = 2, bg = "pink")
AddToTernary(text, data_points, (Monog$Code), cex = .5, font = 2,col="black")

### Add human aDNA data
aDNA = Human.data[which(Human.data$Cat=="aDNA"),]
data_points <- list(xx = c(aDNA$Full[1], aDNA$PaternalHalf[1], aDNA$MaternalHalf[1])) 
for(i in 2:nrow(aDNA)){data_points[[i]] = c(aDNA$Full[i], aDNA$PaternalHalf[i], aDNA$MaternalHalf[i])}
AddToTernary(graphics::points, data_points, pch = 22, cex = 2, bg = "lightblue")
AddToTernary(text, data_points, (aDNA$Society), cex = .5, font = 2,col="black")

## Save as 6x4inches





#### Figure 1b
Humans_prop = mean((Human.data$Full/Human.data$Total)*100)
Ancient = Human.data$Full[which(Human.data$Cat=="aDNA")]/Human.data$Total[which(Human.data$Cat=="aDNA")]*100
Horticultural = Human.data$Full[which(Human.data$Cat=="Horticultural")]/Human.data$Total[which(Human.data$Cat=="Horticultural")]*100
HG = Human.data$Full[which(Human.data$Cat=="HG")]/Human.data$Total[which(Human.data$Cat=="HG")]*100
Agri =Human.data$Full[which(Human.data$Cat=="Agricultural")]/Human.data$Total[which(Human.data$Cat=="Agricultural")]*100
Past =Human.data$Full[which(Human.data$Cat=="Pastoral")]/Human.data$Total[which(Human.data$Cat=="Pastoral")]*100
Mono_prop = Monog$PropFull
Nonmon_prop = Nonmon$PropFull
Ns = c(length(Ancient),length(Horticultural),length(HG),length(Agri),length(Past),length(Nonmon_prop),length(Mono_prop))

labs = c("Ancient","Horticul.","HG","Agricultural","Pastoral","Non-monogamous","Monogamous")

gaps = c(1,2,3,4,5,6.6,7.6)
boxplot(Ancient,Horticultural,HG,Agri,Past,Nonmon_prop,Mono_prop,
        frame=F,
        at = gaps,
        ylab="% full sibs",
        xaxt="n",
        whisklty=1,
        boxwex=.7,
        ylim=c(0,100),col=c("lightblue","grey90","grey90","grey90","grey90","orange","pink"))

axis(1, at = gaps, labels = FALSE)
text(gaps, -10, labs, srt = 0, xpd = TRUE,cex=.9)
text(gaps, -18, Ns, xpd = TRUE,cex=.9)
text(0.3, -18, expression(italic("n =")), xpd = TRUE,cex=.9)
text(2.5, 110, "Humans", xpd = TRUE,cex=.9)
text(7.1, 110, "Other Mammals", xpd = TRUE,cex=.9)

# save as 6x5inches

