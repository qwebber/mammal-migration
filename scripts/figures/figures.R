

libs <- c('data.table', 'ggplot2')
lapply(libs, require, character.only = TRUE)

mammals=fread("input/mammals.csv")
str(mammals)

mammals.swim <- mammals[locomotion == "W"]
mammals.mammals.terr <- mammals[locomotion == "T"] 
mammals.fly <- mammals[locomotion == "A"] 

## FIGURE 2:
par(mfrow=c(2,3))
par(mar=c(5,4.5,2,1)+0.1)
plot(jitter(mig,0.02)~logmass,xlim=c(4,9),xlab="Log(Body Mass(g))",ylab="Probability of Migration",pch=16,las=1,data=mammals.swim,cex.axis=1.5,cex.lab=1.5)
mtext("A)",side=3,at=4.2,0.2,cex=1.2)
g=glm(mig~logmass,family=binomial,data=mammals.swim)
curve(predict(g,data.frame(logmass=x),type="resp"),add=TRUE,col="red",lwd=2)
par(mar=c(5,4.5,2,1)+0.1)
plot(jitter(mig,0.04)~logmass,xlim=c(0,7),ylab="",xlab="Log(Body Mass(g))",pch=16,data=mammals.terr,las=1,cex.lab=1.5,cex.axis=1.5)
mtext("B)",side=3,at=0.2,0.2,cex=1.2)
p=glm(mig~logmass,family=binomial,data=mammals.terr)
curve(predict(p,data.frame(logmass=x),type="resp"),add=TRUE,col="red",lwd=2)
par(mar=c(5,4.5,2,1)+0.1)
plot(jitter(mig,0.02)~logmass,xlim=c(0,3),ylab="",xlab="Log(Body Mass(g))",pch=16,las=1,data=mammals.fly,cex.axis=1.5,cex.lab=1.5)
mtext("C)",side=3,at=0.2,0.2,cex=1.2)
f=glm(mig~logmass,family=binomial,data=mammals.fly)
curve(predict(f,data.frame(logmass=x),type="resp"),add=TRUE,col="red",lwd=2)
### FIGURE ABSLAT
par(mar=c(5,4.5,2,1)+0.1)
plot(jitter(mig,0.02)~abslat,xlim=c(0,80),xlab="Absolute Latitude",ylab="Probability of Migration",pch=16,las=1,cex.lab=1.5,cex.axis=1.5,data=mammals.swim)
g=glm(mig~ abslat,family=binomial,data=mammals.swim)
curve(predict(g,data.frame(abslat =x),type="resp"),add=TRUE,col="red",lwd=2)
mtext("D)",side=3,at=1.7,0.2,cex=1.2)
par(mar=c(5,4,2,1)+0.1)
plot(jitter(mig,0.04)~ abslat,xlim=c(0,80),ylab="",xlab="Absolute Latitude",pch=16,data=mammals.terr,las=1,cex.lab=1.5,cex.axis=1.5)
p=glm(mig~ abslat,family=binomial,data=mammals.terr)
curve(predict(p,data.frame(abslat =x),type="resp"),add=TRUE,col="red",lwd=2)
mtext("E)",side=3,at=1.7,0.2,cex=1.2)
par(mar=c(5,4,2,1)+0.1)
plot(jitter(mig,0.02)~abslat,xlim=c(0,60),ylab="",xlab="Absolute Latitude",pch=16,las=1,data=mammals.fly,cex.lab=1.5,cex.axis=1.5)
f=glm(mig~ abslat,family=binomial,data=mammals.fly)
curve(predict(f,data.frame(abslat =x),type="resp"),add=TRUE,col="red",lwd=2)
mtext("F)",side=3,at=1.7,0.2,cex=1.2)



counts_swim=table(mammals.swim$habitat_sub,mammals.swim$mig)
df_swim=read.table(text="mig non_mig
1 0.63 0.18
2 0.06 0.12",header=T)


13/556
counts_mammals.terr=table(mammals.terr $habitat_sub, mammals.terr $mig)
df_mammals.terr=read.table(text="mig non_mig
1 0 0.003 ## fresh water
2 0.05 0.56 ## forest
3 0.01 0.04 ## grassland
4 0.02 0.11 ## savanna
5 0.01 0.17 ## shrubland
6 0.003 0.02",header=T) ## tundra

1/98
counts_fly=table(mammals.fly $habitat_sub, mammals.fly $mig)
df_fly=read.table(text="mig non_mig
1 0.47 0.46 ## forest
2 0.02 0 ## savanna
3 0.01 0.04",header=T) ## shrubland

## marine = darkblue
## fresh water =light blue
## shrubland = red
## grassland = orange
## forest = darkgreen
## tundra = yellow 
## savanna = purple

## FIGURE 3
par(mfrow=c(1,3))

par(mar=c(8,4.5,6,0)+0.1)
barplot(as.matrix(df_swim),ylab="Proportion of Species",ylim=c(0,1),cex.axis=1.2,las=1,cex.lab=1.6,xaxt="n",main="Swimming Mammals",col=c("darkblue","lightblue"),cex.main=1.5)
axis(side=2,at=0,labels="",tck=1)
mtext("Migratory",side=1,at=0.7,0.5,cex=1)
mtext("Non-Migratory",side=1,at=1.9,0.5,cex=1)

par(mar=c(8,4,6,0.5)+0.1)
barplot(as.matrix(df_mammals.terr),cex.axis=1.2,ylab="",ylim=c(0,1),las=1,xaxt="n",main="mammals.terrestrial Mammals",col=c("lightblue","darkgreen","orange","purple","red","yellow"),cex.main=1.5)
axis(side=2,at=0,labels="",tck=1)
mtext("Migratory",side=1,at=0.7,0.5,cex=1)
mtext("Non-Migratory",side=1,at=1.9,0.5,cex=1)

par(mar=c(8,4,6,0.5)+0.1)
barplot(as.matrix(df_fly),cex.axis=1.2,ylab="",las=1,ylim=c(0,1),xaxt="n",main="mammals.fly Mammals",col=c("darkgreen","purple","red"),cex.main=1.5)
axis(side=2,at=0,labels="",tck=1)
mtext("Migratory",side=1,at=0.7,0.5,cex=1)
mtext("Non-Migratory",side=1,at=1.9,0.5,cex=1)

legend("topright",legend=c("Marine","Fresh Water","Forest","Grassland","Savanna","Shrubland","Tundra"),fill=c("darkblue","lightblue","darkgreen","orange","purple","red","yellow"),bty="n",cex=1.2)



counts_swim_diet=table(mammals.swim$diet,mammals.swim$mig)
df_swim_diet=read.table(text="mig non_mig
1 53 20 ## carnivore
2 3 3 ## herbivore
3 0 2",header=T) ## omnivore

counts_mammals.terr_diet=table(mammals.terr $diet, mammals.terr $mig)
df_mammals.terr_diet=read.table(text="mig non_mig
1 8 60 ## carnivore
2 0 3 ## frugivore
3 46 197 ## herbivore
4 0 94 ## insectivore
5 0 148",header=T)## omnivore

counts_fly_diet=table(mammals.fly $diet, mammals.fly $mig)
df_fly_diet=read.table(text="mig non_mig
1 1 4 ## carnivore
2 10 6 ## frugivore
3 0 1 ## herbivore
4 38 36 ## insectivore
5 0 2",header=T) ## omnivore



## FIGURE 4
par(mfrow=c(1,3))

par(mar=c(8,4.5,6,0)+0.1)
barplot(as.matrix(df_swim_diet),ylab="Number of Species",ylim=c(0,100),cex.axis=1.2,las=1,cex.lab=1.6,xaxt="n",main="Swimming Mammals",col=c("red","darkgreen","blue"),cex.main=1.5)
axis(side=2,at=0,labels="",tck=1)
mtext("Migratory",side=1,at=0.7,0.5,cex=1)
mtext("Non-Migratory",side=1,at=1.9,0.5,cex=1)

par(mar=c(8,4,6,0.5)+0.1)
barplot(as.matrix(df_mammals.terr_diet),cex.axis=1.2,ylab="",ylim=c(0,500),las=1,xaxt="n",main="mammals.terrestrial Mammals",col=c("red","orange","darkgreen","purple","blue"),cex.main=1.5)
axis(side=2,at=0,labels="",tck=1)
mtext("Migratory",side=1,at=0.7,0.5,cex=1)
mtext("Non-Migratory",side=1,at=1.9,0.5,cex=1)

par(mar=c(8,4,6,0.5)+0.1)
barplot(as.matrix(df_fly_diet),cex.axis=1.2,ylab="",las=1,ylim=c(0,100),xaxt="n",main="mammals.fly Mammals",col=c("red","orange","darkgreen","purple","blue"),cex.main=1.5)
axis(side=2,at=0,labels="",tck=1)
mtext("Migratory",side=1,at=0.7,0.5,cex=1)
mtext("Non-Migratory",side=1,at=1.9,0.5,cex=1)

legend("topright",legend=c("Carnivore","Frugivore","Herbivore","Insectivore","Omnivore"),fill=c("red","orange","darkgreen","purple","blue"),bty="n",cex=1.2)



## 0 = non-migratory, homeothermic
##1 =  migratory, homeothermic
##2 = non-migratory, heterothermic
##3 = migratory, heterothermic 
3/556
counts_mammals.terr=table(mammals.terr $habitat_sub, mammals.terr $mig.het1)
counts_mammals.terr=table(mammals.terr $diet, mammals.terr $mig.het1)

df_mammals.terr=read.table(text="non_mig_het mig het het_mig
1 0.43 0.05 0.13 0.003  ## forest
2 0.02 0.01 0.02 0 ## grassland
3 0.09 0.015 0.015 0 ## savanna
4 0.08 0.01 0.09 0  ## shrubland
5 0.018 0.003 0.005 0 ## tundra
6 0.003 0 0 0 ## fresh water",header=T) ## freshwater

boxplot(logmass ~mig.het1,mammals.terr)

38/98
counts_fly=table(mammals.fly $habitat_sub, mammals.fly $mig.het1)
counts_fly=table(mammals.fly $diet, mammals.fly $mig.het1)

df_fly=read.table(text="non_mig_het mig het het_mig
1 0.12 0.08 0.34 0.39 ## forest
2 0 0 0 0.02 ## savanna
3 0 0 0.04 0.01",header=T) ## shrubland

## fresh water
## forest = darkgreen
## shrubland = lightgreen
## grassland = green
## tundra = darkblue
## savanna = blue

## Figure 5: 
par(mfrow=c(1,2))
col=c("grey95","grey80","grey60","grey40")
boxplot(logmass ~mig.het1,mammals.terr,ylim=c(0,7),ylab="log(Body Mass(g))",cex.lab=1.2,cex.axis=1.2,las=1,xaxt="n",col=col,main="Running mammals",cex.main=1.5)
axis(side=1,at=1,"Non-Migratory",cex.axis=0.8)
mtext(side=1,line=2,at=1,"Homeothermic",cex=0.8)
axis(side=1,at=2,"Migratory",cex.axis=0.8)
mtext(side=1,line=2,at=2,"Homeothermic",cex=0.8)
axis(side=1,at=3,"Non-Migratory",cex.axis=0.8)
mtext(side=1,line=2,at=3,"Heterothermic",cex=0.8)
axis(side=1,at=4,"Migratory",cex.axis=0.8)
mtext(side=1,line=2,at=4,"Heterothermic",cex=0.8)
mtext(side=1,line=-22,at=0.65,"A)",cex=1.5)

boxplot(logmass ~mig.het1,mammals.fly,ylim=c(0,4),ylab="log(Body Mass(g))",cex.lab=1.2,cex.axis=1.2,las=1,xaxt="n",col=col,main="mammals.fly mammals",cex.main=1.5)
axis(side=1,at=1,"Non-Migratory",cex.axis=0.8)
mtext(side=1,line=2,at=1,"Homeothermic",cex=0.8)
axis(side=1,at=2,"Migratory",cex.axis=0.8)
mtext(side=1,line=2,at=2,"Homeothermic",cex=0.8)
axis(side=1,at=3,"Non-Migratory",cex.axis=0.8)
mtext(side=1,line=2,at=3,"Heterothermic",cex=0.8)
axis(side=1,at=4,"Migratory",cex.axis=0.8)
mtext(side=1,line=2,at=4,"Heterothermic",cex=0.8)
mtext(side=1,line=-22,at=0.65,"B)",cex=1.5)


## FIGURE 4
par(mfrow=c(2,2))

par(mar=c(3,4,3,2)+0.1)
barplot(as.matrix(df_mammals.terr),cex.axis=1.2,ylab="Proportion of Species",ylim=c(0,1),las=1,xaxt="n",col=c("darkgreen","lightgreen","green","darkblue","blue","lightblue"),cex.lab=1.2)
axis(side=2,at=0,labels="" ,tck=1)
mtext("1",side=1,at=0.7,0.5,cex=1)
mtext("2",side=1,at=1.9,0.5,cex=1)
mtext("3",side=1,at=3.1,0.5,cex=1)
mtext("4",side=1,at=4.3,0.5,cex=1)
mtext("A)",side=3,at=0.5,0.5,cex=1.4)

par(mar=c(3,4,3,2)+0.1)
barplot(as.matrix(df_fly),cex.axis=1.2,ylab="",las=1,ylim=c(0,1),xaxt="n",col=c("darkgreen","green","darkblue"))
axis(side=2,at=0,labels="",tck=1)
mtext("1",side=1,at=0.7,0.5,cex=1)
mtext("2",side=1,at=1.9,0.5,cex=1)
mtext("3",side=1,at=3.1,0.5,cex=1)
mtext("4",side=1,at=4.3,0.5,cex=1)
mtext("B)",side=3,at=0.5,0.5,cex=1.4)

legend("topright",legend=c("Forest","Grassland","Savanna","Shrubland","Tundra","Fresh Water"),fill=c("darkgreen","lightgreen","green","darkblue","blue","lightblue"),bty="n",cex=1)


counts_mammals.terr_diet=table(mammals.terr $diet, mammals.terr $mig.het1)
df_mammals.terr_diet=read.table(text="non_mig_het mig het het_mig
1 0.10 0.01 0.009 0.003 ## carnivore
2 0.003 0 0.0015 0 ## frugivore
3 0.26 0.08 0.09 0 ## herbivore
4 0.08 0 0.08 0 ## insectivore
5 0.20 0 0.06 0 ",header=T)## omnivore


counts_fly_diet=table(mammals.fly $diet, mammals.fly $mig.het1)
df_fly_diet=read.table(text="non_mig_het mig het het_mig
1 0.04 0 0 0.01 ## carnivore
2 0.02 0.08 0.04 0.02## frugivore
3 0 0 0.01 0 ## herbivore
4 0.04 0 0.32 0.39 ## insectivore
5 0.02 0 0 0",header=T) ## omnivore

## red = carnivore
## orange = frugivore
## yellow = herbivore
## lightred = insectivore 
## omnivore = light orange


par(mar=c(3,4,3,2)+0.1)
barplot(as.matrix(df_mammals.terr_diet),cex.axis=1.2,ylab="Proportion of Species",ylim=c(0,1),las=1,xaxt="n",col=c("red","orange","yellow","pink","magenta"),cex.lab=1.2)
axis(side=2,at=0,labels="",tck=1)
mtext("1",side=1,at=0.7,0.5,cex=1)
mtext("2",side=1,at=1.9,0.5,cex=1)
mtext("3",side=1,at=3.1,0.5,cex=1)
mtext("4",side=1,at=4.3,0.5,cex=1)
mtext("C)",side=3,at=0.5,0.5,cex=1.4)

par(mar=c(3,4,3,2)+0.1)
barplot(as.matrix(df_fly_diet),cex.axis=1.2,ylab="",las=1,ylim=c(0,1),xaxt="n",col=c("red","orange","yellow","pink","magenta"))
axis(side=2,at=0,labels="",tck=1)
mtext("1",side=1,at=0.7,0.5,cex=1)
mtext("2",side=1,at=1.9,0.5,cex=1)
mtext("3",side=1,at=3.1,0.5,cex=1)
mtext("4",side=1,at=4.3,0.5,cex=1)
mtext("D)",side=3,at=0.5,0.5,cex=1.4)
legend("topright",legend=c("Carnivore","Frugivore","Herbivore","Insectivore","Omnivore"),fill=c("red","orange","yellow","pink","magenta"),bty="n",cex=1)

boxplot(abslat~mig.het1,mammals.terr)