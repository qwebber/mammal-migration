


libs <- c('data.table', 'ape', 'caper',
          'MASS', 'mvtnorm', 'ggplot2', 'MuMIn')
lapply(libs, require, character.only = TRUE)

Mamm <- fread("input/mammals.csv")

TSFly <- readRDS("output/compTSFlyData.RDS")

## model selection
## global model
TS1=pgls(mig~TS+logmass+diet+abslat,data=TSFly,lambda="ML")
##w/o logmass
TS2=pgls(mig ~ TS+diet+abslat,data=TSFly,lambda="ML")
##w/o TS
TS3=pgls(mig ~ logmass+diet+abslat,data=TSFly,lambda="ML")
##w/o abslat
TS4 = pgls(mig ~ TS+logmass+diet,data=TSFly,lambda="ML")
## w/o diet
TS5= pgls(mig ~ TS+logmass+abslat,data=TSFly,lambda="ML")

AIC(TS1,TS2,TS3,TS4,TS5)

##w/o diet and logmass
TS6 = pgls(mig ~ TS+abslat,data=TSFly,lambda="ML")
##w/o diet and abslat
TS7 = pgls(mig ~ TS+logmass,data=TSFly,lambda="ML")
##w/o diet and TS
TS8 = pgls(mig ~ logmass+abslat,data=TSFly,lambda="ML")

TS9 = pgls(mig ~ TS + diet,data=TSFly,lambda="ML")


AIC(TS1,TS2,TS3,TS4,TS5,TS6,TS7,TS8,TS9)


TSAIC <- data.table(AICc(TS1,TS2,TS3,TS4,TS5,TS6,TS7,TS8),
                      round(Weights(AIC(TS1,TS2,TS3,TS4,TS5,TS6,TS7,TS8)),3))

TSAIC$lambda <- rbind(TS1$param[2], TS2$param[2], TS3$param[2], TS4$param[2],
                      TS5$param[2], TS6$param[2], TS7$param[2], TS8$param[2])

TSAIC$model <- rbind(as.character(TS1$call[2]),as.character(TS2$call[2]),
                     as.character(TS3$call[2]),as.character(TS4$call[2]),
                     as.character(TS5$call[2]),as.character(TS6$call[2]),
                     as.character(TS7$call[2]),as.character(TS8$call[2]))

TSAIC <- TSAIC[order(AICc)]

TSAIC$deltaAIC <- TSAIC$AICc - as.numeric(TSAIC[1,2])


write.csv(TSAIC, "output/TSFlyAIC.csv")

