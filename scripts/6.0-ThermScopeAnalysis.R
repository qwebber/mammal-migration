


libs <- c('data.table', 'ape', 'caper',
          'MASS', 'mvtnorm', 'ggplot2', 'MuMIn')
lapply(libs, require, character.only = TRUE)

Mamm <- fread("input/mammals.csv")

TSMamm <- readRDS("output/compTSData.RDS")

## model selection
## global model
TS1=pgls(mig~locomotion*TS+locomotion*logmass+habitat_sub+diet+abslat,data=TSMamm,lambda="ML")
##w/o logmass*locomotion
TS2=pgls(mig ~ locomotion*TS + habitat_sub+diet+abslat,data=TSMamm,lambda="ML")
##w/o TS*locomotion
TS3=pgls(mig ~ locomotion*logmass + habitat_sub+diet+abslat,data=TSMamm,lambda="ML")
## w/o habitat
TS4 = pgls(mig ~ locomotion*TS+locomotion*logmass+diet+abslat,data=TSMamm,lambda="ML")
##w/o abslat
TS5 = pgls(mig ~ locomotion*TS+locomotion*logmass+habitat_sub+diet,data=TSMamm,lambda="ML")
## w/o diet
TS6= pgls(mig ~ locomotion*TS+locomotion*logmass+habitat_sub+abslat,data=TSMamm,lambda="ML")

AIC(TS1,TS2,TS3,TS4,TS5,TS6)

##w/o habitat and diet
TS7 = pgls(mig ~ locomotion*TS+locomotion*logmass+abslat,data=TSMamm,lambda="ML")
##w/o habitat and abslat
TS8 = pgls(mig ~ locomotion*TS+locomotion*logmass+diet,data=TSMamm,lambda="ML")
##w/o habitat and logmass*locomotion
TS9 = pgls(mig ~ locomotion*TS+diet+abslat,data=TSMamm,lambda="ML")
## w/o habitat and locomotion*TS
TS10 =pgls(mig ~ locomotion*logmass+diet+abslat,data=TSMamm,lambda="ML")
##w/o diet and abslat
TS11 = pgls(mig ~ locomotion*TS+locomotion*logmass+habitat_sub,data=TSMamm,lambda="ML")
##w/o diet and logmass*locomotion
TS12 = pgls(mig ~ locomotion*TS+habitat_sub+abslat,data=TSMamm,lambda="ML")
## w/o diet and locomotion*TS
TS13 =pgls(mig ~ locomotion*logmass+habitat_sub+abslat,data=TSMamm,lambda="ML")
##w/o abslat and logmass*locomotion
TS14 = pgls(mig ~ locomotion*TS+habitat_sub+diet,data=TSMamm,lambda="ML")
## w/o abslat and locomotion*TS
TS15 =pgls(mig ~ locomotion*logmass+habitat_sub+diet,data=TSMamm,lambda="ML")

AIC(TS1,TS2,TS3,TS4,TS5,TS6,TS7,TS8,TS9,TS10,TS11,TS12,TS13,TS14,TS15)

TS16 =pgls(mig ~ locomotion*logmass ,data=TSMamm,lambda="ML")
TS17 =pgls(mig ~ locomotion*TS ,data=TSMamm,lambda="ML")
TS18 =pgls(mig ~ abslat ,data=TSMamm,lambda="ML")
TS19 =pgls(mig ~ diet ,data=TSMamm,lambda="ML")
TS20 =pgls(mig ~ habitat_sub ,data=TSMamm,lambda="ML")


TSAIC <- data.table(AICc(TS1,TS2,TS3,TS4,TS5,TS6,TS7,TS8,TS9,TS10,TS11,TS12,TS13,TS14,TS15,TS16,TS17,TS18,TS19,TS20),
                      round(Weights(AIC(TS1,TS2,TS3,TS4,TS5,TS6,TS7,TS8,TS9,TS10,TS11,TS12,TS13,TS14,TS15,TS16,TS17,TS18,TS19,TS20)),3))

TSAIC$lambda <- rbind(TS1$param[2], TS2$param[2], TS3$param[2], TS4$param[2],
                        TS5$param[2], TS6$param[2], TS7$param[2], TS8$param[2],
                        TS9$param[2], TS10$param[2], TS11$param[2], TS12$param[2], 
                        TS13$param[2], TS14$param[2], TS15$param[2])

TSAIC$model <- rbind(as.character(TS1$call[2]),as.character(TS2$call[2]),
                       as.character(TS3$call[2]),as.character(TS4$call[2]),
                       as.character(TS5$call[2]),as.character(TS6$call[2]),
                       as.character(TS7$call[2]),as.character(TS8$call[2]),
                       as.character(TS9$call[2]),as.character(TS10$call[2]),
                       as.character(TS11$call[2]),as.character(TS12$call[2]),
                       as.character(TS13$call[2]),as.character(TS14$call[2]),
                       as.character(TS15$call[2]))

TSAIC <- TSAIC[order(AICc)]

TSAIC$deltaAIC <- TSAIC$AICc - as.numeric(TSAIC[1,2])


write.csv(TSAIC, "output/TSAIC.csv")

