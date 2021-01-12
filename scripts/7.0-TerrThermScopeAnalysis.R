


libs <- c('data.table', 'ape', 'caper',
          'MASS', 'mvtnorm', 'ggplot2', 'MuMIn')
lapply(libs, require, character.only = TRUE)

Mamm <- fread("input/mammals.csv")
TS = fread("input/thermoregulatory_scope.csv",header = T)
TS <- merge(Mamm, TS, by = "species")

TSTerr <- readRDS("output/compTSterrData.RDS")



TSterr <- TS[locomotion == "T" & habitat_sub != "GTU" & habitat_sub != "TRS" & habitat_sub != "FWW"]
## model selection
## global model
### global model
TS1=pgls(mig~TS + logmass + diet + abslat + habitat_sub,data=TSTerr,lambda="ML")
TS1glm <- glm(mig ~ TS + logmass + diet + abslat + habitat_sub, 
              data=TSterr, 
              family = "binomial")
car::vif(TS1glm) ## remove habitat

TS1.1glm <- glm(mig ~ TS + logmass + diet + abslat, 
              data=TSterr, 
              family = "binomial")
car::vif(TS1.1glm) ## remove habitat

TS2 = pgls(mig ~ TS*diet + logmass + abslat,data=TSTerr,lambda="ML")

TS3 = pgls(mig ~ TS*habitat_sub + logmass + abslat,data=TSTerr,lambda="ML")

TS4 = pgls(mig ~ TS*diet + logmass,data=TSTerr,lambda="ML")

TS5 = pgls(mig ~ TS*diet + latitude,data=TSTerr,lambda="ML")

TS6 = pgls(mig ~ TS*habitat_sub + logmass,data=TSTerr,lambda="ML")

TS7 = pgls(mig ~ TS*habitat_sub + latitude,data=TSTerr,lambda="ML")

TS8 = pgls(mig ~ TS*diet,data=TSTerr,lambda="ML")

TS9 = pgls(mig ~ TS*habitat_sub,data=TSTerrHab,lambda="ML")


TSAIC <- data.table(AICc(TS1,TS2,TS3,TS4,TS5,TS6,TS7,TS8,TS9),
                    round(Weights(AIC(TS1,TS2,TS3,TS4,TS5,TS6,TS7,TS8,TS9)),3))

TSAIC$lambda <- rbind(TS1$param[2], TS2$param[2], TS3$param[2], TS4$param[2],
                      TS5$param[2], TS6$param[2], TS7$param[2], TS8$param[2],
                      TS9$param[2])

TSAIC$model <- rbind(as.character(TS1$call[2]),as.character(TS2$call[2]),
                     as.character(TS3$call[2]),as.character(TS4$call[2]),
                     as.character(TS5$call[2]),as.character(TS6$call[2]),
                     as.character(TS7$call[2]),as.character(TS8$call[2]),
                     as.character(TS9$call[2]))

TSAIC <- TSAIC[order(AICc)]

TSAIC$deltaAIC <- TSAIC$AICc - as.numeric(TSAIC[1,2])



write.csv(TSAIC, "output/TSTerrAIC.csv")

