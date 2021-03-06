


libs <- c('data.table', 'ape', 'caper',
          'MASS', 'mvtnorm', 'ggplot2', 'MuMIn')
lapply(libs, require, character.only = TRUE)

Mamm <- fread("input/mammals.csv")
TS = fread("input/thermoregulatory_scope.csv",header = T)
TS <- merge(Mamm, TS, by = "species")


TSFly <- readRDS("output/compTSFlyData.RDS")
TSFlyDiet <- readRDS("output/compTSFlyDataDiet.RDS")
TSFlyHab <- readRDS("output/compTSFlyDataHab.RDS")


TSfly <- TS[locomotion == "A" & diet == "Insectivore" | diet == "Frugivore"]
### global model
TS1=pgls(mig~TS + logmass + diet + abslat + habitat_sub,data=TSFly,lambda="ML")
TS1glm <- glm(mig ~ TS + logmass + diet + abslat + habitat_sub, 
              data=TSfly, 
              family = "binomial")
car::vif(TS1glm)

TS2 = pgls(mig ~ TS*diet + logmass + abslat,data=TSFlyDiet,lambda="ML")

TS3 = pgls(mig ~ TS*habitat_sub + logmass + abslat,data=TSFlyHab,lambda="ML")

TS4 = pgls(mig ~ TS*diet + logmass,data=TSFlyDiet,lambda="ML")

TS5 = pgls(mig ~ TS*diet + latitude,data=TSFlyDiet,lambda="ML")

TS6 = pgls(mig ~ TS*habitat_sub + logmass,data=TSFlyHab,lambda="ML")

TS7 = pgls(mig ~ TS*habitat_sub + latitude,data=TSFlyHab,lambda="ML")

TS8 = pgls(mig ~ TS*diet,data=TSFlyDiet,lambda="ML")
TS8glm <- glm(mig ~ TS*diet, 
              data=TSfly, family = "binomial")
car::vif(TS8glm)

TS9 = pgls(mig ~ TS*habitat_sub,data=TSFlyHab,lambda="ML")


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


write.csv(TSAIC, "output/TSFlyAIC.csv")

