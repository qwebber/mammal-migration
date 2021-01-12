

libs <- c('data.table', 'ape', 'caper',
          'MASS', 'mvtnorm', 'ggplot2', 'MuMIn')
lapply(libs, require, character.only = TRUE)

Mamm <- fread("input/mammals.csv")

SwimMamm <- readRDS("output/compSwimMammalData.RDS")


mammals.water <- Mamm[locomotion == "W"]

## model selection
## global model
swim1=pgls(mig~logmass+habitat_sub+diet+abslat,data=SwimMamm,lambda="ML")
swim1_glm <- glm(mig ~ logmass + diet + abslat + habitat_sub, 
                data=mammals.water, 
                family = "binomial")
car::vif(swim1_glm)

##w/o logmass
swim2=pgls(mig ~habitat_sub+diet+abslat,data=SwimMamm,lambda="ML")
## w/o habitat
swim3 = pgls(mig ~ logmass+ abslat+diet,data=SwimMamm,lambda="ML")
##w/o abslat
swim4 = pgls(mig ~ logmass +diet+habitat_sub,data=SwimMamm,lambda="ML")
## w/o diet
swim5= pgls(mig ~ logmass +habitat_sub+abslat,data=SwimMamm,lambda="ML")

AIC(swim1,swim2,swim3,swim4,swim5)

##w/o habitat and diet
swim6 = pgls(mig ~ logmass+abslat,data=SwimMamm,lambda="ML")
##w/o habitat and abslat
swim7 = pgls(mig ~ logmass +diet,data=SwimMamm,lambda="ML")
##w/o habitat and logmass
swim8 = pgls(mig ~ diet +abslat,data=SwimMamm,lambda="ML")
## w/o diet and abslat
swim9 =pgls(mig ~ logmass +habitat_sub,data=SwimMamm,lambda="ML")
##w/o diet and logmass
swim10 =pgls(mig ~ abslat+habitat_sub ,data=SwimMamm,lambda="ML")
## w/o logmass and abslat
swim11 =pgls(mig ~ diet+habitat_sub ,data=SwimMamm,lambda="ML")
swim12 =pgls(mig ~ habitat_sub ,data=SwimMamm,lambda="ML")
swim13 =pgls(mig ~ abslat ,data=SwimMamm,lambda="ML")
swim14 =pgls(mig ~ diet ,data=SwimMamm,lambda="ML")
swim15 =pgls(mig ~ logmass ,data=SwimMamm,lambda="ML")


swimAIC <- data.table(AICc(swim1,swim2,swim3,swim4,swim5,swim6,swim7,swim8,swim9,swim10,swim11,swim12,swim13,swim14,swim15),
           round(Weights(AIC(swim1,swim2,swim3,swim4,swim5,swim6,swim7,swim8,swim9,swim10,swim11,swim12,swim13,swim14,swim15)),3))

swimAIC$lambda <- rbind(swim1$param[2], swim2$param[2], swim3$param[2], swim4$param[2],
                        swim5$param[2], swim6$param[2], swim7$param[2], swim8$param[2],
                        swim9$param[2], swim10$param[2], swim11$param[2], swim12$param[2], 
                        swim13$param[2], swim14$param[2], swim15$param[2])

swimAIC$model <- rbind(as.character(swim1$call[2]),as.character(swim2$call[2]),
                       as.character(swim3$call[2]),as.character(swim4$call[2]),
                       as.character(swim5$call[2]),as.character(swim6$call[2]),
                       as.character(swim7$call[2]),as.character(swim8$call[2]),
                       as.character(swim9$call[2]),as.character(swim10$call[2]),
                       as.character(swim11$call[2]),as.character(swim12$call[2]),
                       as.character(swim13$call[2]),as.character(swim14$call[2]),
                       as.character(swim15$call[2]))

swimAIC <- swimAIC[order(AICc)]

swimAIC$deltaAIC <- swimAIC$AICc - as.numeric(swimAIC[1,2])


write.csv(swimAIC, "output/swimAIC.csv")

