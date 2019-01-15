

libs <- c('data.table', 'ape', 'caper',
          'MASS', 'mvtnorm', 'ggplot2', 'MuMIn')
lapply(libs, require, character.only = TRUE)

Mamm <- fread("input/mammals.csv")

Mamm[, .N, by = .(mig, habitat_sub, locomotion)]

FlyMamm <- readRDS("output/compFlyMammalData.RDS")

## model selection
## global model
fly1=pgls(mig~logmass+habitat_sub+diet+abslat,data=FlyMamm,lambda="ML")
##w/o logmass
fly2=pgls(mig ~habitat_sub+diet+abslat,data=FlyMamm,lambda="ML")
## w/o habitat
fly3 = pgls(mig ~ logmass+ abslat+diet,data=FlyMamm,lambda="ML")
##w/o abslat
fly4 = pgls(mig ~ logmass +diet+habitat_sub,data=FlyMamm,lambda="ML")
## w/o diet
fly5= pgls(mig ~ logmass +habitat_sub+abslat,data=FlyMamm,lambda="ML")

AIC(fly1,fly2,fly3,fly4,fly5)

##w/o habitat and diet
fly6 = pgls(mig ~ logmass+abslat,data=FlyMamm,lambda="ML")
##w/o habitat and abslat
fly7 = pgls(mig ~ logmass +diet,data=FlyMamm,lambda="ML")
##w/o habitat and logmass
fly8 = pgls(mig ~ diet +abslat,data=FlyMamm,lambda="ML")
## w/o diet and abslat
fly9 =pgls(mig ~ logmass +habitat_sub,data=FlyMamm,lambda="ML")
##w/o diet and logmass
fly10 =pgls(mig ~ abslat+habitat_sub ,data=FlyMamm,lambda="ML")
## w/o logmass and abslat
fly11 =pgls(mig ~ diet+habitat_sub ,data=FlyMamm,lambda="ML")
fly12 =pgls(mig ~ habitat_sub ,data=FlyMamm,lambda="ML")
fly13 =pgls(mig ~ abslat ,data=FlyMamm,lambda="ML")
fly14 =pgls(mig ~ diet ,data=FlyMamm,lambda="ML")
fly15 =pgls(mig ~ logmass ,data=FlyMamm,lambda="ML")


flyAIC <- data.table(AICc(fly1,fly2,fly3,fly4,fly5,fly6,fly7,fly8,fly9,fly10,fly11,fly12,fly13,fly14,fly15), 
           round(Weights(AIC(fly1,fly2,fly3,fly4,fly5,fly6,fly7,fly8,fly9,fly10,fly11,fly12,fly13,fly14,fly15)),3))

flyAIC$lambda <- rbind(fly1$param[2], fly2$param[2], fly3$param[2], fly4$param[2],
                       fly5$param[2], fly6$param[2], fly7$param[2], fly8$param[2],
                       fly9$param[2], fly10$param[2], fly11$param[2], fly12$param[2], 
                       fly13$param[2], fly14$param[2], fly15$param[2])


flyAIC$model <- rbind(as.character(fly1$call[2]),as.character(fly2$call[2]),
                      as.character(fly3$call[2]),as.character(fly4$call[2]),
                      as.character(fly5$call[2]),as.character(fly6$call[2]),
                      as.character(fly7$call[2]),as.character(fly8$call[2]),
                      as.character(fly9$call[2]),as.character(fly10$call[2]),
                      as.character(fly11$call[2]),as.character(fly12$call[2]),
                      as.character(fly13$call[2]),as.character(fly14$call[2]),
                      as.character(fly15$call[2]))

flyAIC <- flyAIC[order(AICc)]

flyAIC$deltaAIC <- flyAIC$AICc - as.numeric(flyAIC[1,2])


write.csv(flyAIC, "output/flyAIC.csv")

flyAIC <- read.csv("output/flyAIC.csv")
