


libs <- c('data.table', 'ape', 'caper',
          'MASS', 'mvtnorm', 'ggplot2','MuMIn')
lapply(libs, require, character.only = TRUE)

Mamm <- fread("input/mammals.csv")

chisq.test()

TerrMamm <- readRDS("output/compTerrMammalData.RDS")

## model selection
## global model
terr1=pgls(mig~logmass+habitat_sub+diet+abslat,data=TerrMamm,lambda="ML")
##w/o logmass
terr2=pgls(mig ~habitat_sub+diet+abslat,data=TerrMamm,lambda="ML")
## w/o habitat
terr3 = pgls(mig ~ logmass+ abslat+diet,data=TerrMamm,lambda="ML")
##w/o abslat
terr4 = pgls(mig ~ logmass +diet+habitat_sub,data=TerrMamm,lambda="ML")
## w/o diet
terr5= pgls(mig ~ logmass +habitat_sub+abslat,data=TerrMamm,lambda="ML")

AIC(terr1,terr2,terr3,terr4,terr5)

##w/o habitat and diet
terr6 = pgls(mig ~ logmass+abslat,data=TerrMamm,lambda="ML")
##w/o habitat and abslat
terr7 = pgls(mig ~ logmass +diet,data=TerrMamm,lambda="ML")
##w/o habitat and logmass
terr8 = pgls(mig ~ diet +abslat,data=TerrMamm,lambda="ML")
## w/o diet and abslat
terr9 =pgls(mig ~ logmass +habitat_sub,data=TerrMamm,lambda="ML")
##w/o diet and logmass
terr10 =pgls(mig ~ abslat+habitat_sub ,data=TerrMamm,lambda="ML")
## w/o logmass and abslat
terr11 =pgls(mig ~ diet+habitat_sub ,data=TerrMamm,lambda="ML")
terr12 =pgls(mig ~ habitat_sub ,data=TerrMamm,lambda="ML")
terr13 =pgls(mig ~ abslat ,data=TerrMamm,lambda="ML")
terr14 =pgls(mig ~ diet ,data=TerrMamm,lambda="ML")
terr15 =pgls(mig ~ logmass ,data=TerrMamm,lambda="ML")


terrAIC <- data.table(AICc(terr1,terr2,terr3,terr4,terr5,terr6,terr7,terr8,terr9,terr10,terr11,terr12,terr13,terr14,terr15),
           round(Weights(AIC(terr1,terr2,terr3,terr4,terr5,terr6,terr7,terr8,terr9,terr10,terr11,terr12,terr13,terr14,terr15)),3))

terrAIC$lambda <- rbind(terr1$param[2], terr2$param[2], terr3$param[2], terr4$param[2],
                        terr5$param[2], terr6$param[2], terr7$param[2], terr8$param[2],
                        terr9$param[2], terr10$param[2], terr11$param[2], terr12$param[2], 
                        terr13$param[2], terr14$param[2], terr15$param[2])

terrAIC$model <- rbind(as.character(terr1$call[2]),as.character(terr2$call[2]),
                       as.character(terr3$call[2]),as.character(terr4$call[2]),
                       as.character(terr5$call[2]),as.character(terr6$call[2]),
                       as.character(terr7$call[2]),as.character(terr8$call[2]),
                       as.character(terr9$call[2]),as.character(terr10$call[2]),
                       as.character(terr11$call[2]),as.character(terr12$call[2]),
                       as.character(terr13$call[2]),as.character(terr14$call[2]),
                       as.character(terr15$call[2]))

terrAIC <- terrAIC[order(AICc)]

terrAIC$deltaAIC <- terrAIC$AICc - as.numeric(terrAIC[1,2])


write.csv(terrAIC, "output/terrAIC.csv")

terrAIC <- read.csv("output/terrAIC.csv")
