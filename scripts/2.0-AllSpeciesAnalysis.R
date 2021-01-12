



libs <- c('data.table', 'ape', 'caper',
          'MASS', 'mvtnorm', 'ggplot2')
lapply(libs, require, character.only = TRUE)

Mamm <- fread("input/mammals.csv")

allMamm <- readRDS("output/compAllMammalData.RDS")

##3 model comparison of all mammals
mod1 <- pgls(mig ~ logmass + locomotion, data = allMamm,lambda = "ML")
mod2 <- pgls(mig ~ logmass * locomotion, data = allMamm,lambda = "ML")

AIC(mod1, mod2)

summary(mod1)
summary(mod2)
