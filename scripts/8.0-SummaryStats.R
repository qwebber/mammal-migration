

libs <- c('data.table')
lapply(libs, require, character.only = TRUE)

Mamm <- fread("input/mammals.csv")


Mamm[, .N, by = .(mig, locomotion, habitat_sub)]
Mamm[, .N, by = .(mig, locomotion, diet)]

## prop migration bats in TEF + BF
32/49
## prop non-migratory bats in TRF
26/49
## prop carnivorous swimming mammals
(53+12)/68
## prop herbivorous migratory running mammals
46/54
