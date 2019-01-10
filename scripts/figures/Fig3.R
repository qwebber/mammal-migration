

libs <- c('data.table', 'ggplot2','gridExtra')
lapply(libs, require, character.only = TRUE)

## load all mammal data and 
mammals=fread("input/mammals.csv",header=T)
TS = fread("input/thermoregulatory_scope.csv",header = T)

TS <- merge(mammals, TS, by = "species")

TS <- TS[locomotion != "W"]

TS$locomotion[TS$locomotion == "A"] <- 'Flying'  
TS$locomotion[TS$locomotion == "T"] <- 'Running'  


png("graphics/Fig3_migThermoregulatoryScope.png", width = 6000, height = 4000, res = 600, units = "px")
ggplot(TS, aes(TS, mig)) +
  geom_jitter(alpha = 0.5, height = 0.05) +
  ylab("Probability of Migration") +
  xlab("Thermoregulatory Scope") +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = T, color = "darkblue") +
  theme(axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~locomotion*habitat_category, scale = "free")
dev.off()

