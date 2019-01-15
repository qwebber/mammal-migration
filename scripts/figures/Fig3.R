

libs <- c('data.table', 'ggplot2','gridExtra')
lapply(libs, require, character.only = TRUE)

## load all mammal data and 
mammals=fread("input/mammals.csv",header=T)
TS = fread("input/thermoregulatory_scope.csv",header = T)

TS <- merge(mammals, TS, by = "species")

TS <- TS[locomotion != "W"]

TS$locomotion[TS$locomotion == "A"] <- 'Flying'  
TS$locomotion[TS$locomotion == "T"] <- 'Running'  


png("graphics/Fig3_migThermoregulatoryScope.png", width = 2000, height = 6000, res = 600, units = "px")
aa <- ggplot(TS[locomotion == "Running"], aes(TS, mig)) +
  geom_jitter(alpha = 0.5, height = 0.05) +
  ylab("Probability of Migration") +
  xlab("Thermoregulatory Scope") +
  ggtitle('A) Running mammals') +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = T, color = "darkblue") +
  theme(axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
bb <- ggplot(TS[locomotion == "Flying" & diet == "Frugivore"], aes(TS, mig)) +
  geom_jitter(alpha = 0.5, height = 0.05) +
  ylab("Probability of Migration") +
  xlab("Thermoregulatory Scope") +
  ggtitle('B) Frugivorous bats') +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = T, color = "darkblue") +
  theme(axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
cc <- ggplot(TS[locomotion == "Flying" & diet == "Insectivore"], aes(TS, mig)) +
  geom_jitter(alpha = 0.5, height = 0.05) +
  ylab("Probability of Migration") +
  xlab("Thermoregulatory Scope") +
  ggtitle('C) Insectivorous bats') +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = T, color = "darkblue") +
  theme(axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
grid.arrange(aa,bb,cc, ncol = 1, nrow = 3)
dev.off()

