



libs <- c('data.table', 'ggplot2','gridExtra')
lapply(libs, require, character.only = TRUE)

mammals=fread("input/mammals.csv")
str(mammals)


mammals$locomotion[mammals$locomotion == "A"] <- 'Flying'  
mammals$locomotion[mammals$locomotion == "T"] <- 'Running'  
mammals$locomotion[mammals$locomotion == "W"] <- 'Swimming'  

mammals$locomotion2 = factor(mammals$locomotion, levels=c('Running','Flying','Swimming'))

png("graphics/Fig2_migMassLatitude.png", width = 6000, height = 4000, res = 600, units = "px")
aa <- ggplot(mammals, aes(logmass, mig)) +
  geom_jitter(alpha = 0.5, height = 0.05) +
  ggtitle("A)") +
  ylab("Probability of Migration") +
  xlab("Log(Body Mass(g))") +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = T, color = "darkblue") +
  theme(axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~locomotion2, scale = "free")

bb <- ggplot(mammals, aes(abslat, mig)) +
  geom_jitter(alpha = 0.5, height = 0.05) +
  ggtitle("B)") +
  ylab("Probability of Migration") +
  xlab("Absolute Latitude") +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = T, color = "darkblue") +
  theme(axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~locomotion2, scale = "free")
grid.arrange(aa,bb, nrow = 2)
dev.off()



