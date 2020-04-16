## number of migratory species per order: 


libs <- c('data.table', 'ggplot2','gridExtra', 'tidyverse',
          'ade4', 'ape', 'devtools' ,'ggtree', 'ggstance', 'BiocManager')
lapply(libs, require, character.only = TRUE)

## try http if https is not available
BiocManager::install("ggtree")

## load all mammal data and 
mammals=fread("input/mammals.csv",header=T)

b1 <- mammals[, .N, by = .(order, mig)]
b2 <- dcast(b1, order  ~ mig, value.var = "N")
colnames(b2) <- c("Order", "no_mig", "mig")
b3 <- data.table(Order = c("Dermoptera", "Paucituberculata"), no_mig = c(0,0), mig = c(0,0))
b4 <- rbind(b2,b3)
b4[is.na(b4)] <- 0
b4$total <- b4$no_mig + b4$mig
b4$propMig <- b4$mig/b4$total
b4[propMig > 0, allMig := 1]
b4[propMig == 0, allMig := 0]
b4[28:29,]$propMig <- "NA"
b4$propNoMig <- 1 - b4$propMig


### Roskov Y., Abucay L., Orrell T., Nicolson D., Kunze T., Culham A., Bailly N., Kirk P., Bourgoin T., DeWalt R.E., Decock W., De Wever A., eds. (2015). Species 2000 & ITIS Catalogue of Life, 2015 Annual Checklist. Digital resource at www.catalogueoflife.org/annual-checklist/2015. Species 2000: Naturalis, Leiden, the Netherlands. ISSN 2405-884X.
## enter the phylogeny in Newick format
#big.ass.tre <- NULL
#big.ass.tre[1] <- "(((((((Rodentia_[2367_-_0/246],Lagomorpha_[92_-_1/9]),((Primates_[488_-_1/9],Dermoptera_[2]),Scandentia_[20_-_0/3])),((Chiroptera_[1299_-_49/49],(((Artiodactyla_[245_-_38/30],Cetacea_[91_-_37/6]),Perissodactyla_[24_-_4/3]),"
#big.ass.tre[2] <- "(Carnivora_[287_-_24/62],Pholidota_[8_-_0/2]))),(Erinaceomorpha_[24_-_0/6],Soricomorpha_[428_-_0/39]))),(Cingulata_[21_-_0/8],Pilosa_[10_-_0/2])),(((Afrosoricida_[51_-_0/11],Macroscelidea_[15_-_0/7]),Tubulidentata[1_-_0/1]),"
#big.ass.tre[3] <- "((Sirenia_[5_-_3/0],Hyracoidea_[4_-_0/2]),Proboscidea_[3_-_2/0]))),(((((Diprotodontia_[143_-_0/27],Microbiotheria_[1_-_0/1]),(Dasyuromorphia_[75_-_0/19],Notoryctemorphia_[2_-_0/1])),Peramelemorphia_[21_-_0/8]),"
#big.ass.tre[4] <- "Paucituberculata_[6]),Didelphimorphia_[87_-_0/12])),Monotremata_[5_-_0/1]);"

big.ass.tre <- NULL
big.ass.tre[1] <- "(((((((Rodentia,Lagomorpha),((Primates,Dermoptera),Scandentia)),((Chiroptera,(((Artiodactyla,Cetacea),Perissodactyla),"
big.ass.tre[2] <- "(Carnivora,Pholidota))),(Erinaceomorpha,Soricomorpha))),(Cingulata,Pilosa)),(((Afrosoricida,Macroscelidea),Tubulidentata),"
big.ass.tre[3] <- "((Sirenia,Hyracoidea),Proboscidea))),(((((Diprotodontia,Microbiotheria),(Dasyuromorphia,Notoryctemorphia)),Peramelemorphia),"
big.ass.tre[4] <- "Paucituberculata),Didelphimorphia)),Monotremata);"
big.ass.phylo <- read.tree(text = big.ass.tre)

png("graphics/Fig1_phyloTree2.png", width = 8000, height = 4000, res = 600, units = "px")
p <- ggtree(big.ass.phylo) 

tip <- get.tree(big.ass.phylo)$tip.label

mammal.df <- data.table(taxa=factor(b4$Order), 
    host=factor(b4$allMig))

mammal.df[28:29,]$host <- "NA"

## use %<+% operator to attach the information to the tree view
p <- p %<+%mammal.df
## add circles in tips and tip labels align to the right hand side.
## after the attachment via %<+% operator,
## we can use 'host' information to color circles and labels of tips.
p <- p + #geom_tippoint(aes(color = host), size = 1) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "darkgrey"),
                     labels=c("Migratory", "Non-migratory", "No data")) +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "darkgrey")) +
  ggtitle("A)") +
  geom_tiplab(aes(color = host)) + 
  scale_y_continuous(expand = c(0.02,0.02)) +
  xlim_tree(16) +
  theme(plot.margin = margin(0.25, 0.25, 1.2, 0, "cm"),
        legend.position= 'none') + 
  ## add numbers for each order
  annotate("text", size = 3.5, x = 16, y = 29, label = "(37/43)", color = "#D55E00") + #Cetacea
  annotate("text", size = 3.5, x = 16, y = 28, label = "(38/68)", color = "#D55E00") + #Artiodactyla
  annotate("text", size = 3.5, x = 16, y = 27, label = "(4/7)", color = "#D55E00") + #Perisso
  annotate("text", size = 3.5, x = 16, y = 26, label = "(0/2)", color = "#0072B2") + #Pholidota
  annotate("text", size = 3.5, x = 16, y = 25, label = "(37/85)", color = "#D55E00") + #Carnivora
  annotate("text", size = 3.5, x = 16, y = 24, label = "(49/98)", color = "#D55E00") + #Chiroptera
  annotate("text", size = 3.5, x = 16, y = 23, label = "(0/39)", color = "#0072B2") + #Soricomorpha
  annotate("text", size = 3.5, x = 16, y = 22, label = "(0/6)", color = "#0072B2") + #Erinaceomorpha
  #annotate("text", size = 3.5, x = 16, y = 21, label = "(x/y)", color = "#D55E00") + #Dermoptera
  annotate("text", size = 3.5, x = 16, y = 20, label = "(1/10)", color = "#D55E00") + #Primates
  annotate("text", size = 3.5, x = 16, y = 19, label = "(0/3)", color = "#0072B2") + #Scandentia
  annotate("text", size = 3.5, x = 16, y = 18, label = "(1/10)", color = "#D55E00") + #Lagomorpha
  annotate("text", size = 3.5, x = 16, y = 17, label = "(0/246)", color = "#0072B2") + #Rodentia
  annotate("text", size = 3.5, x = 16, y = 16, label = "(0/2)", color = "#0072B2") + #Pilosa
  annotate("text", size = 3.5, x = 16, y = 15, label = "(0/8)", color = "#0072B2") + #Cingulata
  annotate("text", size = 3.5, x = 16, y = 14, label = "(0/2)", color = "#0072B2") + #Hyracoidea
  annotate("text", size = 3.5, x = 16, y = 13, label = "(3/3)", color = "#D55E00") + #Sirenia
  annotate("text", size = 3.5, x = 16, y = 12, label = "(0/2)", color = "#D55E00") + #Proboscidea
  annotate("text", size = 3.5, x = 16, y = 11, label = "(0/7)", color = "#0072B2") + #Macroscelidea
  annotate("text", size = 3.5, x = 16, y = 10, label = "(0/11)", color = "#0072B2") + #Afrosoricida
  annotate("text", size = 3.5, x = 16, y = 9, label = "(0/1)", color = "#0072B2") + #Tubulidentata
  annotate("text", size = 3.5, x = 16, y = 8, label = "(0/1)", color = "#0072B2") + #Noto
  annotate("text", size = 3.5, x = 16, y = 7, label = "(0/19)", color = "#0072B2") + #Dasyuromorphia
  annotate("text", size = 3.5, x = 16, y = 6, label = "(0/1)", color = "#0072B2") + #Microbiotheria
  annotate("text", size = 3.5, x = 16, y = 5, label = "(0/27)", color = "#0072B2") + #Diprotodontia
  annotate("text", size = 3.5, x = 16, y = 4, label = "(0/8)", color = "#0072B2") + #Peramelemorphia
  #annotate("text", size = 3.5, x = 16, y = 3, label = "(x/y)", color = "#0072B2") + #Paucituber
  annotate("text", size = 3.5, x = 16, y = 2, label = "(0/12)", color = "#0072B2") + #Didelophimorphia
  annotate("text", size = 3.5, x = 16, y = 1, label = "(0/1)", color = "#0072B2")  #Montremata
  p


d3 <- data.frame(id = b4$Order, prop = b4$propMig, mig = 1, total = b4$total)
d4 <- data.frame(id = b4$Order, prop = b4$propNoMig, mig = 0, total = b4$total)
## order of phylogenetic tree
target <- c("Monotremata", "Didelphimorphia" , "Paucituberculata", "Peramelemorphia",
            "Diprotodontia", "Microbiotheria",  "Dasyuromorphia", "Notoryctemorphia", 
            "Tubulidentata", "Afrosoricida", "Macroscelidea", "Proboscidea",  "Sirenia",
            "Hyracoidea", "Cingulata", "Pilosa",  "Rodentia", "Lagomorpha",
            "Scandentia", "Primates", "Dermoptera", "Erinaceomorpha", "Soricomorpha",     
            "Chiroptera", "Carnivora", "Pholidota", "Perissodactyla", "Artiodactyla"  ,  
            "Cetacea")
d3 <- d3[match(target, d3$id),]
d4 <- d4[match(target, d4$id),]
d3$taxa <- 1:29
d4$taxa <- 1:29

d5 <- rbind(d3,d4)

mammals[, count_order := length(order), by = .(order)]
temp <- mammals[, .N, by = .(order, count_order)]
temp2 <- data.table(order = c("Dermoptera", "Paucituberculata"), 
                    count_order = c(1,1), N = c(0,0))
temp3 <- rbind(temp, temp2)
colnames(temp3) <- c("taxa", "count_order", "N")


p2 <- ggplot(d5, aes(taxa, prop, fill = factor(mig))) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.5) +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "darkgrey")) +
  coord_flip() +
  xlab("") +
  ggtitle("B)") +
  ylab('Proportion of migrants') +
  scale_x_continuous(expand = c(0.01,0.01)) +
  #geom_text(data = temp3, aes(taxa, count_order, label = N, fill = NULL), 
           # nudge_y = 0.02) +
  theme(legend.position = 'none',
        plot.margin = margin(0.25, 0.25, 0.25, 0, "cm"),
        axis.text.x=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

TS = fread("input/thermoregulatory_scope.csv",header = T)
TS <- merge(mammals, TS, by = "species")
TS$thermScope <- TS$TS

TS2 <- data.frame(Order = b4$Order, allMig = b4$allMig)

d4 <- data.frame(Order = TS$order, thermScope = TS$thermScope)
TS3 <- merge(TS2, d4, by = "Order")
colnames(TS3) <- c("label", "mig", "thermScope")

## add ordres with no data
TS4 <- data.table(label = c("Dermoptera","Paucituberculata"), mig = c(NA,NA),
                  thermScope = c(NA,NA))
TS5 <- rbind(TS3,TS4)

TS5$label<-factor(TS5$label, levels=c("Monotremata", "Didelphimorphia" , "Paucituberculata", "Peramelemorphia",
                                      "Diprotodontia", "Microbiotheria",  "Dasyuromorphia", "Notoryctemorphia", 
                                      "Tubulidentata", "Afrosoricida", "Macroscelidea", "Proboscidea",  "Sirenia",
                                      "Hyracoidea", "Cingulata", "Pilosa",  "Rodentia", "Lagomorpha",
                                      "Scandentia", "Primates", "Dermoptera", "Erinaceomorpha", "Soricomorpha",     
                                      "Chiroptera", "Carnivora", "Pholidota", "Perissodactyla", "Artiodactyla"  ,  
                                      "Cetacea"))
p3 <- ggplot(TS5, aes(label, thermScope)) +
  geom_boxplot(aes(fill = factor(mig)),outlier.color = NA,lwd = 0.6, alpha = 0.25) +
  geom_jitter(aes(color = factor(mig)), shape=16, position=position_jitter(0.2), size = 2, alpha = 0.6) + 
  scale_fill_manual(values = c("#0072B2", "#D55E00", "darkgrey")) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "darkgrey")) +
  coord_flip() +
  xlab("") +
  ggtitle("C)") +
  scale_x_discrete(expand = c(0.02,0.02)) +
  ylab('Thermoregulatory scope') +
  theme(legend.position = 'none',
        plot.margin = margin(0.25, 0.25, 0.25, 0, "cm"),
        axis.text.x=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size=12),
        panel.grid.major = element_line(size = 0.1, linetype = 'dashed',
                                        colour = "black"), 
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

grid.arrange(p,p2,p3, nrow = 1, ncol = 3)

dev.off()
