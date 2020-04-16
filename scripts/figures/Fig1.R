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

#p <- add_colorbar(p, cols, font.size = 3)

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
        legend.position= 'none')
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



#png("graphics/Fig1_phyloTree.png", width = 4000, height = 4000, res = 600, units = "px")
plot.phylo(big.ass.phylo,type = 'phylogram',cex=1,no.margin=T, label.offset=.5,x.lim=50,edge.width=2)
#dev.off()


  
  ## get the path of files used in the examples that are distributed with 
  beast_file <-
    system.file("examples/MCC_FluA_H3.tree", 
                package=  "ggtree")
  rst_file <- system.file("examples/rst", 
                package= "ggtree")
  mlc_file <-
    system.file("examples/mlc", 
                package=  "ggtree")
  ## parsing BEAST & CODEMML trees
  ## and combine their phylogenetic inferences
  beast_tree <- read.beast(beast_file)
  codeml_tree <-read.codeml(rst_file, mlc_file)
  merged_tree <-merge_tree(beast_tree, codeml_tree)
  ## selected specific sites for displaying (with mask_site = FALSE).
  ## if mask_site = TRUE, only those not selected will be displayed.
  site_file <-
    system.file("examples/sites.txt", 
                package= "ggtree")
  site <- read.table(site_file)[,1]
  tree <- mask(merged_tree, "joint_AA_subs", site, 
    mask_site = FALSE)
  ## scale the color of the tree by dN_vs_dS.
  ## scale the time tree by specifying mrsd (most recent sampling date)
  ## round all the numberical evidence with 2 
  ## digits by specifying ndigits = 2.
  ## add colorbar by add_colorbar function.
  cols <- scale_color(merged_tree,   "dN_vs_dS", 
    low= "#0072B2", 
    high=  "#D55E00", interval= seq(0, 1.5, length.out=100))
  p <- ggtree(merged_tree, 
    size=.8, mrsd="2013-01-01", ndigits = 2 , color=cols)
  p <- add_colorbar(p, cols, 
    font.size = 3)
  ## add annotation of clade posterior and colored in darkgreen
  p <- p + geom_text(
    aes(label =
        posterior),
    color =
      "darkgreen",
    vjust = -0.1
    ,
    hjust = -.03
    ,
    size =
      1.8
  )
  ## add annotation of
  #amino acid substitution inferred by joint probabilities
  p <-
    p +
    geom_text(
    aes(x= branch, 
      label = joint_AA_subs),  vjust=-.03, size=1.8)
  ## get the tip labels
  tip <- get.tree(merged_tree)$tip.label
  ## create a new data.frame containing information of host 
  species.host <- rep("Human", length(tip))
  
  host[grep("Swine", tip)] <- "Swine"
  host.df <-
    data.frame(
    taxa=tip, 
    host=factor(host))
  ## use %<+% operator to attach the information to the tree view
  p <- p %<+%host.df
  ## add circles in tips and tip labels align to the right hand side.
  ## after the attachment via %<+% operator,
  ## we can use 'host' information to color circles and labels of tips.
  p <-p +
    geom_tippoint(aes
    (color=
        host), 
    size = 2) +
    geom_tiplab(aes(color=host), 
    align=TRUE, 
    size=3, 
    linesize=0.3)
  ## rescale the color to blue (for human) and red (for swine)
  p <- p +
    scale_color_manual(
    values = c("#377EB8", "#E41A1C"), 
    guide='none')
  ## setting the coordinate system
  p <
    -
    p +
    theme_tree2() +
    scale_y_continuous
  (
    expand=
      c
    (
      0
      , 
      0.6
    )) +
    xlab
  (
    "Time"
  )
  ## 
 # setting size of axis text and title.
  p <
    -
    p +
    theme
  (
    axis.text.x=
      element_text
    (
      size=
        10
    ), 
    axis.title.x =
      element_text
    (
      size=
        12
    ))
  ## reading genotype file
  genotype_file <- system.file(
    "examples/Genotype.txt", 
    package="ggtree"
  )
  genotype <- read.table(genotype_file, sep="\t", 
    stringsAsFactor=
      F)
  colnames
  (genotype) <
    -
    sub
  (
    "
    \
    \
    ."
    , 
    ""
    , 
    colnames
    (genotype))
  genotype[genotype ==
             "trig"
           ] <
    -
    "TRIG"
  genotype[genotype ==
             "pdm"
           ] <
    -
    "Pdm/09"
  ## visualize genotype heatmap with the tree view
  p <- gheatmap(p, genotype, 
    width=.4, 
    offset=7, 
    colnames=
      F) %>%
    scale_x_ggtree
  p <- p +
    scale_fill_brewer(
    palette="Set2"
  )
  p <- p+theme(legend.text=
      element_text(size = 8), 
    legend.key.height=
      unit(.5, 
      "cm"),
    legend.key.width=
      unit(.4, 
      "cm"),
    legend.position=
      c(.13, y=.945))
  ## A compact version of the above figure without tip labels and legend.
  ##
  ## instead of using %<+% to attach additional information, 
  ## we can use groupOTU to classify OTUs, it works with tree/plot objects.
  tree2 <
    -
    groupOTU
  (merged_tree, ti
    p[
      grep
      (
        "Swine"
        , tip)], 
    "host"
  )
  vp1 <
    -
    ggtree
  (tree2, 
    size=
      .
    5
    , 
    mrsd=
      "2013
    -
    01
    -
    01"
    , 
    ndigits =
      2
    , 
    color=
      cols) +
    geom_tippoint
  (
    aes
    (
      color=
        host), 
    size=
      1
  )
  vp1 <
    -
    gheatmap
  (vp1, genotype, 
    width=
      .
    3
    , 
    offset=
      .
    8
    , 
    colnames=
      F) +
    theme
  (
    legend.position=
      "none"
  )
  vp1 <
    -
    vp1+
    geom_aline
  (
    aes
    (
      x=
        2014
      , 
      xend=
        x
      +.1
      , 
      yend=
        y, 
      color=
        host),
    linetype=
      "dotted"
    , 
    size=
      .
    2
  ) +
    scale_color_manual
  (
    values=
      c
    (
      "#377EB8"
      , 
      "#E41A1C"
    )) +
    scale_fill_brewer
  (
    palette=
      "Set2"
  )
  ## A fan version
  vp2 <
    -
    open_tree
  (vp1, 
    180
  )
  ## e
  mbed compact versions above the main plot using subview function 
  p <
    -
    subview
  (p, vp1, 
    x=
      1996
    , 
    y=
      55
    , 
    width=
      .
    3
    , 
    height=
      .
    15
  )
  p <
    -
    subview
  (p, vp2, 
    x=
      1996
    , 
    y=
      30
    , 
    width=
      .
    3
    , 
    height=
      .
    15
  )
  ## In R use dev.new() to allocate a new window with enough space to view the
  figure
  ## If using RStudio, use windows() for Windows or quartz() for Mac OS X 
  platform
  dev.new
  (
    width=
      12
    , 
    height=
      12
  )
  print
  (p)


