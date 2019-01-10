## number of migratory species per order: 


libs <- c('data.table', 'ggplot2','gridExtra',
          'ade4', 'ape', 'devtools' ,'ggtree')
lapply(libs, require, character.only = TRUE)

devtools::install_github('GuangchuangYu/ggtree')

## load all mammal data and 
mammals=fread("input/mammals.csv",header=T)

mammals[, .N, by = order]

### Roskov Y., Abucay L., Orrell T., Nicolson D., Kunze T., Culham A., Bailly N., Kirk P., Bourgoin T., DeWalt R.E., Decock W., De Wever A., eds. (2015). Species 2000 & ITIS Catalogue of Life, 2015 Annual Checklist. Digital resource at www.catalogueoflife.org/annual-checklist/2015. Species 2000: Naturalis, Leiden, the Netherlands. ISSN 2405-884X.
## enter the phylogeny in Newick format
big.ass.tre <- NULL
big.ass.tre[1] <- "(((((((Rodentia_[2367_-_0/246],Lagomorpha_[92_-_1/9]),((Primates_[488_-_1/9],Dermoptera_[2]),Scandentia_[20_-_0/3])),((Chiroptera_[1299_-_49/49],(((Artiodactyla_[245_-_38/30],Cetacea_[91_-_37/6]),Perissodactyla_[24_-_4/3]),"
big.ass.tre[2] <- "(Carnivora_[287_-_24/62],Pholidota_[8_-_0/2]))),(Erinaceomorpha_[24_-_0/6],Soricomorpha_[428_-_0/39]))),(Cingulata_[21_-_0/8],Pilosa_[10_-_0/2])),(((Afrosoricida_[51_-_0/11],Macroscelidea_[15_-_0/7]),Tubulidentata[1_-_0/1]),"
big.ass.tre[3] <- "((Sirenia_[5_-_3/0],Hyracoidea_[4_-_0/2]),Proboscidea_[3_-_2/0]))),(((((Diprotodontia_[143_-_0/27],Microbiotheria_[1_-_0/1]),(Dasyuromorphia_[75_-_0/19],Notoryctemorphia_[2_-_0/1])),Peramelemorphia_[21_-_0/8]),"
big.ass.tre[4] <- "Paucituberculata_[6]),Didelphimorphia_[87_-_0/12])),Monotremata_[5_-_0/1]);"
big.ass.phylo <- read.tree(text = big.ass.tre)


big.ass.phylo <- read.tree(text = big.ass.tre)


ggtree(big.ass.phylo, branch.length="none") +
     geom_nodelab(aes(image=phylopic), geom="phylopic", alpha=.5, color='steelblue')

ggtree(big.ass.phylo) %<+% info + xlim(NA, 6)
p + geom_tiplab(aes(image= imageURL), geom="image", offset=2, align=T, size=.16, hjust=0) +
  geom_tiplab(geom="label", offset=1, hjust=.5)

#%>%
  phylopic("9baeb207-5a37-4e43-9d9c-4cfb9038c0cc", 
           color="darkgreen", alpha=.8, node=4) %>%
  phylopic("2ff4c7f3-d403-407d-a430-e0e2bc54fab0", 
           color="darkcyan", alpha=.8, node=2) %>%
  phylopic("a63a929b-1b92-4e27-93c6-29f65184017e", 
           color="steelblue", alpha=.8, node=3) 


png("graphics/Fig1_phyloTree.png", width = 4000, height = 4000, res = 600, units = "px")
plot.phylo(big.ass.phylo,type = 'phylogram',cex=1,no.margin=T, label.offset=.5,x.lim=50,edge.width=2)
dev.off()


