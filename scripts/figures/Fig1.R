## number of migratory species per order: 
 mammals=read.table("/Users/quinnwebber/Dropbox/Manuscripts/Migration/2014/Data/mammals_main_2014.txt",header=T)

mammals$order

## rodents
rodents=subset(mammals,order=="Rodentia")
str(rodents) ## 246 total rodents (0 migrants)
het_rod=subset(rodents,hetero=="1")
str(het_rod) ## 81 heterothermic
homeo_rod=subset(rodents,hetero=="0")
str(homeo_rod) ## 146 homeothermic

## lagomorphs
lagomorpha=subset(mammals,order=="Lagomorpha")
str(lagomorpha) ## 1 migrant and 0 heterotherms

## primates 
primates=subset(mammals,order=="Primates")
str(primates) ## 1 migrant; 1 heterotherm

## No dermoptera in the dataset
## scandentia
scandentia=subset(mammals,order=="Scandentia")
str(scandentia) ## no migrants, no heterotherms

## chiroptera
bats=subset(mammals,order=="Chiroptera")
str(bats)
het_bats=subset(bats,hetero=="1")
str(het_bats) ## 78 heterothermic + 20 homeothermic
mig_bats=subset(bats,mig=="1")
str(mig_bats) ## 49 mig; 49 non_mig

##artiodactyla: 
artio=subset(mammals,order=="Artiodactyla")
str(artio)
mig_artio=subset(artio,mig=="1")
str(mig_artio) ## 38 migrants; 30 non-migrants; 0 heterotherms

## cetacea:
cetacea=subset(mammals,order=="Cetacea")
str(cetacea) 
mig_cet=subset(cetacea,mig=="1") ## 37 migrants. 6 non migrants and 0 heterotherms
str(mig_cet)

## persso
perisso=subset(mammals,order=="Perissodactyla")
str(perisso)
mig_perisso=subset(perisso,mig=="1") ## 4 migrants, 3 non-migrants, 0 heterotherms
str(mig_perisso)

## carnivora
carnivore=subset(mammals,order=="Carnivora")
str(carnivore) ## 86 species
mig_carn=subset(carnivore,mig=="1")
str(mig_carn) ## 24 migrants; 62 non-migrants
het_carn=subset(carnivore,hetero=="1")
str(het_carn) ## 6 heterotherms; 80 homeotherms

## pholidota
pholidota=subset(mammals,order=="Pholidota")
str(pholidota) ## 0 migrants,  0 heterotherms 

## Erinaceomorpha 
erin=subset(mammals,order=="Erinaceomorpha")
str(erin) ## 0 migrants. 4 heterotherms

## soricomorpha
sorico=subset(mammals,order=="Soricomorpha")
str(sorico) ## 39 species
sorico_hetero=subset(sorico,hetero=="1")
str(sorico_hetero) 8 heterotherms; 31 homeotherms (0 migrants)

## cingulata:
cingulata=subset(mammals,order=="Cingulata")
str(cingulata) ## 0 migrants, 1 heterotherm

## pilosa
pilosa=subset(mammals,order=="Pilosa")
str(pilosa) ## 0 migrants, 0 heterotherms

## afrosoricida 
afro=subset(mammals,order=="Afrosoricida")
str(afro) ## 11 species
afro_het=subset(afro,hetero=="1")
str(afro_het) ## 10 heterotherms

## Macroscelidea 
macroscelidea=subset(mammals,order=="Macroscelidea")
str(macroscelidea) ## 7 species (7 heterotherms, 0 migrants)

## Tubulidentata
Tubulidentata=subset(mammals,order=="Tubulidentata")
str(Tubulidentata) ## 1 species (0 heterotherms 0 migrants)

## sirenia
sirenia=subset(mammals,order=="Sirenia")
str(sirenia) ## 3 species (3 migrants, 0 heterotherms)

## hyracoidea
hyracoidea =subset(mammals,order=="Hyracoidea")
str(hyracoidea) ## 2 species (0 migrants, 0 heterotherms)

## proboscidea
proboscidea =subset(mammals,order=="Proboscidea")
str(proboscidea) ## 2 species (2 migrants, 0 heterotherms)

## diprotodontia
dipro=subset(mammals,order=="Diprotodontia")
str(dipro) ## 27 species (0 migrants)
dipro_het=subset(dipro,hetero=="1")
str(dipro_het) ##7 heterotherms, 20 homeothers

## microbiotheria
microbiotheria=subset(mammals,order=="Microbiotheria")
str(microbiotheria) ## species (1 heterotherm)

## dasyuromorphia 
dasyuromorphia =subset(mammals,order=="Dasyuromorphia")
str(dasyuromorphia) ## 19 species, 0 migrants
dasyuromorphia_het=subset(dasyuromorphia ,hetero=="1")
str(dasyuromorphia_het) ## 13 heterotherms, 6 homeotherms

## notoryctemorphia
notoryctemorphia =subset(mammals,order=="Notoryctemorphia")
str(notoryctemorphia) ## 1 species: 1 heterotherm

## peramelemorphia
peramelemorphia =subset(mammals,order=="Peramelemorphia")
str(peramelemorphia) ## 8 species ( 0 migrants, 0 heterotherms)

## paucituberculata: no species in our dataset

## didelphimorphia 
didelphimorphia =subset(mammals,order=="Didelphimorphia")
str(didelphimorphia) ## 12 species  (0 migrants)
didelphimorphia_het=subset(didelphimorphia,hetero=="1")
str(didelphimorphia_het) ##4 heterotherms, 8 homeotherms

## monotremata
monotremata =subset(mammals,order=="Monotremata")
str(monotremata) ## 1 species, 1 heterotherm (0 migrants)

### Roskov Y., Abucay L., Orrell T., Nicolson D., Kunze T., Culham A., Bailly N., Kirk P., Bourgoin T., DeWalt R.E., Decock W., De Wever A., eds. (2015). Species 2000 & ITIS Catalogue of Life, 2015 Annual Checklist. Digital resource at www.catalogueoflife.org/annual-checklist/2015. Species 2000: Naturalis, Leiden, the Netherlands. ISSN 2405-884X.
library(ade4)
library(ape)

## enter the phylogeny in Newick format
big.ass.tre <- NULL
big.ass.tre[1] <- "(((((((Rodentia_[2367_-_0/246_-_81/165],Lagomorpha_[92_-_1/9_-_0/10]),((Primates_[488_-_1/9_-_1/9],Dermoptera_[2]),Scandentia_[20_-_0/3_-_0/3])),((Chiroptera_[1299_-_49/49_-_78/20],(((Artiodactyla_[245_-_38/30_-_0/68],Cetacea_[91_-_37/6_-_0/43]),Perissodactyla_[24_-_4/3_-_0/7]),"
big.ass.tre[2] <- "(Carnivora_[287_-_24/62_-_6/80],Pholidota_[8_-_0/2_-_0/2]))),(Erinaceomorpha_[24_-_0/6_-_4/2],Soricomorpha_[428_-_0/39_-_8/31]))),(Cingulata_[21_-_0/8_-_1/7],Pilosa_[10_-_0/2_-_0/2])),(((Afrosoricida_[51_-_0/11_-_10/1],Macroscelidea_[15_-_0/7_-_7/0]),Tubulidentata-[1_-_0/1_-_0/1]),"
big.ass.tre[3] <- "((Sirenia_[5_-_3/0_-_0/3],Hyracoidea_[4_-_0/2_-_0/2]),Proboscidea_[3_-_2/0_-_0/2]))),(((((Diprotodontia_[143_-_0/27_-_7/20],Microbiotheria_[1_-_0/1_-_1/0]),(Dasyuromorphia_[75_-_0/19_-_13/6],Notoryctemorphia_[2_-_0/1_-_1/0])),Peramelemorphia_[21_-_0/8_-_0/8]),"
big.ass.tre[4] <- "Paucituberculata_[6]),Didelphimorphia_[87_-_0/12_-_4/8])),Monotremata_[5_-_0/1_-_1/0]);"
big.ass.phylo <- read.tree(text = big.ass.tre)


big.ass.phylo <- read.tree(text = big.ass.tre)

plot.phylo(big.ass.phylo,cex=1,no.margin=T, label.offset=.5,x.lim=50,edge.width=2)



