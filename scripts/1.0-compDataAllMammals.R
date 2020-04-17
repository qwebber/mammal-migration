

libs <- c('data.table', 'ape', 'caper', 
          'MASS')
lapply(libs, require, character.only = TRUE)


mammals=fread("input/mammals.csv",header=T)

mammaltree = read.nexus("input/mammaltree.rtf")

phy <- mammaltree
data <- mammals

if(! inherits(phy, "phylo")) 
  stop("'", deparse(substitute(phy)), "' not of class 'phylo'")
if(! is.rooted(phy)){
  if(force.root){
    phy$root.edge <- 1
  } else {
    stop("'", deparse(substitute(phy)), "' is not rooted or has a basal polytomy.")
  }
}

if(any(duplicated(phy$tip.label))) stop('Duplicate tip labels present in phylogeny')
if(any(duplicated(c(phy$tip.label, phy$node.label)))) stop('Labels duplicated between tips and nodes in phylogeny')


# store original dataset size
origTips <- with(phy, max(edge) - Nnode)
origData <- nrow(data)

# find the intersection between tip.labels and names in data frame
in.both <- intersect(data$species, phy$tip.label)
if(length(in.both) == 0 ) stop("No tips are common to the dataset and phylogeny")

# i >> ditch rows with no tip
row.in.tree <- match(data$species, in.both)
row.not.in.tree <- data$species[is.na(row.in.tree)]
data <- subset(data, !is.na(row.in.tree))

# ii >> ditch tips which have no rows.
tip.in.data <-  match(phy$tip.label, in.both)
to.drop <- phy$tip.label[is.na(tip.in.data)]

#  get subset of phylogeny to be used
if(length(to.drop) > 0) matchedPhy <- drop.tip(phy, to.drop) else matchedPhy <- phy

# useful info...
root <- length(matchedPhy$tip.label) + 1

# get the data into the same order as the tips
tip.order <- match(matchedPhy$tip.label, data$species)
if(any(is.na(tip.order))) stop("Problem with sorting data frame: mismatch between tip labels and data frame labels")
data <- data[tip.order,, drop=FALSE]

# Label the data frame rows by tip label
rownames(data) <- matchedPhy$tip.label

# add or supplement node labels - used as references in contrast calculation
IntNd <- root:max(matchedPhy$edge)
if(is.null(matchedPhy$node.label)){
  matchedPhy$node.label <- IntNd
} else {
  # set up missing node labels
  matchedPhy$node.label <- ifelse(matchedPhy$node.label == "", NA, matchedPhy$node.label)
  if(any(duplicated(na.omit(matchedPhy$node.label)))) stop('Duplicate node labels present in phylogeny')
  matchedPhy$node.label <- ifelse(is.na(matchedPhy$node.label),  IntNd, matchedPhy$node.label)
}


RET <- list(phy=matchedPhy, data = data, 
            data.name = data, phy.name =matchedPhy, 
            dropped=list(tips=to.drop, unmatched.rows=row.not.in.tree))
class(RET) <- 'comparative.data'

# Add a VCV array if requested
if(vcv) {
  RET$vcv <- VCV.array(matchedPhy, dim = vcv.dim)
  RET$vcv.dim <- vcv.dim
}

# NA handling
if(na.omit){
  before.drop.rows <- rownames(RET$data)
  RET <- na.omit(RET, scope)
  if(!identical(rownames(RET$data), before.drop.rows)) RET$dropped$NA.rows <- before.drop.rows
}

if(warn.dropped){
  if(any(sapply(RET$dropped, length) > 0)) warning('Data dropped in compiling comparative data object')
}

saveRDS(RET, "output/compAllMammalData.RDS")


