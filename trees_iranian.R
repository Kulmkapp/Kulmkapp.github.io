# adapted code from:
# R code for the article titled "Testing inferences about language contact on morphosyntax: A typological case study on Alorese – Adang contact", Transactions of the Philological Society



######################################
######         Set up          #######
######################################
###
### Install and load packages
##
## Install glottoTrees
# install.packages(pkgs)
# devtools::install_github("erichround/glottoTrees", 
#                         dependencies = T, 
#                         INSTALL_opts = c("--no-multiarch"))
#
## Install ggtree from Bioconductor
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ggtree")
#
## Install other packages
pkgs <- c("ape", "cowplot", "geosphere", "ggplot2", "ggridges", "ggtree",
          "glottoTrees", "phytools", "Partiallyoverlapping", "readxl", "reshape2",
          "rnaturalearth", "rnaturalearthdata", "sf", "tidyverse", "trend", "viridis")


# Load packages
require(ape)
require(cowplot)
require(geosphere)
require(ggimage)
require(ggplot2)
require(ggridges)
require(ggtree)
require(glottoTrees)
require(phytools)
require(Partiallyoverlapping)
require(readxl)
require(reshape2)
require(rnaturalearth)
require(rnaturalearthdata)
require(sf)
require(tidyverse)
require(trend)
require(viridis)


############################################
######    MAP of sample languages    #######
############################################
# UNTIL LINE 150

## Load and preprocess the data
# Read data to R session
setwd("~/phd/trees")
iranian.data <- read.csv("Zazaki_data(tree+map).csv", sep = ",")
# head(iranian.data) -> all looks fine
# note: osse1243 is changed to digo1242 and sora1257 to cent1972 in the files because of level issues

# Read Glottolog 4.7 data to R session
url = "https://cdstar.eva.mpg.de//bitstreams/EAEA0-2198-D710-AA36-0/languages_and_dialects_geo.csv"
glottolog <- read.csv(url)

# Extract longitudes and latitudes from Glottolog
iranian.sample <- merge(x = glottolog, y = iranian.data, # two dataframes to be merged
                        by.x = "glottocode", # specifying the columns used for merging
                        by.y = "glottocode", all.y = T) # specifying the columns used for merging

# Zooming in on all languages present in the sample
iranian.df <- iranian.sample[iranian.sample$insample != "no",] # selecing all that have NOT the value 'no' in the column "insample"
#View(iranian.df)

## Preprocessing for drawing the maps
# Settings for the maps
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  #  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# Draw the main map
map.iranianlang <- ggplot() + geom_sf(data = worldmap) + theme_bw() +
  scale_x_continuous(limits = c(30, 80)) +
  scale_y_continuous(limits = c(25, 43)) +
  geom_point(data = iranian.df,
             aes(x=longitude, y = latitude, shape = type),
             color = "black",
             size = 3.9) +
  geom_point(data = iranian.df,
             aes(x=longitude, y = latitude,
                 color = set, shape = type),
             size = 3.1) +
  theme(legend.text=element_text(size=11)) +
  scale_color_manual(name = "Language type",
                     labels = c("Focus",
                                "Neighbour (Turkish)",
                                "Benchmark"),
                     values = viridis(3, option = "rocket")[c(2,3,1)]) +
  scale_shape_manual(name = "Classification",
                     values = c(12, 17, 18, 8, 16, 15, 6, 9)) + # selecting the symbols
  labs(x = "Longitude", y = "Latitude")

# Draw the inset map -> this is a small map in the map where the world wide context is shown and zoomed in
map.inset <- ggplot() + geom_sf(data = worldmap) + theme_bw() +
  scale_x_continuous(limits = c(20, 90)) +
  scale_y_continuous(limits = c(15, 60)) +
  ditch_the_axes +
  geom_rect(aes(xmin = 30, xmax = 80,
                ymin = 25, ymax = 43),
            color = "black", linetype = "solid", linewidth = 0.2,
            fill="turquoise1", alpha = 0.35)

# Combine the maps into one and save the map as fig5iranian in tiff format
tiff(file = "fig5_fixed.tif",res = 150,units = "mm",type = "cairo",
     width = 220,height = 110)
ggdraw(map.iranianlang) +
  draw_plot(map.inset, width = 0.29, height = 0.29, 
            x = 0.02, y = 0.32)
dev.off()

##################################################
######    TREE: Iranian    #######
##################################################
# UNTIL LINE 258

# Remove the neighbor language Turkish from the data -> I only remove Turkish
iranian <- iranian.sample[iranian.sample$name != "Turkish",]
# filter out rows with NA
iranian <- iranian[complete.cases(iranian),]

# OPEN ISSUE: there is a problem with jude1256 and mara1373
#  so as an interim solution, I kick them out
#  maybe this doesn't need to be changed since both of them are not in the sample
iranian <- iranian[iranian$glottocode != "jude1256",]
iranian <- iranian[iranian$glottocode != "mara1373",]

# Change the column name "Glottocode" to "tip"
# This is needed for later purposes
names(iranian)[1] = "tip"

# Load Indo-European tree from the Glottolog, which is also
#  part of the glottoTrees library
tree_ie <- get_glottolog_trees(family = "Indo-European", 
                               glottolog_version = "4.7")

# Abridge tip and node labels
tree_ie_abr <- abridge_labels(tree_ie)

# Extract Iranian branch from the Indo-European family tree
tree_iranian = extract.clade(tree_ie_abr, 'iran1269') # this is the glottocode for the top-node of the Iranian clade

# Prune the tree to a selection from the data
tree.pruned <- keep_as_tip(tree_iranian, label = iranian$tip)

# Find the number of the important nodes by inspecting the following tree
ggtree(tree.pruned, branch.length="none") + 
  geom_tiplab() + theme_tree() + 
  geom_text(aes(label=node), hjust=-.3)

# nodes I want: Root, Central Iranian, Southwestern Iranian, Avestan, Pashto, Saka-Wakhi, Central Eastern Iranian, Ormuri-Parachi
# 95: Root
# 100 : Central Iranian
# 138 : Southwestern Iranian
# 1 : Avestan -> this is the node label since Avestan directly splits off Iranian
# 133 : Pashto
# 135 : Saka-Wakhi
# 96 : Central Eastern Iranian
# 132 : Ormuri-Parachi

## Produce the tree graph as iranian_tree
# Create the tree
xt <- ggtree(tree.pruned, branch.length="none") +
  xlim(-2,12)

# Save image to file in tiff format
tiff(file = "Iranian_tree.tif",res = 150,units = "mm",type = "cairo",
     width = 160,height = 220)

# Add tipnames and adjust coloring etc.
# COLORING OF BRANCHES DOESN'T WORK
xt%<+%iranian +
  geom_tiplab(aes(label=language, fill=factor(tree)), size=3, 
              align = TRUE, offset=.3, linetype = NULL,
              geom = "label") +
  scale_fill_manual(values=c(no = "white",yes= "moccasin",# yes = benchmarks
                             f = "green")) + # b = benchmark (Western Balochi), f = focus (Zazaki)
  theme(legend.position = "none") +
  geom_nodelab(aes(subset = (node == 95), label = "Iranian"), # root 
               color = "black", nudge_x = -0.0001, 
               nudge_y = 1, hjust = 1.1, size=3.5) +
  geom_nodelab(aes(subset = (node == 100), label = "Central Iranian"), 
               color = "black", nudge_x = -0.0001, 
               nudge_y = 1.3, hjust = 1.18, size=3.5,
               vjust = -0.05) +
  geom_nodelab(aes(subset = (node == 138), label = "Southwestern Iranian"), 
               color = "black", nudge_x = -0.0001, 
               nudge_y = 2, hjust = 1.3, size=3.5) +
  geom_nodelab(aes(subset = (node == 1), label = "Avestan"), 
               color = "black", nudge_x = -0.0001, 
               nudge_y = 1.3, hjust = 1.3, size=3.5,
               vjust = -0.05) +
  geom_nodelab(aes(subset = (node == 133), label = "Pashto"), 
               color = "black", nudge_x = -0.0001, 
               nudge_y = 1.3, hjust = 1.3, size=3.5,
               vjust = -0.05) +
  geom_nodelab(aes(subset = (node == 135), label = "Saka-Wakhi"), 
               color = "black", nudge_x = -0.0001, 
               nudge_y = 1.3, hjust = 1.3, size=3.5,
               vjust = -0.05) +
  geom_nodelab(aes(subset = (node == 96), label = "Central Eastern Iranian"), 
               color = "black", nudge_x = -0.0001, 
               nudge_y = 1.3, hjust = 1.3, size=3.5,
               vjust = -0.05) +
  geom_nodelab(aes(subset = (node == 132), label = "Ormuri-Parachi"), 
               color = "black", nudge_x = -0.0001, 
               nudge_y = 1.3, hjust = 1.3, size=3.5,
               vjust = -0.05) #+
  #geom_hilight(node = 100, fill = "steelblue", extend = 39) +
  #geom_hilight(node = 138, fill = "blue") +
  #geom_hilight(node = 132, fill = "lightblue") +
  #geom_hilight(node = 133, fill = "grey") +
  #geom_hilight(node = 135, fill = "brown") +
  #geom_hilight(node = 96, fill = "lightgreen") +
  #geom_hilight(node = 132, fill = "violet")
dev.off()

############################################
######    The betaDistro function    #######
############################################

## Author: T. Mark Ellison -> ADAPTED by Nora Muheim for Iranian
## Date of last update: 21 April 2023
## As part of the article titled "A typological approach to language change in contact situations"
##  Article authors: Kaius Sinnemäki, Francesca Di Garbo, T. Mark Ellison, Ricardo Napoleão de Souza
##  Status: under revision
##
## This function determines the distributions over parameters given
##   the proportions of evidence weighting in favour of one item or another.
## Parameters:
##   n1s = number of "1" valued items occurring (the similarity set)
##   outOf = number of items overall (the baseline set)
## Return value is a list with the following elements:
##   $ciBottom, $ciTop - the bottom and top of the 95% confidence intervals
##   $quartile1,$median,$quartile3 - the top values of the
##     first, second and third quartiles respectively
##   $sampler(n) - a function for producing n random samples from
##     the distribution
##   $graph - a ggplot graph object for plotting the distribution - you
##     can add more ggplot2 features to the graph if you like
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##


###############################################
######    Figure: beta distributions    ####### -> not sure if I need this
###############################################
# UNTIL LINE 355

# Select four colors from the Viridis turbo-palette
#  as I understand it, these colors are for the beta distributions with different parameters
c.4 <- viridis(4, option = "turbo") 

# Adjust the BetaDistro function to draw several
#  distributions
betaDistro <- function(n1s,outOf) {
  n0s <- outOf - n1s
  list(
    ciBottom = qbeta(0.025,n1s+1,n0s+1),
    quartile1 = qbeta(0.75,n1s+1,n0s+1),
    median = qbeta(0.5,n1s+1,n0s+1),
    quartile3 = qbeta(0.75,n1s+1,n0s+1),
    ciTop = qbeta(0.975,n1s+1,n0s+1),
    sampler = function(n) { rbeta(n,n1s+1,n0s+1) },
    graph = (function(n) {
      ((0:n)/n) %>%
        data.frame(x = .) %>%
        mutate(y1 = dbeta(x,n1s+1,n0s+1)) %>%
        mutate(y2 = dbeta(x,n1s*5+1,n0s*5+1)) %>%
        mutate(y3 = dbeta(x,n1s*10+1,n0s*10+1)) %>%
        mutate(y4 = dbeta(x,n1s*15+1,n0s*15+1)) %>%
        (function(d) {
          ggplot(data=d) +
            geom_line(aes(x=x,y=y1, color = "2/1"), linewidth = 1) +
            geom_line(aes(x=x,y=y2, color = "10/5"), linewidth = 1) +
            geom_line(aes(x=x,y=y3, color = "20/10"), linewidth = 1) +
            geom_line(aes(x=x,y=y4, color = "30/20"), linewidth = 1) +
            labs(x = "Probability for convergence",
                 y = "Probability density") +
            scale_color_manual(name = "c/s pairs",
                               labels = c("2/1",
                                          "10/5",
                                          "20/10",
                                          "30/20"),
                               values = c("2/1" = c.4[1],
                                          "10/5" = c.4[2],
                                          "20/10" = c.4[3],
                                          "30/20" = c.4[4]),
                               limits = c("2/1",
                                          "10/5",
                                          "20/10",
                                          "30/20"))
        })
    })(10000)
  )
}

# I skip the part where they produce a simulated beta curve graphics for illustrative purposes

######################################
######    set.beta function    #######
######################################

# This function is built on the betaDistro function. It randomly
#  selects one value for as many successes (n1s) out of N (outOf)
#  there are

set.beta <- function(n1s, outOf){
  sto <- numeric(length(n1s))
  for (i in 1:length(n1s)) {
    sto[i] <- betaDistro(n1s[i], outOf[i])$sampler(1)
  }
  return(sto)
}

##############################################
######    Ancestral reconstructions    #######
##############################################

## Load and preprocess data
ir.master <- read.csv("Zazaki_data(Binarization).csv")
ir.data <- merge(x = iranian.sample[,c(1,6:7)], y = ir.master,
                 by.x = "glottocode", by.y = "Glottocode", all.y = T)
names(ir.data)[1] = "tip"

# Remove Dimli, Northern Kurdish, and Turkish for the purpose of ancestral reconstructions
#  Reconstructions are done without Dimli!
#ir.data <- ir.data[ir.data$Language != "Dimli",]
#ir.data <- ir.data[ir.data$Language != "Northern Kurdish",]
ir.data <- ir.data[ir.data$Language != "Turkish",]

# Prune the tree to a selection from the data
tree.ir <- keep_as_tip(tree_iranian, label = ir.data$tip)

# The tree still contains some node that dominate only one tip.
# First find out the nodes which dominate only one tip
as_tibble(tree.ir) %>% 
  group_by(parent) %>% 
  filter( n() < 2 ) %>% 
  select(parent) -> tip1

# Number of tip-labels, we need this information for matching tip1
#  with the node.labels
ntips = length(tree.ir$tip.label)

# Remove those nodes that dominate only one tip.
tree <- collapse_node(tree.ir,
                      label = tree.ir$node.label[tip1$parent-ntips])

# writing out tree to a .tree file to load it later again
write.tree(tree, "tree.tree")

## Inspect visually the node numbers for the selected nodes
ggtree(tree, branch.length="none") + 
  geom_tiplab() + theme_tree() + 
  geom_text(aes(label=node), hjust=-.3)


#### Ancestral Character Estimations via Bayesian Inference
# I have saved the asr.master file, it can be loaded this way:
# To load it later:
#asr.master <- readRDS("asr.master.rds")

# Set the sample, burnin and iterations
sample = 1000
burnin = 200000
iterations = 2200000

# critical p = 0.90
# Create an empty dataframe for the reconstructions
res.bi_0.9 <- data.frame(Type = c(rep("ASR, BI, p = 0.9", 7)),
                         Branch = c("Central Iranian", "Southwestern Iranian", "Avestan", "Pashto", "Saka-Wakhi", "Central Eastern Iranian", "Ormuri-Parachi"),
                         Glottocode = c("cent2317", "sout3157", "aves1237", "pash1269", "saka1303", "cent2399", "ormu1249"))
xx = paste0("A", 1:(ncol(ir.data)-3-2))
res.bi_0.9[xx] <- numeric(1)

# Set the critical probability
a.bi_0.9 = 0.9

# Compute the ACEs, using the ancTresh method in the Phytools library
#  using default settings for priors and threshold
# NOTE: default is Brownian Motion
# ALSO TRY OUT ORNSTEIN-UHLENBECK
for (j in 6:(ncol(ir.data))){
  trai <- ir.data[,j]
  names(trai) <- ir.data[,1]
  # ASRs
  if (length(unique(ir.data[,j])) == 1) {
    res.bi_0.9[,j-2] <- unique(ir.data[,j])
  } else {asr.master <- ancThresh(tree, trai, ngen = iterations,
                                  control = list(sample = sample,
                                                 burnin = burnin))
  asr <- asr.master$ace[c(1:3,6),]
  for (i in 1:4){
    if (asr[i,1] > asr[i,2] && asr[i,1] >= a.bi_0.9){
      res.bi_0.9[i,j-2] <- colnames(asr)[1]
    } else if (asr[i,1] < asr[i,2] && asr[i,2] >= a.bi_0.9){
      res.bi_0.9[i,j-2] <- colnames(asr)[2]
    } else {res.bi_0.9[i,j-2] <- NA}
  }
  }
}

# critical p = 0.70
# Create an empty dataframe for the reconstructions
res.bi_0.7 <- data.frame(Type = c(rep("ASR, BI, p = 0.7", 7)),
                         Branch = c("Central Iranian", "Southwestern Iranian", "Avestan", "Pashto", "Saka-Wakhi", "Central Eastern Iranian", "Ormuri-Parachi"),
                         Glottocode = c("cent2317", "sout3157", "aves1237", "pash1269", "saka1303", "cent2399", "ormu1249"))
xx = paste0("A", 1:(ncol(ir.data)-3-2))
res.bi_0.7[xx] <- numeric(1)

# Set the critical probability
a.bi_0.7 = 0.7

# Compute the ACEs, using the ancTresh method in the Phytools library
#  using default settings for priors and threshold
# NOTE: default is Brownian Motion
# ALSO TRY OUT ORNSTEIN-UHLENBECK
for (j in 6:(ncol(ir.data))){
  trai <- ir.data[,j]
  names(trai) <- ir.data[,1]
  # ASRs
  if (length(unique(ir.data[,j])) == 1) {
    res.bi_0.7[,j-2] <- unique(ir.data[,j])
  } else {asr.master <- ancThresh(tree, trai, ngen = iterations,
                                  control = list(sample = sample,
                                                 burnin = burnin))
  asr <- asr.master$ace[c(1:3,6),]
  for (i in 1:4){
    if (asr[i,1] > asr[i,2] && asr[i,1] >= a.bi_0.7){
      res.bi_0.7[i,j-2] <- colnames(asr)[1]
    } else if (asr[i,1] < asr[i,2] && asr[i,2] >= a.bi_0.7){
      res.bi_0.7[i,j-2] <- colnames(asr)[2]
    } else {res.bi_0.7[i,j-2] <- NA}
  }
  }
}

res.bi.all <- rbind(res.bi_0.7, res.bi_0.9)

# saving asr.master to load it later
# Save entire object
saveRDS(asr.master, file = "asr.master.rds")

# To load it later:
asr.master <- readRDS("asr.master.rds")



#### Compute the similarity score
# Combine the feature matrices
res.all <- ir.master
names(res.bi.all) <- names(res.all)
res.all <- rbind(res.all, res.bi.all)

# Create a storage for the results
ir.var <- data.frame(Type = res.all$Type,
                     Language = res.all$Language,
                     Glottocode = res.all$Glottocode)
xx = colnames(res.all)[4:(ncol(res.all))]
ir.var[xx] <- numeric(1)

# Remove the Focus and Neighbor from the results
ir.var <- ir.var[ir.var$Language != "Dimli",]
ir.var <- ir.var[ir.var$Language != "Northern Kurdish",]
ir.var <- ir.var[ir.var$Language != "Turkish",]

write.csv(ir.var, "ir.var.csv", row.names = FALSE)
write.csv(ir.master, "ir.master.csv", row.names = FALSE)
write.csv(ir.data, "ir.data.csv", row.names = FALSE)
write.csv(ir_df, "ir_df.csv", row.names = FALSE)

# Compute the similarity scores
for (i in 4:(ncol(res.all))) {
  for (j in 3:nrow(res.all)){
    if(is.na(res.all[j,i])){
      ir.var[(j-2),i] <- NA
    } else if(res.all[j,i] == res.all[2,i]){
      ir.var[(j-2),i] <- NA
    } else {
      if(res.all[1,i] == res.all[2,i]){
        ir.var[(j-2),i] <- 1
      } else {
        ir.var[(j-2),i] <- 0
      }
    }
  }
}

# Remove some dependencies in the "_Type" features
#  This removes all "_Type" features except those where the only 0s and 1s
#  come from the "_Type" feature (concerns z:n, n:pr_, and n:n).
#  In the type z:n, a few dependencies are manually assigned NA

value = "_Type"
drop <- names(ir.var)[grepl("_Type", names(ir.var), fixed = TRUE)]
drop <- drop[-c(8:10)]
ir.var.final = ir.var[,!(names(ir.var) %in% drop)]
ir.var.final$`z:n_Type`[c(1,3,16)] <- NA # ATTENTION: some issue here


# Not done yet:
##########################################
######    ISOLATION BY DISTANCE    #######
##########################################

## Compute geographic distances
# Get longitudes and latitudes for the benchmarks from Glottolog
glottolog_phylo_geo_v4.7 %>%
  filter(is.element(glottocode, ir.data$tip)) %>%
  select(glottocode, latitude, longitude) -> iranian_geo

# Merge with the similarity scores
ir_df = merge(bs[1:20,], iranian_geo,
              by.x = "Glottocode", by.y = "glottocode")

# Get longitude and latitude for Zazaki from Glottolog
glottolog_phylo_geo_v4.7 %>%
  filter(is.element(glottocode, "diml1238")) %>%
  select(glottocode, latitude, longitude) -> diml

# Calculate distance of a benchmark from Zazaki
#  using the distm function from the geosphere library
diml_km = as.numeric(
  distm(as.matrix(diml[,3:2]),
        as.matrix(ir_df[,8:7]),
        fun = distGeo)/1000)

# DO THE SAME FOR NORTHERN KURDISH
glottolog_phylo_geo_v4.7 %>%
  filter(is.element(glottocode, "nort2641")) %>%
  select(glottocode, latitude, longitude) -> nort

# Calculate distance of a benchmark from Northern Kurdish
#  using the distm function from the geosphere library
nort_km = as.numeric(
  distm(as.matrix(nort[,3:2]),
        as.matrix(ir_df[,8:7]),
        fun = distGeo)/1000)

### Compute probability for convergence with function set.beta
# using 100 random values
B = 10000

# Create a storage for the randomized values
var = matrix(data = 0, ncol = B, nrow = nrow(ir_df))

# Run B number of simulations
for (i in 1:B) {
  # Set a storage for the randomly selected values from the Beta-distribution
  var[,i] <- set.beta(ir_df$convergence, ir_df$baseline)
}

# Select the median for the probability 
diml_dist = vector(mode = "numeric", length = nrow(ir_df))
for (i in 1:length(diml_dist)){
  diml_dist[i] <- median(var[i,])
}

# FIGURE OUT HERE HOW TO DO THE SAME WITH NORTHERN KURDISH
# Combine the distance calculations with ir_df and rename columns
df_geo = as.data.frame(cbind(diml_dist, diml_km, ir_df[,1:3]))
names(df_geo)[1:2] = c("features", "km")

# Combine the beta-sampled probabilities with distances and preproces
var1.df <- as.data.frame(cbind(df_geo[,-c(1,3:4)], var))
res <- melt(var1.df[,], id.vars = c("Language", "km"))

# Confidence intervals for the beta-sampled probabilities
res %>%
  group_by(Language) %>%
  mutate(top_bottom = case_when(
    value > quantile(value, 0.975) ~"top 2.5%", 
    value < quantile(value, 0.025) ~"bottom 2.5%", 
    TRUE ~ "95%")) -> res.dd

res  %>%
  group_by(Language) %>%
  arrange(Language, desc(value)) %>%
  mutate(top_bottom = 
           ifelse(row_number() %in% c(B*0.025), "2.5", "no")) %>%
  filter(top_bottom != "no") -> res_high_dd

res  %>%
  group_by(Language) %>%
  arrange(Language, desc(value)) %>%
  mutate(top_bottom = 
           ifelse(row_number() %in% c(B*0.975), "2.5", "no")) %>%
  filter(top_bottom != "no") -> res_low_dd

res %>%
  group_by(Language) %>%
  mutate(median = median(value)) %>%
  select(Language, km, median) %>%
  distinct()-> res.ibd.median

## Draw beta-sampled probabilities against geographic distance

# Adjust top x-axis to color the labels and to avoid overlap
res_low_dd$km2 <- with(res_low_dd, # right
                       ifelse(Language == "Gilaki" |
                                Language == "Bashkardi" | 
                                Language == "Old Persian" |
                                Language == "Parthian", km+15, km))
res_low_dd$km2 <- with(res_low_dd, # a lot more to the right
                       ifelse(Language == "Kavir", km+15, km))
res_low_dd$km2 <- with(res_low_dd, # left
                       ifelse(Language == "Laki" |
                                Language == "Western Farsi", km2-12, km2))
#res_low_dd$branch <- with(res_low_dd,
#                          ifelse(km2 < 250, "brown", "blue","green", "grey", "orange", "violet"))
central <- c("Gilaki", "Gurani", "Kavir", "Koroshi", "Laki", "Mazanderani", "North-Central Talysh", 
             "Parthian", "Sorkhei-Aftari", "Western Balochi")
res_low_dd$branch[res_low_dd$Language %in% central] <- "brown" # Central Iranian

southwestern <- c("Bashkardi", "Dari", "Muslim Tat", "Old Persian", "Tajik", "Western Farsi")
res_low_dd$branch[res_low_dd$Language %in% southwestern] <- "blue" # Southwestern Iranian

pashto <- c("Central Pashto", "Northern Pashto")
res_low_dd$branch[res_low_dd$Language %in% pashto] <- "green" # Pashto

centraleastern <- c("Sarikoli")
res_low_dd$branch[res_low_dd$Language %in% centraleastern] <- "violet" # Central Eastern Iranian

res_low_dd$branch[res_low_dd$Language == "Avestan"] <- "black"

tiff(file = "fig7.tif",res = 150,units = "mm",type = "cairo",
     width = 220,height = 150)
ggplot(res.dd, aes(x = km, y = value, group = Language)) + 
  geom_point(aes(color = top_bottom)) +
  geom_smooth(method='loess', se = F,
              data = res_low_dd, aes(x=km, y=value, group=NULL)) +
  geom_smooth(method='loess', se = F,
              data = res_high_dd, aes(x=km, y=value, group=NULL)) +
  geom_smooth(method='loess', se = F,
              data = res.ibd.median, aes(x=km, y=median, group=NULL),
              color = "black") +
  scale_x_continuous(breaks = c(250,500,750,1000,1250,1500,1750,2000,2250,2500,2750,3000,3250),
                     sec.axis = dup_axis(name = NULL,
                                         labels = res_low_dd$Language,
                                         breaks = res_low_dd$km2,
                                         guide = guide_axis(angle = 90))) +
  scale_y_continuous(breaks = c(0.0,0.2,0.4,0.6,0.8,1.0),
                     sec.axis = 
                       dup_axis(breaks = c(0.3,0.5,0.7,0.9),
                                name = NULL)) +
  scale_color_discrete(name = "Conf. int.",
                       labels = c("top 2.5%", "95%", "bottom 2.5%"),
                       limits = c("top 2.5%", "95%", "bottom 2.5%")) +
  labs(x = "Distance from Zazaki (km)",
       y = "Probability of convergence") +
  theme(axis.text.x.top =
          element_text(color = res_low_dd$branch, face="bold"))

dev.off()


####### Mann-Kendall monotonic trend test
# Perform one-sided Mann-Kendall calculations.
# H0: no monotonic trend; H1: negative monotonic trend

# Order the data in terms of geographic distance from Alorese.
var.mk <- var1.df[order(var1.df$km),]

# Create storage for the estimates
mk <- numeric(B)

# Compute Mann-Kendall trend tests
for (i in 3:ncol(var.mk)){
  mk[i-2] <- mk.test(var.mk[,i])$estimate[[3]]
}

# Turn the vector into a dataframe
ir_df = data.frame(Estimate = mk,
                     y = rep("", length(mk)))

tiff(file = "fig8.tif",res = 150,units = "mm",type = "cairo",
     width = 150,height = 60)
ggplot(data=ir_df, aes(x = Estimate, y = y, fill = factor(after_stat(quantile))))+
  # Set the "quantiles" to 0.025 and 0.975
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantiles = c(0.025, 0.975),
                      scale = 0.9, alpha = 0.7) +
  # For the legend, set the title, the value names and the corresponding colors
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("bottom 2.5%", "95%", "top 2.5%")
  ) +
  # Draw a dotted black vertical line to zero
  scale_x_continuous(breaks =c(-0.5,-0.25,0,0.25,0.5)) +
  theme_ridges(center_axis_labels = T) +
  labs(y = NULL, x = "Kendall's tau")
dev.off()


###############################################
######    Compare benchmarks to ASRs    #######
###############################################

# Select single Benchmarks
ace.ir <- ir.var.final[1:20,]

# merge data about geographic distance and order the data
#  in terms of distance from Zazaki
ace.ir <- merge(x = ace.ir, y = df_geo[,2:3], by = "Glottocode")
ace.ir <- ace.ir[,c(1:3,ncol(ace.ir),4:(ncol(ace.ir)-1))]
ace.ir <- ace.ir[order(ace.ir$km),]


##### Select ACE BI p = 0.9
ace.BI_0.9_CentralIranian <- as.numeric(ir.var.final[45, -c(1:3)]) # this is the row number of Central Iranian, -c(1:3) hops over 'Type, language, glottocode'
ace.BI_0.9_SouthwesternIranian <- as.numeric(ir.var.final[46, -c(1:3)])
ace.BI_0.9_Avestan <- as.numeric(ir.var.final[47, -c(1:3)])
ace.BI_0.9_Pashto <- as.numeric(ir.var.final[48, -c(1:3)])
ace.BI_0.9_SakaWakhi <- as.numeric(ir.var.final[49, -c(1:3)])
ace.BI_0.9_CentralEasternIranian <- as.numeric(ir.var.final[50, -c(1:3)])
ace.BI_0.9_OrmuriParachi <- as.numeric(ir.var.final[51, -c(1:3)])

# create empty matrices for the results
res.ace.BI_0.9_CentralIranian <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                     dimnames = list(
                                       ace.ir$Language,
                                       c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.9_SouthwesternIranian <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                       dimnames = list(
                                         ace.ir$Language,
                                         c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.9_Avestan <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                        dimnames = list(
                                          ace.ir$Language,
                                          c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.9_Pashto <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                         dimnames = list(
                                           ace.ir$Language,
                                           c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.9_CentralEasternIranian <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                            dimnames = list(
                              ace.ir$Language,
                              c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.9_SakaWakhi <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                            dimnames = list(
                              ace.ir$Language,
                              c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.9_OrmuriParachi <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                            dimnames = list(
                              ace.ir$Language,
                              c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))


## Compute the partially overlapping samples z-tests
# The function Prop.test() produces an estimated difference in
# proportions between two variables a and b. If the estimated
# difference in proportions < 0, then the proportion for b is greater
# that that for a. This can be interpreted in the context of this
# paper so that the variable b (from language b) suggests greater
# evidence for convergence than variable a (from language a). If, however,
# the estimated difference in proportions > 0, then the proportion
# for b is smaller that that for a. This can be interpreted in the
# context of this paper so that the variable b (from language b) 
# suggests less evidence for convergence than variable a (from 
# language a).

# Comparing to Central Iranian
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.9_CentralIranian,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.9_CentralIranian[i,1] <- test$statistic
  res.ace.BI_0.9_CentralIranian[i,2] <- test$estimate
  res.ace.BI_0.9_CentralIranian[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.9_CentralIranian[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.9_CentralIranian[i,5] <- test$p.value
}

# Comparing to Southwestern Iranian
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.9_SouthwesternIranian,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.9_SouthwesternIranian[i,1] <- test$statistic
  res.ace.BI_0.9_SouthwesternIranian[i,2] <- test$estimate
  res.ace.BI_0.9_SouthwesternIranian[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.9_SouthwesternIranian[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.9_SouthwesternIranian[i,5] <- test$p.value
}

# Comparing to Avestan
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.9_Avestan,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.9_Avestan[i,1] <- test$statistic
  res.ace.BI_0.9_Avestan[i,2] <- test$estimate
  res.ace.BI_0.9_Avestan[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.9_Avestan[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.9_Avestan[i,5] <- test$p.value
}

# Comparing to Pashto
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.9_Pashto,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.9_Pashto[i,1] <- test$statistic
  res.ace.BI_0.9_Pashto[i,2] <- test$estimate
  res.ace.BI_0.9_Pashto[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.9_Pashto[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.9_Pashto[i,5] <- test$p.value
}

# Comparing to Central Eastern Iranian # SOME PROBLEM HERE
#  res.ace.BI_0.9_CentralEasternIranian has only 0 as values
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.9_CentralEasternIranian,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.9_CentralEasternIranian[i,1] <- test$statistic
  res.ace.BI_0.9_CentralEasternIranian[i,2] <- test$estimate
  res.ace.BI_0.9_CentralEasternIranian[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.9_CentralEasternIranian[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.9_CentralEasternIranian[i,5] <- test$p.value
}

# Comparing to Sakha-Waki
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.9_SakaWakhi,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.9_SakaWakhi[i,1] <- test$statistic
  res.ace.BI_0.9_SakaWakhi[i,2] <- test$estimate
  res.ace.BI_0.9_SakaWakhi[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.9_SakaWakhi[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.9_SakaWakhi[i,5] <- test$p.value
}


# Comparing to Ormuri-Parachi # SOME PROBLEM HERE
#  res.ace.BI_0.9_OrmuriParachi has only 0 as values
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.9_OrmuriParachi,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.9_OrmuriParachi[i,1] <- test$statistic
  res.ace.BI_0.9_OrmuriParachi[i,2] <- test$estimate
  res.ace.BI_0.9_OrmuriParachi[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.9_OrmuriParachi[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.9_OrmuriParachi[i,5] <- test$p.value
}

##### Select ACE BI p = 0.7
ace.BI_0.7_CentralIranian <- as.numeric(ir.var.final[38, -c(1:3)]) # this is the row number of Central Iranian, -c(1:3) hops over 'Type, language, glottocode'
ace.BI_0.7_SouthwesternIranian <- as.numeric(ir.var.final[39, -c(1:3)])
ace.BI_0.7_Avestan <- as.numeric(ir.var.final[40, -c(1:3)])
ace.BI_0.7_Pashto <- as.numeric(ir.var.final[41, -c(1:3)])
ace.BI_0.7_SakaWakhi <- as.numeric(ir.var.final[42, -c(1:3)])
ace.BI_0.7_CentralEasternIranian <- as.numeric(ir.var.final[43, -c(1:3)])
ace.BI_0.7_OrmuriParachi <- as.numeric(ir.var.final[44, -c(1:3)])

# create empty matrices for the results
res.ace.BI_0.7_CentralIranian <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                        dimnames = list(
                                          ace.ir$Language,
                                          c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.7_SouthwesternIranian <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                             dimnames = list(
                                               ace.ir$Language,
                                               c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.7_Avestan <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                 dimnames = list(
                                   ace.ir$Language,
                                   c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.7_Pashto <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                dimnames = list(
                                  ace.ir$Language,
                                  c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.7_CentralEasternIranian <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                               dimnames = list(
                                                 ace.ir$Language,
                                                 c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.7_SakaWakhi <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                   dimnames = list(
                                     ace.ir$Language,
                                     c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))
res.ace.BI_0.7_OrmuriParachi <- matrix(data = 0, nrow = nrow(ace.ir), ncol = 5,
                                       dimnames = list(
                                         ace.ir$Language,
                                         c("Statistic", "estimate", "CI.low", "CI.high", "p.value")))


## Compute the partially overlapping samples z-tests
# The function Prop.test() produces an estimated difference in
# proportions between two variables a and b. If the estimated
# difference in proportions < 0, then the proportion for b is greater
# that that for a. This can be interpreted in the context of this
# paper so that the variable b (from language b) suggests greater
# evidence for convergence than variable a (from language a). If, however,
# the estimated difference in proportions > 0, then the proportion
# for b is smaller that that for a. This can be interpreted in the
# context of this paper so that the variable b (from language b) 
# suggests less evidence for convergence than variable a (from 
# language a).

# Comparing to Central Iranian
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.7_CentralIranian,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.7_CentralIranian[i,1] <- test$statistic
  res.ace.BI_0.7_CentralIranian[i,2] <- test$estimate
  res.ace.BI_0.7_CentralIranian[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.7_CentralIranian[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.7_CentralIranian[i,5] <- test$p.value
}

# Comparing to Southwestern Iranian
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.7_SouthwesternIranian,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.7_SouthwesternIranian[i,1] <- test$statistic
  res.ace.BI_0.7_SouthwesternIranian[i,2] <- test$estimate
  res.ace.BI_0.7_SouthwesternIranian[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.7_SouthwesternIranian[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.7_SouthwesternIranian[i,5] <- test$p.value
}

# Comparing to Avestan
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.7_Avestan,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.7_Avestan[i,1] <- test$statistic
  res.ace.BI_0.7_Avestan[i,2] <- test$estimate
  res.ace.BI_0.7_Avestan[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.7_Avestan[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.7_Avestan[i,5] <- test$p.value
}

# Comparing to Pashto
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.7_Pashto,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.7_Pashto[i,1] <- test$statistic
  res.ace.BI_0.7_Pashto[i,2] <- test$estimate
  res.ace.BI_0.7_Pashto[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.7_Pashto[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.7_Pashto[i,5] <- test$p.value
}

# Comparing to Central Eastern Iranian
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.7_CentralEasternIranian,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.7_CentralEasternIranian[i,1] <- test$statistic
  res.ace.BI_0.7_CentralEasternIranian[i,2] <- test$estimate
  res.ace.BI_0.7_CentralEasternIranian[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.7_CentralEasternIranian[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.7_CentralEasternIranian[i,5] <- test$p.value
}

# Comparing to Sakha-Waki
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.7_SakaWakhi,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.7_SakaWakhi[i,1] <- test$statistic
  res.ace.BI_0.7_SakaWakhi[i,2] <- test$estimate
  res.ace.BI_0.7_SakaWakhi[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.7_SakaWakhi[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.7_SakaWakhi[i,5] <- test$p.value
}


# Comparing to Ormuri-Parachi # SOME PROBLEM HERE
#  res.ace.BI_0.9_OrmuriParachi has only 0 as values
for (i in 1:nrow(ace.ir)){
  test <- Prop.test(
    ace.BI_0.7_OrmuriParachi,
    as.numeric(ace.ir[i,5:ncol(ace.ir)]),
    stacked=TRUE,conf.level=.95)
  res.ace.BI_0.7_OrmuriParachi[i,1] <- test$statistic
  res.ace.BI_0.7_OrmuriParachi[i,2] <- test$estimate
  res.ace.BI_0.7_OrmuriParachi[i,3] <- test$conf.int[[1]]
  res.ace.BI_0.7_OrmuriParachi[i,4] <- test$conf.int[[2]]
  res.ace.BI_0.7_OrmuriParachi[i,5] <- test$p.value
}

# Create the combined tables
res.ace_comb_CentralIranian <- round(cbind(res.ace.BI_0.9_CentralIranian[,-c(3:4)],
                                        res.ace.BI_0.7_CentralIranian[,-c(3:4)]),3)
res.ace_comb_SouthwesternIranian <- round(cbind(res.ace.BI_0.9_SouthwesternIranian[,-c(3:4)],
                                          res.ace.BI_0.7_SouthwesternIranian[,-c(3:4)]),3)
res.ace_comb_Avestan <- round(cbind(res.ace.BI_0.9_Avestan[,-c(3:4)],
                                           res.ace.BI_0.7_Avestan[,-c(3:4)]),3)
res.ace_comb_Pashto <- round(cbind(res.ace.BI_0.9_Pashto[,-c(3:4)],
                                            res.ace.BI_0.7_Pashto[,-c(3:4)]),3)
res.ace_comb_SakaWakhi <- round(cbind(res.ace.BI_0.9_SakaWakhi[,-c(3:4)],
                                   res.ace.BI_0.7_SakaWakhi[,-c(3:4)]),3)
res.ace_comb_CentralEasternIranian <- round(cbind(res.ace.BI_0.9_CentralEasternIranian[,-c(3:4)],
                                   res.ace.BI_0.7_CentralEasternIranian[,-c(3:4)]),3)
res.ace_comb_OrmuriParachi <- round(cbind(res.ace.BI_0.9_OrmuriParachi[,-c(3:4)],
                                   res.ace.BI_0.7_OrmuriParachi[,-c(3:4)]),3)

# write them out to csv files
write.csv(res.ace_comb_CentralIranian, "res.ace_comb_CentralIranian.csv", row.names = FALSE)
write.csv(res.ace_comb_SouthwesternIranian, "res.ace_comb_SouthwesternIranian.csv", row.names = FALSE)
write.csv(res.ace_comb_Avestan, "res.ace_comb_Avestan.csv", row.names = FALSE)
write.csv(res.ace_comb_Pashto, "res.ace_comb_Pashto.csv", row.names = FALSE)
write.csv(res.ace_comb_SakaWakhi, "res.ace_comb_SakaWakhi.csv", row.names = FALSE)
write.csv(res.ace_comb_CentralEasternIranian, "res.ace_comb_CentralEasternIranian.csv", row.names = FALSE)
write.csv(res.ace_comb_OrmuriParachi, "res.ace_comb_OrmuriParachi.csv", row.names = FALSE)


##################################################
######    Compute all similarity scores    #######
##################################################

# Create a storage for the calculations
bs <- cbind(ir.var.final[,1:3],
            matrix(data = 0, ncol = 3, nrow = nrow(ir.var.final)))
colnames(bs)[4:6] <- c("stability", "convergence", "baseline")

# Compute the similarity scores
for (i in 1:nrow(ir.var.final)){
  x <- suppressWarnings(as.numeric(ir.var.final[i,4:ncol(ir.var.final)]))
  x.tab <- table(x[!is.na(x)])
  bs[i,4:6] <- c(x.tab, sum(x.tab))
}

######################################################
######    Ancestral reconstructions on TREE    #######
######################################################

# Part of the following script is adapted from Randi H. Griffin's
#  tutorial "Enhanced annotation of a primate phylogeny with ggtree"
#  dated May 11, 2017. Available at:
#  http://www.randigriffin.com/2017/05/11/primate-phylogeny-ggtree.html
#  

### Select head marking of pronouns as an example 
#  this is feature h.pr_Type which is the 6th column (by leaving the space before the comma empty, all rows are selected)

# Ensure trait.bi is a named factor
trait.bi <- factor(ir.data[, 6])
names(trait.bi) <- ir.data$tip

# Bayesian ACE
asr.bi_tree.master <- ancThresh(tree, trait.bi, ngen = iterations,
                                control = list(sample = sample,
                                               burnin = burnin))
asr.bi_tree <- as.data.frame(asr.bi_tree.master$ace)
asr.bi_tree$node <- rownames(asr.bi_tree)

# Central Iranian 40: no: 0.64567716; yes: 0.35432284
# NW Iranian 42: no: 0.09495252; yes: 0.90504748


# Plot tree (ensure label column exists)
#print(class(tree))
#print(str(tree$edge))
asr.bi_tree.plot <- ggtree(tree, branch.length = "none", aes(label = label)) +
  xlim(-2, 12)

# Trait data for heatmap
trait.bi.df <- data.frame(trait = factor(trait.bi))
rownames(trait.bi.df) <- names(trait.bi)

# Add heatmap to the tree plot
asr.bi_tree.plot <- gheatmap(asr.bi_tree.plot,
                             trait.bi.df,
                             offset = 0.3, width = 0.03,
                             colnames = FALSE, font.size = 2, color = "black") +
  scale_fill_manual(values = c("darkorange1", "blue"),
                    labels = levels(trait.bi),
                    name = "Head marking, Pro") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

# Plot discrete ASRs with pie charts
asr.bi_tree.pies <- nodepie(asr.bi_tree, cols=1:2, alpha=0.9,
                            color = c("darkorange1","blue"))
asr.bi_tree.plot.inset <- inset(asr.bi_tree.plot,
                                asr.bi_tree.pies,
                                width=0.12, height=0.12, hjust=0)
asr.bi_tree.plot.inset%<+%ir.data +
  geom_tiplab(aes(label=Language), size=3.2, 
              align = TRUE, offset=.8, linetype = NULL) -> plot.bi

tiff(file = "fig6.tif",res = 150,units = "mm",type = "cairo",
     width = 140,height = 110)
cowplot::plot_grid(plot.bi + theme(legend.position = 'bottom'))
dev.off()

# Reconstructed probabilities for "no", ACE for head marking of
#  pronoun possessors

head(asr.bi_tree)
# outcome
#no        yes node
#35 0.94352824 0.05647176   35
#36 0.94652674 0.05347326   36
#37 0.96701649 0.03298351   37
#38 0.65217391 0.34782609   38
#39 0.57921039 0.42078961   39
#40 0.08145927 0.91854073   40
