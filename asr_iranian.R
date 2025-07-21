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
# until line 150

## Load and preprocess the data
# Read data to R session
setwd("~/phd/trees")
iranian.data <- read.csv("Zazaki_data(tree+map).csv", sep = ",")
# head(iranian.data) -> all looks fine
# note: osse1243 and sora1257 are in weird positions in glottolog
#  coded instead digo1242 as in sample but assigned the ossetic data to it
#  same for sora1257 -> cent1972

# Read Glottolog 4.7 data to R session
url = "https://cdstar.eva.mpg.de//bitstreams/EAEA0-2198-D710-AA36-0/languages_and_dialects_geo.csv"
glottolog <- read.csv(url)

# any(glottolog$glottocode == "nucl1301") # checking if a glottocode is in dataframe glottolog

# Extract longitudes and latitudes from Glottolog
iranian.sample <- merge(x = glottolog, y = iranian.data, # two dataframes to be merged
                        by.x = "glottocode", # specifying the columns used for merging
                        by.y = "glottocode", all.y = T) # specifying the columns used for merging

# Zooming in on all languages present in the sample
iranian.df <- iranian.sample[iranian.sample$insample != "no",] # selecing all that have NOT the value 'no' in the column "insample"
iranian.df <- iranian.sample
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
                     values = viridis(3, option = "rocket")[c(2,1,3)]) +
  scale_shape_manual(name = "Classification",
                     labels = c("Turkish",
                                "Central Iranian",
                                "Southwestern Iranian",
                                "Avestan",
                                "Pashto",
                                "Central Eastern Iranian",
                                "Saka-Wakhi",
                                "Ormuri-Parachi"),
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
tiff(file = "fig5iranian.tif",res = 150,units = "mm",type = "cairo",
     width = 220,height = 90)
ggdraw(map.iranianlang) +
  draw_plot(map.inset, width = 0.3, height = 0.3, 
            x = 0.02, y = 0.32)
dev.off()

##################################################
######    TREE: Iranian    #######
##################################################

# not sure if I need to do anything
# I put here everything with which I modify Chundra's tree

# load tree



# Prune the tree to a selection from the data
tree.pruned <- keep_as_tip(tree_bima, label = bima$tip) # to keep only the tips that are also in the excel file, and discard all other Bima languages not in the sample



