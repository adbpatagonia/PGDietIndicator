## ADB
## Jan 3, 2018
## Script to explore the potential to use either diet or reprodutive rates of harp seals as indicators
## of their prey base: specifically capelin, arctic cod, and herring

## load libraries ----
library(dplyr)
library(cowplot)

## load data ----
# diet
load('data/harp_percbio_aggregate2J3KL.Rdata')
diet <- filter(percbio, order == 13 | order == 11 | order == 10)
# arctic cod
boreo <- read.csv('data/boreogadus.csv')
# atlantic herring
ah <- read.csv('data/atlherring.csv')


boreo <- boreo %>%
  filter(type == 'scaled') %>%
  merge(percbio[which(percbio$order == 12), c('year', 'preycat', 'percbio')], by = c('year'))




p <- ggplot(boreo, aes(x = year))
p <- p + geom_line(aes(y = boreo_biomass), colour = 'red')

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = percbio*80000))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./80000, name = "Percent in diet"))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#p <- p + facet_grid(nafo ~ area + season, drop = TRUE, scales = 'free')
p <- p + ggtitle("Biomass of arctic cod compared to percent on PG diet")
p <- p + theme(plot.title = element_text(size = 15, face = "bold"),
                 strip.text = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 13, face = "bold"))
p

p <- ggplot(boreo, aes(x = boreo_biomass, y = percbio))
p <- p + geom_point()
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p

#save_plot("output/AH_CatchRate_Diet.png", p, base_width = 10, base_height = 10)#, dpi = 900) # make room for figure legend)

