## ADB
## Jan 3, 2018
## Script to explore the potential to use either diet or reprodutive rates of harp seals as indicators
## of their prey base: specifically capelin, arctic cod, and herring

## load libraries ----
library(dplyr)

## load data ----
# diet
load('data/harp_percbio.Rdata')
diet <- filter(percbio, order == 13 | order == 11 | order == 10)
# arctic cod
boreo <- read.csv('data/boreogadus.csv')
# atlantic herring
ah <- read.csv('data/atlherring.csv') %>% group_by(year, bay) %>%
  summarize(total.cpue = sum(ah_cpue)) %>%
  merge(percbio[which(percbio$order == 10), c('year', 'nafo', 'area', 'season', 'preycat', 'percbio')], by = 'year')

p <- ggplot(ah, aes(x = year))
p <- p + geom_line(aes(y = total.cpue), colour = 'red')

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = percbio*900))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./900, name = "Percent in diet"))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p <- p + facet_wrap(nafo ~ area + season, ncol = 2)
p




wp <- ggplot(diet, aes(x = year, y = percbio,
                          color = as.factor((order)), fill = as.factor((order)),width = 0.7))
wp <- wp + geom_bar(stat = 'identity')
wp <- wp + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
wp <- wp  + xlab("Year") + ylab("%Biomass")
wp <- wp + scale_y_continuous(limits = c(0,1.09), labels = scales::percent, breaks = c(0,0.5,1))

# wp <- wp + scale_fill_manual(name = "", values = mypalette ,
#                              breaks = as.factor(nrow(preycat):1),
#                              labels = preys)#,
# #guide = guide_legend( nrow = 2, byrow = T))
# wp <- wp + scale_color_manual(name = "", values = mypalette ,
#                               breaks = as.factor(nrow(preycat):1),
#                               labels = preys)#,
#guide = guide_legend( nrow = 2, byrow = T))
wp <- wp + scale_x_continuous(breaks = seq(1980, 2009, 5),limits = c(1978, 2007))#,
wp <- wp + facet_wrap(nafo ~ area + season)
wp <- wp + theme(legend.position = "bottom")
wp <- wp + ggtitle("Diet composition")
wp <- wp + theme(plot.title = element_text(size = 15, face = "bold"),
                 strip.text = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 13, face = "bold"))
wp
