## ADB
## Jan 3, 2018
## Script to explore the potential to use either diet or reprodutive rates of harp seals as indicators
## of their prey base: specifically capelin, arctic cod, and herring

## load libraries ----
library(dplyr)
library(cowplot)

## load data ----
# diet
load('data/harp_percbio_noseason.Rdata')
percbio <- percbionoseason
rm(percbionoseason)
diet <- filter(percbio, order == 13 | order == 11 | order == 10)
# arctic cod
boreo <- read.csv('data/boreogadus.csv')
# atlantic herring
ah <- read.csv('data/atlherring.csv') %>% group_by(year, bay) %>%
  summarize(total.cpue = sum(ah_cpue)) %>%
  mutate(nafo =  ifelse(bay == "WBNB","3K","3L")) %>%
  merge(percbio[which(percbio$order == 10 & percbio$area == 'Inshore'), c('year', 'nafo', 'area', 'preycat', 'percbio')], by = c('year', 'nafo'))


p <- ggplot(ah, aes(x = year))
p <- p + geom_line(aes(y = total.cpue), colour = 'red')

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = percbio*900))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./900, name = "Percent in diet"))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p <- p + facet_grid(nafo ~ area, drop = TRUE)
p <- p + ggtitle("Catch rates of Atlantic Herring compared to percent on PG diet")
p <- p + theme(plot.title = element_text(size = 15, face = "bold"),
                 strip.text = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 13, face = "bold"))
p

save_plot("output/AH_CatchRate_Diet_noseason.png", p, base_width = 10, base_height = 10)#, dpi = 900) # make room for figure legend)

