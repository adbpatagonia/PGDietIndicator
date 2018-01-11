## ADB
## Jan 3, 2018
## Script to explore the potential to use either diet or reprodutive rates of harp seals as indicators
## of their prey base: specifically capelin, arctic cod, and herring

## load libraries ----
library(dplyr)
library(cowplot)

## load data ----
# diet
#load('data/harp_percbio.Rdata')
#percbio <- percbionoseason
# arctic cod
boreo <- read.csv('data/boreogadus.csv')
# atlantic herring
ah <- read.csv('data/atlherring.csv')
# capelin
capelin <- read.csv('data/capelin_acous.csv')
# fecundity
fecun <- read.csv('data/fecun.csv') %>%
  rename(year = cohort_year)

pelagics <- c(9, 10, 12)

fec <- fecun %>%
  right_join(capelin, by = c('year')) %>%
  arrange(year) %>%
  select(year, fecrate, abrate, meancond, cap_bio, ice.1y.jan)

scales <- 15


p <- ggplot(fec, aes(x = year))
p <- p + geom_line(aes(y = abrate), colour = 'red')
#p <- p + ylim(0.2,1)
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = log(cap_bio)/scales))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./scales, name = "capelin biomass"))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#p <- p + facet_grid(nafo ~ area + season, drop = TRUE, scales = 'free')
#p <- p + facet_grid(bay ~ spawner , drop = TRUE)
p <- p + ggtitle("Biomass of capelin compared to PG pregnancy rate")
p <- p + theme(plot.title = element_text(size = 15, face = "bold"),
                 strip.text = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 13, face = "bold"))
p1 <- p

p <- ggplot(fec, aes(x = meancond, y = cap_bio))
p <- p + geom_point()
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#p <- p + facet_wrap(bay ~ spawner , drop = TRUE, scales = 'free')
p2 <- p

fecs <- ah %>%
  group_by(year, bay) %>%
  summarise(total.cpue = sum(ah_cpue)) %>%
  left_join(fecun, by = 'year') %>%
  arrange(year) %>%
  select(year, fecrate, abrate, meancond, total.cpue, bay)

p <- ggplot(fecs, aes(x = year))
p <- p + geom_line(aes(y = fecrate), colour = 'red')
#p <- p + ylim(0.2,1)
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = total.cpue/scales))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./scales, name = "Boreogadus biomass"))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#p <- p + facet_grid(nafo ~ area + season, drop = TRUE, scales = 'free')
p <- p + facet_grid(bay ~ . , drop = TRUE)
p <- p + ggtitle("Biomass of arctic cod compared to PG pregnancy rate")
p <- p + theme(plot.title = element_text(size = 15, face = "bold"),
               strip.text = element_text(size = 12),
               legend.text = element_text(size = 11),
               axis.text = element_text(size = 10),
               axis.title = element_text(size = 13, face = "bold"))
p3 <- p
p <- ggplot(fecs, aes(x = fecrate, y = total.cpue))
p <- p + geom_point()
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p <- p + facet_grid(. ~ bay , drop = TRUE, scales = 'free')
p4 <- p

scales <- 1000

fecs <- ah %>%
  group_by(year) %>%
  summarise(total.cpue = sum(ah_cpue)) %>%
  left_join(fecun, by = 'year') %>%
  arrange(year) %>%
  select(year, fecrate, abrate, meancond, total.cpue)

p <- ggplot(fecs, aes(x = year))
p <- p + geom_line(aes(y = fecrate), colour = 'red')
#p <- p + ylim(0.2,1)
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = total.cpue/scales))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./scales, name = "AH catch rate"))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#p <- p + facet_grid(nafo ~ area + season, drop = TRUE, scales = 'free')
p <- p + ggtitle("Catch rates of Atlantic herring compared to PG pregnancy rate")
p <- p + theme(plot.title = element_text(size = 15, face = "bold"),
               strip.text = element_text(size = 12),
               legend.text = element_text(size = 11),
               axis.text = element_text(size = 10),
               axis.title = element_text(size = 13, face = "bold"))
p5 <- p
p <- ggplot(fecs, aes(x = fecrate, y = total.cpue))
p <- p + geom_point()
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p6 <- p

save_plot("output/fecrate_atlanticherring.png", p5, base_width = 15, base_height = 15)#, dpi = 900) # make room for figure legend)
save_plot("output/fecrate_capelin.png", p1, base_width = 15, base_height = 15)


fecs <- fecs %>%
  group_by(year) %>%
  left_join(select(fecun, year, ice.1y.jan), by = 'year')

with(fecs, plot(ice.1y.jan, total.cpue, pch = 16))


p <- ggplot(fecs, aes(x = year))
p <- p + geom_line(aes(y = ice.1y.jan), colour = 'red')
#p <- p + ylim(0.2,1)
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = total.cpue/scales))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./scales, name = "AH catch rate"))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#p <- p + facet_grid(nafo ~ area + season, drop = TRUE, scales = 'free')
p <- p + ggtitle("Catch rates of Atlantic herring compared to ice extent on Jan 29")
p <- p + theme(plot.title = element_text(size = 15, face = "bold"),
               strip.text = element_text(size = 12),
               legend.text = element_text(size = 11),
               axis.text = element_text(size = 10),
               axis.title = element_text(size = 13, face = "bold"))
p5bis <- p
save_plot("output/icejan_atlanticherring.png", p5bis, base_width = 15, base_height = 15)#, dpi = 900) # make room for figure legend)


fec <- fecun %>%
  right_join(boreo, by = c('year')) %>%
  arrange(year) %>%
  select(year, fecrate, abrate, meancond, boreo_biomass, type)

scales <- 15000


p <- ggplot(filter(fec, type == 'scaled'), aes(x = year))
p <- p + geom_line(aes(y = fecrate), colour = 'red')
#p <- p + ylim(0.2,1)
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = boreo_biomass/scales))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./scales, name = "boreogadus biomass"))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#p <- p + facet_grid(nafo ~ area + season, drop = TRUE, scales = 'free')
#p <- p + facet_grid(bay ~ spawner , drop = TRUE)
p <- p + ggtitle("Biomass of boreogadus compared to PG pregnancy rate")
p <- p + theme(plot.title = element_text(size = 15, face = "bold"),
               strip.text = element_text(size = 12),
               legend.text = element_text(size = 11),
               axis.text = element_text(size = 10),
               axis.title = element_text(size = 13, face = "bold"))
p7 <- p

p <- ggplot(fec, aes(x = fecrate, y = cap_bio))
p <- p + geom_point()
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#p <- p + facet_wrap(bay ~ spawner , drop = TRUE, scales = 'free')
p8 <- p


