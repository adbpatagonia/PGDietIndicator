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
  right_join(ah, by = c('year')) %>%
  arrange(year) %>%
  select(year, fecrate, abrate, meancond, ah_cpue, spawner, bay)

scales <- 900


p <- ggplot(fec, aes(x = year))
p <- p + geom_line(aes(y = fecrate), colour = 'red')
#p <- p + ylim(0.2,1)
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = ah_cpue/scales))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./scales, name = "Boreogadus biomass"))
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#p <- p + facet_grid(nafo ~ area + season, drop = TRUE, scales = 'free')
p <- p + facet_grid(bay ~ spawner , drop = TRUE)
p <- p + ggtitle("Biomass of arctic cod compared to PG pregnancy rate")
p <- p + theme(plot.title = element_text(size = 15, face = "bold"),
                 strip.text = element_text(size = 12),
                 legend.text = element_text(size = 11),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 13, face = "bold"))
p1 <- p

p <- ggplot(fec, aes(x = fecrate, y = ah_cpue))
p <- p + geom_point()
p <- p + theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p <- p + facet_wrap(bay ~ spawner , drop = TRUE, scales = 'free')
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
