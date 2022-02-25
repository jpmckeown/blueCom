library(tidyverse)
library(knitr)
library(readxl)
library(janitor)
library(RColorBrewer)
library(svglite)
library(ggpubr)
library(magick)
library(scales)
library(systemfonts)
library(grid)
library(ragg)

## Themes

barOnlyTheme <-  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.ticks.length = unit(0, "pt"),
  legend.position = "none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  plot.margin = unit(c(0,0,0,0), "mm")
)

## Colour gradients

BlueLong <- colorRampPalette(brewer.pal(9, 'Blues'))(14)
midBlues5 <- BlueLong[3:7]
show_col(midBlues5)

#assemble Biosphere blues
show_col(biosphereBlues)

## Data

original_xls <- "data/DATA EXTRACTION FINAL (16).xlsx"
de <- read_excel(original_xls, sheet = "Summary DE", .name_repair = "minimal")
Table_1_xls <- "data/Tables for report (23).xlsx"
t1 <- read_excel(Table_1_xls, sheet = "Table 1 final")

bar_data <- de %>% 
  select(Country, `Biosphere reserve`)
names(bar_data) <- c('Country', 'Biosphere')

## Countries bar

# Country data, and calculate proportion
thisData <- bar_data %>% 
  select(Country) %>% 
  drop_na()

thisTable <- tabyl(thisData$Country)
names(thisTable)[1] <- 'Country'

thisTable <- thisTable[order(thisTable$n),]

Name <- thisTable$Country
Value <- thisTable$percent
Absolute <- thisTable$n
yPos <- cumsum(lag(Value, default = 0))
percentage <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percentage, '%)')
n <- sum(thisTable$n)

# factor to keep order
df <-  data.frame(Name, Absolute, Value, yPos, Str) %>%
  mutate(Name = factor(Name, levels = Name)) 

country_plot_df <- df # store

thisPlot <- ggplot(data = df, aes(x = 1, y = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.2) +
  geom_text(aes(label = Name),
            position = position_stack(), 
            size  = ifelse(Name == 'Indonesia', 6.4, 7), 
            vjust = ifelse(Name == 'Indonesia', 1.8, 2.2),
            alpha = ifelse(Name == 'Thailand', 0, 1) ) +
  geom_text(aes(label = paste0('(', percentage, '%)')),
            position = position_stack(), 
            size  = ifelse(Name == 'Indonesia', 5.5, 6), 
            vjust = ifelse(Name == 'Indonesia', 4.2, 4.5) ,
            alpha = ifelse(Name == 'Thailand', 0, 1) ) +
  geom_text(aes(label = paste0(Name, ' (', percentage, '%)') ),
            position = position_stack(), size = 5.5, 
            vjust = 1.7,
            alpha = ifelse(Name == 'Thailand', 1, 0) ) +
  scale_y_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midBlues5) +
  coord_cartesian(clip = 'off') +
  barOnlyTheme


  
thisPlot
countryVerticalPlot <- thisPlot

ggsave("png/country_vertical_AGG_centred.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=0.9, height=3.5,
       scaling = 0.45)

# Sequence biosphere by country, remove NA, tidy values

tmp <- bar_data %>% 
  drop_na() %>% 
  arrange(Country, Biosphere) %>% 
  arrange(match(Country, c('Philippines', 'Vietnam', 'Cambodia', 'Indonesia', 'Thailand')))

tmp <- tmp[nrow(tmp):1,] # reverse rows if want high freq country at bottom

biosphereBycountryData <- tmp %>%
  mutate(Biosphere = replace(Biosphere, Biosphere=='Komodo MPA', 'Komodo')) %>% 
  mutate(Biosphere = replace(Biosphere, Biosphere=='Cat Tien (Dong Nai)', 'Cat Tien'))

biosphereBycountryData

