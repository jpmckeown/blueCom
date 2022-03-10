## Figure with one bar vertical stack for Countries alongside
## biosphere reserves aligned with appropriate country.

# expects subfolders /data/ for downloaded spreadsheets,
#  and /png/ for plot image files, and combined output image.

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(scales)
library(ragg)
library(patchwork)
library(magick)


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

# blue gradient for Countries
BlueLong <- colorRampPalette(brewer.pal(9, 'Blues'))(14)
midBlues5 <- BlueLong[3:7]
show_col(midBlues5) 

# Biosphere colour array customised so each matches Country
biosphereBlues <- c("#D8E7F5", "#C9DDF0", "#C9DDF0", "#B3D3E8", 
                    "#9AC7E0", "#9AC7E0", "#9AC7E0", "#7AB6D9")
show_col(biosphereBlues)

GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(16)
midGreens8 <- GreenLong[4:11]
show_col(midGreens8)


## Data

original_xls <- "data/DATA EXTRACTION FINAL (16).xlsx"
de <- read_excel(original_xls, sheet = "Summary DE", .name_repair = "minimal")

bar_data <- de %>% 
  select(Country, `Biosphere reserve`)
names(bar_data) <- c('Country', 'Biosphere')


## Countries bar

# get country data, and calculate proportions %
thisData <- bar_data %>% 
  select(Country) %>% 
  drop_na()

thisTable <- tabyl(thisData$Country)
names(thisTable)[1] <- 'Country'

thisTable <- thisTable[order(thisTable$n),]
country_table <- thisTable # store
thisTable <- country_table

Name <- thisTable$Country
Value <- thisTable$percent
Absolute <- thisTable$n
yPos <- cumsum(lag(Value, default = 0))
percentage <- (round(Value * 100, digits=0))
Str <- paste0('(', percentage, '%)')
# Str <- paste0(Absolute, ' (', percentage, '%)')
n <- sum(thisTable$n)

# factor to keep order
df <-  data.frame(Name, Absolute, Value, yPos, Str) %>%
  mutate(Name = factor(Name, levels = Name)) 

country_plot_df <- df # store
df <-  country_plot_df

# Plot: country name and % on separate lines, except
#       Thailand name & % on same line because segment is narrow.
# Elsewhere (on geog map) Country bar shows absolute number.  

thisPlot <- ggplot(data = df, aes(x = 1, y = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.2) +
  geom_text(aes(label = Name),
            position = position_stack(), 
            size  = ifelse(Name == 'Indonesia', 6.8, 7.4), 
            vjust = ifelse(Name == 'Indonesia', 1.8, 2.2),
            alpha = ifelse(Name == 'Thailand', 0, 1) ) +
  geom_text(aes(label = Str),
            position = position_stack(), 
            size  = ifelse(Name == 'Indonesia', 5.5, 6), 
            vjust = ifelse(Name == 'Indonesia', 4.2, 4.5) ,
            alpha = ifelse(Name == 'Thailand', 0, 1) ) +
  geom_text(aes(label = paste(Name, Str)),
            position = position_stack(), 
            size = 5.5, 
            vjust = 1.7,
            alpha = ifelse(Name == 'Thailand', 1, 0) ) +
  scale_y_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midBlues5) +
  coord_cartesian(clip = 'off') +
  barOnlyTheme

#   geom_text(aes(label = paste0('(', percentage, '%)')),
#   geom_text(aes(label = paste0(Name, ' (', percentage, '%)') ),
thisPlot
countryVerticalPlot <- thisPlot

ggsave("png/country_vertical_centred.png", plot=thisPlot,
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

biosphereFreq <- tabyl(biosphereBycountryData$Biosphere)
names(biosphereFreq)[1] <- 'Biosphere'

biosphereFreqA <- biosphereFreq %>% 
  arrange(match(Biosphere, c('Ranong', 'Lore Lindu', 'Komodo', 'Tonle Sap', 'Red River Delta', 'Cu Lao Cham', 'Cat Tien', 'Palawan')))
thisTable <- biosphereFreqA

Name <- thisTable$Biosphere
Value <- thisTable$percent
Absolute <- thisTable$n
percentage <- (round(Value * 100, digits=0))
Str <- paste0('(', Absolute, ')')
n <- sum(thisTable$n)
# yPos <- cumsum(lag(Value, default = 0)) + Value / 2
# R built-in position_stack() used instead of yPos

# factor to keep order
df <-  data.frame(Name, Absolute, Value, Str) %>%
  mutate(Name = factor(Name, levels = Name)) 

biosphere_plot_df <- df # store
df <-  biosphere_plot_df

thisPlot <- ggplot(data = df, aes(x = 1, y = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.2) +
  geom_text(aes(label = paste(Name, Str)),
            position = position_stack(), 
            size = ifelse(Absolute == 1, 5.5, 
                          ifelse(Absolute == 2, 6.2, 6.8)),
            vjust = ifelse(Absolute == 1, 1.8, 
                           ifelse(Absolute == 2, 2.2, 3))) +
  scale_y_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = biosphereBlues) +
  coord_cartesian(clip = 'off') +
  barOnlyTheme

# earlier version used green gradient for biosphere
#  but green is used for one of horizontal bars.
#  scale_fill_manual(values = midGreens8) +
#   biosphereVerticalPlotGreen <- thisPlot

thisPlot
biosphereVerticalPlot <- thisPlot

ggsave("png/biosphere_vertical_blues.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=3.5,
       scaling = 0.45)


# uses ImageMagick to stitch images together

country_img <- image_read('png/country_vertical_centred.png')
biosphere_blue_img <- image_read('png/biosphere_vertical_blues.png')

image_info(country_img)
image_info(biosphere_blue_img)

img2b <- c(country_img, biosphere_blue_img)
cbb <- image_append(img2b)
image_write(cbb, path='png/countriesBiosphereBlue.png', format='png')


## Earlier green version
# biosphere_green_img <- image_read('png/biosphere_vertical_greens.png')
# img2g <- c(country_img, biosphere_green_img)
# cbg <- image_append(image_scale(img2g))
# image_write(cbg, path='png/countriesBiosphereGreen.png', format='png')