library(tidyverse)
library(readxl)
library(janitor)
library(RColorBrewer)
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

# blue gradient for Countries
BlueLong <- colorRampPalette(brewer.pal(9, 'Blues'))(14)
midBlues5 <- BlueLong[3:7]
show_col(midBlues5) 

# Biosphere customised to match Country
biosphereBlues <- c("#D8E7F5", "#C9DDF0", "#C9DDF0", "#B3D3E8", 
                    "#9AC7E0", "#9AC7E0", "#9AC7E0", "#7AB6D9")
show_col(biosphereBlues)

# midGreens8 <- GreenLong[4:11]
# GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(16)


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
yPos <- cumsum(lag(Value, default = 0))
percentage <- (round(Value * 100, digits=0))
Str <- paste0('(', Absolute, ')')
n <- sum(thisTable$n)

# factor to keep order
df <-  data.frame(Name, Absolute, Value, yPos, Str) %>%
  mutate(Name = factor(Name, levels = Name)) 

biosphere_plot_df <- df # store
df <-  biosphere_plot_df

thisPlot <- ggplot(data = df, aes(x = 1, y = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.2) +
  geom_text(aes(label = paste(Name, Str)),
            position = position_stack(), 
            size = ifelse(Absolute == 1, 5.5, 6.4),
            vjust = ifelse(Absolute == 1, 1.8, 2.2)) +
  scale_y_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midGreens8) +
  coord_cartesian(clip = 'off') +
  barOnlyTheme
#   scale_fill_manual(values = midGreens8) +
# scale_fill_manual(values = biosphereBlues) +
thisPlot
biosphereVerticalPlotGreen <- thisPlot

# Error when try patchwork to combine R plots before output image file.
library(patchwork)
cb <- countryVerticalPlot + biosphereVerticalPlot +
  plot_layout(widths = c(0.9, 1.2))
cg <- countryVerticalPlot + biosphereVerticalPlotGreen +
  plot_layout(widths = c(0.9, 1.2))

ggsave("png/country_biosphere_vertical_green.png", plot=cg,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=2, height=3.5,
       scaling = 0.45)
ggsave("png/biosphere_vertical_blues.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=3.5,
       scaling = 0.45)

country_img <- image_read('png/country_vertical_centred.png')
biosphere_green_img <- image_read('png/biosphere_vertical_greens.png')
biosphere_blue_img <- image_read('png/biosphere_vertical_blues.png')
image_info(country_img)
image_info(biosphere_blue_img)

img2g <- c(country_img, biosphere_green_img)
cbg <- image_append(image_scale(img2g))
image_write(cbg, path='png/countriesBiosphereGreen.png', format='png')

img2b <- c(country_img, biosphere_blue_img)
cbb <- image_append(image_scale(img2b))
image_write(cbb, path='png/countriesBiosphereBlue.png', format='png')

## problem was vector to make df had same name as df columns

# experiment 1 using biosphere plot with country data
# fails - so the problem is data
# thisPlot <- ggplot(data = df, aes(x = 1, y = Absolute, fill = Name)) +
#   geom_col(color = 'black', size = 0.2) +
#   geom_text(aes(label = paste(Name, Str)),
#             position = position_stack(), 
#             size = ifelse(Absolute == 1, 5.5, 
#                           ifelse(Absolute == 2, 6.2, 6.8)),
#             vjust = ifelse(Absolute == 1, 1.8, 
#                            ifelse(Absolute == 2, 2.2, 3))) +
#   scale_y_continuous(limits=c(0, n), expand = c(0, 0)) +
#   scale_x_discrete(expand = c(0, 0)) +
#   coord_cartesian(clip = 'off') +
#   barOnlyTheme

# experiment 2, using biosphere data in country ggplot
# df <-  biosphere_plot_df
# no error, suggesting data is problem, but not see any difference!

# experiment 3, simplify plot
# runs until add ifelse 
thisPlot <- ggplot(data = df, aes(x = 1, y = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.2) +
  # geom_text(aes(label = Name),
  #           position = position_stack(),
  #           size = 5.5,
  #           vjust = 1.7 ) +
  geom_text(aes(label = Name),
            position = position_stack(),
            size  = ifelse(Name == 'Indonesia', 6.8, 7.4) 
  ) +
  scale_y_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midBlues5) +
  coord_cartesian(clip = 'off') +
  barOnlyTheme

# experiment 4, ifelse test Absolute instead of Name
# same error
thisPlot <- ggplot(data = df, aes(x = 1, y = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.2) +
  geom_text(aes(label = Name),
            position = position_stack(),
            # size = 6
            size = ifelse(Absolute == 2, 6.8, 7.4) ) +
  scale_y_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midBlues5) +
  coord_cartesian(clip = 'off') +
  barOnlyTheme

thisPlot
