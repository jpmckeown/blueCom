# Map of Sout-East Asia with invidual country bars
library(tidyverse)
library(janitor)
library(png)
library(magick)

## background map

# download original from Wikimedia 
# convert to PNG format via Inkscape export
# map is background (bg) for country bars and labels

bg_file <- 'png/background_map.png'


## Theme for single country bar ####

onebarTheme <-  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(), 
  
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.ticks.length = unit(0, "pt"),
  legend.position = "none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", color = "transparent")
) 

country_label_size = 28
number_label_size = 11
rightMargin = 0.2
leftMargin = 1.3


## get country data, and calculate proportions %

original_xls <- "data/Script-friendly P1_SLR data extraction sheet 180222_Combined (1).xlsx"

column_names <- read_excel(original_xls, 
                           sheet = "Data gathering Syikin", 
                           .name_repair = "minimal") %>% names()

mde <- read_excel(original_xls, sheet = "Data gathering Syikin", 
                  skip = 2, col_names = column_names) 


bar_data <- de %>% 
  select(Country) %>% 
  drop_na()

thisTable <- tabyl(thisData$Country)
names(thisTable)[1] <- 'Country'
country_table <- thisTable # store

df <-  data.frame(thisTable$Country, thisTable$n) 
colnames(df) <- c('Name', 'Absolute')

# factor to keep order
df <- df %>% 
  mutate(Name = factor(Name, levels = Name)) 

countries_bar_df <- df # store


## plot each country's bar ###########

df <- countries_bar_df %>% 
  filter(Name == 'Philippines')

Philippines_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Philippines_bar


df <- countries_bar_df %>% 
  filter(Name == 'Thailand')

Thailand_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1, hjust=0.78)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Thailand_bar


df <- countries_bar_df %>% 
  filter(Name == 'Indonesia')

Indonesia_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1.05, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Indonesia_bar


df <- countries_bar_df %>% 
  filter(Name == 'Vietnam')

Vietnam_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1.1, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Vietnam_bar


df <- countries_bar_df %>% 
  filter(Name == 'Cambodia')

Cambodia_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1.01, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Cambodia_bar


## save each bar as image PNG file

ggsave("png/Indonesia_bar.png", plot=Indonesia_bar,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.2,
       scaling = 0.45)

ggsave("png/Vietnam_bar.png", plot=Vietnam_bar,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.2,
       scaling = 0.45)

ggsave("png/Cambodia_bar.png", plot=Cambodia_bar,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.2,
       scaling = 0.45)

ggsave("png/Philippines_bar.png", plot=Philippines_bar,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.2,
       scaling = 0.45)

ggsave("png/Thailand_bar.png", plot=Thailand_bar,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.2,
       scaling = 0.45)


## superimpose bars on background map #########

map_w7031 <- image_read('png/background_map_w7031.png')
# add a border line around map
map_w7031 <- image_border(map_w7031, "black", "2x2")

# can review background map
# print(map_w7031)

# import country bars as ImageMagick objects

thailand_img <- image_read("png/Thailand_bar.png")
vietnam_img <- image_read("png/Vietnam_bar.png")
indonesia_img <- image_read("png/Indonesia_bar.png")
philippines_img <- image_read("png/Philippines_bar.png")
cambodia_img <- image_read("png/Cambodia_bar.png")

## superimpose country bars one at a time

# position each bar -- offset +X+Y -- by trial and error
map_bars_t <- image_composite(map_w7031, thailand_img, offset="+400+400")
map_bars_vt <- image_composite(map_bars_t, vietnam_img, offset="+1750+1100")
map_bars_ivt <- image_composite(map_bars_vt, indonesia_img, offset="+1600+3500")
map_bars_pivt <- image_composite(map_bars_ivt, philippines_img, offset="+3400+2000")
map_bars_cpivt <- image_composite(map_bars_pivt, cambodia_img, offset="+1000+1900")

image_write(map_bars_cpivt, path = "png/map_bars_all.png", format = "png")
