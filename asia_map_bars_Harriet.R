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

original_xls <- "data/Script-friendly P1_SLR data extraction sheet 180222_Combined (2).xlsx"

column_names <- read_excel(original_xls, 
                           sheet = "Data gathering Syikin", 
                           .name_repair = "minimal") %>% names()

mde <- read_excel(original_xls, sheet = "Data gathering Syikin", 
                  skip = 2, col_names = column_names) 

bar_data <- mde %>% 
  select(Country) %>% 
  drop_na()

bar_data <- bar_data %>% 
  mutate(Country = ifelse(Country == 'Panguil Bay, Mindanao, Philippines', 
         'Philippines', Country))

thisTable <- tabyl(bar_data$Country)
names(thisTable)[1] <- 'Country'
country_table_Harriet <- thisTable # store

df <-  data.frame(thisTable$Country, thisTable$n) 
colnames(df) <- c('Name', 'Absolute')

# factor to keep order
df <- df %>% 
  mutate(Name = factor(Name, levels = Name)) 

countries_bar_df_H <- df # store


## plot each country's bar ###########

df <- countries_bar_df_H %>% 
  filter(Name == 'Philippines')

Philippines_bar_H <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 8)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Philippines_bar_H


df <- countries_bar_df_H %>% 
  filter(Name == 'Thailand')

Thailand_bar_H <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 8)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1, hjust=0.78)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Thailand_bar_H


df <- countries_bar_df_H %>% 
  filter(Name == 'Indonesia')

Indonesia_bar_H <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 8)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1.05, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Indonesia_bar_H


df <- countries_bar_df_H %>% 
  filter(Name == 'Vietnam')

Vietnam_bar_H <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 8)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1.1, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Vietnam_bar_H


df <- countries_bar_df_H %>% 
  filter(Name == 'Malaysia')

Malaysia_bar_H <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 8)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1.01, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0, rightMargin, 0, leftMargin), "in"))
Malaysia_bar_H


## save each bar as image PNG file

ggsave("png/Indonesia_bar_H.png", plot=Indonesia_bar_H,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.8,
       scaling = 0.45)

ggsave("png/Vietnam_bar_H.png", plot=Vietnam_bar_H,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.8,
       scaling = 0.45)

ggsave("png/Malaysia_bar_H.png", plot=Malaysia_bar_H,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.8,
       scaling = 0.45)

ggsave("png/Philippines_bar_H.png", plot=Philippines_bar_H,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.8,
       scaling = 0.45)

ggsave("png/Thailand_bar_H.png", plot=Thailand_bar_H,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=1.2, height=2.8,
       scaling = 0.45)


## superimpose bars on background map #########

map_w7031 <- image_read('png/background_map_w7031.png')
# add a border line around map
map_w7031 <- image_border(map_w7031, "black", "2x2")

# can review background map
# print(map_w7031)

# import country bars as ImageMagick objects

thailand_img <- image_read("png/Thailand_bar_H.png")
vietnam_img <- image_read("png/Vietnam_bar_H.png")
indonesia_img <- image_read("png/Indonesia_bar_H.png")
philippines_img <- image_read("png/Philippines_bar_H.png")
malaysia_img <- image_read("png/Malaysia_bar_H.png")

## superimpose country bars one at a time

# position each bar -- offset +X+Y -- by trial and error
map_bars_H_t <- image_composite(map_w7031, thailand_img, offset="+400+300")
map_bars_H_vt <- image_composite(map_bars_H_t, vietnam_img, offset="+1500+0")
map_bars_H_ivt <- image_composite(map_bars_H_vt, indonesia_img, offset="+2250+3100")
map_bars_H_pivt <- image_composite(map_bars_H_ivt, philippines_img, offset="+3800+1700")
map_bars_H_cpivt <- image_composite(map_bars_H_pivt, malaysia_img, offset="+1200+1450")

image_write(map_bars_H_cpivt, path = "png/map_bars_H_all.png", format = "png")
