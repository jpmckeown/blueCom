# Map of SE Asia with bars
library(grImport2)
library(rsvg)
library(grid)

# download from Wikimedia in SVG format, convert to PNG
# instead imported into Inkscape and saved to PNG from there

rsvg_svg('svg/Southeast_Asia_blank_political_map.svg' , 'svg/map_CairoFormat.svg')
mapBackground <- readPicture('svg/map_CairoFormat.svg')
bg <- grid.picture(mapBackground)

ggsave("png/map_background.png", plot=bg,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3.4, height=2.8,
       scaling = 0.45)
# fail, result blank image

library(ggpubr)
library(png)

bg_file <- 'png/map_fullwidth.png'
smbg_file <- 'png/seAsia.png'
smimg <- png::readPNG(smbg_file)
g <- rasterGrob(img, interpolate=TRUE)
smg <- rasterGrob(img, interpolate=TRUE)

p_full <- ggplot(iris, aes(Species, Sepal.Length))+
  background_image(img)+
  geom_boxplot(aes(fill = Species), color = "white")

ggsave("png/R_output_bg7031w_map.png", plot=p_full,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=7.4, height=5.3,
       scaling = 0.45)

## one country bars ####

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

df <- country_plot_df %>% filter(Name == 'Philippines')
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

df <- country_plot_df %>% filter(Name == 'Thailand')
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

df <- country_plot_df %>% filter(Name == 'Indonesia')
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

df <- country_plot_df %>% filter(Name == 'Vietnam')
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

df <- country_plot_df %>% filter(Name == 'Cambodia')
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

# suggested intermediate route? bad scaling, 
# and still has white artefact lines top and bottom
ggsave("svg/Thailand_bar.svg", plot=Thailand_bar, dpi = 1000,
       units="in", width=1.2, height=2.2)

# superimpose bar png on map png
library(magick)
map_w7031 <- image_read('png/map_fullwidth.png')
map_w7031 <- image_border(map_w7031, "black", "2x2")
print(map_w7031)

map_w7031_txt <- image_annotate(map_w7031, "Number of studies by country", 
               size = 30, color = "black", location = "+150+100")

# test shows it isnt composite making the white border
penguin_img <- image_read("bitmap/Tux.png")
map_test <- image_composite(map_w7031, penguin_img)
image_write(map_test, path = "png/map_test_composite.png", format = "png")

thailand_img <- image_read("png/Thailand_bar.png")
vietnam_img <- image_read("png/Vietnam_bar.png")
indonesia_img <- image_read("png/Indonesia_bar.png")
philippines_img <- image_read("png/Philippines_bar.png")
cambodia_img <- image_read("png/Cambodia_bar.png")

map_bars_t <- image_composite(map_w7031, thailand_img, offset="+400+400")
map_bars_vt <- image_composite(map_bars_t, vietnam_img, offset="+1750+1100")
map_bars_ivt <- image_composite(map_bars_vt, indonesia_img, offset="+1600+3500")
map_bars_pivt <- image_composite(map_bars_ivt, philippines_img, offset="+3400+2000")
map_bars_cpivt <- image_composite(map_bars_pivt, cambodia_img, offset="+1000+1900")

image_write(map_bars_cpivt, path = "png/map_bars_all.png", format = "png")

# cannot save magick object
# ggsave("png/map_bars.png", plot=map_bars,
#        device = ragg::agg_png, dpi = 1000,
#        units="in", width=7.4, height=5.3,
#        scaling = 0.45)
