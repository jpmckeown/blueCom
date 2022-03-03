# Map of SE Asia with bars
library(grImport2)
library(rsvg)
library(grid)
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
library(grid)

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
  plot.background = element_rect(fill = "transparent")
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
                                   angle = 30, vjust=1, hjust=0.9)) +
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
                                   angle = 30, vjust=1, hjust=0.8)) +
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
                                   angle = 30, vjust=1, hjust=0.8)) +
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
                                   angle = 30, vjust=1, hjust=0.8)) +
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

library(patchwork)
smg + Thailand_bar # fails

# cowplot
library(cowplot)
library(magick)
small_file <- "png/seAsia.png"
small_map <- image_read(small_file)
image_browse(small_map)

ggdraw() +
  draw_image(
    small_map, scale = .3, x = 1,
    hjust = 1, halign = 1, valign = 0
  ) +
  draw_plot(Thailand_bar)

draw_image(smimg)
plot_grid()

# grid
library(gridExtra)




# deleted
#   scale_y_continuous(limits=c(0, 5), expand = c(0, 0)) +
# scale_x_discrete(expand = c(0, 0)) +
# , expand = c(0, 0)
# plot.margin = unit(c(0,0,0,0), "mm")
# Number only, omit country label

df <- country_plot_df %>% filter(Name == 'Philippines')
Philippines_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  barNoCountryTheme 
Philippines_bar

df <- country_plot_df %>% filter(Name == 'Thailand')
Thailand_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  barNoCountryTheme
Thailand_bar

df <- country_plot_df %>% filter(Name == 'Indonesia')
Indonesia_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0,0,0,1), "in"))
Indonesia_bar

df <- country_plot_df %>% filter(Name == 'Vietnam')
Vietnam_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0,0.2,0,1), "in"))
Vietnam_bar

df <- country_plot_df %>% filter(Name == 'Cambodia')
Cambodia_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme
Cambodia_bar