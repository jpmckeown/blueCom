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
bg_file <- 'png/seAsia.png'
img <- png::readPNG(bg_file)
g <- rasterGrob(img, interpolate=TRUE)

p_full <- ggplot(iris, aes(Species, Sepal.Length))+
  background_image(img)+
  geom_boxplot(aes(fill = Species), color = "white")

ggsave("png/R_output_bg7031w_map.png", plot=p_full,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=7.4, height=5.3,
       scaling = 0.45)
