# heatmap with marginal histograms

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(scales)
library(grid)
library(gridExtra)

GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(10)
lowGreens <- GreenLong[1:5]
show_col(lowGreens)

heatmap_theme <-  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.ticks.length = unit(0, "pt"),
  legend.position = "none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  plot.margin = unit(c(0,0,0,0), "mm"),
  panel.spacing = unit(c(0,0,0,0), "mm")
)

extract_xls <- "data/DATA EXTRACTION FINAL (17).xlsx"
de <- read_excel(extract_xls, sheet = "Summary DE")

# won't limit combos because Author-date only shows up once
inOutNA <- de %>% 
  select('Author-date', 'Intervention category', 'Outcome category')
colnames(inOutNA) <- c('Author', 'Intervention', 'Outcome')

# if all NA remove row
inOut <- inOutNA[rowSums(is.na(inOutNA)) != ncol(inOutNA), ]

inOut <- inOut %>%
  fill(Author) %>% 
  fill(Intervention) %>% 
  fill(Outcome)

inOut <- inOut %>% 
  mutate(Intervention = replace(Intervention, Intervention=='Resource Use Management', 'Resource use management')) %>% 
  mutate(Outcome = replace(Outcome, Outcome=='Governance (and empowerment)', 'Governance & empowerment'))
  
# uniqueness tested by including Author column
reductionist <- unique(inOut) 

# table count and then save in long tidy format
Freqs <- table(reductionist$Intervention, reductionist$Outcome) 

long <- as.data.frame(Freqs) 
# in_out_long <- as.data.frame(Freqs, stringsAsFactors = FALSE) 
colnames(long) = c('in_y', 'out_x', 'value')

in_order <- c("Habitat management",
              "Resource use management",
              "CBNRM",
              "CBNRM and Health intervention",
              "Health intervention",         
              "Livelihood intervention")
in_order <- rev(in_order)

out_order <- c("Economic living standards",
               "Material living standards",
               "Health",
               "Education",
               "Social relations",
               "Governance",
               "Subjective well-being")

long_original <- long
long$out_x <- factor(long$out_x, out_order)

long_x_out_factor <- long
long$in_y <- factor(long$in_y, in_order)

# Summarise data for marginal plots
in_y_df <- long %>% 
  group_by(in_y) %>% 
  summarise(value = sum(value)) %>% 
  mutate(value = value / sum(value)) %>% 
  mutate(str = paste0( round(value * 100, digits=0), '%' ))

out_x_df <- long %>% 
  group_by(out_x) %>% 
  summarise(value = sum(value)) %>% 
  mutate(value = value / sum(value)) %>% 
  mutate(str = paste0( round(value * 100, digits=0), '%' ))

# Heatmap
# scale_fill_manual(values = lowGreens) +
ph <- ggplot(long, aes(out_x, in_y, fill = value)) +
  geom_tile(color = 'black', size = 0.1) +
  coord_equal() +
  geom_text(aes(label = value), size = 16 / .pt) +
  scale_fill_gradient(low = "#E9F6E5", high = "#84CB83") +
  heatmap_theme +
  theme(panel.spacing = unit(0, "cm")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 30, size = 12,
                                   vjust = 0.96, hjust=0.92))
# ph

# Marginal plots
py <- ggplot(in_y_df, aes(value, in_y)) +
  geom_col(width = .7, fill = 'gray64') +
  geom_text(aes(label = str), hjust = -0.2, size = 13 / .pt) +
  scale_x_continuous(expand = expansion(mult = c(.0, .25))) +
  theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "mm"))

# options(repr.plot.width=6, repr.plot.height=4)
# options(repr.plot.width=6)
px <- ggplot(out_x_df, aes(out_x, value)) +
  geom_col(width = .7, fill = 'gray8') +
  geom_text(aes(label = str), vjust = -0.6, size = 13 / .pt) +
  scale_y_continuous(expand = expansion(mult = c(.0, .25))) +
  theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "mm"))

# Glue plots together
heatPlot <- plot_spacer() + px + plot_spacer() + 
  plot_spacer() + ph + py + 
  plot_layout(ncol = 3, widths = c(1, 2, 0.8), heights = c(1.2, 2))
heatPlot




# prep version with no axis labels on heatmap

dfy <- data.frame(y=1:6)
yLabelsPlot <- ggplot(in_y_df, aes(in_y)) +
  geom_text(label = str)
yLabelsPlot <- ggplot(dfy, aes(y)) +
  geom_text(label = str)

yLabelsPlot <- plot_spacer()
xLabelsPlot <- plot_spacer()

options(repr.plot.width=6)
h2 <- ggplot(long, aes(out_x, in_y, fill = value)) +
  geom_tile(color = 'black', size = 0.2) +
  coord_equal() +
  scale_x_discrete(limits = out_order, expand = c(0,0)) +
  scale_y_discrete(limits = in_order, expand = c(0,0)) +
  geom_text(aes(label = value), size = 12 / .pt) +
  scale_fill_gradient(guide='none',
                      low = "#E9F6E5", high = "#84CB83") +
  heatmap_theme +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_blank())

# h3 <- ggplot(long, aes(out_x, in_y, fill = value)) +
#   geom_tile(color = 'black', size = 0.2) +
#   coord_equal() +
#   geom_text(aes(label = value), size = 12 / .pt) +
#   scale_fill_gradient(low = "#E9F6E5", high = "#84CB83") +
#   theme_void() +
#   theme(legend.position = 0) +
#   labs(x = NULL, y = NULL, fill = NULL)

heatmap_minus_axisText <- h2
heatmap_minus_axisText

# Version with axis labels not in heatplot
squarePlot <- plot_spacer() + px + plot_spacer() + 
  yLabelsPlot + h2 + py +
  plot_spacer() + xLabelsPlot + plot_spacer() +
  plot_layout(ncol = 3, widths = c(1, 2, 1), heights = c(1.2, 2, 1.3))
squarePlot

ggsave("png/heatplot_axisLabels_value15.png", plot=ph,
       device = ragg::agg_png, dpi = 2000,
       units="in", width=4, height=3.5,
       scaling = 0.45)
ggsave("png/heatmap_topbars_f13.png", plot = px,
       device = ragg::agg_png, dpi = 2000,
       units="in", width=3, height=1.5,
       scaling = 0.45)
ggsave("png/heatmap_sidebars_f13.png", plot = py,
       device = ragg::agg_png, dpi = 2000,
       units="in", width=1.5, height=3,
       scaling = 0.45)
ggsave("png/heatmap_square_alone.png", plot = h2,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=2, height=2,
       scaling = 0.45)
ggsave("png/heatmap_axes.png", plot = ph,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=2, height=2,
       scaling = 0.45)

library(gridExtra)
pxgrob <- ggplotGrob(px)
pygrob <- ggplotGrob(py)
h2grob <- ggplotGrob(h2)
t <- textGrob("Empty")

grid.arrange(pxgrob, pygrob, h2grob)

gs <- list(t, pxgrob, pygrob, h2grob)

lay <- rbind(c(1,2,1),
             c(1,4,3),
             c(1,1,1))
grid.arrange(grobs = gs, layout_matrix = lay)

# pieces for Inkscape assembly

ggsave("svg/heatmap_side_bars.svg", plot = py)

ggsave("png/heatmap.png", plot = h2,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=2, height=2,
       scaling = 0.45)
ggsave("png/heatmap_axes.png", plot = ph,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=2, height=2,
       scaling = 0.45)