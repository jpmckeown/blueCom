### Heatmap with marginal bars above and right #######

## Early efforts to make as one plot, in one pass, tried R packages
# iheatmapr: made heatmap with marginal bars, 
#   but unclear how to put numeric labels inside heatmap cells
# complexheatmap: colour palette handling differs, 
#   and the package looks too complicated for this purpose.

# Next plan was to plot in pieces (heatmap, top bars, right bars) 
#   and assemble using patchwork, or cowplot, or gridextra.
# But problem of whitespace around axes when text-labels included,
#   so made square version of heatmap (no intervention or outcome labels)
#   stitch better, but still failed to align nicely. 

# Currently sidestepping problems by manual assembly in Powerpoint.

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(scales)
library(stringi)
library(stringr)

## colour palette 

GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(10)
lowGreens <- GreenLong[1:5]
show_col(lowGreens)

## remove unwanted legend, ticks, title space

heatmap_theme <-  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.ticks.length = unit(0, "null"),
  axis.line = element_blank(),
  legend.position = "none",
  legend.margin = unit(0, "null"),
  panel.grid = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.spacing = unit(c(0,0,0,0), "null"),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  plot.margin = unit(c(0,0,0,0), "null")
)


## Get data and rearrange for heatmap #####

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
  mutate(Outcome = replace(Outcome, Outcome=='Governance (and empowerment)', 'Governance'))
  
# uniqueness tested by including Author column
reductionist <- unique(inOut) 

# table count and then save in long tidy format
Freqs <- table(reductionist$Intervention, reductionist$Outcome) 

long <- as.data.frame(Freqs) 
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


## plot heatmap with labels

ph <- ggplot(long, aes(out_x, in_y, fill = factor(value))) +
  geom_tile(color = 'black', size = 0.2) +
  coord_equal() +
  geom_text(aes(label = value), size = 24 / .pt) +
  scale_fill_manual(values = lowGreens) +
  heatmap_theme +
  theme(panel.spacing = unit(0, "cm")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.y = element_text(size = 18, hjust=1,
                                   margin = margin(r = 0))) +
  theme(axis.text.x = element_text(angle = 45, size = 18,
                                   vjust = 1.07, hjust=1)) +
  scale_y_discrete(labels = function(in_y) str_wrap(in_y, width = 18))
ph

ggsave("png/heatplot_axisLabels_wrapY.png", plot=ph,
       device = ragg::agg_png, dpi = 2000,
       units="in", width=3.7, height=3,
       scaling = 0.45)


## Marginal bar plots ##################

# Summarise data for bars
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

py <- ggplot(in_y_df, aes(value, in_y)) +
  geom_col(width = .7, fill = 'gray64') +
  geom_text(aes(label = str), hjust = -0.2, size = 13 / .pt) +
  scale_x_continuous(expand = expansion(mult = c(.0, .25))) +
  theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "mm"))
py

px <- ggplot(out_x_df, aes(out_x, value)) +
  geom_col(width = .7, fill = 'gray8') +
  geom_text(aes(label = str), vjust = -0.6, size = 13 / .pt) +
  scale_y_continuous(expand = expansion(mult = c(.0, .25))) +
  theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "mm"))
px

ggsave("png/heatmap_topbars_f13.png", plot = px,
       device = ragg::agg_png, dpi = 2000,
       units="in", width=3, height=1.5,
       scaling = 0.45)

ggsave("png/heatmap_sidebars_f13.png", plot = py,
       device = ragg::agg_png, dpi = 2000,
       units="in", width=1.5, height=3,
       scaling = 0.45)
