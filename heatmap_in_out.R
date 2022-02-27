# heatmap with marginal histograms

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(scales)
library(janitor) 

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
  plot.margin = unit(c(0,0,0,0), "mm")
)

extract_xls <- "data/DATA EXTRACTION FINAL (16).xlsx"
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

in_out_long <- as.data.frame(Freqs, stringsAsFactors = FALSE) 
colnames(in_out_long) = c('in_y', 'out_x', 'value')

# Summarise data for marginal plots
in_y_df <- in_out_long %>% 
  group_by(in_y) %>% 
  summarise(value = sum(value)) %>% 
  mutate(value = value / sum(value))

out_x_df <- in_out_long %>% 
  group_by(out_x) %>% 
  summarise(value = sum(value)) %>% 
  mutate(value = value / sum(value)) %>% 
  mutate(str = paste0( round(value * 100, digits=0), '%' ))

# Heatmap
ph <- ggplot(in_out_long, aes(out_x, in_y, fill = value)) +
  geom_tile(color = 'black', size = 0.2) +
  coord_equal() +
  geom_text(aes(label = value), size = 12 / .pt) +
  scale_fill_gradient(low = "#E9F6E5", high = "#84CB83") +
  # scale_fill_manual(values = lowGreens) +
  heatmap_theme +
  # theme(panel.spacing = unit(0, "cm")) +
  labs(x = NULL, y = NULL, fill = NULL)

# axis.text.x = element_blank(),
# axis.text.y = element_blank(),

# Marginal plots
py <- ggplot(in_y_df, aes(value, in_y)) +
  geom_col(width = .7, fill = 'gray64') +
  geom_text(aes(label = scales::percent(value)), hjust = -0.1, size = 10 / .pt) +
  scale_x_continuous(expand = expansion(mult = c(.0, .25))) +
  theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "mm"))

px <- ggplot(out_x_df, aes(out_x, value)) +
  geom_col(width = .7, fill = 'gray8') +
  geom_text(aes(label = str), vjust = -0.5, size = 10 / .pt) +
  # scale_y_continuous(expand = expansion(mult = c(.0, .25))) +
  scale_y_continuous(expand = expansion(mult = c(.0, .25))) +
  theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "mm"))

# Glue plots together
px + plot_spacer() + 
  ph + py + 
  plot_layout(ncol = 2, widths = c(2, 1), heights = c(1, 2))

