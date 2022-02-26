# Heatmap with marginal barcharts

library(tidyverse)
library(iheatmapr)
library(RColorBrewer)
library(ragg)

in_out <- data.frame(
  'Economic' = c(1,1,1,5,4),
  'Education' = c(0,0,0,1,1),
  'Health' = c(1,0,1,0,0),
  'Social' = c(1,1,0,3,1) )
rownames(in_out) <- c('Habitat', 'Resource', 'Combined', 'Protected', 'Livelihood')

GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(12)
lowGreens <- GreenLong[0:5]

in_out_matrix <- as.matrix(in_out)
main_heatmap(in_out_matrix, colors = lowGreens)

in_out_plot <- iheatmap(in_out_matrix,
  colors=lowGreens) %>% 
  add_col_labels() %>% 
  add_row_labels() %>% 
  add_col_barplot(y = colSums(bcio)/total) %>% 
  add_row_barplot(x = rowSums(bcio)/total)
in_out_plot

save_iheatmap(in_out_plot, "png/iheatmapr_test.png")

ggsave("png/iheatmapr_test.png", plot = in_out_plot,
       device = ragg::agg_png, dpi = 72,
       units="in", width=3.453, height=3.03,
       scaling = 0.45)

main_heatmap(in_out_matrix) %>% 
  add_col_labels() %>% 
  add_row_labels() %>% 
  add_col_summary() %>% 
  add_row_summary()

# bcio_matrix %>% save_iheatmap("matrix_inOut.html") # save interactive HTML
bcio_matrix %>% save_iheatmap("bitmap/matrix_inOut.png") # save static plot

in_out_plot1 <- iheatmap(in_out_matrix,
                         colors=lowGreens,
                         col_title = "Outcome",
                         row_title = "Intervention") %>% 
  add_col_labels() %>% 
  add_row_labels() %>% 
  add_col_barplot(y = colSums(bcio)/total) %>% 
  add_row_barplot(x = rowSums(bcio)/total)
in_out_plot1