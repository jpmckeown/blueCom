# heatmap with marginal histograms

library(tidyverse)
library(readxl)
library(iheatmapr)
library(RColorBrewer)
library(scales)

extract_xls <- "data/DATA EXTRACTION FINAL (13).xlsx"
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
  
reductionist <- unique(inOut) # uniqueness test includes Author column

Freqs <- table(reductionist$Intervention, reductionist$Outcome) 
Freqs
library(ztable)
options(ztable.type="html")
z=ztable(Freqs) 
print(z,caption="Intervention and Outcome matrix")

library(janitor) 
t1 <- reductionist %>% 
  tabyl(Intervention, Outcome) 
t1

bcio_table_matrix <- as.matrix(Freqs)
bcio_vector <- c(Freqs)

# bcio <- matrix(sample.int(5, 25, replace=TRUE), nrow=5, byrow=TRUE)
bcio <- matrix(bcio_vector, nrow=6, byrow=FALSE)
colSums(bcio)
rowSums(bcio)
total <- sum(colSums(bcio))

# rownames(bcio) <- c('CBNRM', 'CBNRM & Health', 'Habitat management', 'Health', 'Livelihood', 'Resource management')
rownames(bcio) <- c('CBNRM', 'CBNRM & Health', 'Habitat', 'Health', 'Livelihood', 'Resource')

# colnames(bcio) <- c('Economic living standards', 'Education', 'Governance', 'Health', 'Material living standards', 'Social relations', 'Subjective well-being')
colnames(bcio) <- c('Economic', 'Education', 'Governance', 'Health', 'Material', 'Social', 'Well-being')

GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(12)
lowGreens <- GreenLong[0:5]
show_col(lowGreens)

main_heatmap(bcio, colors=lowGreens)

bcio_matrix <- iheatmap(bcio,
  colors=lowGreens,
  col_title = "Outcome",
  row_title = "Intervention") %>% 
  add_col_labels() %>% 
  add_row_labels() %>% 
  add_col_barplot(y = colSums(bcio)/total) %>% 
  add_row_barplot(x = rowSums(bcio)/total) %>% 
  geom_text()
bcio_matrix

main_heatmap(bcio) %>% 
  add_col_labels() %>% 
  add_row_labels() %>% 
  add_col_summary() %>% 
  add_row_summary()

bcio_matrix %>% save_iheatmap("matrix1.html") # save interactive HTML
bcio_matrix %>% save_iheatmap("bitmap/matrix.png") # save static plot
