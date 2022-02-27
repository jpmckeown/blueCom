# heatmap with marginal histograms

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(scales)
library(janitor) 

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

as.data.frame(Freqs) 

library(ztable)
options(ztable.type="html")
z=ztable(Freqs) 
print(z,caption="Intervention and Outcome matrix")

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
rownames(bcio) <- c('CBNRM', 'CBNRM+Health', 'Habitat', 'Health', 'Livelihood', 'Resource')

# colnames(bcio) <- c('Economic living standards', 'Education', 'Governance', 'Health', 'Material living standards', 'Social relations', 'Subjective well-being')
colnames(bcio) <- c('Economic', 'Education', 'Governance', 'Health', 'Material', 'Social', 'Well-being')

GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(12)
lowGreens <- GreenLong[0:5]
show_col(lowGreens)
