# devtools::install_github("ropensci/iheatmapr")
# pkgs <- c('magrittr', 'glue', 'rlang', 'fansi', 'yaml', 'jsonlite')
# detach("package:glue", unload=TRUE)
# remove.packages(pkgs)
# install.packages(pkgs)

library(iheatmapr)
library(datasets)
library(reshape2)

Indometh_matrix <- acast(Indometh, Subject ~ time, value.var = "conc")
Indometh_matrix <- Indometh_matrix[as.character(1:6),]
rownames(Indometh_matrix) <- paste("Patient",rownames(Indometh_matrix))
Indometh_patient_cor <- cor(t(Indometh_matrix))

patient_max_conc <- apply(Indometh_matrix,1,max)
patient_min_conc <- apply(Indometh_matrix,1,min)
patient_groups <- c("A","A","B","A","B","A") # Arbitrary groups

example_heatmap <- main_heatmap(Indometh_patient_cor, name = "Correlation")
example_heatmap


data(measles, package = "iheatmapr")
main_heatmap(measles, name = "Measles<br>Cases", x_categorical = FALSE,
             layout = list(font = list(size = 8))) %>%
  add_col_groups(ifelse(1930:2001 < 1961,"No","Yes"),
                 side = "bottom", name = "Vaccine<br>Introduced?",
                 title = "Vaccine?",
                 colors = c("lightgray","blue")) %>%
  add_col_labels(ticktext = seq(1930,2000,10),font = list(size = 8)) %>%
  add_row_labels(size = 0.3,font = list(size = 6)) %>% 
  add_col_summary(layout = list(title = "Average<br>across<br>states"),
                  yname = "summary")  %>%                 
  add_col_title("Measles Cases from 1930 to 2001", side= "top") %>%
  add_row_summary(groups = TRUE, 
                  type = "bar",
                  layout = list(title = "Average<br>per<br>year",
                                font = list(size = 8)))
