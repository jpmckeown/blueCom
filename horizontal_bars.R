library(tidyverse)
library(knitr)
library(readxl)
library(janitor)
library(RColorBrewer)
library(svglite)
library(ggpubr)
library(magick)
library(scales)
library(systemfonts)
library(grid)
library(ragg)
library(patchwork)

## Colour gradients #####

GreyLong <- colorRampPalette(brewer.pal(9, 'Greys'))(15)
lowGreys2 <- GreyLong[7:6]
show_col(lowGreys2)

GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(14)
midGreens4 <- GreenLong[7:4]
show_col(midGreens4)

OrangeLong <- colorRampPalette(brewer.pal(9, 'Oranges'))(15)
midOranges4 <- OrangeLong[8:5]
show_col(midOranges4)

PurpleLong <- colorRampPalette(brewer.pal(9, 'Purples'))(16)
midPurples4 <- PurpleLong[8:5]
show_col(midPurples4)

# Get data

original_xls <- "data/DATA EXTRACTION FINAL (16).xlsx"
de <- read_excel(original_xls, sheet = "Summary DE", .name_repair = "minimal")
Table_1_xls <- "data/Tables for report (23).xlsx"
t1 <- read_excel(Table_1_xls, sheet = "Table 1 final")

# Themes

barOnlyTheme <-  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.ticks.length = unit(0, "pt"),
  legend.position = "none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  plot.margin = unit(c(0,0,0,0), "mm")
)

    
## Study Design ##################

# table 1 lacks rows which messes up the results
# replicate, DE only if unique within author

allRows <- de %>% 
  select(`Author-date`, `Study design`)

names(allRows)[1] <- 'Author'
names(allRows)[2] <- 'Study_design'

# if NA in all 3 columns, remove row
authorBlanks <- allRows[rowSums(is.na(allRows)) != ncol(allRows), ]

authorMulti <- authorBlanks %>%
  fill(Author)

# uniqueness test using Author column
thisData <- unique(authorMulti) 

thisTable <- tabyl(thisData$Study_design)

names(thisTable)[1] <- 'Study_design'

col_order <- rev(c('Non-controlled', 'CI', 'BA', 'BACI'))

studyDesignTableT1 <- thisTable %>% 
  slice(match(col_order, Study_design))

# make df to plot from
thisTable <- studyDesignTableT1
Name <- thisTable$Study_design
Value <- thisTable$percent
Absolute <- thisTable$n
percentage <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percentage, '%)')
wideLab <- rev(c(TRUE, TRUE, FALSE, FALSE))
n <- sum(Absolute)

df <-  data.frame(Name, Absolute, Value, Str, wideLab)
df$Name <- factor(df$Name, levels = df$Name)
df
T1studyDesign_df <- df

# plot without whitespace

df <- T1studyDesign_df

thisPlot <- ggplot(data = df, aes(y = 1, x = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.1) +
  geom_text(aes(label = Name),
            position = position_stack(vjust = 0.5), vjust = -0.5,
            size = ifelse(df$wideLab, 6, 5),
            alpha = ifelse(df$Name == 'BACI', 0, 1)) +
  geom_text(aes(label = Str),
            position = position_stack(vjust = 0.5), vjust = 1.5,
            size = ifelse(df$wideLab, 5, 4),
            alpha = ifelse(df$Name == 'BACI', 0, 1)) +
  geom_text(aes(label = Name),
            position = position_stack(vjust = 0.5),
            alpha = ifelse(df$Name == 'BACI', 1, 0),
            size = 4.5, angle = 90, hjust = 1.2) +
  geom_text(aes(label = Str),
            position = position_stack(vjust = 0.5),
            alpha = ifelse(df$Name == 'BACI', 1, 0),
            size = 4, angle = 90, hjust = -0.1) +
  scale_x_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midOranges4) +
  barOnlyTheme

thisPlot
table1_studyDesignHorizontalPlot3 <- thisPlot

ggsave("png/studyDesign_horizontal_AGG_3.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3, height=0.7,
       scaling = 0.45)


## Type of data ############################

thisData <- de %>% 
  select(`Type of data`) %>% 
  drop_na()

thisTable <- tabyl(thisData$`Type of data`)
names(thisTable)[1] <- 'Type_of_data'

# thisTable <- thisTable[order(-thisTable$n),]
typeOfDataTable <- thisTable

# prep df for plot
thisTable <- typeOfDataTable
Name <- thisTable$Type_of_data
Value <- thisTable$percent
Absolute <- thisTable$n
percentage <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percentage, '%)')
n <- sum(thisTable$n)

# factor to keep order
df <-  data.frame(Name, Absolute, Value, Str) %>%
  mutate(Name = factor(Name, levels = Name)) 
TypeOfData_df <- df
TypeOfData

# plot
df <- TypeOfData_df

# increase font for wide segments
thisPlot <- ggplot(data = df, aes(y = 1, x = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.1) +
  geom_text(aes(label = Name),
            position = position_stack(vjust = 0.5), size = 6, vjust = -0.5,
            alpha = ifelse(df$Name == 'BACI', 0, 1)) +
  geom_text(aes(label = Str),
            position = position_stack(vjust = 0.5), size = 5, vjust = 1.5,
            alpha = ifelse(df$Name == 'BACI', 0, 1)) +
  scale_x_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = lowGreys2) +
  barOnlyTheme

thisPlot
typeOfDataHorizontalPlot3 <- thisPlot

ggsave("png/typeOfData_horizontal_AGG_3.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3, height=0.7,
       scaling = 0.45)


## Validity ###########################

thisData <- t1 %>% 
  select(`Internal validity rating`) %>% 
  drop_na()

thisTable <- tabyl(thisData$`Internal validity rating`)
names(thisTable)[1] <- 'Validity'

#thisTable <- thisTable[order(-thisTable$n),]
# validity is Ordinal
orderValidity <- c("High", "Moderate", "Low", "Unclear")
thisTable <- thisTable %>% 
  slice(match(orderValidity, Validity))

# formattable(thisTable, align='l')
validity_table1 <- thisTable

# make df to draw one stacked plot
# thisTable <- validity_table1
Name <- thisTable$Validity
Value <- thisTable$percent
Absolute <- thisTable$n
percentage <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percentage, '%)')
n <- sum(thisTable$n)

# factor to keep order
df <-  data.frame(Name, Absolute, Value, Str) %>%
  mutate(Name = factor(Name, levels = Name)) 
validity_plot_df <- df

# plot
df <- validity_plot_df

# vary font size by segment width
thisPlot <- ggplot(data = df, aes(y = 1, x = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.1) +
  geom_text(aes(label = Name),
            position = position_stack(vjust = 0.5), 
            size = ifelse(df$Name == 'Unclear', 5, 6), 
            vjust = -0.5) +
  geom_text(aes(label = Str),
            position = position_stack(vjust = 0.5), 
            size = ifelse(df$Name == 'Unclear', 4.5, 5), 
            vjust = 1.5) +
  scale_x_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midGreens4) +
  barOnlyTheme

thisPlot
validityHorizontalPlot3 <- thisPlot

ggsave("png/validity_horizontal_AGG_3.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3, height=0.7,
       scaling = 0.45)

#dim(png::readPNG('png/validity_horizontal_AGG.png'))


## Source of data  #################################

thisData <- de %>% 
  select(`Data source`) %>% 
  drop_na()

thisData[thisData == 'Conference abstract',] <- 'Conf. Abstract'
thisData[thisData == 'Peer-reviewed published literature',] <- 'Peer-reviewed'

thisTable <- tabyl(thisData$`Data source`)
names(thisTable)[1] <- 'Data_source'

# thisTable <- thisTable[order(-thisTable$n),]  # order by Frequency
# Awkward long label on narrow slice may need arrow to external label
#  so move it to end of bar to give option of placing label at side.

orderSource <- c('Peer-reviewed', 'Book', 'Thesis', 'Conf. Abstract')
thisTable <- thisTable %>% 
  slice(match(orderSource, Data_source))

# formattable(thisTable, align='l')
dataSourceTable <- thisTable  # preserve for inspection

# prep df for plotting
thisTable <- dataSourceTable
Name <- thisTable$Data_source
Value <- thisTable$percent
Absolute <- thisTable$n
# Str <- paste0(Name, ' (', Absolute, ')')
# original request was number only
percentage <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percentage, '%)')
# indicates which must be displayed vertically
vLab <- c(FALSE, TRUE, TRUE, TRUE)
n <- sum(thisTable$n)

# factor to keep order
df <-  data.frame(Name, Absolute, Value, Str) %>%
  mutate(Name = factor(Name, levels = Name)) 

df <- data.frame(Name, Absolute, Value, Str, vLab) %>%
  mutate(Name = factor(Name, levels = Name)) 
dataSource_plot_df <- df
df

# deep version not needing off-bar label
df <- dataSource_plot_df # in case rerun after other plot

thisPlot <- ggplot(data = df, aes(y = 1, x = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.1) +
  geom_text(aes(label = Name),
            position = position_stack(vjust = 0.5), size = 6, vjust = -0.5,
            alpha = ifelse(df$vLab, 0, 1)) +
  geom_text(aes(label = Str),
            position = position_stack(vjust = 0.5), size = 5, vjust = 1.5,
            alpha = ifelse(df$vLab, 0, 1)) +
  geom_text(aes(label = Name),
            position = position_stack(vjust = 0.5),
            alpha = ifelse(df$vLab, 1, 0),
            size = ifelse(df$Name=='Conf. Abstract', 4, 5),
            hjust = ifelse(df$Name=='Conf. Abstract', 0.8, 1.2),
            angle = 90) +
  geom_text(aes(label = Str),
            position = position_stack(vjust = 0.5),
            alpha = ifelse(df$vLab, 1, 0),
            hjust = ifelse(df$Name=='Conf. Abstract', -0.7, -0.3),
            size = ifelse(df$Name=='Conf. Abstract', 4, 4.5),
            angle = 90) +
  scale_x_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midPurples4) +
  barOnlyTheme

thisPlot
dataSourceHorizontal3 <- thisPlot

ggsave("png/dataSource_horizontal_AGG_3.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3, height=0.8,
       scaling = 0.45)
# 
# dim(png::readPNG('png/dataSource_horizontal_AGG.png'))


#### combine 4 bars ###############

# vars <- c('Type of data', 'Study design', 
#           'Internal validity rating', 'Source\n of data')
vars <- c('Type\nof data', 'Study\ndesign', 
          'Internal\nvalidity\nrating', 'Source\nof data')

label_plot <- function(label) {
  ggplot() + 
    geom_text(aes(x = 0, y = 0, label = label), 
              size=5, lineheight=1, hjust=0.5, fontface = 'plain') + 
    theme_void() + 
    theme(plot.margin = margin(0,0,0,0,'pt'))
}

figure_hbars <- label_plot(vars[1]) + typeOfDataHorizontalPlot3 +
  plot_spacer() + plot_spacer() +
  label_plot(vars[2]) + table1_studyDesignHorizontalPlot3 + 
  plot_spacer() + plot_spacer() +
  label_plot(vars[3]) + validityHorizontalPlot3 +
  plot_spacer() + plot_spacer() +
  label_plot(vars[4]) + dataSourceHorizontal3 +
  plot_layout(nrow = 7, widths = c(323, 3220), 
              heights = c(700, 30, 700, 30, 700, 30, 840))
figure_hbars

ggsave("png/horizontal_bars_AGG_3.png", plot=figure_hbars,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3.453, height=3.03,
       scaling = 0.45)

dim(png::readPNG('png/horizontal_bars_AGG_3.png'))
