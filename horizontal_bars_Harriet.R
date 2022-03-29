## Figure of 4 horizontal bars for 
## Type of data, Source, Study design, Internal validity.

# expects subfolders /data/ for downloaded spreadsheets,
#  and /png/ for plot image files.

library(tidyverse)
library(readxl)
library(janitor)
library(RColorBrewer)
# library(ggpubr)
# library(magick)
library(scales)
library(systemfonts)
library(grid)
library(ragg)
library(patchwork)


## Colour gradients #####

GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(14)
midGreens4 <- GreenLong[7:4]
show_col(midGreens4)

OrangeLong <- colorRampPalette(brewer.pal(9, 'Oranges'))(15)
midOranges4 <- OrangeLong[8:5]
show_col(midOranges4)

PurpleLong <- colorRampPalette(brewer.pal(9, 'Purples'))(16)
midPurples4 <- PurpleLong[8:5]
midPurples2 <- c("#B6B6D8", "#D6D6E9")
show_col(midPurples2)


## Themes ##
# removes axis, labels, legend, and whitespace margins

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


## Get data ########

original_xls <- "data/Script-friendly P1_SLR data extraction sheet 180222_Combined.xlsx"

column_names <- read_excel(original_xls, 
                           sheet = "Data gathering Syikin", 
                           .name_repair = "minimal") %>% names()

mde <- read_excel(original_xls, sheet = "Data gathering Syikin", 
                  skip = 2, col_names = column_names) 

# mde2 <- read_excel(original_xls, sheet = "Syikin new search includes", .name_repair = "minimal")
# 
# check_colnames_match <- function(x,y) {
#   for (i in names(x)) {
#     if (!(i %in% names(y))) {
#       print('Warning: Names are not the same')
#       break
#     }  
#     else if(i==tail(names(y),n=1)) {
#       print('Names are identical')
#     }
#   }
# }
# check_colnames_match(mde, mde2)


## Validity ###########################

thisData <- mde %>% 
  select(`Internal validity rating`) %>% 
  drop_na()

thisTable <- tabyl(thisData$`Internal validity rating`)
names(thisTable)[1] <- 'Validity'

# If ever wanting to reverse sequence 
# thisTable <- thisTable[order(-thisTable$n),]

# validity is Ordinal so specify order
orderValidity <- c("High", "Moderate", "Low", "Unclear")
thisTable <- thisTable %>% 
  slice(match(orderValidity, Validity))

validity_table1 <- thisTable

# prep df to make one stacked bar plot
Name <- thisTable$Validity
Value <- thisTable$percent
Absolute <- thisTable$n
percentage <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percentage, '%)')
n <- sum(thisTable$n)

# factor to keep order
df <-  data.frame(Name, Absolute, Value, Str) %>%
  mutate(Name = factor(Name, levels = Name)) 

# temporary archive, if adjusting plot after work on another bar
validity_Malaysia_df <- df
df <- validity_Malaysia_df

# Plot: reduce font size for narrow segment 'Unclear'

thisPlot <- ggplot(data = df, aes(y = 1, x = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.1) +
  geom_text(aes(label = Name),
            alpha = ifelse(df$Name == 'Low', 0, 1),
            position = position_stack(vjust = 0.5), 
            size = ifelse(df$Name == 'Unclear', 5, 6), 
            vjust = -0.5) +
  geom_text(aes(label = Str),
            alpha = ifelse(df$Name == 'Low', 0, 1),
            position = position_stack(vjust = 0.5), 
            size = ifelse(df$Name == 'Unclear', 4.5, 5), 
            vjust = 1.5) +
  geom_text(aes(label = paste(Name)),
            alpha = ifelse(df$Name == 'Low', 1, 0),
            angle = 90,
            position = position_stack(vjust = 0.5), 
            size = 5, 
            hjust = 1.5) +
  geom_text(aes(label = paste(Str)),
            alpha = ifelse(df$Name == 'Low', 1, 0),
            angle = 90,
            position = position_stack(vjust = 0.5), 
            size = 4.5, 
            hjust = -0.2) +
  scale_x_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midGreens4) +
  barOnlyTheme

thisPlot
validityHorizontalPlot_M <- thisPlot  # ready for stitching

ggsave("png/validity_Malaysia.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3, height=0.7,
       scaling = 0.45)

# to check dimensions of output
# dim(png::readPNG('png/validity_horizontal.png'))


## Source of data  #################################

thisData <- mde %>% 
  select(`Publication type`) %>% 
  drop_na()

thisData[thisData == 'Grey literature organisational report',] <- 'Grey literature\norganisational report'
thisData[thisData == 'Peer-reviewed published literature',] <- 'Peer-reviewed'

thisTable <- tabyl(thisData$`Publication type`)
names(thisTable)[1] <- 'Data_source'

# If order by Frequency
# thisTable <- thisTable[order(-thisTable$n),]  

# custom specified order
# seems to make no difference, Peer-reviewed goes left
orderSource <- c('Peer-reviewed', 'Grey literature\norganisational report')
# orderSource <- c('Grey literature\norganisational report', 'Peer-reviewed')
thisTable <- thisTable %>%
 slice(match(orderSource, Data_source))

# formattable(thisTable, align='l')
dataSourceTable_Malaysia <- thisTable  # preserve for inspection

# prep df for plotting
# thisTable <- dataSourceTable_Malaysia
Name <- thisTable$Data_source
Value <- thisTable$percent
Absolute <- thisTable$n
percentage <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percentage, '%)')
n <- sum(thisTable$n)

# factor to keep order
df <-  data.frame(Name, Absolute, Value, Str) %>%
  mutate(Name = factor(Name, levels = Name)) 

# df <- data.frame(Name, Absolute, Value, Str, vLab) %>%
#   mutate(Name = factor(Name, levels = Name)) 

dataSource_Malaysia_df <- df
df <- dataSource_Malaysia_df # in case rerun after other plot

# df <- df %>% 
#   mutate(Name = ifelse(Name=='Grey literature organisational report', 
#                        'Grey literature\norganisational report', 'Peer-reviewed'))  


# Plot: alpha hides or shows labels vertical or horizontal

thisPlot <- ggplot(data = df, aes(y = 1, x = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.1) +
  geom_text(aes(label = Name),
            position = position_stack(vjust = 0.5), 
            size = 6, vjust = -0.5) +
  geom_text(aes(label = Str),
            position = position_stack(vjust = 0.5), 
            size = 5, vjust = 1.5) +
  # geom_text(aes(label = Name),
  #           position = position_stack(vjust = 0.5),
  #           alpha = ifelse(df$vLab, 1, 0),
  #           size = ifelse(df$Name=='Conf. Abstract', 4, 5),
  #           hjust = ifelse(df$Name=='Conf. Abstract', 0.8, 1.2),
  #           angle = 90) +
  # geom_text(aes(label = Str),
  #           position = position_stack(vjust = 0.5),
  #           alpha = ifelse(df$vLab, 1, 0),
  #           hjust = ifelse(df$Name=='Conf. Abstract', -0.7, -0.3),
  #           size = ifelse(df$Name=='Conf. Abstract', 4, 4.5),
  #           angle = 90) +
  scale_x_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = midPurples2) +
  barOnlyTheme

thisPlot
dataSourceHorizontalPlot_Malaysia <- thisPlot  # ready for stitching

ggsave("png/dataSource_Malaysia.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3, height=0.8,
       scaling = 0.45)
 
# dim(png::readPNG('png/dataSource_horizontal.png'))


## Study Design ##################

# table 1 lacks spreadsheet rows which messes up results
# replicate from DE by selecting unique within author

allRows <- mde %>% 
  select(`Study ID`, `Study design`)

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

# custom order of segments
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

T1studyDesign_df <- df  # temporary archive
df <- T1studyDesign_df

# Plot: alpha 0 hides horizontal labels for BACI segment,
#  and alpha 1 shows vertical label only for BACI.

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
table1_studyDesignHorizontalPlot <- thisPlot

ggsave("png/studyDesign_horizontal.png", plot=thisPlot,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3, height=0.7,
       scaling = 0.45)


#### combine 4 bars ###############

vars <- c('Type\nof data', 'Study\ndesign', 
          'Internal\nvalidity\nrating', 'Source\nof data')

label_plot <- function(label) {
  ggplot() + 
    geom_text(aes(x = 0, y = 0, label = label), 
              size=5, lineheight=1, hjust=0.5, fontface = 'plain') + 
    theme_void() + 
    theme(plot.margin = margin(0,0,0,0,'pt'))
}

figure_hbars <- label_plot(vars[1]) + typeOfDataHorizontalPlot +
  plot_spacer() + plot_spacer() +
  label_plot(vars[2]) + table1_studyDesignHorizontalPlot + 
  plot_spacer() + plot_spacer() +
  label_plot(vars[3]) + validityHorizontalPlot +
  plot_spacer() + plot_spacer() +
  label_plot(vars[4]) + dataSourceHorizontalPlot +
  plot_layout(nrow = 7, widths = c(323, 3220), 
              heights = c(700, 30, 700, 30, 700, 30, 840))
figure_hbars

ggsave("png/horizontal_bars.png", plot=figure_hbars,
       device = ragg::agg_png, dpi = 1000,
       units="in", width=3.453, height=3.03,
       scaling = 0.45)

dim(png::readPNG('png/horizontal_bars.png'))
