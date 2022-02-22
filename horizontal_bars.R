library(tidyverse)
library(knitr)
library(readxl)
library(janitor)
library(RColorBrewer)
library(svglite)
library(ggpubr)
library(magick)

### Get data

original_xls <- "data/DATA EXTRACTION FINAL (15).xlsx"
de <- read_excel(original_xls, sheet = "Summary DE", .name_repair = "minimal")
Table_1_xls <- "data/Tables for report (22).xlsx"
t1 <- read_excel(Table_1_xls, sheet = "Table 1 final")




library(scales)
GreyLong <- colorRampPalette(brewer.pal(9, 'Greys'))(12)
lowGreys2 <- GreyLong[4:5]
show_col(lowGreys2)
GreenLong <- colorRampPalette(brewer.pal(9, 'Greens'))(12)
lowGreens2 <- GreenLong[5:6]
#show_col(lowGreens2)
OrangeLong <- colorRampPalette(brewer.pal(9, 'Oranges'))(14)
midOranges2 <- OrangeLong[5:6]
#show_col(midOranges2)
RedLong <- colorRampPalette(brewer.pal(9, 'Reds'))(15)
# midReds4 <- RedLong[4:8]
midReds4 <- c("#FCC5AF", "#FCAF93", "#FC9168", "#FB6A4A")
show_col(midReds4)
PurpleLong <- colorRampPalette(brewer.pal(9, 'Purples'))(16)
midPurples4 <- PurpleLong[5:8]
#show_col(midPurples4)

## Validity

thisData <- t1 %>% 
  select(`Internal validity rating`) %>% 
  drop_na()

thisTable <- tabyl(thisData$`Internal validity rating`)
names(thisTable)[1] <- 'Validity'

thisTable <- thisTable[order(-thisTable$n),]

formattable(thisTable, align='l')
validity_table1 <- thisTable

# make df to draw one stacked plot
thisTable <- validity_table1
Name <- thisTable$Validity
Value <- thisTable$percent
Absolute <- thisTable$n
percent <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percent, '%)')
Stack <- 'validity'

# 
df <-  data.frame(Name, Absolute, Value, Str, Stack)
validity_plotinfo <- df

# plot

df <- validity_plotinfo
thisPlot <- ggplot(data = df, aes(x = Stack, y = Absolute, fill = Name)) +
  geom_col() +
  geom_text(aes(label = Name),
            position = position_stack(vjust = 0.5), 
            size = 5, hjust = 1) +
  geom_text(aes(label = Str),
            position = position_stack(vjust = 0.5), size = 5) +
  labelonly +
  scale_fill_manual(values = midPurples4)+
  coord_flip() +
  theme(plot.margin=unit(c(0,0,0,0),"mm"))

labelonly <-  theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  legend.position="none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank()
)

yTitleOnlyTheme <-  theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  legend.position = "none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  plot.margin = unit(c(0,0,0,0),"mm"),
  axis.title.y = element_text(size=12)
)

# without flip ?
thisPlot <- ggplot(data = df, aes(y = Stack, x = Absolute, fill = Name)) +
  geom_col(show.legend = FALSE) +
  theme(plot.margin = unit(c(0,0,0,0),"mm")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  scale_x_continuous(limits=c(0, 16), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(axis.title.y = element_text(size=12)) +
  ylab('Internal Validity')

thisPlot <- ggplot(data = df, aes(y = Stack, x = Absolute, fill = Name)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(limits=c(0, 16), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab('Internal Validity') +
  yTitleOnlyTheme

thisPlot <- ggplot(data = df, aes(y = Stack, x = Absolute, fill = Name)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(limits=c(0, 16), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab('Internal Validity') +
  yTitleOnlyTheme

thisPlot

ggsave(file = "png/validity_horizontal_blank.png",
       units = 'mm', width = 90, height = 20, dpi = 1000,
       plot = thisPlot)

ggsave(file = "eps/validity_horizontal_blank.pdf",
       units = 'mm', width = 90, height = 20,
       plot = thisPlot)
ggsave(file = "eps/validity_horizontal_blank.eps",
       units = 'mm', width = 90, height = 20,
       plot = thisPlot)

dim(png::readPNG('png/validity_horizontal_blank.png'))

validityHorizontalPlot <- thisPlot  
validityHorizontalPlot

library(ragg)
agg_png(file = "png/validity_horizontal_blank_RAGG.png",
        units='mm', width=90, height=24, res=1000)

ggplot(data = df, aes(y = Stack, x = Absolute, fill = Name)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(limits=c(0, 16), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ylab('Internal Validity') +
  yTitleOnlyTheme

invisible(dev.off())

## Study Design

thisData <- de %>% 
  select(`Study design`) %>% 
  drop_na()

thisTable <- tabyl(thisData$`Study design`)
names(thisTable)[1] <- 'Study_design'

thisTable <- thisTable[order(-thisTable$n),]
rownames(thisTable) <- c()
formattable(thisTable, align='l')
studyDesignTableDE <- thisTable


# get from Table 1 to see difference
thisData <- t1 %>% 
  select(`Study design`) %>% 
  drop_na()

thisTable <- tabyl(thisData$`Study design`)
names(thisTable)[1] <- 'Study_design'

thisTable <- thisTable[order(-thisTable$n),]
rownames(thisTable) <- c()
studyDesignTableT1 <- thisTable

formattable(studyDesignTableT1, align='l')


# OK table 1 lacks rows which messes up the results.
# what's needed is DE unique by author

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

# want Non-comparative first so don't order by n
#thisTable <- thisTable[order(-thisTable$n),]

col_order <- c('Non-controlled', 'CI', 'BA', 'BACI')

orderedTable <- thisTable %>% 
  slice(match(col_order, Study_design))

formattable(orderedTable, align='l')
studyDesignTableT1 <- orderedTable


thisTable <- studyDesignTableT1
Name <- thisTable$Study_design
Value <- thisTable$percent
Absolute <- thisTable$n
# percent <- format(round(Value * 100, 0), nsmall = 0)
percent <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percent, '%)')

# calculate interval position
df <-  data.frame(Name, Absolute, Value, Str) %>%
  mutate(Name = factor(Name, levels = Name),
         Pos = cumsum(lag(Value, default = 0)) + Value/2) 
studyDesign_df <- df
studyDesign_df


# 
df <- studyDesign_df
thisPlot <- ggplot(data = df, aes(x = Pos, y = 0.2, 
                        width = Value, fill = Name)) +
  geom_col(position = "identity", show.legend = FALSE) +
  geom_text(aes(label = Name),
            position = position_fill(vjust = 0.13), size = 5) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.08), size = 5) +
  labelonly +
  scale_fill_manual(values = lowReds4)

#replot with smaller vertical labels
thisPlot <- ggplot(data = df, aes(x = Pos, y = 0.2, 
                        width = Value, fill = Name)) +
  geom_col(position = "identity", show.legend = FALSE) +
  geom_text(aes(label = Name),
            position = position_fill(vjust = 0.13), size = 5,
            alpha = ifelse(df$Name == 'BACI', 0, 1)) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.08), size = 4.5,
            alpha = ifelse(df$Name == 'BACI', 0, 1)) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.08), size = 4.5,
            alpha = ifelse(df$Name == 'BACI', 0, 1)) +
  labelonly +
  scale_fill_manual(values = lowReds4)
  
thisPlot

# ggsave(file="svg/studyDesign_horizontal_raw.svg", plot=thisPlot)
ggsave(file="png/table1_studyDesign_horizontal_raw.png", plot=thisPlot)
table1_studyDesignHorizontalPlot <- thisPlot  


#tmp1 <- image_read('png/studyDesign_horizontal_raw.png')
tmp1 <- image_read('png/table1_studyDesign_horizontal_raw.png')
tmp2 <- image_trim(tmp1)
#image_write(tmp2, path='png/studyDesign_horizontal.png', format='png')
image_write(tmp2, path='png/table1_studyDesign_horizontal.png', format='png')
studyDesign_img <- tmp2
image_info(tmp1)
image_info(tmp2)


## Type of data ############################

thisData <- de %>% 
  select(`Type of data`) %>% 
  drop_na()

thisTable <- tabyl(thisData$`Type of data`)
names(thisTable)[1] <- 'Type_of_data'

thisTable <- thisTable[order(-thisTable$n),]
rownames(thisTable) <- c()
formattable(thisTable, align='l')
typeOfDataTable <- thisTable

# prep df for plot
thisTable <- typeOfDataTable
Name <- thisTable$Type_of_data
Value <- thisTable$percent
Absolute <- thisTable$n
percent <- (round(Value * 100, digits=0))
Str <- paste0(Absolute, ' (', percent, '%)')

# calculate interval position
df <-  data.frame(Name, Absolute, Value, Str) %>%
  mutate(Name = factor(Name, levels = Name),
         Pos = cumsum(lag(Value, default = 0)) + Value/2) 
TypeOfData <- df
TypeOfData

# plot
df <- TypeOfData
thisPlot <- ggplot(data = df, aes(x = Pos, y = 0.2, 
                        width = Value, fill = Name)) +
  geom_col(position = "identity", show.legend = FALSE) +
  geom_text(aes(label = Name),
            position = position_fill(vjust = 0.13), size = 5) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.08), size = 5) +
  labelonly +
  scale_fill_manual(values = lowGreys2)

# ggsave(file="svg/typeOfData_horizontal_raw.svg", plot=thisPlot, width=10, height=8)
ggsave(file="png/typeOfData_horizontal_grey_raw.png", plot=thisPlot)

typeHorizontalPlot <- thisPlot  
typeHorizontalPlot

# crop
library(magick)
tmp1 <- image_read('bitmap/typeOfData_horizontal_raw.png')
tmp2 <- image_trim(tmp1)
image_write(tmp2, path='bitmap/typeOfData_horizontal.png', format='png')
typeOfData_img <- tmp2
image_info(tmp1)
image_info(tmp2)


## Data source #################################

sourceData <- de %>% 
  select(`Data source`) %>% 
  drop_na()

thisData <- sourceData  # to save changing common code

thisData[thisData == 'Conference abstract',] <- 'Conf. Abstract'
thisData[thisData == 'Peer-reviewed published literature',] <- 'Peer-reviewed'

thisTable <- tabyl(thisData$`Data source`)
names(thisTable)[1] <- 'Data_source'

# thisTable <- thisTable[order(-thisTable$n),]  # order by Frequency

# Awkward long label on narrow slice will need arrow to external label
#  so move it to end of bar to give option of placing label at side.

orderSource <- c('Peer-reviewed', 'Book', 'Thesis', 'Conf. Abstract')
thisTable <- thisTable %>% 
  slice(match(orderSource, Data_source))

# rownames(thisTable) <- c()  
# another table display package was showing row numbers?
formattable(thisTable, align='l')

dataSourceTable <- thisTable  # preserve for inspection



thisTable <- dataSourceTable
Name <- thisTable$Data_source
Value <- thisTable$percent
Absolute <- thisTable$n
Str <- paste0(Name, ' (', Absolute, ')')
vLab <- c(0, 1, 1, 0)
percent <- format(round(Value * 100, 0), nsmall = 0)

# calculate interval position
df <-  data.frame(Name, Absolute, Value, Str, vLab) %>%
  mutate(Name = factor(Name, levels = Name),
         Pos = cumsum(lag(Value, default = 0)) + Value/2) 
dataSource <- df
dataSource


# deep sideways version
df <- dataSource
thisPlot <- ggplot(data = df, aes(x = Pos, y = 0.22, 
                        width = Value, fill = Name)) +
  geom_col(position = "identity", show.legend = FALSE) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.08),
            alpha = ifelse(df$Name == 'Peer-reviewed', 0, 1),
            size = 4, angle = 90, hjust = 0.35) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.11),
            alpha = ifelse(df$Name == 'Peer-reviewed', 1, 0),
            size = 5, hjust = 0.35) +
  labelonly +
  scale_fill_manual(values = midPurples4)
thisPlot
dataSourceHorizontalDeep <- thisPlot
ggsave(file="png/dataSource_horizontal_deep_raw.png", plot=thisPlot)


df <- dataSource
thisPlot <- ggplot(data = df, aes(x = Pos, y = 0.2, 
                        width = Value, fill = Name)) +
  geom_col(position = "identity", show.legend = FALSE) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.08),
            alpha = ifelse(df$Name == 'Peer-reviewed', 0, 1),
            size = 4, angle = 90, hjust = 0.35) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.11),
            alpha = ifelse(df$Name == 'Peer-reviewed', 1, 0),
            size = 5, hjust = 0.35) +
  labelonly +
  scale_fill_manual(values = midPurples4)
thisPlot




df <- dataSource
# alpha = ifelse(df$Name == 'Peer-reviewed' | df$Name == 'Conf. Abstract', 0, 1),

thisPlot <- ggplot(data = df, aes(x = Pos, y = 0.2, 
                        width = Value, fill = Name)) +
  geom_col(position = "identity", show.legend = FALSE) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.08),
            alpha = ifelse(vLab == 1, 1, 0),
            size = 4, angle = 90, hjust = 0.35) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.07),
            alpha = ifelse(df$Name == 'Peer-reviewed', 1, 0),
            size = 5, hjust = 0.35) +
  geom_text(data = subset(df, Name == 'Conf. Abstract'), 
            label = 'Conf. Abstract (1)',
            size = 4, hjust = 0.2, vjust = -0.2) +
  labelonly +
  scale_fill_manual(values = midPurples4)

# ggsave(file="eps/dataSource_horizontal_raw.eps", plot=thisPlot)
ggsave(file="png/dataSource_horizontal_overflow_raw.png", plot=thisPlot)

dataSourceHorizontalPlot <- thisPlot  
dataSourceHorizontalPlot


thisPlot <- ggplot(data = df, aes(x = Pos, y = 0.15, 
                width = Value, fill = Name)) +
  geom_col(position = "identity", show.legend = FALSE) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.06),
            alpha = ifelse(vLab == 1, 1, 0),
            size = 4, angle = 90, hjust = 0.35) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.08),
            alpha = ifelse(df$Name == 'Peer-reviewed', 1, 0),
            size = 5, hjust = 0.35) +
  geom_text(data = subset(df, Name == 'Conf. Abstract'), 
            label = 'Conf. Abstract (1)',
            size = 4, hjust = 0.9, vjust = 9.5) +
  geom_segment(x = 0.96, y = -0.01, 
            xend = 0.96, yend = 0.05, 
            arrow = arrow(length = unit(0.3, 'cm'))) +
  labelonly +
  scale_fill_manual(values = midPurples4)
thisPlot
ggsave(file="png/dataSource_horizontal_arrow_raw.png", plot=thisPlot)


library(magick)
#tmp1 <- image_read('bitmap/dataSource_horizontal_raw.png')
tmp1 <- image_read('png/dataSource_horizontal_arrow_raw.png')
#tmp1 <- image_read('png/dataSource_horizontal_deep_raw.png')
tmp2 <- image_trim(tmp1)
#image_write(tmp2, path='png/dataSource_horizontal_deep.png', format='png')
image_write(tmp2, path='png/dataSource_horizontal_arrow.png', format='png')
dataSource_img <- tmp2
image_info(tmp1)
image_info(tmp2)

# compare bar sizes
type_mgk <- image_read('png/typeOfData_horizontal.png')
design_mgk <- image_read('png/table1_studyDesign_horizontal.png')
source_mgk <- image_read('png/dataSource_horizontal_deep.png')
image_info(type_mgk)
image_info(design_mgk)
image_info(source_mgk)

# combine three bars
# theme_set(theme_pubr())
figure <- ggarrange(typeHorizontalPlot, 
                    dataSourceHorizontalDeep, 
                    designHorizontalPlot,
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)
figure
