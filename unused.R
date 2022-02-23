# unused

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

# crop
library(magick)
tmp1 <- image_read('bitmap/typeOfData_horizontal_raw.png')
tmp2 <- image_trim(tmp1)
image_write(tmp2, path='bitmap/typeOfData_horizontal.png', format='png')
typeOfData_img <- tmp2
image_info(tmp1)
image_info(tmp2) 

# compare bar sizes
type_mgk <- image_read('png/typeOfData_horizontal.png')
design_mgk <- image_read('png/table1_studyDesign_horizontal.png')
source_mgk <- image_read('png/dataSource_horizontal_deep.png')
image_info(type_mgk)
image_info(design_mgk)
image_info(source_mgk)


#tmp1 <- image_read('png/studyDesign_horizontal_raw.png')
tmp1 <- image_read('png/table1_studyDesign_horizontal_raw.png')
tmp2 <- image_trim(tmp1)
#image_write(tmp2, path='png/studyDesign_horizontal.png', format='png')
image_write(tmp2, path='png/table1_studyDesign_horizontal.png', format='png')
studyDesign_img <- tmp2
image_info(tmp1)
image_info(tmp2)

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
  axis.title.y = element_text(size=12)
)

# rownames(thisTable) <- c()  
# another table display package was showing row numbers?

# Study Design from DE
thisData <- de %>% 
  select(`Study design`) %>% 
  drop_na()
thisTable <- tabyl(thisData$`Study design`)
names(thisTable)[1] <- 'Study_design'
thisTable <- thisTable[order(-thisTable$n),]
rownames(thisTable) <- c()
formattable(thisTable, align='l')
studyDesignTableDE <- thisTable

# data from Table 1 to see difference
thisData <- t1 %>% 
  select(`Study design`) %>% 
  drop_na()
thisTable <- tabyl(thisData$`Study design`)
names(thisTable)[1] <- 'Study_design'
thisTable <- thisTable[order(-thisTable$n),]
rownames(thisTable) <- c()
studyDesignTableT1 <- thisTable
formattable(studyDesignTableT1, align='l')

# want Non-comparative first so don't order by n
#thisTable <- thisTable[order(-thisTable$n),]

# percent <- format(round(Value * 100, 0), nsmall = 0)

# ggplot does this automatically?
# calculate interval position
df <-  data.frame(Name, Absolute, Value, Str) %>%
  mutate(Name = factor(Name, levels = Name),
         Pos = cumsum(lag(Value, default = 0)) + Value/2) 

# earlier plot using Pos, needs magick trim
thisPlot <- ggplot(data = studyDesign_df, aes(x = Pos, y = 0.2, 
                                              width = Value, fill = Name)) +
  geom_col(position = "identity", show.legend = FALSE) +
  geom_text(aes(label = Name),
            position = position_fill(vjust = 0.13), size = 5) +
  geom_text(aes(label = Str),
            position = position_fill(vjust = 0.08), size = 5) +
  labelonly +
  scale_fill_manual(values = lowReds4)

geom_text(aes(label = paste(Name, Str)),
          position = position_stack(vjust = 0.5),
          alpha = ifelse(df$Name == 'BACI', 1, 0),
          size = 4.5, angle = 90, hjust = 0.55) +
  
  # labels vertical & smaller, still using Pos
  thisPlot <- ggplot(data = studyDesign_df, aes(x = Pos, y = 0.2, 
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

# manual calculation of label Pos, now done by position_stack
mutate(Name = factor(Name, levels = Name),
       Pos = cumsum(lag(Value, default = 0)) + Value/2) 

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

thisPlot <- ggplot(data = df, 
                   aes(y = Stack, x = Absolute, fill = Name)) +
  geom_col(show.legend = FALSE, color = 'black') +
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