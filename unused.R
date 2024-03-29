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


thisPlot <- ggplot(data = df, aes(y = 1, x = Absolute, fill = Name)) +
  geom_col(color = 'black', size = 0.1) +
  geom_text(aes(label = Name),
            position = position_stack(vjust = 0.5), size = 5, vjust = -0.5,
            alpha = ifelse(df$Name == 'BACI', 0, 1)) +
  geom_text(aes(label = Str),
            position = position_stack(vjust = 0.5), size = 4.5, vjust = 1.5,
            alpha = ifelse(df$Name == 'BACI', 0, 1)) +
  scale_x_continuous(limits=c(0, n), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = lowGreys2) +
  barOnlyTheme

RedLong <- colorRampPalette(brewer.pal(9, 'Reds'))(15)
midReds4 <- c("#FCC5AF", "#FCAF93", "#FC9168", "#FB7552")
midReds4 <- rev(midReds4)
show_col(midReds4)
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

## from horizontal_bars.R ####
# theme_set(theme_pubr())
figure <- ggarrange(typeHorizontalPlot, 
                    dataSourceHorizontalDeep, 
                    designHorizontalPlot,
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)
figure


# combining 4 horizontal bars
# left-side titles

values <- 1
titlesTest_df <- data.frame(var_titles, values)
titles_only <- ggplot(data = titlesTest_df, 
                      mapping = aes(x=values, y=var_titles)) +
  geom_col() +
  geom_text(var_titles)

titles_only
ggsave("combined_hbars_ggsave.png", titles_only, 
       device = ragg::agg_png, dpi=1000, scaling=2,
       units="in", width=3.543, height=4)

# theme_set(theme_pubr())
figure <- ggarrange(typeHorizontalPlot, 
                    dataSourceHorizontalDeep, 
                    designHorizontalPlot,
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)
figure


## Cowplot ###
var_titles <- c('Study Design', 
                'Type of data', 
                'Source of data', 
                'Internal validity rating')

row_1 <- plot_grid(NULL, typeOfDataHorizontalPlot2,
                   nrow=1, ncol=2,
                   rel_widths = c(543/3543, 3000/3543),
                   labels = var_titles[1],
                   label_x = c(-0.3),
                   label_y = c(0.6))
row_2 <- plot_grid(NULL, table1_studyDesignHorizontalPlot2,
                   nrow=1, ncol=2,
                   rel_widths = c(543/3543, 3000/3543),
                   labels = var_titles[2],
                   label_x = c(-0.3),
                   label_y = c(0.6))
row_3 <- plot_grid(NULL, validityHorizontalPlot2,
                   nrow=1, ncol=2,
                   rel_widths = c(543/3543, 3000/3543),
                   labels = var_titles[3],
                   label_x = c(-0.4),
                   label_y = c(0.6))
row_4 <- plot_grid(NULL, dataSourceHorizontalPurple,
                   nrow=1, ncol=2,
                   rel_widths = c(543/3543, 3000/3543),
                   labels = var_titles[4],
                   label_x = c(0),
                   label_y = c(0.5))

thisPlot <- plot_grid(row_1, row_2, row_3, row_4,
                      nrow = 4,
                      rel_heights = c(7/29, 7/29, 7/29, 8/29))

thisPlot <- ggdraw() +
  draw_plot_label('draw_plot_label', x=0.1, y=0.8)

thisPlot # Cowplot version


# , width = 0.4
# color = ifelse(df$Name == '', 'white', 'black')) +
# position = position_fill(), 
# nudge_x = -3,
# weird reverse postioning & relatiev to plot area
# thisPlot <- ggplot(data = df, aes(x = 1, y = Value, fill = Name)) +
#   geom_col(color = 'black', size = 0.2) +
#   geom_text(aes(y = yPos, label = paste(Name, Str)),
#             vjust = 2,
#             size = 5) +
#   scale_fill_manual(values = midBlues5) +
#   barOnlyTheme

# thisPlot <- ggplot(data = df, aes(x = 1, y = Absolute, fill = Name)) +
#   geom_col(color = 'black', size = 0.2) +
#   geom_text(aes(label = Name),
#             position = position_stack(vjust = 0.6), size = 6, 
#             hjust = 0,
#             alpha = ifelse(Name == 'Thailand', 0, 1) ) +
#   geom_text(aes(label = Str),
#             position = position_stack(vjust = 0.4), size = 5, 
#             hjust = 0,
#             alpha = ifelse(Name == 'Thailand', 0, 1) ) +
#   geom_text(aes(label = Name),
#             position = position_stack(vjust = 0.5), size = 5.5, 
#             hjust = 3,
#             alpha = ifelse(Name == 'Thailand', 1, 0) ) +
#   geom_text(aes(label = Str),
#             position = position_stack(vjust = 0.5), size = 4.5, 
#             hjust = 0,
#             alpha = ifelse(Name == 'Thailand', 1, 0) ) +
#   scale_y_continuous(limits=c(0, n), expand = c(0, 0)) +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_fill_manual(values = midBlues5) +
#   barOnlyTheme

# thisPlot <- ggplot(data = df, aes(x = 1, y = Absolute, fill = Name)) +
#   geom_col(color = 'black', size = 0.2) +
#   geom_text(aes(label = Name),
#             position = position_stack(), 
#             size  = ifelse(Name == 'Indonesia', 6.4, 7), 
#             hjust = 0, 
#             vjust = ifelse(Name == 'Indonesia', 1.8, 2.2),
#             alpha = ifelse(Name == 'Thailand', 0, 1) ) +
#   geom_text(aes(label = Str),
#             position = position_stack(), 
#             size  = ifelse(Name == 'Indonesia', 5.5, 6), 
#             hjust = 0, 
#             vjust = ifelse(Name == 'Indonesia', 4.2, 4.5) ,
#             alpha = ifelse(Name == 'Thailand', 0, 1) ) +
#   geom_text(aes(label = paste(Name, Str)),
#             position = position_stack(), size = 5.5, 
#             hjust = 0, vjust = 1.7,
#             alpha = ifelse(Name == 'Thailand', 1, 0) ) +
#   # geom_text(aes(label = Str),
#   #           position = position_stack(), size = 4.5, 
#   #           hjust = 0, vjust = 1,
#    #         alpha = ifelse(Name == 'Thailand', 1, 0) ) +
#   scale_y_continuous(limits=c(0, n), expand = c(0, 0)) +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_fill_manual(values = midBlues5) +
#   coord_cartesian(clip = 'off') +
#   barOnlyTheme

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

# fail - only CBNRM included
xSliceSorted <- in_out_long %>% 
  slice(match(out_order, out_x))

# fails
bothSorted <- in_out_long %>% 
  slice(match(out_order, out_x)) %>% 
  slice(match(in_order, in_y))

# works to sort Outcome but inside groups Intervention not ordered
xSorted <- in_out_long %>%
  arrange_at(1:2, desc) %>%
  arrange(match(out_x, out_order))

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


barNoCountryTheme <-  theme(
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
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent"),
  theme(plot.margin = unit(c(0,0,0,0), "in"))
) 
#   panel.background = element_blank()

library(patchwork)
smg + Thailand_bar # fails

# cowplot
library(cowplot)
library(magick)
small_file <- "png/seAsia.png"
small_map <- image_read(small_file)
image_browse(small_map)

ggdraw() +
  draw_image(
    small_map, scale = .3, x = 1,
    hjust = 1, halign = 1, valign = 0
  ) +
  draw_plot(Thailand_bar)

draw_image(smimg)
plot_grid()

# grid
library(gridExtra)


# deleted
#   scale_y_continuous(limits=c(0, 5), expand = c(0, 0)) +
# scale_x_discrete(expand = c(0, 0)) +
# , expand = c(0, 0)
# plot.margin = unit(c(0,0,0,0), "mm")
# Number only, omit country label

df <- country_plot_df %>% filter(Name == 'Philippines')
Philippines_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  barNoCountryTheme 
Philippines_bar

df <- country_plot_df %>% filter(Name == 'Thailand')
Thailand_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  barNoCountryTheme
Thailand_bar

df <- country_plot_df %>% filter(Name == 'Indonesia')
Indonesia_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0,0,0,1), "in"))
Indonesia_bar

df <- country_plot_df %>% filter(Name == 'Vietnam')
Vietnam_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = country_label_size, color='black', 
                                   angle = 30, vjust=1, hjust=0.8)) +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme +
  theme(plot.margin = unit(c(0,0.2,0,1), "in"))
Vietnam_bar

df <- country_plot_df %>% filter(Name == 'Cambodia')
Cambodia_bar <- ggplot(data = df, aes(x = Name, y = Absolute)) +
  geom_col(fill = '#0052cc', size = 0.2) +
  scale_y_continuous(limits=c(0, 6)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  geom_text(aes(label = Absolute), size = number_label_size, vjust = -0.2) +
  onebarTheme
Cambodia_bar