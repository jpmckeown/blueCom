library(googledrive)
library(googlesheets4)


# original extracted data
extract_id <- as_sheets_id('https://docs.google.com/spreadsheets/d/1UolosQyc6ktmfPPRSdhNIlMRQnQ_96iP/edit#gid=1834745113') %>% 
  as.character()

# fails, will have to download XLS or TSV
extract_data <- read_sheet(extract_id, sheet ='Summary DE')

mytheme <-  theme(
  axis.title.x=element_blank(),
  #axis.text.x=element_blank(),
  #axis.ticks.x=element_blank(),
  axis.title.y=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks.y=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank()
)

Name <- c("One", "Two", "Three", "Four")
Value <- c(2, 5, 7, 8)

df <-  data.frame(Name, Value) %>%
  mutate(Name = factor(Name, levels = Name),
         xPos = cumsum(lag(Value, default = 0)) + Value/2) # calculate interval position

ggplot(data = df, aes(x = xPos, y = 1, width = Value, fill = Name)) +
  geom_col(position = "identity",
           show.legend = FALSE) +
  geom_text(aes(label = Name),
            position = position_fill(vjust = 0.5)) +
  coord_fixed(expand = FALSE) +
  mytheme

# old vertical bars
```{r eval = TRUE, echo = FALSE}
sourceTable <- thisTable[order(thisTable$n),]
Name <- sourceTable$Biosphere_reserve
Value <- thisTable$percent
absolute <- thisTable$n
percent <- format(round(Value * 100, 0), nsmall = 0)
str <- paste0(Name, '  ( ', absolute, ' )')

df <-  data.frame(Name, Value) %>%
  mutate(Name = factor(Name, levels = Name),
         yPos = cumsum(lag(Value, default = 0)) + Value/2) # calculate interval position

thisPlot <- ggplot(data = df, aes(x = 'x', y = Value, fill = Name)) +
  geom_col(position = "fill", show.legend = FALSE, width = 0.4) +
  geom_text(aes(label = str),
            position = position_fill(), vjust = 2,
            size = ifelse(df$Name == 'Palawan', 4.5, 4),
            color = ifelse(df$Name == 'Palawan', 'white', 'black')) +
  labelonly +
  scale_fill_manual(values = midGreens)

ggsave(file="img/biosphere_vertical_raw.png", plot=thisPlot)
biosphereVerticalPlot <- thisPlot  
biosphereVerticalPlot
```

```{r}
library(magick)
typeOfData_label_img <- image_read('img/biosphere_vertical_raw.png')
image_info(typeOfData_label_img)
tmp <- image_trim(typeOfData_label_img)
image_write(tmp, path='img/biosphere_vertical.png', format='png')
image_info(tmp)
```

```{r eval = TRUE, echo = TRUE}
thisData <- de %>% 
  select('Data source') %>% 
  drop_na()

names(thisData) <- 'Data_source'
thisData[thisData == 'Grey literature organisational report',] <- 'Report'
thisData[thisData == 'Peer-reviewed published literature',] <- 'Peer-reviewed'

thisTable <- tabyl(thisData$Data_source)
names(thisTable)[1] <- 'Data source'
```


Country data, and calculate proportion
```{r eval = TRUE, echo = TRUE}
thisData <- de %>% 
  select(Country) %>% 
  drop_na()

thisTable <- tabyl(thisData$Country)
names(thisTable)[1] <- 'Country'

thisTable <- thisTable[order(thisTable$n),]
```

```{r eval = TRUE, echo = FALSE}
Name <- thisTable$Country
Value <- thisTable$percent
absolute <- thisTable$n
percentage <- format(round(Value * 100, 0), nsmall = 0)
str <- paste0(Name, '  ( ', absolute, ' )')

df <-  data.frame(Name, Value) %>%
  mutate(Name = factor(Name, levels = Name),
         yPos = cumsum(lag(Value, default = 0)) + Value/2) # calculate interval position

thisPlot <- ggplot(data = df, aes(x = 'x', y = Value, fill = Name)) +
  geom_col(position = "fill", show.legend = FALSE, width = 0.4) +
  geom_text(aes(label = str),
            position = position_fill(), vjust = 2,
            size = ifelse(df$Name == '', 4.5, 4),
            color = ifelse(df$Name == '', 'white', 'black')) +
  labelonly +
  scale_fill_manual(values = midBlues)

ggsave(file="img/country_vertical_raw.png", plot=thisPlot)
countryVerticalPlot <- thisPlot  
countryVerticalPlot
```

```{r}
typeOfData_label_img <- image_read('img/country_vertical_raw.png')
image_info(typeOfData_label_img)
tmp <- image_trim(typeOfData_label_img)
image_write(tmp, path='img/country_vertical.png', format='png')
image_info(tmp)
```

