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
