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