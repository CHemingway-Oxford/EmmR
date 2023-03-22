#####load relevant packages

library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
######load dataset DatenEmma
DatenEmma <- read_excel("/Users/ChrisHemingway/Desktop/Doktorarbeit/r_programming/EMMA-Paper/DatenEmma.xlsx", 
                        sheet = "Tabelle1", col_types = c("text", 
                                                          "text",
                                                          "text",
                                                          "text",
                                                          "text", 
                                                          "text",
                                                          "text",
                                                          "numeric",
                                                          "numeric", 
                                                          "numeric",
                                                          "numeric",
                                                          "numeric", 
                                                          "numeric"),
                        na = "NA")
####data clean up and selection:
data_defapexh60 <- DatenEmma %>% 
  select(patch, thickness, deflection_height_60, test_series_nr) %>% 
  mutate(patch = ifelse(patch == "PP" , "Pericardium" , "Bacterial Cellulose")) %>% 
  drop_na(deflection_height_60) %>%
  filter(test_series_nr != "1") %>% 
  na.omit() # hash-out if height_max

colour_palette <- c("#FB3640" , "#558B6E" , "#3A1772" , "#F9DB6D")


#####scatter plot
p60 <- data_defapexh60 %>% ggplot(aes(x=thickness, y=deflection_height_60, color = test_series_nr)) + 
  geom_point(
    shape=20,
    size=2,
    alpha=1
  ) +
  scale_color_manual(values = colour_palette[1:3] , labels = c("2" , "3" , "4"))+
  ylab(label = "deflection height at \n60 mmHg [mm]")+
  xlab(label = "patch thickness [mm]")+
  labs(color = "Test Series")+
  theme_classic()+
  theme(
        # legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10))
p60 

#####with marginal box plot if left uncommented:
# p60_marg <- ggMarginal(p, type="boxplot", size = 10, margins = 'y', groupColour = TRUE, groupFill = TRUE)
 
#####save file as tiff, @dpi 1000 for publication qualitiy image. Dimensions can be further specified. 
  # ggsave(filename = "Fig2-defapexh_60", plot = p60, device = "tiff" , dpi = 1000 ,
  #        units = "cm" , width = 16.8 , height = 12)
  