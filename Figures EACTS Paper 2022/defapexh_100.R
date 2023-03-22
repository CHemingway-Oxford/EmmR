library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)

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
data_defapexh100 <- DatenEmma %>% 
  select(patch, thickness, deflection_height_100, test_series_nr) %>% 
  mutate(patch = ifelse(patch == "PP" , "Pericardium" , "Bacterial Cellulose")) %>% 
  drop_na(deflection_height_100) %>%
  filter(test_series_nr != "1") %>% 
  na.omit()

colour_palette <- c("#FB3640" , "#558B6E" , "#3A1772" , "#F9DB6D")


#scatter plot
p100 <- ggplot(data_defapexh100 , aes(x=thickness, y=deflection_height_100, color=test_series_nr)) + 
  geom_point(
    shape=20,
    size=2,
    alpha=1
  ) +
  scale_color_manual(values = colour_palette , labels=c("2" , "3" , "4" , "Pericardium"))+
  ylab(label = "100 mmHg [mm]")+
  xlab(label = "patch thickness [mm]")+
  labs(color = "Test Series")+
  theme_classic()+
  theme(
        # legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10))
p100

# p100_marg <- ggMarginal(p_h100, type="boxplot", size = 10, margins = 'y', groupColour = TRUE, groupFill = TRUE)
# p100_marg


# ggsave(filename = "Fig2-defapexh_100", plot = p100, device = "tiff" , dpi = 1000 ,
#        units = "cm" , width = 16.8 , height = 12)
# 
