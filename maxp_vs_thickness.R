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
## extra data for thickness vs maximum withstood pressure

data_maxp <- DatenEmma %>% 
  select(patch, thickness, max_pressure , test_series_nr) %>% 
  mutate(patch = ifelse(patch == "PP" , "Pericardium" , "Bacterial Cellulose")) %>% 
  drop_na(max_pressure) %>%
  drop_na(thickness) %>% 
  filter(test_series_nr != "1") %>% 
  mutate(test_series_n = ifelse(test_series_nr == "3", 2, test_series_nr))
#na.omit() # hash-out if height_max

colour_palette <- c("#FB3640" , "#558B6E" , "#3A1772" , "#F9DB6D")


#scatter plot
p_maxp <- data_maxp %>% ggplot(aes(x=thickness, y=max_pressure, color=test_series_nr)) + 
  geom_point(
    inherit.aes = TRUE,
    #fill="black",
    shape=20,
    size=3,
    alpha=1
    #stroke = 1
  ) +
  geom_smooth(method = "lm", se = FALSE,
              inherit.aes = FALSE,
              aes(x=thickness, y=max_pressure , group = test_series_n),
              size = 0.5,
              color = "black",
              alpha = 0.5
              )+
  scale_color_manual(values = colour_palette)+
  theme_classic()+
  theme(legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))+
  xlab(label = "Thickness [mm]")+
  ylab(label = "Maximum \n withstood pressure [mmHg]")+
  ggtitle(label = "Pressure vs Thickness")+
  labs(color = "Test Series")

p1_maxpmarg <- ggMarginal(p_maxp, type="boxplot", size = 10, margins = 'y', groupColour = TRUE, groupFill = TRUE)
p_maxp


ggsave(filename = "Fig2-maxp-thickness", plot = p_maxp, device = "tiff" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 12)

p_maxp