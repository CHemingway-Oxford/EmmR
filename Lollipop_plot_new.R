#Load libraries for plotting
library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
library(grid)
library(cowplot)

#Load and clean Data 
DatenEmma <- read_excel("Doktorarbeit II/Tabellen/Medien_Vgl_Dicken,Gewichte&Pressure.xlsx", 
                                                sheet = "max Pressure", col_types = c("skip", 
                                                                                      "text", "skip", "numeric", "skip", 
                                                                                      "text", "skip"), na ="NA" )

#View(DatenEmma) 

colour_palette <- c('#66c2a5' , '#fc8d62' , '#8da0cb' , '#e78ac3')

data_lollipop <- DatenEmma %>% 
  select(Pressure_max_mmHg, Rupture, Medium) %>% 
  drop_na(Pressure_max_mmHg) %>%
  mutate(ID = c(1:24))

#create horizontal lollipop Plot
p_lolli <- data_lollipop %>%
  arrange(Pressure_max_mmHg) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(ID=factor(ID, levels=ID)) %>%# This trick updates the factor levels
  
  ggplot( aes(x=ID, y=Pressure_max_mmHg)) +
  #geom_segment(aes(xend=ID, yend=400) , size  = 0.6) +
  geom_point(aes(shape = Rupture , color = Medium) ,  size=3 , stroke = 1) +
  scale_shape_manual(values = c(20, 4) , labels  = c("No" , "Yes")) +
  scale_color_manual(values = colour_palette , labels=c("45|45|10" , "3|3|3" , "50|50" , "C_100"))+
  scale_y_continuous(limits = c(600,1415) , breaks=c(600 , 800 , 1000,1200) ,
                     expand = c(0,0))+
  scale_x_discrete(breaks = NULL)+
  coord_flip()+
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(),
        panel.grid = element_blank(),
        panel.border = element_blank()
        
  )+
  ylab("Pressure [mmHg]")+
  xlab("")+
  ggtitle(label = "Maximum Withstood Pressure")

ggsave(path = "C:/Users/Asus/Documents/Doktorarbeit II/R_Coding/Github/EmmR", filename = "Max_WithstoodPressure_MediumComp.png", plot = p_lolli, device = "png" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 12)


#####generate common figure legend with uniform labelling:
legend <- get_legend(
  p_lolli + 
    theme(legend.box.margin = margin(
      2,6,2,6 , unit = "cm")
    ) +
    labs(color = "Test series" , shape = "Ruptured in test"))

