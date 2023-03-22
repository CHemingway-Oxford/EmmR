library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
library(grid)
library(cowplot)

DatenEmma <- read_excel("C:/Users/Asus/Documents/Doktorarbeit II/Tabellen/DatenEmma.xlsx", 
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
## plot horizontally: lollipop plot (categorial -> test series Nr ,
#continuous -> pressure, if rupture end point -> red, if not rupture -> blue)

colour_palette <- c("#FB3640" , "#558B6E" , "#3A1772" , "#F9DB6D")

data_lollipop <- DatenEmma %>% 
  select(patch, max_pressure , test_series_nr, test_serie_alpha , rupture) %>% 
  mutate(patch = ifelse(patch == "PP" , "Pericardium" , "Bacterial Cellulose")) %>% 
  drop_na(max_pressure) %>%
  filter(test_series_nr != "1") %>% 
  mutate(ID = c(1:20))
#na.omit() # hash-out if height_max


#create horizontal lollipop-plot
p_lolli <- data_lollipop %>%
  arrange(max_pressure) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(ID=factor(ID, levels=ID)) %>%   # This trick update the factor levels
  ggplot( aes(x=ID, y=max_pressure)) +
  geom_segment( aes(xend=ID, yend=0) , size  = 0.6) +
  geom_point(aes(shape = rupture , color = test_series_nr) ,  size=3 , stroke = 1) +
  scale_shape_manual(values = c(20, 4) , labels  = c("No" , "Yes")) +
  scale_color_manual(values = colour_palette , labels=c("2" , "3" , "4" , "Pericardium"))+
  scale_y_continuous(limits = c(0,205) , breaks=c(0 , 60 , 100 , 200) ,
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

# ggsave(filename = "Fig2_maxp_lollipop", plot = , device = "tiff" , dpi = 1000 , 
#        units = "cm" , width = 16.8 , height = 12)


#####generate common figure legend with uniform labelling:
legend <- get_legend(
  p_lolli + 
    theme(legend.box.margin = margin(
      2,6,2,6 , unit = "cm")
    ) +
    labs(color = "Test series" , shape = "Ruptured in test")
  # +
  # guides(color = guide_legend(override.aes = list(size=2 , stroke = 2)))
)

####make figure 3_complete:
source("/Users/ChrisHemingway/Desktop/Doktorarbeit/r_programming/Figure2/maxp_vs_thickness.R")
p_maxp_th <- p_maxp

row_1 <- plot_grid(
  p_maxp_th +
    theme(
      legend.position = "none"), 
  p_lolli +
    theme(
      legend.position = "none"), 
  
  nrow = 1, labels = c("a" , "b"))

figure3_final <- plot_grid(
  row_1,
  legend,
  ncol = 1,
  rel_heights = c(12,1)
)
figure3_final

ggsave(filename = "Fig3_full", plot = figure3_final, device = "tiff" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 9)

