library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
library(grid)
library(cowplot)

zugversuche_daten <- read_excel("~/Desktop/Doktorarbeit/r_programming/EMMA-Paper/ZugversucheNeu.xlsx",
                                col_types = c("numeric",
                                              "numeric",
                                              "text"))

colour_palette <- c("#de425b","#e67f83","#c1e7ff","#6996b3","#004c6d")
p_zugversuch <- zugversuche_daten %>%
  ggplot( aes(x=`Dehnung [%]`, y=`Standardkraft [N]`, group = Probe, colour = Probe))+
  geom_line()+
  scale_colour_manual(values = colour_palette)+
  theme_minimal_grid()+
  ggtitle(label = "Dehnung vs Standardkraft")
p_zugversuch

# ggsave(filename = "Dehnung_vs_SK", plot = p_zugversuch, device = "tiff" , dpi = 300 ,
#                    units = "cm" , width = 16.8 , height = 9)

