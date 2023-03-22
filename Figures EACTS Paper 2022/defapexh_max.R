library("tidyverse") #includes dplyr and ggplot2
library(readxl)
library(tidyr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)
library(gtable)
library(cowplot)


#create and clean data frame
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
data_defapexhmax <- DatenEmma %>% 
  select(patch, thickness, deflection_height_max, test_series_nr) %>% 
  mutate(patch = ifelse(patch == "PP" , "Pericardium" , "Bacterial Cellulose")) %>% 
  drop_na(deflection_height_max) %>%
  filter(test_series_nr!="1" & test_series_nr!="4") %>% 
  mutate(thickness = ifelse(patch == "Pericardium", 1,thickness))
  

colour_palette <- c("#FB3640" , "#558B6E" , "#3A1772" , "#F9DB6D")

#scatter plot
p_hmax <- data_defapexhmax %>% ggplot(aes(x=thickness, y=deflection_height_max, color=test_series_nr)) + 
  geom_point(
    shape=20, #filled circles
    size=2,
    alpha=1
  ) +
  scale_color_manual(values = colour_palette[c(1,2,4)] , labels=c("2" , "3" , "Pericardium"))+
  theme_classic()+
  theme(
        # legend.position = "bottom",
        plot.title = element_text(size=16),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10))+
  labs(color = "Test Series")+
  ylab(label = "maximum \npressure [mm]")+
  xlab(label = "patch thickness [mm]")
  

# p1_hmax <- ggMarginal(p_hmax, type="boxplot", size = 10, margins = 'y', groupColour = TRUE, groupFill = TRUE)
p_hmax


ggsave(filename = "Fig2-defapexh_max", plot = p_hmax, device = "tiff" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 12)


p_hall <- DatenEmma %>% 
  select(thickness , max_pressure ,   test_series_nr , patch) %>% 
  mutate(patch = ifelse(patch == "PP" , "Pericardium" , "Bacterial Cellulose")) %>% 
  filter(test_series_nr!="1") %>% 
  # na.omit() %>% 
  mutate(thickness = ifelse(patch == "Pericardium", 1,thickness)) %>% 
  ggplot(aes(x = thickness , y = max_pressure , color = test_series_nr))+
  geom_point(
    shape=20, #filled circles
    size=3,
    alpha=1,
    stroke = 1
  )+
  scale_color_manual(values = colour_palette[c(1,2,3,4)] , 
                     labels= c("2" , "3" , "4" , "Pericardium"))+
  labs(color = "Test Series")+
  theme_classic()+
  theme(
    legend.position = "bottom",
    plot.title = element_text(size=16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )




# #marginal box plots: BC vs Pericardium
# pmarg_hmax <- data_defapexhmax %>% ggplot(aes(x = patch, y = deflection_height_max)) + 
#   geom_boxplot(outlier.colour = NA) + 
#   geom_jitter(position = position_jitter(width = 0.05)) + 
#   #scale_y_continuous(expand = c(0, 0)) + 
#   #expand_limits(y = c(min(data_defapexhmax$deflection_height_max) - 0.1 * diff(range(data_defapexhmax$deflection_height_max)), 
#     #                  max(data_defapexhmax$deflection_height_max) + 0.1 * diff(range(data_defapexhmax$deflection_height_max)))) +
#   theme(axis.text = element_blank(), 
#         axis.title = element_blank(), 
#         axis.ticks = element_blank(), 
#         plot.margin = unit(c(0.2, 1, 0.5, -0.5), "lines"))
# 
# #get table
# gt1 <- ggplot_gtable(ggplot_build(p_hmax))
# gt2 <- ggplot_gtable(ggplot_build(pmarg_hmax))
# 
# ##Set the maximum widths and heights for x-axis and y-axis titles and text
# # Get maximum widths and heights
# #maxWidth <- unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
# maxHeight <- unit.pmax(gt1$heights[4:5], gt2$heights[4:5])
# 
# ## Set the maximums in the gtables for gt1, gt2 and gt3
# #gt1$widths[2:3] <- as.list(maxWidth)
# #gt2$widths[2:3] <- as.list(maxWidth)
# 
# gt1$heights[4:5] <- as.list(maxHeight)
# gt2$heights[4:5] <- as.list(maxHeight)
# 
# 
# ## create plot array
# # Create a new gtable
# gt <- gtable(widths = unit(c(7, 1), "null"), height = unit(c(1, 7), "null"))
# # Instert gt1, gt2 and gt3 into the new gtable
# gt <- gtable_add_grob(gt, gt1, 2, 1)
# #gt <- gtable_add_grob(gt, gt2, 1, 1)
# gt <- gtable_add_grob(gt, gt2, 2, 2)
# # And render the plot
# grid.newpage()
# grid.draw(gt)
# grid.rect(x = 0.5, y = 0.5, height = 0.995, width = 0.995, default.units = "npc", 
#           gp = gpar(col = "black", fill = NA, lwd = 1))
source("/Users/ChrisHemingway/Desktop/Doktorarbeit/r_programming/Figure2/defapexh_60.R")
source("/Users/ChrisHemingway/Desktop/Doktorarbeit/r_programming/Figure2/defapexh_100.R")
legend <- get_legend(
  p_hall + 
    theme(legend.box.margin = margin(
      0,0,0,0 , unit = "cm")
    ) +
    labs(color = "Test series")
  # +
  # guides(color = guide_legend(override.aes = list(size=2 , stroke = 2)))
)
plot_grid_defapex <- plot_grid(p60+theme(legend.position = "none") ,
                               p100+theme(legend.position = "none"),
                               p_hmax+theme(legend.position = "none"), nrow = 1)
plot_grid_defapex_wlegend <- plot_grid(plot_grid_defapex , legend ,
                                       ncol = 1 , rel_heights = c(11,1))

plot(plot_grid_defapex_wlegend)

ggsave(filename = "Fig2_full", plot = plot_grid_defapex_wlegend, device = "tiff" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 9)




