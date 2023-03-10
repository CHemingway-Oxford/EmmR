#required packages
library(readxl)
library("tidyverse")
library(viridis)
library(RColorBrewer)
library(ggExtra)

#load and clean data
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
data_inctime <- DatenEmma%>% 
  filter( 
           test_series_nr == "3"| 
           test_series_nr == "4") %>% 
  select(patch, test_series_nr , incubation_time, thickness)

#mutate column incubation_time -> factor with levels "6", "12"
data_inctime$incubation_time <- as.factor(data_inctime$incubation_time)
data_inctime$incubation_time <- factor(
  data_inctime$incubation_time, levels = c("6","12"))

# sample size
sample_size = data_inctime %>% group_by(incubation_time) %>% summarize(num=n())

#box plot
p1 <- data_inctime %>% 
  left_join(sample_size) %>%
  mutate(myaxis = paste0(incubation_time, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y=thickness, fill = incubation_time)) + 
  geom_boxplot(
    outlier.colour = NA#,
    # custom boxes
    #color="blue",
    #fill="blue",
    #alpha=0.2,
    
    # Notch?
    #notch=FALSE,
    #notchwidth = 0.8,
    
    # custom outliers
    #outlier.colour="red",
    #outlier.fill="red",
    #outlier.size=3
  ) + 
  expand_limits(y = c(0:3)) +
  scale_fill_brewer(palette="PuRd") +
  geom_jitter(color="black", size=1, alpha=0.9, width = 0.05) +
  ggtitle("Effects of incubation time on patch yield") +
  theme_light()+
  theme(legend.position="none", 
        plot.title = element_text(size=20),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 11)
        ) +
  xlab("Incubation time (days)")+
  ylab(label = c("Thickness"))

ggsave(filename = "Fig1C-inctime", plot = p1, device = "tiff" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 9)
p1
