#required packages
library(readxl)
library("tidyverse")
library(viridis)
library(RColorBrewer)
library(ggExtra)

#load and clean data
DatenEmma <- read_excel("~/Desktop/Doktorarbeit/RProgramming/EMMA-Paper/DatenEmma.xlsx", 
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

data_incubator <- DatenEmma %>% 
  select(patch, test_series_nr , incubator, thickness) %>% 
  filter(test_series_nr == "1") %>% 
  mutate(incubator = ifelse(incubator == "FALSE", "no", "yes"))

# sample size
sample_size = data_incubator %>% group_by(incubator) %>% summarize(num=n())

#box plot
p1 <- data_incubator %>% 
left_join(sample_size) %>%
  mutate(myaxis = paste0(incubator, "\n", "n=", num)) %>%
ggplot(aes(x = myaxis, y=thickness, fill = incubator)) + 
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
  geom_point(color="black", size=1, alpha=0.9) +
  ggtitle("Effects of an incubator on patch yield") +
  theme_light(
  )+
  theme(legend.position="none", 
        plot.title = element_text(size=20),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 11)
        ) +
  xlab("Incubator?")+
  ylab(label = c("Thickness"))

ggsave(filename = "Fig1A-incubator", plot = p1, device = "tiff" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 9)



p1



