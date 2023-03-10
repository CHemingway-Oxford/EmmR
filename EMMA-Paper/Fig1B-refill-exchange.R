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
data_mediumcond <- DatenEmma %>% 
  filter(test_series_nr == "1") %>% 
  select(patch, test_series_nr , refill_exchange, thickness)

#mutate column refill_exchange -> factor with levels "rrr", "rer", "eee"
data_mediumcond$refill_exchange <- as.factor(data_mediumcond$refill_exchange)
data_mediumcond$refill_exchange <- factor(
  data_mediumcond$refill_exchange, levels = c("rrr","rer","eee"))

# sample size
sample_size = data_mediumcond %>% group_by(refill_exchange) %>% summarize(num=n())

#box plot
p1 <- data_mediumcond %>% 
  left_join(sample_size) %>%
  mutate(myaxis = paste0(refill_exchange, "\n", "n=", num)) %>%
  ggplot(aes(x = myaxis, y=thickness, fill = refill_exchange)) + 
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
  ggtitle("Effects of medium refill vs exchange") +
  theme_light()+
  theme(legend.position="none", 
        plot.title = element_text(size=20),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 11)
        ) +
  xlab("Modality of medium refill vs exchange")+
  ylab(label = c("Thickness"))

ggsave(filename = "Fig1B-medium", plot = p1, device = "tiff" , dpi = 1000 , 
       units = "cm" , width = 16.8 , height = 9)
p1