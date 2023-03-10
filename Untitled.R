library(ggplot2)
library(gtable)
data(mtcars)

p1 <- ggplot(mtcars, aes(mpg, hp)) + 
  geom_point() + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  expand_limits(y = c(0, 
                      max(mtcars$hp) + 0.1 * diff(range(mtcars$hp)))) + 
  expand_limits(x = c(0, 
                      max(mtcars$mpg) + 0.1 * diff(range(mtcars$mpg))))

# Horizontal marginal boxplot - to appear at the top of the chart
p2 <- ggplot(mtcars, aes(x = factor(1), y = mpg)) + 
  geom_boxplot(outlier.colour = NA) + 
  geom_jitter(position = position_jitter(width = 0.05)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  expand_limits(y = c(0, 
                      max(mtcars$mpg) + 0.1 * diff(range(mtcars$mpg)))) + 
  coord_flip() + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        plot.margin = unit(c(1, 0.2, -0.5, 0.5), "lines"))

# Vertical marginal boxplot - to appear at the right of the chart
p3 <- ggplot(mtcars, aes(x = factor(1), y = hp)) + 
  geom_boxplot(outlier.colour = NA) + 
  geom_jitter(position = position_jitter(width = 0.05)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  expand_limits(y = c(0, 
                      max(mtcars$hp) + 0.1 * diff(range(mtcars$hp)))) + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        plot.margin = unit(c(0.2, 1, 0.5, -0.5), "lines"))

gt1 <- ggplot_gtable(ggplot_build(p1))
gt2 <- ggplot_gtable(ggplot_build(p2))
gt3 <- ggplot_gtable(ggplot_build(p3))

# Get maximum widths and heights
maxWidth <- unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
maxHeight <- unit.pmax(gt1$heights[4:5], gt3$heights[4:5])

# Set the maximums in the gtables for gt1, gt2 and gt3
gt1$widths[2:3] <- as.list(maxWidth)
gt2$widths[2:3] <- as.list(maxWidth)

gt1$heights[4:5] <- as.list(maxHeight)
gt3$heights[4:5] <- as.list(maxHeight)

# Create a new gtable
gt <- gtable(widths = unit(c(7, 1), "null"), height = unit(c(1, 7), "null"))

# Instert gt1, gt2 and gt3 into the new gtable
gt <- gtable_add_grob(gt, gt1, 2, 1)
gt <- gtable_add_grob(gt, gt2, 1, 1)
gt <- gtable_add_grob(gt, gt3, 2, 2)


# And render the plot
grid.newpage()
grid.draw(gt)

grid.rect(x = 0.5, y = 0.5, height = 0.995, width = 0.995, default.units = "npc", 
          gp = gpar(col = "black", fill = NA, lwd = 1))

