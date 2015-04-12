library(ggplot2)
library(gtable)

# Main scatterplot
p1 <- ggplot(LOR4reg@data, aes(x=Mietechgr, y=Alosechg, size=E_E.2012),shape=12) +
  geom_point() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y = c(min(LOR4reg@data$Alosechg) - 0.1 * diff(range(LOR4reg@data$Alosechg)),
                      max(LOR4reg@data$Alosechg) + 0.1 * diff(range(LOR4reg@data$Alosechg)))) +
  expand_limits(x = c(min(LOR4reg@data$Mietechgr)- 0.1 * diff(range(LOR4reg@data$Mietechgr)),
                      max(LOR4reg@data$Mietechgr)+ 0.1 * diff(range(LOR4reg@data$Mietechgr)))) +
  theme(plot.margin= unit(c(0, 0, 0.5, 0.5), "lines"),
        legend.position = "none")
# To remove all axis labelling and marks from the two marginal plots
theme_remove_all <- theme(axis.text = element_blank(),
                          axis.title = element_blank(),
                          axis.ticks = element_blank(),
                          axis.ticks.margin = unit(0, "lines"),
                          axis.ticks.length = unit(0, "cm"))
# Horizontal marginal boxplot - to appear at the top of the chart
p2 <- ggplot(LOR4reg@data, aes(x = factor(1), y = Mietechgr)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = position_jitter(width = 0.05)) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y = c(min(LOR4reg@data$Mietechgr) - 0.1 * diff(range(LOR4reg@data$Mietechgr)),
                      max(LOR4reg@data$Mietechgr) + 0.1 * diff(range(LOR4reg@data$Mietechgr)))) +
  coord_flip() +
  theme_remove_all +
  theme(plot.margin= unit(c(0.5, 0, 0, 0.5), "lines"))
# Vertical marginal boxplot - to appear at the right of the chart
p3 <- ggplot(LOR4reg@data, aes(x = factor(1), y = Alosechg)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(position = position_jitter(width = 0.05)) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y = c(min(LOR4reg@data$Alosechg) - 0.1 * diff(range(LOR4reg@data$Alosechg)),
                      max(LOR4reg@data$Alosechg) + 0.1 * diff(range(LOR4reg@data$Alosechg)))) +
  theme_remove_all +
  theme(plot.margin= unit(c(0, 0.5, 0.5, 0), "lines"))
# Get the gtables
gt1 <- ggplot_gtable(ggplot_build(p1))
gt2 <- ggplot_gtable(ggplot_build(p2))
gt3 <- ggplot_gtable(ggplot_build(p3))
# Get maximum widths and heights for x-axis and y-axis title and text
maxWidth = unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
maxHeight = unit.pmax(gt1$heights[4:5], gt3$heights[4:5])
# Set the maximums in the gtables for gt1, gt2 and gt3
gt1$widths[2:3] <- as.list(maxWidth)
gt2$widths[2:3] <- as.list(maxWidth)
gt1$heights[4:5] <- as.list(maxHeight)
gt3$heights[4:5] <- as.list(maxHeight)
# Combine the scatterplot with the two marginal boxplots
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

