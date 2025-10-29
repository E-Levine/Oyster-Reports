# Set formatting for Report

#Base formatting for figure consistency
BaseForm <- theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA), #Add border
        plot.title = element_text(hjust = 0, vjust = -7, family = "serif", color = "black", size = 12), #Add plot title for section
        axis.title = element_text(family = "serif", color = "black", size = 12), #Adjust axis title and text
        axis.text = element_text(family = "serif", color = "black", size = 10),
        axis.line = element_line(color = "black"),
        axis.ticks.length.x = unit(0.20, "cm"),
        axis.ticks.length.y = unit(0.1, "cm"),
        legend.position = "top", legend.justification = "right",  legend.box.spacing = unit(0.05, "line"), #Adjust legend position
        legend.title = element_blank(), legend.key.height = unit(0.5, "line"), legend.key.width = unit(1, "lines"), #Adjust legend title and spacing
        legend.text = element_text(family = "serif", color = "black", size = 9)) #Adjust legend text
