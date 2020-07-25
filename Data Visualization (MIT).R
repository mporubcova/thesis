
######### Introduction to Visualization ###########################

install.packages("ggplot2")
library(ggplot2)

# ggplot2 code has 3 elements:
    # data
    # aesthetics - color, shape...
    # graphics - points, lines, bars etc.

WHO = read.csv("WHO.csv")
str(WHO)

colors() # All colors available in R

# R base graphs
plot(WHO$GNI, WHO$FertilityRate)

# ggplot2 graphs - connect parts with "+"
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_point(color = "blue", size = 3, shape = 15) 
WHOPlot = scatterplot + geom_point() + ggtitle ("Gross National Income")

# Save the plot
pdf("WHOPlot.pdf")
print(WHOPlot)
dev.off()
