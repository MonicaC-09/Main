library(tidyr)
library(ggplot2)
library(dplyr)

# Histogram


# Set plotting margins
par(mar=c(5, 4, 4, 2))

# Create the histogram
mpghist <- hist(mtcars$mpg, freq = TRUE,
     breaks = 5, 
     col = "gray80",
     border = "black",
     main = "Miles Per Gallon of Cars",
     ylab = "Frequency",
     xlab = "Miles Per Gallon (MPG)")

# Create density object 

densitymt <- density(mtcars$mpg)

#Scale the data for density curve
scaling <- diff(mpghist$mids[1:2]) * length(mtcars$mpg)
lines(densitymt$x,densitymt$y * scaling, 
      col = "black", lwd = 2)

# Bar Chart

data("mtcars")
top5mpg <- mtcars$mpg[1:5]
carnames <- row.names(mtcars)[1:5]


#         I. Vertical

par(las=1, mar=c(5, 12, 5, 3), cex=0.7) 
barplot(top5mpg, names.arg = carnames, horiz = TRUE,
        col = brewer.pal(length(carnames), "Greens"),
        main = "Miles Per Gallon of Top 5 Cars",
        xlab = "Miles Per Gallon (MPG)",
        xlim = c(0, max(top5mpg) + 5))


#           II. Horizental
par(las=1, mar=c(6, 5, 7, 5), cex=0.8) 
barplot(top5mpg, names.arg = carnames, horiz = FALSE,
        col = brewer.pal(length(carnames), "Greens"),
        main = "Miles Per Gallon of Top 5 Cars",
        ylab = "Miles Per Gallon (MPG)",
        xlab = "Car Names",
        ylim = c(0, max(top5mpg) + 5))


# Pie Chart


require(RColorBrewer)
View(mtcars)
top5 <- setNames(mtcars$mpg[1:5],row.names(mtcars)[1:5])
print(top5)
indices <- top5 !=0

#adjust pie chart to show mpg in the slices
pie(top5[indices], labels = top5[indices], 
    col = brewer.pal(length(top5[indices]), "Set1"), main = "Top Five Reliable MPG cars")

 par(mar=c(4, 3, 4, 3), xpd=FALSE, cex=0.8)
legend("bottomleft", legend = names(top5)[indices],  
       fill = brewer.pal(length(top5[indices]), "Set1"), pch = 7)



# Boxplot 

data = mtcars

# Set plotting parameters
par(las=1, mar=c(6, 5, 7, 5), cex=0.8) 

# Create the boxplot for Miles per Gallon by number of Cylinders from mpg data
boxplot(mpg ~ cyl, data = mtcars, 
        xlab = "number of cylinders", 
        ylab = "Miles per Gallon", 
        main = "Reliable Cars mpg vs cyl")
  
# Scatterplot

library(ggplot2)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, size = displ)) +
  geom_point(color = "blue", pch = 16, size = 1.5)+ 
  geom_smooth(method = "loess", color = "black") + scale_x_log10() +
  xlab("MPG on Highway") +
  facet_wrap(~drv, nrow=3)  + # drv is the type of drive train, where f = front-wheel drive, r = rear wheel drive, 4 = 4wd
  theme_bw() + 
  theme(text=element_text(size=10, family="Palatino"))

