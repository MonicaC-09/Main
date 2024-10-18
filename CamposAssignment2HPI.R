View(DataVizualizationHPI)
myHPIdata <- DataVizualizationHPI
colnames(myHPIdata)
str(myHPIdata)
head(myHPIdata)

unique_continents <- unique(myHPIdata$Continent)
colors <- rainbow(length(unique_continents)) 
continent_colors <- setNames(colors, unique_continents)


#Scatterplot
par(las=1, mar=c(6, 5, 7, 5), cex=0.8) 
plot(x = myHPIdata$`GDP per capita ($)`, y = myHPIdata$`Carbon Footprint (tCO2e)`, 
     col = continent_colors[myHPIdata$Continent],
     main = "Carbon Footprint by GDP",
    xlab = "GDP", ylab = "Carbon Footprint",
    ylim = c(1,30),
    xlim = c(1000,50000),
    pch = 16, frame = FALSE)

par(las=1, mar=c(4, 3, 4, 3), cex=0.7) 
legend("topright", legend = unique_continents, col = continent_colors, pch = 16)


#Barplot

install.packages("RColorBrewer")
library(RColorBrewer)

colors <- brewer.pal(7, "Greens")

CarbonByCountry <- tapply(myHPIdata$`Carbon Footprint (tCO2e)`, myHPIdata$Country, sum)
TopSevenCountries <- sort(CarbonByCountry, decreasing = TRUE)[1:7]
print(TopSevenCountries)
par(las=1, mar=c(6, 5, 7, 5), cex=0.5) 
barplot(TopSevenCountries,
        main = "Top Seven Countries by Carboon Footprints", 
        ylab = "Carbon Footprint",
        xlab = "Countries",
        col = colors,
        family = "serif")

#Histogram

myHPIdata <- DataVizualizationHPI

# Make sure no Y exceed [-3.5, 3.5]
# Set up a range and remove values outside of it
Y <- myHPIdata$`Carbon Footprint (tCO2e)`
Y[Y < -3.5 | Y > 3.5] <- NA  # Set values outside the range to NA

# Define the sequence for normal distribution curve
x <- seq(-3.5, 3.5, by = 0.1)
dn <- dnorm(x)

# Set plotting margins
par(mar=c(4.5, 4.1, 3.1, 0))

# Create the histogram
hist(Y, breaks = seq(-5, 5, by = 0.5), 
     ylim = c(0, 0.5), 
     col = "gray80", 
     freq = FALSE,  # Use density instead of frequency
     main = "Histogram of Carbon Footprint with Normal Curve",
     xlab = "Carbon Footprint (tCO2e)", 
     ylab = "Density")

# Overlay the normal distribution curve
lines(x, dnorm(x), lwd = 2)

# Add a legend
legend("topright", 
       legend = c("Histogram", "Normal Curve"),
       fill = c("gray80", NA), 
       border = c("gray80", NA),
       lty = c(NA, 1), lwd = c(NA, 2), col = c(NA, "black"))


#Boxplot

# Set plotting parameters
par(las=1, mar=c(6, 5, 7, 5), cex=0.8) 

# Create the boxplot for Carbon Footprint (tCO2e) by Continent
boxplot(`Carbon Footprint (tCO2e)` ~ Continent, 
        data = myHPIdata,
        main = "Boxplot of Carbon Footprint by Continent", 
        xlab = "Continent", 
        ylab = "Carbon Footprint (tCO2e)",
        col = c("lightblue", "lightgreen", "orange", "pink", "purple", "gray"), 
        ylim = c(0.5, max(myHPIdata$`Carbon Footprint (tCO2e)`, na.rm = TRUE)))  

#Pie()

par(mar=c(6, 5, 7, 5), xpd=FALSE, cex=0.8)
library(pct)
TopFiveData
pie(TopFiveData$`Life Expectancy (years)`,
    labels = paste(TopFiveData$`Life Expectancy (years)`),
    col = rainbow(length(TopFiveData$Country)),
    main = "The Life Expectancy of the Top Five Countries")

par(mar=c(6, 5, 7, 5), xpd=FALSE, cex=0.8)
legend("topright", legend = TopFiveData$Country,  col = rainbow(length(TopFiveData$Country)), pch = 20)



# Persp()
# To illustrate simple right circular cone
cone <- function(x = TopFiveData$Country, y = TopFiveData$`Life Expectancy (years)`){
  sqrt(x ^ 2 + y ^ 2)
}

# prepare variables.
x <- y <- seq(-1, 1, length = 30)
z <- outer(x, y, cone)

# plot the 3D surface
persp(x, y, z)
