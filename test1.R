install.packages('blotter', repos="http://R-Forge.R-project.org", dependencies = TRUE) 

install.packages('blotter', repos="http://R-Forge.R-project.org", dependencies = TRUE, type="source") 

install.packages("devtools")
install.packages("FinancialInstrument")
install.packages("PerformanceAnalytics")

install.packages("blotter_0.9.1695.zip")
install.packages("blotter", repos="http://R-Forge.R-project.org")
library(zoo)
require(rCharts)

## Example 1 Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

## Example 2 Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')

r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = 'point', color = 'gear')
r1
hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, 
            type = 'multiBarChart')
n1
