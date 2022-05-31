library(plyr)
library(ggplot2)
library(countrycode)
library(rworldmap)
library(qcc)
library(plotly)
library(scatterplot3d)
library(tidyverse)
library(ggpubr)
library(funModeling)

data_2015 <- read.csv ("datasets/2015.csv")
data_2016 <- read.csv ("datasets/2016.csv")
data_2017 <- read.csv ("datasets/2017.csv")
data_2018 <- read.csv ("datasets/2018.csv")
data_2019 <- read.csv ("datasets/2019.csv")

head(list(data_2015, data_2016, data_2017, data_2018, data_2019))

#stergerea coloanelor care nu sunt necesare
data_2015 <- subset(data_2015, select=-c(Standard.Error, Dystopia.Residual))
data_2016 <- subset(data_2016, select=-c(Lower.Confidence.Interval, Upper.Confidence.Interval, Dystopia.Residual))
data_2017 <- subset(data_2017, select=-c(Whisker.high,Whisker.low, Dystopia.Residual))

data_2015 = plyr::rename(data_2015, c("Economy..GDP.per.Capita."="GDP", "Trust..Government.Corruption."="Corruption", "Health..Life.Expectancy." = "Health", "Trust..Government.Corruption." = "Corruption", "Happiness.Rank" = "Rank", "Happiness.Score" = "Happiness", "Family" = "Social"))
data_2016 = plyr::rename(data_2016, c("Economy..GDP.per.Capita."="GDP", "Trust..Government.Corruption."="Corruption", "Health..Life.Expectancy." = "Health", "Trust..Government.Corruption." = "Corruption", "Happiness.Rank" = "Rank", "Happiness.Score" = "Happiness", "Family" = "Social"))
data_2017 = plyr::rename(data_2017, c("Happiness.Rank" = "Rank", "Happiness.Score" = "Happiness", "Economy..GDP.per.Capita." = "GDP", "Health..Life.Expectancy." = "Health", "Trust..Government.Corruption." = "Corruption", "Family" = "Social"))
data_2018 = plyr::rename(data_2018, c("Overall.rank" = "Rank", "Country.or.region" = "Country", "Score" = "Happiness", "GDP.per.capita" = "GDP", "Social.support" = "Social", "Healthy.life.expectancy" = "Health", "Freedom.to.make.life.choices" = "Freedom",  "Perceptions.of.corruption" = "Corruption"))
data_2019 = plyr::rename(data_2019, c("Overall.rank" = "Rank", "Country.or.region" = "Country", "Score" = "Happiness", "GDP.per.capita" = "GDP", "Social.support" = "Social", "Healthy.life.expectancy" = "Health", "Freedom.to.make.life.choices" = "Freedom",  "Perceptions.of.corruption" = "Corruption"))

data_2018[data_2018 == "N/A"] <- ""
data_2018$Corruption <- as.double(data_2018$Corruption)

data_2015$Region = countrycode(sourcevar = data_2015$Country, origin = "country.name",destination = "region")
data_2016$Region = countrycode(sourcevar = data_2016$Country, origin = "country.name",destination = "region")
data_2017$Region = countrycode(sourcevar = data_2017$Country, origin = "country.name",destination = "region")
data_2018$Region = countrycode(sourcevar = data_2018$Country, origin = "country.name",destination = "region")
data_2019$Region = countrycode(sourcevar = data_2019$Country, origin = "country.name",destination = "region")

plot_num(subset(data_2019, select=-c(Rank)))

##world map creation


mapDF <- data.frame (country = countrycode(sourcevar = data_2019$Country, origin = "country.name",destination = "iso3c"), happiness = floor(data_2019$Happiness))
hpMap <- joinCountryData2Map(mapDF, joinCode = "ISO3", nameJoinColumn = "country")
mapCountryData(hpMap, nameColumnToPlot="happiness", catMethod = "categorical", colourPalette = c("red2", "red1", "orange3", "yellow2", "olivedrab3", "olivedrab4" ))

ParetoTopBottom <- length(data_2019$Rank)
ParetoTopBottom <- ParetoTopBottom/100*20
ParetoTopBottom <- as.integer(floor(ParetoTopBottom))
list(head(data_2019, ParetoTopBottom), tail(data_2019, ParetoTopBottom))

Happiest_20_percent <- head(data_2019$Happiness,ParetoTopBottom)
names(Happiest_20_percent) <- head(data_2019$Country, ParetoTopBottom)
pareto.chart(Happiest_20_percent)

compIndex <- c(mean(data_2015$GDP),mean(data_2016$GDP),mean(data_2017$GDP),mean(data_2018$GDP),mean(data_2019$GD),mean(data_2015$Happiness),mean(data_2016$Happiness),mean(data_2017$Happiness),mean(data_2018$Happiness),mean(data_2019$Happiness))
years <- c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
categ<- c("GDP","GDP","GDP","GDP","GDP","Happiness","Happiness","Happiness","Happiness","Happiness")
compDf <- data.frame(compIndex,categ, years) 

comp_gdp_hp<-ggplot(compDf, aes(x=years, y=compIndex, group=categ)) +
  geom_line(aes(color=categ))+
  geom_point(aes(color=categ))
comp_gdp_hp

compIndex2 <- c(mean(data_2015$GDP)-0.5374293,mean(data_2016$GDP)-0.5374293,mean(data_2017$GDP)-0.5374293,mean(data_2018$GDP)-0.5374293,mean(data_2019$GDP)-0.5374293,mean(data_2015$Happiness)-5,mean(data_2016$Happiness)-5,mean(data_2017$Happiness)-5,mean(data_2018$Happiness)-5,mean(data_2019$Happiness)-5)
years2 <- c(2015,2016,2017,2018,2019,2015,2016,2017,2018,2019)
categ2<- c("GDP","GDP","GDP","GDP","GDP","Happiness","Happiness","Happiness","Happiness","Happiness")
compDf2 <- data.frame(compIndex2,categ2, years2)
comp_gdp_hp2<-ggplot(compDf2, aes(x=years2, y=compIndex2, group=categ2)) +
  geom_line(aes(color=categ2))+
  geom_point(aes(color=categ2))
ggplotly(comp_gdp_hp2)

plot(x = data_2019$GDP, y = data_2019$Happiness, col = 6)

fig1 <- ggplot(data_2019, aes(y=Happiness, x=GDP, color = Region) ) + geom_point()
ggplotly(fig1)

regionByHappiness <- setNames(aggregate(data_2019$Happiness, list(data_2019$Region), FUN=mean, ) , c("Region", "Happiness"))
regionByHappiness$GDP<-aggregate(data_2019$GDP, list(data_2019$Region), FUN=mean)$x
regionByHappiness

Region_HP_GDP <- plot_ly(regionByHappiness, x = regionByHappiness$Region, y = regionByHappiness$Happiness, type = 'bar', name = 'Happiness')
Region_HP_GDP <- Region_HP_GDP %>% add_trace(y = regionByHappiness$GDP, name = 'GDP') %>% layout(yaxis = list(title = 'Score'), barmode = 'group')
Region_HP_GDP

scatterplot3d(data_2019$Social, data_2019$Health, data_2019$Happiness, type="h", pch = data_2019$Corruption*10+15, color=data_2019$Happiness,
              main="Generosity/Trust in Government",
              xlab = "Social",
              ylab = "Health",
              zlab = "Happiness")

values<- c(mean(data_2019$GDP),mean(data_2019$Social), mean(data_2019$Health), mean(data_2019$Freedom), mean(data_2019$Generosity), mean(data_2019$Corruption))
labels <- c("GDP", "Social", "Health", "Freedom", "Generosity", "Corruption")
hpShare <- data.frame(values, labels)
plot_ly(data=hpShare,labels=~labels, values=~values, type="pie")

lvl2015 <- data.frame(country = data_2015$Country, d2015 = data_2015$Happiness)
lvl2019 <- data.frame(country = data_2019$Country, d2019 = data_2019$Happiness)
increase_2015_2019 <- merge(lvl2015, lvl2019)
increase_2015_2019

comp <- ggplot(increase_2015_2019 ) + 
  geom_point(aes(d2015, country, color = "2015")) + 
  geom_point(aes(d2019, country, color = "2019")) +
  labs(x = "happiness")
ggplotly(comp)

prcnt_2015_2019 <- data.frame(country = increase_2015_2019$country, percentage = (((increase_2015_2019$d2019/increase_2015_2019$d2015)-1)*100))
prcnt_2015_2019 <- prcnt_2015_2019[order(prcnt_2015_2019$percentage,decreasing = TRUE),]
prcnt_2015_2019

prcnt_2015_2019top <-data.frame(country = prcnt_2015_2019[1:30,1, drop=FALSE], percentage = prcnt_2015_2019[1:30,2, drop=FALSE])
 
top30perc <- ggplot(prcnt_2015_2019top ) + 
  geom_point(aes(percentage, reorder(country, percentage))) + 
  labs(x = "Increase")
ggplotly(top30perc)

prcnt_2015_2019bottom <-data.frame(country = tail(prcnt_2015_2019$country,30), percentage = tail(prcnt_2015_2019$percentage,30))
bottom30perc <- ggplot(prcnt_2015_2019bottom ) + 
  geom_point(aes(percentage, reorder(country, -percentage))) + 
  labs(x = "Decrease")
ggplotly(bottom30perc)

GDP2015 <- data.frame(country = data_2015$Country, d2015 = data_2015$GDP)
GDP2019 <- data.frame(country = data_2019$Country, d2019 = data_2019$GDP)
GDP_2015_2019 <- merge(lvl2015, lvl2019)
GDPprcnt_2015_2019 <- data.frame(country = increase_2015_2019$country, hasRaised = 0<(((increase_2015_2019$d2019/increase_2015_2019$d2015)-1)*100))
modelDF <- merge (prcnt_2015_2019, GDPprcnt_2015_2019, by="country")
modelDF$hasRaised <- as.integer(modelDF$hasRaised)
modelDF

mod <- glm(data = modelDF, hasRaised ∼ percentage, family = binomial)
summary(mod)

ggplot(modelDF, aes(x=percentage, y=hasRaised)) + geom_point() +
      stat_smooth(method="glm", color="green", se=FALSE,
                method.args = list(family=binomial))

ggplot(data_2019, aes(x = GDP, y = Happiness)) +
  geom_point() +
  stat_smooth()

cor(data_2019$Happiness, data_2019$GDP)

model <- lm(Happiness ~ GDP, data = data_2019)
summary(model)

ggplot(data_2019, aes(GDP, Happiness)) +
  geom_point() +
  stat_smooth(method = lm)
