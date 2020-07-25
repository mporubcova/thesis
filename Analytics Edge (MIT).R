


# Search functions in data
which.max(FluTrain$Queries) #returns row number with max value of queries
which(FluTest$Week == "2012-03-11 - 2012-03-17") # returns row number of specified week in dataset
PredTest1[11]

#VECTORS 
#do not combine diverse classes in one vector
c(2,3,5,8,24) #combine
Country = c("Brazil", "China", "India", "Switzerland","USA")
Country [1]
LifeExpectancy = c(74,75,76,77,78)
LifeExpectancy[1]

seq(0,100,2) #beginning, end, step

#DATA FRAMES and modification
CountryData = data.frame(Country, LifeExpectancy) #connect 2 vectors
CountryData$Population = c(199,139,124,0.799,318) #add new variable to data.frame with $
Country = c("Australia", "Greece") #re-write current content of variable
LifeExpectancy = c(82,83) #re-write current content of variable
Population= c(230, 220) #re-write current content of variable
NewCountryData = data.frame(Country, LifeExpectancy, Population)

#Combine data.frames
AllCountryData = rbind(CountryData, NewCountryData) #adding new observations to data.frame
AllCountryData

#Subsets & Outliers
WHO = read.csv("WHO.csv")
WHO_Europe = subset(WHO, Region == "Europe") #creates 
WHO_Europe = subset(WHO, Year >= 2006) 
WHO$Under15 #access variable
mean(WHO$Under15)
sd(WHO$Under15)
  #What is the sta
which.min(WHO$Under15) #returns row of observation with
which.max(WHO$Under15)
WHO$Country[124] #verify which country

#Plots 
plot(WHO$GNI,WHO$FertilityRate) #plot(x,y)
Outliers = subset(WHO,GNI > 10000 & FertilityRate >2.5) #show me who are outliers in the plot based on visual
nrow(Outliers) #number of rows (=outlying observations)
Outliers [c("Country","GNI", "FertilityRate")] #show only select variables of data.frame

hist(WHO$CellularSubscribers) #plot one variable
boxplot(WHO$LifeExpectancy ~ WHO$Region) #shows box of 1st-3rd quantile spead & outliers as dots (if distance from 1st quartile is larger than 3rd minus 1st quartile)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy" )

#TASK 2 - NATIONAL NUTRITION DATA ##########
USDA <-read.csv("USDA.csv")

#Find the observation matching concrete case of variable
USDA$Sodium [match("CAVIAR", USDA$Description)] #prints the value of observation belongin to index in []

#Plots
plot(USDA$Protein, USDA$TotalFat, xlab = "Protein", ylab = "Fat", main = "Protein vs. Fat", col = "red")
#Adjusting scale - xlim = c(min,max) adjusts x axis span, breaks = number of columns the data should break into
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Vitamin C levels", xlim = c(0,100), breaks=2000) 
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels", ylab ="Sugar (g)") 

#Add new variable that shows 1 if sodium < mean, 0 otherwise
USDA$HighSodium = as.numeric (USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))
USDA$HighProtein = as.numeric (USDA$Protein > mean(USDA$Protein, na.rm = TRUE))
USDA$HighTotalFat = as.numeric (USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbohydrate = as.numeric (USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))

#Which foods have value 1 in high Fat? (1x2)
table(USDA$HighSodium) #show me count of all categories of variable HighSodium
table(USDA$HighSodium == "") #if looking for number of time a categorical value appears

#Which foods have value 1 in both high Fat and Sodium? (2x2)
table(USDA$HighSodium, USDA$HighTotalFat) 

#What is the AVERAGE value of IRON in HIGH and LOW PROTEIN subgroups?
  #tapply (arg1, arg2, arg3) - group argumnet 1 by argument 2 and apply argument 3
  #feed iron data in filter based on HighProtein (0/1) and print average Iron in each of 2 subgroups (1/0)
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)
  #Exercise 2: MAX level of vitamin C in HIGH and LOW CARB foods
tapply(USDA$VitaminC, USDA$HighCarbohydrate, max, na.rm = TRUE)
tapply(USDA$VitaminC, USDA$HighCarbohydrate, summary, na.rm = TRUE)

# Test: FBI
mvt = read.csv("mvtWeek1.csv")
table(mvt$LocationDescription == "ALLEY")
mvt$Date
#DATE - R does not automatically recognize entries that look like dates
    # as.Date - change to Date form of data
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M")) 
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

hist(mvt$Date, breaks = 1000)
boxplot(mvt$Date ~ mvt$Arrest)
prop.table(table(mvt$Arrest, mvt$Year)) # % instead of count in table
sort(table(mvt$LocationDescription)) #sort table on values if many categories
Top5 = subset(mvt,LocationDescription == "STREET"|LocationDescription ==  "PARKING LOT/GARAGE(NON.RESID.)"| LocationDescription == "ALLEY"|LocationDescription == "GAS STATION"|LocationDescription == "DRIVEWAY - RESIDENTIAL")

Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$Arrest,Top5$LocationDescription)
table(Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL", Top5$Weekday)

######### 2. REGRESSION - PREDICTING QUALITY OF WINE ######################################
wine = read.csv("wine.csv")
str(wine)
summary(wine)

# Regression model (1-factor regression)
model1 = lm(Price ~ AGST, data=wine) #lm(y ~ x1 + x2 + xn, data=nameofdata)
summary(model1)
#summary - Estimate (Interncept, )
#R-squared - multiple - always increases with additional ind.var., adjusted - adjusts for number of variables and may decrease if a useless variable is added
model1$residuals
SSE = sum (model1$residuals^2)

# ADD EXPLANATORY VARIABLE (2-factor regression)
model2 = lm (Price ~ AGST + HarvestRain, data = wine)
summary(model2)
SSE = sum(model2$residuals^2)

# ADD EXPLANATORY VARIABLE (5-factor regression)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain +Age + FrancePop, data=wine)
summary(model3)
SSE = sum(model3$residuals^2)
SSE
  # FrancePop and Age show no significance (no * or .), lets remove FrancePop

# REMOVE EXPLANATORY VARIABLE (4-factor regression), removing non significant variable
model4 = lm (Price ~ AGST + HarvestRain + WinterRain + Age , data = wine )
summary(model4)
  # Age suddenly becomes significant - caused by MULTICOLINEARITY between Age and FrancePop

# CORRELATION between INDEPENDENT variables
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
  # Age and FrancePop are highly correlated -0.99
  # Correlation between independent and dependent variable is normal as we are trying to explain one with another
cor(wine) #correlation matrix between all the variables in the dataset


# Automated model building - find the BEST choice of VARIABLES (AKAIKE INFORMATION CRITERIA - AIC)
  # step(model with all the variables)   
  # AIC penalizes model for number of variables
model_AIC = step(model3)
summary(model_AIC)

# PREDICTION with trained model - best adj.R^2 for model4
  # training data = on which we build model, test data = new data on which we test the quality of model
  # overfitting - high R^2  but poor performance on unknown data
wineTest = read.csv("wine_test.csv")
str(wineTest)

predictTest = predict(model4,newdata = wineTest)
predictTest

# Test the accuracy of predictive model on TEST DATA 
SSE = sum((wineTest$Price - predictTest)^2)
SSE
SST = sum((wineTest$Price - mean(wine$Price))^2)
SST
1-(SSE/SST)
#RMSE
RMSE = sqrt(SSE/(N-1))
  #R-squared od 0.79 is a fairly good performance on out-of-sample data BUT the data sample was very small - would need to train on larger set


######### 2. MULTIPLE REGRESSION - predicting pisa reading score  ######################################
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

str(pisaTrain)
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# Remove missing value from data set 
  # na.omit(df)
  # linear regression anyways discards observations with missing data
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)

# Include UNORDERED FACTORS in regression
  # define one level as "reference"(the most common value ideally)
  # add BINARY variable for each of other levels = total (n-1) binary variables
  # e.g. data (red, blue, green) - reference = "green"
      # green (colorred = 0, colorblue = 0)
      # blue (colorred = 0, colorblue = 1)
      # red (colorred = 1, colorblue = 0)
  # you can now add these new BINARY VARIABLES into your model using relevel

  # Reference - R automatically sets first in alphabet, we want the most common
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# Multiregression model (for very high number of independent variables use simply . instead of naming them)
ImScore = lm(readingScore ~., data = pisaTrain)
ImScore
summary(ImScore)
ImScore$residuals

# SSE & RMSE training data
SSE = sum(ImScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain)) #RMSE adjusts for number of observations
RMSE

# Prediction model
predTest = predict(ImScore, newdata = pisaTest)
predTest

# SSE & RMSE & SST & R out-of-sample
max(predTest)-min(predTest)
SSE = sum((pisaTest$readingScore-predTest)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore-mean(pisaTrain$readingScore))^2)
SST

R = 1 - (SSE/SST)
R

######### 2. TIME SERIES ARIMA WITH 1 LAGGED TERM - Detecting Flu Epidemics  ######################################

FluTrain = read.csv("FluTrain.csv")
str(FluTrain)

# ILI (% of queries at doctor regarding influenza) - DEPENDENT
# ILILag2 (ILI lagged by 2 weeks serving as a new independent variable)
# Queries (% of google search related to influenza) 

# Find max
which.max(FluTrain$Queries) #returns row number with max value of queries
FluTrain$Week[303] #finds week in row 303

# Check the distribution of variable
  # dependent variable ILI is RIGHT SKEWED (has many very small values)
  # this could cause large influence on SSE as there would be many high differences
  # use LOGARITHM of DEP.VAR.
hist(FluTrain$ILI)
hist(log(FluTrain$ILI))
plot(log(FluTrain$ILI), FluTrain$Queries)
  # There seems to be a POSITIVE LINEAR relation beween ILI and Queries
  # We can use LINEAR MODEL

# Regression model of ILI based on Queries
FluTrend1 = lm(log(ILI)~Queries, data = FluTrain)
summary(FluTrend1)

# Test data & verify SSE, RMSE
FluTest = read.csv("FluTest.csv")

PredTest1 = exp(predict(FluTrend1, newdata = FluTest))

which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

(FluTest$ILI[11]-PredTest1[11])/FluTest$ILI[11]

SSE = sum((FluTest$ILI-PredTest1)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

# Train Time Series Model
  # Predicting current value of dependent var. (ILI) with PAST (LAGGED) value of INDEP.VAR.(Queries)
  # lag = 2 weeks (ILI values are reported with a 1-2 week lag)
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI),-2,na.pad=TRUE) #lag the df by 2 weeks BACK (-2), na.pad=TRUE fills in 2 values that are missing in new df as a reult of lag
FluTrain$ILILag2 = coredata(ILILag2)

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

# Add lagged variable also to test df
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

# FILL IN the missing VALUES for ILILag2 in FluTest (missing as a result of lag)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
str(FluTest)

#Verify the model quality with RMSE
PredTest2 = exp(predict(FluTrend2, newdata = FluTest))

SSE = sum((FluTest$ILI-PredTest2)^2)
SSE
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
  # RMSE = 0.2942
  # FluTrend2 is a better prediction model than FluTrend 1
  # FluTrend2 includes a lagged (-2) dependent variable ILI

################ USA 1970s DATA - Predicting life expectancy (revision of the above only) ###############
 
#data from the 1970s on all fifty US states. For each state, the dataset includes the population, per capita income, illiteracy rate, murder rate, high school graduation rate, average number of frost days, area, latitude and longitude, division the state belongs to,  region the state belongs to, and two-letter abbreviation.
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)
plot(statedata$x, statedata$y)

# Which region of the US (West, North Central, South, or Northeast) has the highest average high school graduation rate
tapply(statedata$HS.Grad, statedata$state.region, mean)
boxplot(statedata$Murder ~ statedata$state.region)

# Build a model to predict LIFE EXPECTANCY by state using the state statistics we have in our dataset.
  # 7 independent variables
PredLife1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata )
  # non-significant - Illiteracy, Area
summary(PredLife1) 
  # income seems negatively related to life expectancy
plot(statedata$Income, statedata$Life.Exp)
  # but plotting these variables shows positive linear relationship - MULTICOLLINEARITY
  # Akaike criteria to find most efficient model, alternatively take insignificant varies out one by one
PredLife_AIC = step(PredLife1)
summary(PredLife_AIC)
  # non-significant - Illiteracy, Area, adj. R-sq = 0.7126 


