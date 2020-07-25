

################# LOGISTIC REGRESSION - Predicting quality of medical care ##############################
  # extention of linear regression BUT:
      # linear for continuous otcomes
      # LOGISTIC for categorical outcomes
      # here BINARY (poor quality = 1, good quality = 0) -> y = 1, this is on purpose as:
      # the higher the prob. of bad care the more the function will head towards 1, otherwise it will collidate to 0 = good care
      # Logistics response function (non-lin transformation of lin. reg. to produce number between 0 and 1)
      # positiive Beta value in logistic function increases probability to 1 (poor care)

quality = read.csv("quality.csv")
str(quality)

# How many of 131 patients received poor care?
table(quality$PoorCare)
  # poor 33/131, good:
98/131
  # BASELINE solution for logistic regression is the most common level 
    # baseline = good care, accuracy "R squared" 0.77480


library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare,SplitRatio = 0.75)
split

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
                     
nrow(qualityTrain)
nrow(qualityTest)

# Logistic model creation 
  # glm(y~x, family = binomial, data=df) 
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, family = binomial, data = qualityTrain, )
summary(QualityLog)

# AIC - minimize, R adjusted for number of variables comapred to observations



