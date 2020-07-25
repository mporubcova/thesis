#tools ->intall packages-> load with library function
library(openintro)
library(plyr)
library(tibble)
library(dplyr)
library(ggplot2)

#Load data
data("trees")
data("email50")
data("mammals")
data("bdims")
data("hsb2") #students data
#Data structure
str(mammals)
glimpse(email50) #tibble package

#create subsets of table
table(email50$number)

####SQL#############
email50_big <- email50 %>%
#Group_by - code follows in these groups
    group_by(to_multiple) %>%
#Mutate - adds new variable to chosen dataset  
#    mutate(nameofnewvariable = calculationfromcurrentvariables)
#Filter - shows results only for one stance of variable
    filter(number == "big")

#Create assesment matrix with "count"
  count(sent_email,spam)

#print+assign in one step ()
(avg_height<-mean(trees$Height))

#turning numerical variable into categorical - calculate median number of
#characters then assign separate name to below and above median values
(med_num_char <- median(email50$num_char))
email50_fortified <- email50 %>%
  mutate(num_char_cat = ifelse(num_char<med_num_char,"below median", "at or above median"))





#CORRELATION AND REGRESSION IN R (DATACAMP)
#ggplot2 - data, aes(variables) + type of graph
ggplot(data=hsb2, aes(x=science, y=math))+geom_point()
#Easy to add new variables - control performance for program
#if control variable cathegorical use color = factor(nameofvariable)
ggplot(data=hsb2, aes(x=science, y=math, color = prog))+geom_point()
#Discretize scatterplot into boxplot (discretize axis x)
ggplot(data=hsb2,aes(x=cut()))
#Change axis to log_axis with coord_trans
#Alpha makes points transparent to see density
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point(alpha=0.5) + 
  coord_trans(x = "log10", y = "log10")
# Scatterplot with scale_x_log10() and scale_y_log10()
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10()

# FILTER the OUTLIER data
ab_gt_200 %>%
  filter(AB>=200, OBP<0.2)

# Compute CORRELATION
ncbirths %>%
  summarize(N = n(), r = cor(weight, mage))
# Compute correlation for all non-missing pairs, CORRECTION FOR NA
ncbirths %>%
  summarize(N = n(), r = cor(weight, weeks, use = "pairwise.complete.obs"))
#SPURRIOUS correlation
#every time 2 variables are connected over TIME there is a chance for spurious correlation

#SIMPLE REGRESSION
# Linear model for weight as a function of height
lm(wgt ~ hgt, data = bdims)
# Log-linear model for body weight as a function of brain weight
lm(log(BodyWt) ~ log(BrainWt), data = mammals)
# Scatterplot with regression line - geom_smooth
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point ()+ 
  geom_smooth(method = "lm", se = FALSE)
# Add slope and intercept, mutate adds new factors to table
bdims_summary %>%
  mutate(slope = r * sd_wgt / sd_hgt, 
         intercept = mean_wgt - slope * mean_hgt)






#operators
#equals 
