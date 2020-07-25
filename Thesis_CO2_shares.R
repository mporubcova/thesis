library(knitr) # code structure 
library(plyr) # load before dplyr otherwise buggy functions
library(dplyr)
library(tidyselect)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggridges) #multiple non-overlaying distributions in one plot
library(wesanderson) #color palette
library(psych)
library(PerformanceAnalytics)
library(DescTools)
library(quantmod) # LS pf
library(reshape2) # LS pf
if(!require(tbl2xts)) install.packages("tbl2xts") # LS pf
if(!require(rmsfuns)) install.packages("rmsfuns") # LS pf
library(stargazer) #LaTeX tables - https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf
 
setwd("~/Desktop/Future/Thesis/Mutual_Funds_Data")

##########################################################################################################################

##### H.1. The time-varying carbon intensity decreases on average ######
##### H.1. The time-varying return on low-carbon stocks increases on average ######
##### Robustness - market market  sensitivity, investment style, or industry-specific factors.

### LOADING RAW DATA ###########################################################

# 1 # Stock data  - load share-level data ##

```{r}
data_stock_monthly_load = read.csv("CRSP_stock_monthly_Jan-2005-Dec-2019.csv") 

data_stock_monthly = data_stock_monthly_load %>%
  mutate (mktcap = PRC * SHROUT) %>%
  separate(date, into = c('date2', 'day'), sep = -2, convert = TRUE) %>%
  separate(date2, into = c('year', 'month'), sep = -2, convert = TRUE) %>%
  select(-day)
```

# Generate a list of unique cusips to download carbon data #

CUSIP_list_dirty = data_stock_monthly$CUSIP
CUSIP_list_clean = unique(CUSIP_list_dirty) 
rm(CUSIP_list_dirty)
write.csv(CUSIP_list_clean, "~/Desktop/Future/Thesis/Mutual_Funds_Data/CUSIP_list_CO2.txt" ) 
CUSIP_list_clean #13,672 Levels

# Load names to match NAICS codes

NAICS_legend = read.csv("NAICS_legend.csv", colClasses=c("numeric", "character"))

data_sector = data_stock_monthly %>%
  select(NAICS, CUSIP) %>%
  separate(NAICS, into = c('sector1', 'rest'), sep = - 4, convert = TRUE) %>%
  mutate(sector = as.numeric(as.character(sector1))) %>%
  left_join(NAICS_legend, by = c('sector')) %>%
  distinct(CUSIP, .keep_all = TRUE) %>%
  select(-rest, -sector1) %>%
  rename(sector_name = name, sector_number = sector) %>%
  drop_na(sector_name)

# 2 # Carbon data - company level ##

data_TR_CO2_load = read.csv("Eikon_CO2_data_2007-2019.csv") #155k observations 
x = str_replace_all(data_TR_CO2_load$date, "[[:punct:]]", "")
data_TR_CO2_dirty = cbind(data_TR_CO2_load,x)

summary(as.numeric(as.character(data_TR_CO2$Revenue.tot)))

data_TR_CO2 = data_TR_CO2_dirty %>%
  select (-date) %>%
  rename (date = x, CUSIP = cusip) %>%
  select(date, everything()) %>%
  separate(date, into = c('date', 'day'), sep = -2, convert = TRUE) %>%
  mutate(date2 = date) %>%
  separate(date2, into = c('year', 'month'), sep = -2, convert = TRUE) %>%
  filter (as.numeric(as.character(Revenue.tot)) > "0" & year > "2006") %>% 
  select(-day,-month) %>%
  select(year, everything()) %>%
  mutate (CO2.MilRev = as.numeric(as.character(CO2.emiss.tot.estim)) / as.numeric(as.character(Revenue.tot)) * 1000000) %>%
  mutate (CO2.MilRev = na_if(CO2.MilRev, Inf)) %>%
  drop_na(CO2.MilRev)
  # 22,238 annual observations of 13,672 CUSIPs

which(data_TR_CO2$CO2.MilRev == 0, arr.ind=TRUE)
data_TR_CO2 ["8034", "CO2.MilRev"] = 0.105
data_TR_CO2 ["8035", "CO2.MilRev"] = 0.105

n_CO2 = unique(data_TR_CO2$CUSIP)
rm(data_TR_CO2_dirty,x)

# 3 # Fama and French return factors ##

data_FamaFrench_monthly_load = read.csv("CRSP_F&F_Jan-2005_Dec-2019.csv")
data_FamaFrench_monthly = data_FamaFrench_monthly_load %>%
  select(mktrf, everything()) %>%
  select(rf, everything()) %>%
  select(dateff, everything()) %>%
  ungroup()

# 4 # Other corporate level data ##

# Book-to-market ratio at PERMNO level
data_bm_load = read.csv("Compustat_bm_Jan-2007-Dec-2019.csv") #ADATE and QDATE refers to the year-end date and quarter-end date for which the variables were used for calculating the related financial ratios. The Public_Date is roughly when this information becomes publicly available. The current treatment is to set it to be two months after the ADATE or QDATE.
data_bm = data_bm_load %>%
  rename (PERMNO = permno, date = public_date)

##########################################################################################################################

## DATA CLEANING AND MATCHING ##

# Match CRSP stock data with Eikon carbon data

data_stock_CO2 = data_stock_monthly %>%
  inner_join(data_TR_CO2, by = c('year','CUSIP')) %>%
  group_by(year, month) %>%
  arrange(year, month, CO2.MilRev) %>% 
  drop_na(CO2.MilRev) %>%
  ungroup() 

n_stock_CO2 = unique(data_stock_CO2$CUSIP)
# 247k observations of 13,672 CUSIPs

# Match industry names with respecitve NAICS code using data_sector

data_stock_CO2_sector = data_stock_CO2 %>%
  inner_join(data_sector, by = c('CUSIP')) %>%
  mutate(year = as.factor(year), sector_name = as.factor(sector_name))

# Visualize to indentify outliers 

data_carbon = data_stock_CO2_sector %>%
  select(CUSIP, COMNAM, year, sector_name, CO2.MilRev) %>%
  arrange(CO2.MilRev) %>%
  distinct(year,CUSIP, .keep_all = TRUE) 

summary(data_carbon$CO2.MilRev)

ggplot(data_carbon, aes(x = CO2.MilRev, y = sector_name)) +
  geom_boxplot()

# Winsorization - treating outliers by changing extreme values into less extreme

data_CO2_winsor_1perc = Winsorize(data_stock_CO2_sector$CO2.MilRev, minval = NULL, maxval = NULL, probs = c(0.01, 0.99), na.rm = FALSE, type = 7)
summary(data_CO2_winsor_1perc)
data_CO2_winsor_2perc = Winsorize(data_stock_CO2_sector$CO2.MilRev, minval = NULL, maxval = NULL, probs = c(0.02, 0.98), na.rm = FALSE, type = 7)
summary(data_CO2_winsor_2perc)

# Add winsorized CO2 variable to main dataset

data_stock_CO2_sector_wins = data_stock_CO2_sector %>%
  mutate(CO2.MilRev_winsor_1perc = data_CO2_winsor_1perc, CO2.MilRev_winsor_2perc = data_CO2_winsor_2perc) 

# Plot winsorized variables 

ggplot(data_stock_CO2_sector_wins, aes(x = CO2.MilRev_winsor_1perc, y = sector_name)) +
  geom_boxplot()

ggplot(data_stock_CO2_sector_wins, aes(x = CO2.MilRev_winsor_2perc, y = sector_name)) +
  geom_boxplot()

ggplot(data_stock_CO2_sector, aes(x = CO2.MilRev, y = year)) +
  geom_density_ridges(aes(fill = year)) 

ggplot(data_stock_CO2_sector, aes(y = CO2.MilRev, x = seq(length(CO2.MilRev)) )) +
  theme_bw() + 
  facet_wrap(~sector_name) +
  geom_point() 

ggplot(data_stock_CO2_sector_wins, aes(x=CO2.MilRev_winsor_2perc, fill= year)) +
  scale_color_brewer(palette="Paired") +
  theme_linedraw() +
  facet_wrap(~year) + # multiple plots
  geom_density(col = NA, alpha = 0.35) +
  labs(x = "CO2 per revenue of 1 million", y = "") 

# Verify any selection bias - comparison of dataset with F&F data 

# Decriptive characteristics of main variable (CO2.MilRev)

# Get LaTeX table code

stargazer(attitude)

# Create portfolios 

data_pf = data_stock_CO2_sector_wins %>%
  select(year, month, CUSIP, COMNAM, sector_name, CO2.MilRev, CO2.MilRev_winsor_1perc, CO2.MilRev_winsor_2perc, RET) %>%
  arrange(year, sector_name, CO2.MilRev_winsor_1perc)

# Sufficient portfolio diversification - min of 50 stocks per year per industry sector in order to be satisfied

stocks <- c('AEIS', 'ABC', 'AMGN', 'BBY', 'HRB', 'BKE', 'GIB')
getSymbols(stocks, from = "2016-01-01")
prices.data <- do.call(merge, lapply(stocks, function(x) Cl(get(x))))
returns <- setNames(do.call(cbind, lapply(prices.data, monthlyReturn)), stocks)

FactorInfo <- 
  tibble(assets = stocks, FactorScore = rnorm(n = length(stocks)))

data <- 
  returns %>% xts_tbl() %>% 
  gather(assets, returns, -date) %>%
  left_join(., FactorInfo, by = "assets") %>% 
  mutate(RankPctile = percent_rank(FactorScore))

LongStocks <- data %>% select(assets, FactorScore) %>% unique() %>% arrange(FactorScore) %>% head(2) # pick two worst to short, as example
ShortStocks <- data %>% select(assets, FactorScore) %>% unique() %>% arrange(FactorScore) %>% tail(2) # pick two best to long, as example



pf_larger50 = data_pf %>%
  select(year, CUSIP, sector_name) %>%
  distinct(year, CUSIP, .keep_all = TRUE) %>%
  select(-CUSIP) %>%
  group_by(year, sector_name) %>%
  tally() %>%
  filter(n > 50) %>%
  left_join(data_pf, by = c('year', 'sector_name')) %>%
  distinct(year, CUSIP, .keep_all = TRUE) %>%
  arrange(year, sector_name, CO2.MilRev_winsor_2perc) %>%
  group_by(year, sector_name) %>%
  mutate(FactorScore = rnorm(n = length(CO2.MilRev_winsor_2perc))) %>%
  mutate(RankPctile = percent_rank(FactorScore))

  mutate(FactorScore = seq(length(CO2.MilRev))) %>%
  
LongStocks <- data_pf %>% 
  select(CUSIP, FactorScore) %>%
  unique() %>% 
  arrange(FactorScore) %>%
  head(2) # pick two worst to short, as example

ShortStocks <- data_pf %>%
  select(CUSIP, FactorScore) %>%
  unique() %>% 
  arrange(FactorScore) %>% 
  tail(2) # pick two best to long, as example


data_group_CO2 = data_input %>%
  select(year, month, CUSIP, CO2.MilRev, mktcap, RET) %>%
  group_by(year, month) %>%
  arrange(year,month,CO2.MilRev) %>%
  mutate(csum = cumsum(mktcap))

## Regress portfolio returns on F&F factors' returns ##




#################################################
# portfolio construction

stocks <- c('AEIS', 'ABC', 'AMGN', 'BBY', 'HRB', 'BKE', 'GIB') # CPLA doesn't fetch
getSymbols(stocks, from = "2016-01-01")

prices.data <- do.call(merge, lapply(stocks, function(x) Cl(get(x))))
returns <- setNames(do.call(cbind, lapply(prices.data, monthlyReturn)), stocks)

FactorInfo <- 
  tibble(assets = stocks, FactorScore = rnorm(n = length(stocks)))

data <- 
  returns %>% xts_tbl() %>% 
  gather(assets, returns, -date) %>%
  left_join(., FactorInfo, by = "assets") %>% 
  mutate(RankPctile = percent_rank(FactorScore))

LongStocks <- data %>% select(assets, FactorScore) %>% unique() %>% arrange(FactorScore) %>% head(2) # pick two worst to short, as example
ShortStocks <- data %>% select(assets, FactorScore) %>% unique() %>% arrange(FactorScore) %>% tail(2) # pick two best to long, as example

Long_Port <- data %>% filter(assets %in% unique(LongStocks$assets)) %>%     tbl_xts(cols_to_xts = "returns", spread_by = "assets")
Short_Port <- data %>% filter(assets %in% unique(ShortStocks$assets)) %>% tbl_xts(cols_to_xts = "returns", spread_by = "assets")

# Fully funded long-short position
W_Long <- data %>% filter(assets %in% unique(LongStocks$assets)) %>% filter(date == first(date)) %>% mutate(Weight = 1 / n()) %>% tbl_xts(cols_to_xts = "Weight", spread_by = "assets")
W_Short <- data %>% filter(assets %in% unique(ShortStocks$assets)) %>% filter(date == first(date)) %>% mutate(Weight = -1 / n()) %>% tbl_xts(cols_to_xts = "Weight", spread_by = "assets")


#################################################



# 2 # Monthly return = load share-level data ##
# variation in portfolio size every month 
# number of portfolio set such that the in-portfolio variation is sufficient 

# value weighted vs xx-weigted 
# To minimize the look-ahead bias, we form portfolios using the previous year’s carbon efficiency data 



## Bias - is the sample representative of the market? Is it diversified enough? ##  


## Check for the effect of INDUSTRIES (NAICS) ##

# pmg(return ~ log(ME) + log(BM) + X1 + X2, data=YourData, index=c("Year","StockID"))




