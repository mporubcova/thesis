library(plyr) # load ALWAYS before dplyr otherwise buggy functions
library(dplyr)
library(tidyselect)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

setwd("~/Desktop/Future/Thesis/Mutual_Funds_Data")

##########################################################################################################################

## MFs' QUARTERLY HOLDINGS DATA 2008-2019 ##

# 50M observations, fdate, cusip, fundno, shares (in units)

data_TR_MF_holdings_load = read.csv("TR_MF_holdings_Jan-2008-Dec-2019.csv")

data_TR_MF_holdings = data_TR_MF_holdings_load %>%
  rename(date = fdate) %>%
  separate(date, into = c('date', 'day'), sep = -2, convert = TRUE) %>%
  select(-day) 

# Get a list of unique CUSIPs held between 2008-2019 

CUSIP_list_dirty = data_TR_MF_holdings$cusip
CUSIP_list_clean = unique(CUSIP_list_dirty) 
rm(CUSIP_list_dirty)
write.csv(CUSIP_list_clean, "~/Desktop/Future/Thesis/Mutual_Funds_Data/CUSIP_list.txt" ) 
CUSIP_list_clean #38940 Levels

##########################################################################################################################

# 1 # Load fund-level data (NAV, country of incorporation) ##

data_country_NAV_load = read.csv("TR_MF_country_NAV_Dec-2008-Dec-2019.csv") 

data_country_NAV = data_country_NAV_load %>% #190k of 900k NAV is NA
  rename(date = fdate, NAV_x10000 = assets) %>%
  separate(date, into = c('date', 'day'), sep = -2, convert = TRUE) %>%
  select(-rdate,-ioc, -day) 

# 2 # Load share-level data (price, shares outstanding) ##

data_price_shrout_monthly = read.csv("CRSP_price_shrout.csv") #900k monthly observations, prc(price as of time t in USD), shrout (shares outstanding in '000)

data_price_shrout_quarterly = data_price_shrout_monthly  %>% #320k quarterly observations
  separate(date, into = c('date', 'day'), sep = -2, convert = TRUE) %>%
  mutate(date2 = date) %>%
  separate(date2, into = c('year', 'month'), sep = -2, convert = TRUE) %>%
  filter(month == 3 | month == 6 | month == 9 | month == 12 ) %>% #filter for quarterly data
  rename (cusip = CUSIP, prc = PRC, shrout_x1000 = SHROUT) %>%
  select(-PERMNO, -day, -month)
  # Is this a legit way?

# 3 # Load share-level data (Carbon intensity) ##

data_TR_CO2_load = read.csv("TR_Eikon_CO2_Dec-2009-Dec-2019.csv") #190k observations, annual data on carbon intensity
x = str_replace_all(data_TR_CO2_load$date, "[[:punct:]]", "")
data_TR_CO2_dirty = cbind(data_TR_CO2_load,x)

data_TR_CO2 = data_TR_CO2_dirty %>%
  select (-date) %>%
  rename (date = x) %>%
  select(date, everything()) %>%
  separate(date, into = c('date', 'day'), sep = -2, convert = TRUE) %>%
  mutate(date2 = date) %>%
  separate(date2, into = c('year', 'month'), sep = -2, convert = TRUE) %>%
  filter (year >"2008") %>% 
  select(-day,-month, -Ratio, -date) %>%
  select(year, everything()) %>%
  mutate (CO2_per_1mRevenue = as.numeric(as.character(CO2.total)) / as.numeric(as.character(Revenue.total.USD)) * 1000000) %>%
  mutate (CO2_per_1mRevenue = na_if(CO2_per_1mRevenue, Inf)) %>%
  drop_na(CO2_per_1mRevenue) 

## Match # 1 + 2 + 3 datasets into master file ##

data_MF_master = data_TR_MF_holdings %>% 
  left_join (data_country_NAV, by = c('date', 'fundno')) %>%
  left_join (data_price_shrout_quarterly, by = c('date', 'cusip')) %>%
  left_join (data_TR_CO2, by = c('cusip', 'year'))

## Restrict to US funds only ##

data_MF_US_master = data_MF_master %>%
  filter(country== "UNITED STATES") %>%
  select(-country, - year) %>%
  mutate(year = date) %>%
  separate(year, into = c('year', 'month'), sep = -2, convert = TRUE) %>%
  select (-month) %>%
  mutate(shares_owned = shares / shrout_x1000 / 1000) %>%
  mutate(CO2_owned = shares_owned * as.numeric(as.character(CO2_per_1mRevenue))) 

y = colSums(is.na(data_MF_US_master))
print(y)

##########################################################################################################################

## Filtering final time series data - MFs with at least 67% of fund assets 

# Sum/ average annual data at fund-level ##

data_MF_US_fund = data_MF_US_master %>% 
  select(date, year, fundno,NAV_x10000, CO2_owned) %>%
  mutate (NAV_x10000 = na_if(NAV_x10000, "0")) %>%
  group_by(year, fundno) %>%
  summarise(CO2_fund_total = sum(CO2_owned, na.rm = T), NAV_ann_average = mean(NAV_x10000, na.rm = T)) %>%
  mutate(CO2_fund_norm = CO2_fund_total / NAV_ann_average ) %>% # normalize annual CO2 emissions of fund based on fund size
  drop_na(CO2_fund_norm) %>%
  filter(CO2_fund_norm > 0) # 55k funds

# Choose funds with at least 67% of assets covered by CO2 intensity research

# TR MF holdings data are not complete, resulting in discrepacy between TR total fund asset data and weighted sum of shares held and their prices

# Calculate total fund NAV from available data 

data_NAV_calc = data_MF_US_master %>% # 92k observations in final set
  select (date, year, cusip, fundno, prc, shares, CO2_per_1mRevenue) %>%
  mutate (share_capital = prc * shares) %>% # calculate AuM at cusip level
  group_by(date, year, fundno) %>%
  summarise(total_assets = sum(share_capital, na.rm = T)) %>% # sum AuM at cusip level to "calculated NAV"
  mutate (total_assets = na_if(total_assets, "0")) %>%
  group_by(year, fundno) %>%
  summarise(NAV_ann_average_calc = mean(total_assets, na.rm = T))  # average annual NAV from quarterly data

# Calculate portion of NAV that has CO2 intensity rating

data_rated_calc = data_MF_US_master %>%
  select (date, year, cusip, fundno, prc, shares, CO2_per_1mRevenue) %>%
  mutate (share_capital = prc * shares) %>%
  drop_na(CO2_per_1mRevenue) %>%
  filter(CO2_per_1mRevenue > 0) %>% #filter only for cusips with available CO2 rating
  group_by(date, year, fundno) %>%
  summarise(rated_assets = sum(share_capital, na.rm = T)) %>% # sum AuM at cusip level to "calculated NAV"
  mutate (rated_assets= na_if(rated_assets, "0")) %>%
  group_by(year, fundno) %>%
  summarise(rated_ann_average_calc = mean(rated_assets, na.rm = T)) # average annual NAV from quarterly data 

# Get funds with at least 67% of assets with CO2 rating 

fund_list_final = data_MF_US_fund %>% # 36k funds (150 of which portion >1 due to averaging of quarterly data)
  left_join(data_NAV_calc, c('year', 'fundno')) %>%
  left_join(data_rated_calc, c ('year', 'fundno')) %>%
  mutate (rated_portion = rated_ann_average_calc / NAV_ann_average_calc) %>%
  filter (rated_portion > 0.67) # rated at each point of time series 

## Filter funds with a complete 9 year time-series ##

data_fund_names_load = read.csv("TR_MF_fund_names.csv") 
data_fund_names = data_fund_names_load %>%
  select(-rdate)

fund_list = data_MF_US_fund %>% 
  filter(year > 2009 & year < 2019 ) %>%
  group_by(fundno) %>%
  tally() %>%
  filter (n > 8) %>%  #106 funds 
  left_join(data_fund_names, c('fundno')) %>%
  distinct(fundno,fundname)

# Filter funds with 

fund_list_65 = fund_list %>%
  left_join(fund_list_final, c('fundno')) %>%
  arrange(fundno, year)

u = unique(fund_list_65$fundno)  # 374 unique funds

##########################################################################################################################

## Decriptive statistics ## 

ggplot(data=fund_list_65) +
  geom_line(aes(x = year, y = CO2_fund_norm *100 , group=fundno))+
  scale_fill_hue(l=40, c=35) +
  ylim(0, 1) 

fund_list_65 %>%
  filter (CO2_fund_norm > 0.0075)

## Descriptive table ##

data_TR_CO2_load = read.csv("TR_Eikon_CO2_Dec-2009-Dec-2019.csv") #190k observations, annual data on carbon intensity
x = str_replace_all(data_TR_CO2_load$date, "[[:punct:]]", "")
data_TR_CO2_dirty = cbind(data_TR_CO2_load,x)

data_TR_CO2 = data_TR_CO2_dirty %>%
  select (-date) %>%
  rename (date = x) %>%
  select(date, everything()) %>%
  separate(date, into = c('date', 'day'), sep = -2, convert = TRUE) %>%
  mutate(date2 = date) %>%
  separate(date2, into = c('year', 'month'), sep = -2, convert = TRUE) %>%
  filter (year >"2008") %>% 
  select(-day,-month, -Ratio, -date) %>%
  select(year, everything()) %>%
  mutate (CO2_per_1mRevenue = as.numeric(as.character(CO2.total)) / as.numeric(as.character(Revenue.total.USD)) * 1000000) %>%
  mutate (CO2_per_1mRevenue = na_if(CO2_per_1mRevenue, Inf)) %>%
  drop_na(CO2_per_1mRevenue) %>%
  group_by(cusip) %>%
  tally() %>%
  filter (n > 9)


data_stock_returns = read.csv("CRSP_stock_returns_Jan-2008-Dec-2019.csv")
str(data_stock_returns)




##########################################################################################################################

# Filter for equity funds only
# Cut off extreme values

# What if fund ceases to exist/ founded in a course of period? - not included in time series 

# Warning: NAs introduced by coercion 
# total revenue vs. net revenue
# historical dates from Eikon