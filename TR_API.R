install.packages("devtools")
library(devtools)
install_github("ahmedmohamedali/eikonapir")
library(eikonapir)
eikonapir::set_proxy_port(9000L)
eikonapir::set_app_id('60537ab9119a497d82010c7da1b0ddeefca74068')
result <- get_symbology(list("MSFT.O", "GOOG.O", "IBM.N"),"RIC",list("ISIN"),raw_ouput = FALSE,debug=FALSE)
print(result)

#result1 = ek.get_timeseries (("44918770"),
                            start_date=datetime.datetime(2008, 1, 1),
                            end_date=datetime.datetime(2016, 12, 31),
                            interval='annual',
 #                           fields=['CLOSE'])


TR.AnalyticEstimatedCO2Total

#x = get_data('IBM.N', ['TR.CO2EmissionTotal'])

#get_timeseries <- TR.AnalyticEstimatedCO2Total("MSFT.O",fields='VALUE', start_date=NULL, end_date=NULL, interval='yearly', normalize=FALSE, count=NULL,
 #                          calendar=NULL, corax=NULL,raw_output=FALSE, debug=FALSE)


fields = list(TR_Field('tr.revenue'), TR_Field('tr.open',NULL,'asc',1),TR_Field('TR.GrossProfit',list('Scale'=6, 'Curn'='EUR'),'asc',0))
              data = get_data(list("IBM","MSFT.O"),fields)
              
              get_data('IBM.N','TR.CO2EmissionTotal')
              