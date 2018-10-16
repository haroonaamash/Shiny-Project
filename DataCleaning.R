

##cleaning table

library(dplyr)
library(readxl)
library(leaflet)
library(dplyr)
library(tidyverse)
#statenisland
#setting column names 
rollingsales_statenisland <- read_excel("rollingsales_statenisland.xls")
statenisland=rollingsales_statenisland
statenisland=statenisland[-c(1:3),]
colnames(statenisland)=statenisland[1,]
statenisland=statenisland[-1,]

statenisland1= statenisland %>% separate(ADDRESS, into = paste("V", 1:2), sep = ",") 
statenisland1=statenisland1[-c(10)]
statenisland= statenisland %>% mutate(., FullAddress= paste0(statenisland1$`V 1`,  ' Staten Island New York', ' ', statenisland$`ZIP CODE`))


#queens
#setting column names
rollingsales_queens <- read_excel("rollingsales_queens.xls")
queens=rollingsales_queens
queens=queens[-c(1:3),]
colnames(queens)=queens[1,]
queens=queens[-1,]

queens1= queens %>% separate(ADDRESS, into = paste("V", 1:2), sep = ",") 
queens1=queens1[-c(10)]
queens= queens %>% mutate(., FullAddress= paste0(queens1$`V 1`,  ' Queens New York', ' ', queens$`ZIP CODE`))


#brooklyn
#setting column names
rollingsales_brooklyn <- read_excel("rollingsales_brooklyn.xls")
brooklyn=rollingsales_brooklyn
brooklyn=brooklyn[-c(1:3),]
colnames(brooklyn)=brooklyn[1,]
brooklyn=brooklyn[-1,]

brooklyn1= brooklyn %>% separate(ADDRESS, into = paste("V", 1:2), sep = ",") 
brooklyn1=brooklyn1[-c(10)]
brooklyn= brooklyn %>% mutate(., FullAddress= paste0(brooklyn1$`V 1`,  ' Brooklyn New York', ' ', brooklyn$`ZIP CODE`))

#bronx
#setting column names
rollingsales_bronx <- read_excel("rollingsales_bronx.xls")
bronx=rollingsales_bronx
bronx=bronx[-c(1:3),]
colnames(bronx)=bronx[1,]
bronx=bronx[-1,]

bronx1= bronx %>% separate(ADDRESS, into = paste("V", 1:2), sep = ",") 
bronx1=bronx1[-c(10)]
bronx= bronx %>% mutate(., FullAddress= paste0(bronx1$`V 1`,  ' Bronx New York', ' ', bronx$`ZIP CODE`))

#manhattan
#setting column names
rollingsales_manhattan <- read_excel("rollingsales_manhattan.xls")
manhattan=rollingsales_manhattan
manhattan=manhattan[-c(1:3),]
colnames(manhattan)=manhattan[1,]
manhattan=manhattan[-1,]
head(manhattan)

manhattan1= manhattan %>% separate(ADDRESS, into = paste("V", 1:2), sep = ",") 
manhattan1=manhattan1[-c(10)]
manhattan= manhattan %>% mutate(., FullAddress= paste0(manhattan1$`V 1`,  ' New York New York', ' ', manhattan$`ZIP CODE`))

#merge all bouroughs data to nyc.df
nyc.df=Reduce(function(...) merge(..., all=TRUE), list(queens,bronx,brooklyn,manhattan,statenisland))
colnames(nyc.df)=sapply(colnames(nyc.df) , function(x)gsub(" ",".",x))
names(nyc.df)


#change date format from excel
nyc.df = nyc.df %>% mutate(., DATE=as.Date(as.numeric(SALE.DATE), origin = "1900-01-01"))
#remove unwanted columns

names(nyc.df)
nyc.df=nyc.df[ ,-c(4,5,6,7,8,15,16,19)]

#removed all tax class 4 rows as they contain non-residential properties
nyc.df=nyc.df[c(nyc.df$TAX.CLASS.AT.TIME.OF.SALE)!='4',]

#check for NA values in any columns
colnames(nyc.df)[colSums(is.na(nyc.df))>0]

#remove all transactions where sale price was 0
nyc.df=nyc.df[nyc.df$SALE.PRICE!='0',]



#---------------------------------------------------------------------------
#income data
library(readxl)    
library(readr)


Data_USA_Geo_Map_of_Income_by_Location_in_New_York_Ny_ = read_csv("Data USA - Geo Map of Income by Location in New York, Ny .csv")
income.by.loc=Data_USA_Geo_Map_of_Income_by_Location_in_New_York_Ny_
income.by.loc=income.by.loc[,-c(7:8)]
#remove spaces in column names
colnames(income.by.loc)=sapply(colnames(income.by.loc) , function(x)gsub(" ",".",x))

names(income.by.loc)

#---------------------------------------------------------------------------
#Historic transaction data for all 5 boroughs


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets = readxl::excel_sheets(filename)
  x = lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
a =read_excel_allsheets("staten_island_sales_prices.xls")
b =read_excel_allsheets( "queens_sales_prices.xls")
c =read_excel_allsheets( "brooklyn_sales_prices.xls")
d =read_excel_allsheets( "bronx_sales_prices.xls")
e =read_excel_allsheets("manhattan_sales_prices.xls")

#join all files and sort them by year
length(e)
ls= list()
for (i in 1:length(e)){
  ls = append(ls, a[i])
  ls = append(ls, b[i])
  ls = append(ls, c[i])
  ls = append(ls, d[i])
  ls = append(ls, e[i])
  print(i)
}

ls= as.list(rep("",length(e)))
for (i in 1:length(e)){
  temp_df = rbind(a[[i]],b[[i]],c[[i]],d[[i]],e[[i]])
  ls[[i]] = temp_df
  
  print(i)
}

historic_2017=ls[[1]]
historic_2016=ls[[2]]
historic_2015=ls[[3]]
historic_2014=ls[[4]]
historic_2013=ls[[5]]
historic_2012=ls[[6]]
historic_2011=ls[[7]]
historic_2010=ls[[8]]
historic_2009=ls[[9]]
historic_2008=ls[[10]]
historic_2007=ls[[11]]
historic_2006=ls[[12]]
historic_2005=ls[[13]]

historic_2005= historic_2005 %>% mutate(., Year='2005')
historic_2006= historic_2006 %>% mutate(., Year='2006')
historic_2007= historic_2007 %>% mutate(., Year='2007')
historic_2008= historic_2008 %>% mutate(., Year='2008')
historic_2009= historic_2009 %>% mutate(., Year='2009')
historic_2010= historic_2010 %>% mutate(., Year='2010')
historic_2011= historic_2011 %>% mutate(., Year='2011')
historic_2012= historic_2012 %>% mutate(., Year='2012')
historic_2013= historic_2013 %>% mutate(., Year='2013')
historic_2014= historic_2014 %>% mutate(., Year='2014')
historic_2015= historic_2015 %>% mutate(., Year='2015')
historic_2016= historic_2016 %>% mutate(., Year='2016')
historic_2017= historic_2017 %>% mutate(., Year='2017')

historical.final= bind_rows(historic_2005,historic_2006,historic_2007,historic_2008,historic_2009,historic_2010,historic_2011,historic_2012,historic_2013,historic_2014,historic_2015,historic_2016,historic_2017)
dim(historical.final)
names(historical.final)

#-----------------------------------------------

#interactive map display

library(rgdal)
library(httr)
r=GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods = readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

nyc.map=leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")



#---------------------------------------------------------

#create 2 new columns for latitude and longitude
nyc.df$lat <- " "
nyc.df$lon <- " "

nyc.df$FullAddress= gsub(" ", '+', nyc.df$FullAddress)


#filling in value of lattitude and longitude


#latt<- for (i in 7645:55603) {
  
  
#  pattern = "ll=[[:digit:]]{2}.[[:digit:]]+,-[[:digit:]]{2}.[[:digit:]]+"
#  r <- GET(paste0("https://www.google.com/maps/place/", nyc.df$FullAddress[i]))
  
#  x = as.character(r)
#  m = gregexpr(pattern, x, perl=TRUE)
  
#  nyc.df$lat[i]= regmatches(x, m)[[1]][1]
  
#}


#---------------------------------------------------------

#checking for all NA values in lat
sum(is.na(nyc.df$lat))
na.in.nycdf= subset(nyc.df,is.na(nyc.df$lat))
dim(na.in.nycdf)
head(na.in.nycdf$FullAddress)

gsub(pattern == '+++', replacement = '+',na.in.nycdf$FullAddress)
gsub(pattern == '+', replacement = '',na.in.nycdf$FullAddress)
gsub(pattern == '+', replacement = '',na.in.nycdf$FullAddress)

#run for loop again to get lat values
#latt<- for (i in 1:9667) {


#  pattern = "ll=[[:digit:]]{2}.[[:digit:]]+,-[[:digit:]]{2}.[[:digit:]]+"
#  r <- GET(paste0("https://www.google.com/maps/place/", na.in.nycdf$FullAddress[i]))

#  x = as.character(r)
#  m = gregexpr(pattern, x, perl=TRUE)

#  na.in.nycdf$lat[i]= regmatches(x, m)[[1]][1]

#}

#----------------------------------------------------------
#remove (unnecessary) lon column
names(nyc.df)
nyc.df=nyc.df[-17]

#split lattitude and longitude into 2 columns and clean it up
nycfinal.df= nyc.df %>% separate(., lat, into= c('lattitude','longitude'), sep = ',')
names(nycfinal.df)
head(nycfinal.df)
nycfinal.df$lattitude= gsub('ll=' , '' , nycfinal.df$lattitude)


write.csv(nycfinal.df, file = 'NYCfinal.csv')
write.csv(historical.final, file = "historical_final.csv")
