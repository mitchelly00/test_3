#clean the enviroment
rm(list = ls())

# load the library
library(tidycensus)

#load api
census_api_key("279c9b589cc5452c32299f2c9e93deb5c9113c5d",
               install=TRUE,overwrite = TRUE)
#loading the dataset
v15<- load_variables(year=2015,
                     "acs5")


#getting the specific variable
US_2015 <- get_acs(geography = "state",
                 variables = c(gini = c("B19083_001")), 
                  year = 2015)

#loading dataset for 2010 
v10<- load_variables(year=2010,
                     "acs5")

#getting the specific variable for 2010
US_2010 <- get_acs(geography = "state",
                   variables = c(gini = c("B19083_001")), 
                   year = 2010)

#merging the data together
library(dplyr)

inequaility_panel = left_join(x=US_2010,
                          y=US_2015,
                          by=c("GEOID","NAME"))

#taking extranious variables
inequaility_panel$variable.y <- NULL
inequaility_panel$variable.x <- NULL
inequaility_panel$moe.x<- NULL
inequaility_panel$moe.y<- NULL

#changing names
library(data.table)
setnames(inequaility_panel,"estimate.x","gini_2010")

setnames(inequaility_panel,"estimate.y","gini_2015")
setnames(inequaility_panel,"NAME","state")

#quick peak
head(inequaility_panel)

####Question 3 
inequaility_wide<-inequaility_panel
head(inequaility_panel)


####Question 4
library(tidyr)

inequaility_long<-
  inequaility_panel%>%
  pivot_longer(cols = starts_with("gini"), # use columns starting with "year" 
               names_to ="year", # name of new column 
               names_prefix = "gini_", # part of string to drop 
               values_to = "gini", # where to put numeric values 
               values_drop_na = FALSE) %>% # don't drop NAs 
  filter(!(gini==0)) # drop observations with no disbursements
 
#### q5 

summary(inequaility_wide)
summary(inequaility_panel)  

#### q6 collapse

collapsed_collapsed<-
  inequaility_long %>% 
  group_by(state,GEOID) %>% 
  summarise(across(where(is.numeric),mean))

#### q7
library(sf)
library(ggplot2)
us_borders <- st_read("C:/Users/schon/Downloads/Gov 355/test_3/cb_2018_us_state_500k")
state_borders <- st_transform(us_borders,"+proj=latlong +ellps=WGS84 +datum=WGS84")
rm(us_borders)

us_basic = ggplot()+
  geom_sf(data=state_borders)

us_basic


####q8 
library(WDI)
gdp<- WDI(country="all",indicator='NY.GDP.MKTP.CD',  
                  start=2006, end=2007)
library(data.table) 
setnames(gdp,"NY.GDP.MKTP.CD", "gdp_current")

####q9
deflator_data = WDI(country = "all", indicator = c("NY.GDP.DEFL.ZS"),
                    start = 2001, 
                    end = 2017, 
                    extra = FALSE, cache = NULL)
#rename variables
library(data.table) 
setnames(deflator_data,"NY.GDP.DEFL.ZS", "deflator")

# select only the United States data 
usd_deflator = subset(deflator_data, country=="United States")
subset(usd_deflator, deflator==100)

#remove deflator
rm(deflator_data)

#drop unecisary variables
usd_deflator$country <- NULL 
usd_deflator$iso2c <- NULL

#merging the data
gdp_deflated= left_join(gdp, usd_deflator, by=c("year"))

gdp_deflated$deflated_amount = 
  gdp_deflated$current_amount/ (gdp_deflated$deflator/100) 

head(gdp_deflated)

##  q 11 pull pdf
library(pdftools)
armeniatext=pdf_text(pdf = "https://pdf.usaid.gov/pdf_docs/PA00TNMG.pdf")

armeniatext=as.data.frame(armeniatext) 
armeniatext$page=c(1:65) 
colnames(armeniatext)[which(names(armeniatext) == "armeniatext")] <- "text" #change column name

####q13 
library(tidytext)
armeniatext=armeniatext %>% unnest_tokens(word, text)

#in order to get rid of stop words: 
data(stop_words)
armeniatext <- armeniatext %>% anti_join(stop_words)

#q14
#word frequencies 
hpfreq <- armeniatext %>% count(word, sort = TRUE) 
head(hpfreq,5)

#q15
library(rvest)
library(dplyr)
hot100page <- "https://www.billboard.com/charts/hot-100" 
hot100 <- read_html(hot100page)

#q16 
body_nodes <- hot100 %>%
  html_node("body") %>%
  html_children() 
body_nodes

#Q17
library(xml2)

rank <-chart_page%>%
  rvest::html_nodes('body')%>%
  xml2::xml_find_all("//span[contains(@class,'chart-element__rank__number')]")%>%
  rvest::html_text()

artist <-chart_page%>%
  rvest::html_nodes('body')%>%
  xml2::xml_find_all("//span[contains(@class,'chart-element__information__artist')]")%>%
  rvest::html_text()

title <-chart_page%>%
  rvest::html_nodes('body')%>%
  xml2::xml_find_all("//span[contains(@class,'chart-element__information__song')]")%>%
  rvest::html_text()