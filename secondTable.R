#################
# Second Table: Tax Revenues
#################

# Load packages
library(httr)
library(dplyr)
library(XML)
library(rsdmx)

#################
# OECD: Tax Revenues 2009 - 2012
#################

# URL
URL <- 'http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/REV/NES.1000.TAXNAT.FIN?startTime=2009&endTime=2012'
sdmx <- readSDMX('http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/REV/NES.1000.TAXNAT.FIN?startTime=2009&endTime=2012')

# Data Frame
Tax09.12 <- as.data.frame(sdmx)

# Making It ****TiDDDDDYyyyyyYYYYyyy****
drops <- c("GOV","TAX","TIME_FORMAT","Unit","PowerCode","VAR","COU")
Tax09.12 <- Tax09.12[,!(names(Tax09.12) %in% drops)]

as.numeric('obsTime', 'obsValue' )

names(Tax09.12)[1] <- "Year"
names(Tax09.12)[2] <- "Total_Tax_Revenue"

################
# Tax Revenues 2013
################

Tax13 <- data.frame(Year="2013", Total_Tax_Revenue ="30.780")
as.numeric('Year', 'Total_Tax_Revenue' )
Tax09.13 <- rbind( Tax09.12, Tax13 )

################
# Tax Rates
################

Tax09.13$Tax_Revenue <- c("30,50", "30,00", "30,00","29,75","31,75")
as.numeric('Tax_Revenue')




################
# old try
################

# URL
URL <- 'http://www.stat.fi/til/vermak/2012/vermak_2012_2013-07-11_tau_001_en.html'

#### Gather content and parse all tables ####
tables <- URL %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()

# Identify correct table
names(tables) # the table does not have an ID

# select first table with taxData
Taxes20112012 <- tables[[1]]
Taxes20112012 <- head(Taxes20112012)[, 1:3]


Taxes20112012 <- plyr::rename(x = Taxes20112012,
                             replace = c("Sector" = "Tax categroy",
                                         "Tax category" = "2011",
                                         "2011" = "2012"
                             ))
drops <- c("Tax category")

names(Taxes20112012)[1] <- "Year1"
names(Taxes20112012)[2] <- "Year2"


Taxes20112012 <- data.frame(t(Taxes20112012))

