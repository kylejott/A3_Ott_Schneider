#################
# first scrape
#################

# Load packages
library(httr)
library(dplyr)
library(XML)
library(ggplot2)
library(stringr)
library(car)
library(devtools)
library(rsdmx)
library(stargazer)
library(knitr)



## 2013 data
tables2013 = data.frame()

for (i in 1:30){

# URL with the medals table
URL_temp2013 <- paste0('http://www.taloussanomat.fi/verotiedot/2013/suurituloisimmat/?n=', i)
if (i==1) { tables2013 <- URL_temp2013 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
            tables2013 <- tables2013[[1]] }
else if (i!=1){
#### Gather content and parse all tables ####
table_temp2013 <- URL_temp2013 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()

# Identify correct table
# names(table) # the table does not have an ID

# select first table with taxData
tables_df_temp2013 <- table_temp2013[[1]]

tables2013 <- rbind(tables2013, tables_df_temp2013)

}
##end loop
}
tables2013$year <- 2013

## 2012 data
tables2012 = data.frame()

for (i in 1:30){
  
  # URL with the medals table
  URL_temp2012 <- paste0('http://www.taloussanomat.fi/verotiedot/2012/suurituloisimmat/?n=', i)
  if (i==1) { tables2012 <- URL_temp2012 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
              tables2012 <- tables2012[[1]] }
  else if (i!=1){
    #### Gather content and parse all tables ####
    table_temp2012 <- URL_temp2012 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
    
    # Identify correct table
    # names(table) # the table does not have an ID
    
    # select first table with taxData
    tables_df_temp2012 <- table_temp2012[[1]]
    
    tables2012 <- rbind(tables2012, tables_df_temp2012)
  }
  ##end loop
}

tables2012$year <- 2012


## 2011 data
tables2011 = data.frame()

for (i in 1:28){
  
  # URL with the medals table
  URL_temp2011 <- paste0('http://www.taloussanomat.fi/verotiedot/2011/suurituloisimmat/?n=', i)
  if (i==1) { tables2011 <- URL_temp2011 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
              tables2011 <- tables2011[[1]] }
  else if (i!=1){
    #### Gather content and parse all tables ####
    table_temp2011 <- URL_temp2011 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
    
    # Identify correct table
    # names(table) # the table does not have an ID
    
    # select first table with taxData
    tables_df_temp2011 <- table_temp2011[[1]]
    
    tables2011 <- rbind(tables2011, tables_df_temp2011)
  }
  ##end loop
}

tables2011$year <- 2011

## 2010 data
tables2010 = data.frame()

for (i in 1:29){
  
  # URL with the medals table
  URL_temp2010 <- paste0('http://www.taloussanomat.fi/verotiedot/2010/suurituloisimmat/?n=', i)
  if (i==1) { tables2010 <- URL_temp2010 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
              tables2010 <- tables2010[[1]] }
  else if (i!=1){
    #### Gather content and parse all tables ####
    table_temp2010 <- URL_temp2010 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
        
    # select first table with taxData
    tables_df_temp2010 <- table_temp2010[[1]]
    
    tables2010 <- rbind(tables2010, tables_df_temp2010)
  }
  ##end loop
}

tables2010$year <- 2010

## 2009 data
tables2009 = data.frame()

for (i in 1:25){
  
  # URL with the medals table
  URL_temp2009 <- paste0('http://www.taloussanomat.fi/verotiedot/2009/suurituloisimmat/?n=', i)
  if (i==1) { tables2009 <- URL_temp2009 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
              tables2009 <- tables2009[[1]] }
  else if (i!=1){
    #### Gather content and parse all tables ####
    table_temp2009 <- URL_temp2009 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
    
    # select first table with taxData
    tables_df_temp2009 <- table_temp2009[[1]]
    
    tables2009 <- rbind(tables2009, tables_df_temp2009)
  }
  ##end loop
}

tables2009$year <- 2009

all <- rbind(tables2009, tables2010, tables2011, tables2012, tables2013)

class(all$year)

#changing the titles to english
all <- plyr::rename(x = all,
                           replace = c("Nimi" = "name",
                                       "Tulot yht" = "total_inc",
                                       "Verot" = "taxes_paid",
                                       "Suhde" = "ratio"
                                       ))
str(all)

#cleaning ratio
all$ratio2 <- str_sub(all$ratio, 1, 2)
summary(all$ratio2)
all$ratio3 <- as.numeric(all$ratio2, length=2)
summary(all$ratio3)

#cleaning taxes_paid
sub(' â¬ $', '',all$taxes_paid)
all$taxes_paid2 <- sub(' â¬$', '',all$taxes_paid)
all$taxes_paid3 <- str_trim(all$taxes_paid2)
all$taxes_paid4 <-sub(' ', '',all$taxes_paid3)
all$taxes_paid5 <-sub(' ', '',all$taxes_paid4)
all$taxes_paid6 <- as.numeric(all$taxes_paid5, length=9)
summary(all$taxes_paid6)

#cleaning total_inc
all$total_inc2 <- sub(' â¬$', '',all$total_inc)
all$total_inc3 <- str_trim(all$total_inc2)
all$total_inc4 <-sub(' ', '',all$total_inc3)
all$total_inc5 <-sub(' ', '',all$total_inc4)
all$total_inc6 <- as.numeric(all$total_inc5, length=13)
summary(all$total_inc6)

#dropping name and keeping rank in given year
all$name2 <- str_sub(all$name, 1, 5)
all$name3 <- sub('\\. ..$', '',all$name2)
all$name4 <- sub('\\. .$', '',all$name3)
all$name5 <- sub('\\. $', '',all$name4)
all$name6 <- sub('\\.$', '',all$name5)
all$name7 <- as.numeric(all$name6, length=5)
summary(all$name7)

clean <- all[, (colnames(all) %in% c("name7", "total_inc6", "taxes_paid6", "ratio3", "year"))]
str(clean)
clean <- plyr::rename(x = clean,
                             replace = c("total_inc6" = "total_inc",
                                         "taxes_paid6" = "taxes_paid",
                                         "name7" = "rank",
                                         "ratio3" = "ratio"
                             ))

## now we have a clean and tidy dataset!


# add 0.1 to taxes paid if it is zero
clean$taxes_paid <- replace(clean$taxes_paid,clean$taxes_paid<=1, 1)

# log transforming the income variables
clean$log_taxes_paid <-log(clean$taxes_paid)
clean$log_total_inc <- log(clean$total_inc)


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

names(Tax09.12)[1] <- "year"
names(Tax09.12)[2] <- "Total_Tax_Revenue"
Tax09.12$year <- as.numeric(Tax09.12$year)
Tax09.12$Total_Tax_Revenue <- as.numeric(Tax09.12$Total_Tax_Revenue)

################
# Tax Revenues 2013
################

Tax09.12$Total_Tax_Revenue <- Tax09.12$Total_Tax_Revenue*1000000000
Tax13 <- data.frame(year=2013, Total_Tax_Revenue =30780000000)
Tax09.13 <- rbind( Tax09.12, Tax13 )

################
# Tax Rates & Delta Tax Rates & GDP
################

Tax09.13$Tax_Rate <- c(30.50, 30.00, 30.00,29.75,31.75)
# GDP in constant prices, national base year
Tax09.13$Total_GDP <- c(181664000000, 187100000000, 191910000000,189111000000,186831000000)
Tax09.13$DELTA_Tax_Rate <- c(NA, 0.5, 0,-0.25,1.0)

################
# Merge Data Sets
################

FINAL <- merge(clean, Tax09.13,
               by = c('year'))

################
# Create Year Dummies
################

FINAL <- within(FINAL, yr2009<-ifelse(year==2009, 1, 0))
FINAL <- within(FINAL, yr2010<-ifelse(year==2010, 1, 0))
FINAL <- within(FINAL, yr2011<-ifelse(year==2011, 1, 0))
FINAL <- within(FINAL, yr2012<-ifelse(year==2012, 1, 0))
FINAL <- within(FINAL, yr2013<-ifelse(year==2013, 1, 0))

################
# Create Dep Var
################

FINAL$total2009 <- with(FINAL, sum(FINAL[yr2009==1, "taxes_paid"]))
FINAL$total2010 <- with(FINAL, sum(FINAL[yr2010==1, "taxes_paid"])) 
FINAL$total2011 <- with(FINAL, sum(FINAL[yr2011==1, "taxes_paid"]))  
FINAL$total2012 <- with(FINAL, sum(FINAL[yr2012==1, "taxes_paid"])) 
FINAL$total2013 <- with(FINAL, sum(FINAL[yr2013==1, "taxes_paid"])) 

FINAL$share2009 <- FINAL$total2009/FINAL$Total_Tax_Revenue
FINAL$share2010 <- FINAL$total2010/FINAL$Total_Tax_Revenue 
FINAL$share2011 <- FINAL$total2011/FINAL$Total_Tax_Revenue 
FINAL$share2012 <- FINAL$total2012/FINAL$Total_Tax_Revenue 
FINAL$share2013 <- FINAL$total2013/FINAL$Total_Tax_Revenue 



################
#Descriptive Statistics
################
qplot(ratio, data=FINAL, geom="histogram")

## think of a better plot here
qplot(ratio, taxes_paid, data=FINAL)
qplot(ratio, taxes_paid, data=FINAL, ylim=c(0,15000))

#boxplot(all$taxes_paid6)





car::scatterplotMatrix(clean)

logit1 <- glm(ratio ~ log_total_inc + year + rank, data = clean, family = 'binomial')
confint(logit1)

fitted <- with(clean,
               data.frame(gre = mean(gre),
                          gpa = mean(gpa),
                          rank = factor(1:4)))


