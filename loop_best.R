#################
# first scrape
#################

# Load packages
library(httr)
library(dplyr)
library(XML)
library(ggplot2)


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

all2011_2013 <- rbind(tables2011, tables2012, tables2013)


class(all2011_2013$year)
#changing the titles to english
all2011_2013 <- plyr::rename(x = all2011_2013,
                           replace = c("Nimi" = "name",
                                       "Tulot yht" = "total_inc",
                                       "Verot" = "taxes_paid",
                                       "Suhde" = "ratio"
                                       ))
str(all2011_2013)

#cleaning ratio
all2011_2013$ratio2 <- str_sub(all2011_2013$ratio, 1, 2)
summary(all2011_2013$ratio2)
all2011_2013$ratio3 <- as.numeric(all2011_2013$ratio2, length=2)
summary(all2011_2013$ratio3)

qplot(ratio3, data=all2011_2013, geom="histogram")

#cleaning taxes_paid
sub(' â¬ $', '',all2011_2013$taxes_paid)
all2011_2013$taxes_paid2 <- sub(' â¬$', '',all2011_2013$taxes_paid)
all2011_2013$taxes_paid3 <- str_trim(all2011_2013$taxes_paid2)
all2011_2013$taxes_paid4 <-sub(' ', '',all2011_2013$taxes_paid3)
all2011_2013$taxes_paid5 <-sub(' ', '',all2011_2013$taxes_paid4)
all2011_2013$taxes_paid6 <- as.numeric(all2011_2013$taxes_paid5, length=9)
summary(all2011_2013$taxes_paid6)

## think of a better plot here
qplot(ratio3, taxes_paid6, data=all2011_2013)

boxplot(all2011_2013$taxes_paid6)


#cleaning total_inc
all2011_2013$total_inc2 <- sub(' â¬$', '',all2011_2013$total_inc)
all2011_2013$total_inc3 <- str_trim(all2011_2013$total_inc2)
all2011_2013$total_inc4 <-sub(' ', '',all2011_2013$total_inc3)
all2011_2013$total_inc5 <-sub(' ', '',all2011_2013$total_inc4)
all2011_2013$total_inc6 <- as.numeric(all2011_2013$total_inc5, length=13)
summary(all2011_2013$total_inc6)

## think of a better plot here

#dropping name and keeping rank
all2011_2013$name2 <- str_sub(all2011_2013$name, 1, 5)
all2011_2013$name3 <- sub('![1-15000]', '',all2011_2013$name2)


clean <- all2011_2013[, (colnames(all2011_2013) %in% c("name", "total_inc6", "taxes_paid6", "ratio3", "year"))]
str(clean)
clean <- plyr::rename(x = clean,
                             replace = c("total_inc6" = "total_inc",
                                         "taxes_paid6" = "taxes_paid",
                                         "ratio3" = "ratio"
                             ))






##from his class example
#### Clean ####
# Drop unwanted variables
fintaxes <- select(fintaxes, -NULL.V2, -NULL.V5)



# Give new variable names
names(fintaxes) <- c('Name', 'Total Rev', 'Total Paid Taxs', 'Tax Rate')




# Convert variable classes
medals$country <- as.character(medals$country)
for (i in 2:5) medals[, i] <- as.integer(medals[, i])

# Sort by total medals in descending order
medals <- arrange(medals, desc(total))