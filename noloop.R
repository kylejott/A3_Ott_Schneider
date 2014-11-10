#################
# first scrape
#################

# Load packages
library(httr)
library(dplyr)
library(XML)
  
  # URL with the medals table
  URL_temp <- 'http://www.taloussanomat.fi/verotiedot/2010/suurituloisimmat/?n=1'
  
  #### Gather content and parse all tables ####
  table2010_1 <- URL_temp %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
  
  # Identify correct table
  names(table2010_1) # the table does not have an ID
  
  # select first table with taxData
  tax2010_1 <- table2010_1[[1]]

  data_tax2010_1 <- as.data.frame(tax2010_1)

# URL with the medals table
URL_temp2 <- 'http://www.taloussanomat.fi/verotiedot/2010/suurituloisimmat/?n=2'

#### Gather content and parse all tables ####
table2010_2 <- URL_temp2 %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()

# Identify correct table
names(table2010_2) # the table does not have an ID

# select first table with taxData
tax2010_2 <- table2010_2[[1]]

data_tax2010_2 <- as.data.frame(tax2010_2)

combined2010 = rbind(tax2010_1, tax2010_2)












# Convert to a data frame
fintax2010 <- as.data.frame(tables)

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