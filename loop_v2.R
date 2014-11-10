#################
# first scrape
#################

# Load packages
library(httr)
library(dplyr)
library(XML)

nimi_temp=list()

for (i in 1:2){
  
  # URL with the medals table
  URL_temp <- paste0('http://www.taloussanomat.fi/verotiedot/2010/suurituloisimmat/?n=', i)
  
  #### Gather content and parse all tables ####
  table <- URL_temp %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
  
  # Identify correct table
  names(table) # the table does not have an ID
  
  # select first table with taxData
  tables <- table[[1]]
  # names[[i]] <- tables[, 1]
  nimi_temp[[i]] <- tables$ Nimi   
  nimi_all <- c(tables$ Nimi   , nimi_temp)
  
  ##end loop
}

allnames <- as.data.frame(nimi_temp)

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