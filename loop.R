#################
# first scrape
#################

# Load packages
library(httr)
library(dplyr)
library(XML)

tables = list()

for (i in 1:3){

# URL with the medals table
URL_temp <- paste0('http://www.taloussanomat.fi/verotiedot/2010/suurituloisimmat/?n=', i)

#### Gather content and parse all tables ####
table <- URL_temp %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()

# Identify correct table
names(table) # the table does not have an ID

names <- c

# select first table with taxData
tables[[i]] <- table[[1]]
##rearrange the list somehow???

##end loop
}

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