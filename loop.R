#################
# first scrape
#################

# Load packages
library(httr)
library(dplyr)
library(XML)

tables = data.frame()

for (i in 1:30){

# URL with the medals table
URL_temp <- paste0('http://www.taloussanomat.fi/verotiedot/2013/suurituloisimmat/?n=', i)
if (i==1) { tables <- URL_temp %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()
            tables <- tables[[1]] }
else if (i!=1){
#### Gather content and parse all tables ####
table_temp <- URL_temp %>% GET() %>% content(as = 'parsed') %>% readHTMLTable()

# Identify correct table
# names(table) # the table does not have an ID

# select first table with taxData
tables_df_temp <- table_temp[[1]]

tables <- rbind(tables, tables_df_temp)
}
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