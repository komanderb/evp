# Load packages
install.packages("openxlsx")
library(tidyverse)
library(openxlsx)

# set wd 
setwd("C:/Users/lenovo/Documents/BSE/RA/Data/Data/EVP")

# load data 
df <- read.csv("evp_final.csv")
df_coo <- read.csv("batch_5_coo.csv")

### function -------------------------------------------------------------------
evp_split <- function(data){
  
  #remove unnecc. columns 
  data <- subset(data, select = -c(national_winner, votes_national_winner, cor_iso3, coo_iso3, valid_votes2))
  
  # the next step could have been solved better but actually nice for naming 
  # iterate through every individual election 
  unique_countries <- unique(data$country_of_origin)
  
  for (country in unique_countries){
    
    country_df <- data %>% filter(country_of_origin == country)
    
    for (date in unique(country_df$election_date)){
      election_df <- country_df %>% filter(election_date == date)
      
      for (election in unique(election_df$election_type)){
        lowest_df <- election_df %>% filter(election_type == election)
        # pivot from wide to long 
        lowest_df <- lowest_df %>% pivot_longer(votes_party_1:party_party_55,  names_to = c(".value", "num"), 
                     names_pattern = '([a-zA-Z]+)_party_(\\d+)')
        # num was needed because of the name, but unneccesary 
        lowest_df <- subset(lowest_df, select= -num)
        # get into right format
        lowest_df <- lowest_df %>% pivot_wider(names_from = party, values_from = votes, values_fn = sum)
        # remove additional columns
        lowest_df <- subset(lowest_df, select = -c(country_of_origin, election_date, election_type))
        # rename to desired names
        names(lowest_df)[1:7] <- c("SCOPE", "REGISTERED", "ACTUAL", "VALID", "NULL", "BLANK", "INVALID")
        # reformat date string
        print_date <- gsub("-", "", date)
        # check election type to make simpler string
        if (election == "Legislative"){
          print_election = "Leg"
        }
        else if (election == "Presidential"){
          print_election = "Pres"
        }
        # for most of the election pivoting will result in one NA column 
        # remove that 
        if (any(names(lowest_df) == "NA")){
          lowest_df <-lowest_df[-which(names(lowest_df) == "NA")]
        }
        # make the file name 
        name_string = paste(country, print_date, print_election,"COR.xlsx",  sep = "_")
        
        # write xlsx 
        write.xlsx(lowest_df, file = name_string)
        
      }
    }
  }
}

### Apply COR --------------------------------------------------------------
# set directory to output dir -> drive
setwd("G:/.shortcut-targets-by-id/1BiiqfA9L9DaS6ELE2OPJkvYdmM6Ws-x6/EVP. Data/publication_data")
# for cor

evp_split(df)

### Apply COO -----------------------------------------------------------------

# -> to many adaptations to use same function:
evp_split_coo <- function(data){
  
  #remove unnecc. columns 
  data <- subset(data, select = -c(national_winner, votes_national_winner))
  
  # the next step could have been solved better but actually nice for naming 
  # iterate through every individual election 
  unique_countries <- unique(data$country_of_origin)
  
  for (country in unique_countries){
    
    country_df <- data %>% filter(country_of_origin == country)
    
    for (date in unique(country_df$election_date)){
      election_df <- country_df %>% filter(election_date == date)
      
      for (election in unique(election_df$election_type)){
        lowest_df <- election_df %>% filter(election_type == election)
        # pivot from wide to long 
        lowest_df <- lowest_df %>% pivot_longer(votes_party_1:party_party_25,  names_to = c(".value", "num"), 
                                                names_pattern = '([a-zA-Z]+)_party_(\\d+)')
        # num was needed because of the name, but unneccesary 
        lowest_df <- subset(lowest_df, select= -num)
        # get into right format
        lowest_df <- lowest_df %>% pivot_wider(names_from = party, values_from = votes, values_fn = sum)
        # remove additional columns
        lowest_df <- subset(lowest_df, select = -c(country_of_origin, election_date, election_type))
        lowest_df <- lowest_df %>% select(country, registered_voters, total_votes, valid_votes, null_votes, blanco_votes, invalid_votes, everything())
        # rename to desired names
        names(lowest_df)[1:7] <- c("SCOPE", "REGISTERED", "ACTUAL", "VALID", "NULL", "BLANK", "INVALID")
        # reformat date string
        print_date <- gsub("-", "", date)
        # check election type to make simpler string
        if (election == "Legislative"){
          print_election = "Leg"
        }
        else if (election == "Presidential"){
          print_election = "Pres"
        }
        # for most of the election pivoting will result in one NA column 
        # remove that 
        if (any(names(lowest_df) == "NA")){
          lowest_df <-lowest_df[-which(names(lowest_df) == "NA")]
        }
        # make the file name 
        name_string = paste(country, print_date, print_election,"COR.xlsx",  sep = "_")
        
        # write xlsx 
        write.xlsx(lowest_df, file = name_string)
        
      }
    }
  }
}


evp_split_coo(df_coo)



                      