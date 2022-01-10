
## Functions: 

### renamer
renamer <- function(df, colnum){
  names(df)[colnum] <- 'country'
  return(df)
}

### party_sorter
top_party <- function(df){
  df <- df %>% arrange(country, -votes)
  df <- df %>% group_by(country) %>% top_n(3, votes)
  return(df)
}

party_sorter <- function(df){
  df <- df %>% arrange(country, -votes)
  df <- df %>% group_by(country) #dont know if neccessary
  return(df)
}

##gatherer: 
gatherer <- function(df, col_start, col_end){
  df <- df %>% 
    gather(key = 'party',
           value = 'votes',
           c(all_of(col_start):all_of(col_end)))
  return(df)
}


### all in one 

slim_fitter <- function(df, col_start, col_end){
  df <- gatherer(df, col_start, col_end)
  df <- party_sorter(df)
  return(df)
}

## prepare for pivot longer 

looper <- function(df){
  a = count(df, country)
  ans = vector()
  for (i in a$n){
    for (b in 1:i){
      out = paste0('party_', b)
      ans = append(ans, out)
    }
  }
  return(ans)## that is the right result // 
}


########## for afterwards // ###
# not working
#my_pivoter <- function(df){
 # spec <- df %>%
  #  build_wider_spec(names_from = ph, values_from = c(votes, party))
  #df <- spec %>%
   # arrange(factor(str_sort(ph, numeric = T))) %>%
  #  pivot_wider_spec(df, .)
  #return(df)
#}

my_pivoter <- function(df){
  df <- df %>%
    pivot_wider(names_from = ph,
                values_from = c(votes, party))
  return(df)
}

column_sorter <- function(df, colstart, colend){
  x <- names(df)[colstart:colend]
  x_num = parse_number(x)
  ord = order(x_num)
  df[colstart:colend] <- df[x[ord]]
  names(df)[colstart:colend] <- x[ord]
  return(df)
}

national_winner <- function(df1, df2){
  names(df2)[2:3] <- c('national_winner', 'votes_national_winner')
  df1 <- left_join(df1, df2, by = 'country')
  return(df1)
}


main_function <- function(df, slim_start, slim_end, sort_start, national_winner){
  df <- slim_fitter(df, slim_start, slim_end)
  df$ph <- looper(df)
  df_nw <- df %>% filter(party == national_winner)
  df_nw <- df_nw[c(1, sort_start, (sort_start + 1))]
  df <- my_pivoter(df)
  sort_end <- ncol(df)
  df <- column_sorter(df, sort_start, sort_end)
  df <- national_winner(df, df_nw)
  return(df)
}

extra_cols <- function(df, cor, date, type){
  df <- add_column(df, country_of_origin = cor, .after = 'country')
  df <- add_column(df, election_date = as.Date(date), .after = 'country_of_origin')
  df <- add_column(df, election_type = type, .after =  "election_date")
  return(df)
}

split_number <- function(n1, n2){
  number = c()
  for (i in n1:n2){
    if(i %% 2 != 0){
      number = append(number, i)
    }
  }
  return(number)
}
## maybe rename 
s
## more special stuff: 
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
elecid_gen <- function(df, iso3c, year, type){
  df <- add_column(df, elec_id = paste0(iso3c, year, type), .before = 'country')
  return(df)
}

### 
english_country_name <- function(vector, languagecode){
  custom_dict <- data.frame(portugese = countrycode::codelist$cldr.name.pt,
                            english = countrycode::codelist$cldr.name.en,
                            stringsAsFactors = FALSE)
  custom_dict$portugese <- tolower(custom_dict$portugese)
  countrycode(v4, 'portugese', 'english', custom_dict = custom_dict)
}


### okay okay okay
dictionary_maker <- function(df, dic_number, row_number, start_col, end_col){
  dictionary_dic_number <- df[rownumber, c(start_col:end_col)]
  return(dictionary_dic_number)
}


