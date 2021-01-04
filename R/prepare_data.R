#' Download Car Cards
#' 
#' @param gsheet_url A string with gsheet URL with Car Cards
#' @importFrom dplyr %>% filter mutate
#' @importFrom googlesheets4 read_sheet
#' @importFrom janitor clean_names
#' @return A data frame with Car Cards

download_data = function(gsheet_url){
  
  # download cards
  cards = read_sheet(gsheet_url, sheet = 1, col_types = 'cccnncnnnnn') %>% 
    clean_names() %>% 
    filter(!is.na(year)) %>% 
    replace(., is.na(.), '') %>% 
    mutate(name = paste(make, model, type,
                        paste0('(', year, ')')) %>% 
             gsub(pattern = '\\s+', replacement = ' '))
  
  return(cards)
}

#' Split cards between players an shuffle
#' 
#' @param cards A data frame with Car Cards
#' @importFrom dplyr slice
#' @return A list of data frames with each player's Car Cards 
split_cards = function(cards){
  
  rows_player1 = sample(nrow(cards), nrow(cards)/2)
  cards_player1 = cards[rows_player1, ]
  cards_player2 = setdiff(cards, cards_player1)
  
  # shuffle
  cards_player1 = slice(cards_player1, sample(1:n()))
  cards_player2 = slice(cards_player2, sample(1:n()))
  
  return(list(cards_player1, cards_player2))
}
