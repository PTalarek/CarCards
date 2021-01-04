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

