#' Download Car Cards
#'
#' @param gsheet_url A string with gsheet URL with Car Cards
#' @importFrom dplyr %>% filter mutate
#' @importFrom googlesheets4 read_sheet
#' @importFrom janitor clean_names
#' @return A data frame with Car Cards
#' @export

download_data_gs = function(gsheet_url){

  # download cards
  cards = read_sheet(gsheet_url, sheet = 1, col_types = 'cccnncnnnnn') %>%
    clean_names() %>%
    filter(!is.na(year)) %>%
    replace(., is.na(.), '') %>%
    mutate(name = paste(make, model, type,
                        paste0('(', year, ')')) %>%
             gsub(pattern = '\\s+', replacement = ' '),
           uid = paste(make, model, type, year, sep = '_') %>%
             tolower() %>%
             gsub(pattern = '\\s+|\\(|\\)', replacement = '') %>%
             gsub(pattern = '\\__', replacement = '\\_'))

  return(cards)
}

#' Upload processed Car Cards to mySQL server
#'
#' @param cards A data frame with Car Cards
#' @param dbname String with database name
#' @param host String with database host
#' @param username String with database user
#' @param password String with database password
#' @param newdb Logical indicating if new \code{cards} table should be created
#' @importFrom DBI dbConnect dbGetQuery dbSendQuery dbWriteTable sqlCreateTable
#' @importFrom RMySQL MySQL
#' @return Content of \code{cards} SQL table
#' @export
upload_data_sql = function(cards, dbname, host, username, password, newdb = FALSE){

  # create connection
  conn = dbConnect(MySQL(),
                   dbname = dbname,
                   host = host,
                   username = username,
                   password = password)

  # create table
  if (newdb){
    dbSendQuery(conn, sqlCreateTable(conn, "cards", cards))
  }

  # populate table
  dbWriteTable(conn, 'cards', cards, overwrite = TRUE)

  # print table
  dbGetQuery(conn, 'SELECT * FROM cards')
}

#' Download Car Cards from mySQL server
#'
#' @param dbname String with database name
#' @param host String with database host
#' @param username String with database user
#' @param password String with database password
#' @importFrom DBI dbConnect dbGetQuery dbSendQuery dbWriteTable sqlCreateTable
#' @importFrom RMySQL MySQL
#' @return Content of \code{cards} SQL table
#' @export
download_data_sql = function(dbname, host, username, password){

  # create connection
  conn = dbConnect(MySQL(),
                   dbname = dbname,
                   host = host,
                   username = username,
                   password = password)

  # get the data
  dbGetQuery(conn, 'SELECT * FROM cards')
}

#' Split cards between players an shuffle
#'
#' @param cards A data frame with Car Cards
#' @importFrom dplyr n setdiff slice
#' @return A list of data frames with each player's Car Cards
#' @export
split_cards = function(cards){

  rows_player1 = sample(nrow(cards), nrow(cards)/2)
  cards_player1 = cards[rows_player1, ]
  cards_player2 = setdiff(cards, cards_player1)

  # shuffle
  cards_player1 = slice(cards_player1, sample(1:n()))
  cards_player2 = slice(cards_player2, sample(1:n()))

  return(list(cards_player1 = cards_player1, cards_player2 = cards_player2))
}

#' Determine which player will start the game
#'
#' @param players_n Number of players
#' @return Starting player number
#' @export
draw_starting_player = function(players_n){
  starting_player = sample(2, 1)
  }

