library(CarCards)
source('inst/config/config.R')

cards = download_data_gs(gsheet_url)
upload_data_sql(cards, dbname, host, username, password, newdb = FALSE)
