library(data.table)
library(dplyr)
library(googlesheets4)
library(janitor)
library(magick)
library(tibble)

source('inst/config/config.R')

# download cards
cards = read_sheet(gsheet_url, sheet = 1, col_types = 'cccnncnnnnn') %>% 
  clean_names() %>% 
  filter(!is.na(year)) %>% 
  replace(., is.na(.), '') %>% 
  mutate(name = paste(make, model, type,
                      paste0('(', year, ')')) %>% 
           gsub(pattern = '\\s+', replacement = ' '))

# split cards beetween players
rows_player2 = sample(nrow(cards), nrow(cards)/2)

cards_player2 = cards[rows_player2, ]
cards_player1 = setdiff(cards, cards_player2)

# shuffle
cards_player2 = slice(cards_player2, sample(1:n()))
cards_player1 = slice(cards_player1, sample(1:n()))

# who starts
attacking_player = sample(2, 1)

while (nrow(cards_player1) > 0 & nrow(cards_player2) > 0){
  
  # attacking player gets first card from the stack
  attacking_stack = 'cards_player' %>% 
    paste0(attacking_player) %>% 
    get() %>% 
    head(1)
  
  # remove the card from attacking player's main stack
  assign('cards_player' %>% 
           paste0(attacking_player),
         'cards_player' %>% 
           paste0(attacking_player) %>% 
           get() %>% 
           tail(-1))
  
  defending_player = setdiff(1:2, attacking_player)
  
  # defending player get first card from the stack
  defending_stack = 'cards_player' %>% 
    paste0(defending_player) %>% 
    get() %>% 
    head(1)
  
  # remove the card from defending player's main stack
  assign('cards_player' %>% 
           paste0(defending_player),
         'cards_player' %>% 
           paste0(defending_player) %>% 
           get() %>% 
           tail(-1))
  
  round_end = FALSE
  
  while(round_end == FALSE){
    
    attacking_card = attacking_stack %>% 
      tail(1)
    
    # show attacking card
    attacking_stack %>% 
      print()
    
    car_name = attacking_card$name
    card_frame = image_read('D:/R/CarCards/img/card_frame.png')
    card_image = image_read('D:/R/CarCards/img/pic1.png')
    attacking_card_image = image_composite(card_frame, card_image, gravity = 'North', offset = '+0+10') %>% 
      image_annotate(car_name, font = 'Agency FB', size = 27, location = '+20+230') %>% 
      
      image_annotate('Engine capacity [ccm]', font = 'Agency FB', size = 20, location = '+20+290') %>% 
      image_annotate(attacking_card$capacity, font = 'Agency FB', size = 20, location = '+320+290') %>% 
      
      image_annotate('Cylinders', font = 'Agency FB', size = 20, location = '+20+320') %>% 
      image_annotate(attacking_card$cylinders, font = 'Agency FB', size = 20, location = '+320+320') %>% 
      
      image_annotate('Engine type', font = 'Agency FB', size = 20, location = '+20+350') %>% 
      image_annotate(attacking_card$engine, font = 'Agency FB', size = 20, location = '+320+350') %>% 
      
      image_annotate('Power [HP]', font = 'Agency FB', size = 20, location = '+20+380') %>% 
      image_annotate(attacking_card$power, font = 'Agency FB', size = 20, location = '+320+380') %>% 
      
      image_annotate('Weight [kg]', font = 'Agency FB', size = 20, location = '+20+410') %>% 
      image_annotate(attacking_card$weight, font = 'Agency FB', size = 20, location = '+320+410') %>% 
      
      image_annotate('Acceleration 0-100 km/h [s]', font = 'Agency FB', size = 20, location = '+20+440') %>%
      image_annotate(attacking_card$acceleration, font = 'Agency FB', size = 20, location = '+320+440') %>%
      
      image_annotate('Top speed [km/h]', font = 'Agency FB', size = 20, location = '+20+470') %>% 
      image_annotate(attacking_card$vmax, font = 'Agency FB', size = 20, location = '+320+470')
    
    # select attribute
    selected_attr = paste0(paste0('player_', attacking_player, '_name') %>% 
                             get(), ', select attribute: ') %>% 
      readline()
    
    # show defending card
    defending_stack %>% 
      print()
    
    # get attacking attribute value
    attacking_attr = attacking_card
    select(selected_attr) %>% 
      pull()
    
    # get defending attribute value
    defending_attr = defending_stack %>% 
      tail(1) %>% 
      select(selected_attr) %>% 
      pull()
    
    Sys.sleep(3)
    
    # 1 - attacking player wins, 2 - defending player wins, 0 - draw
    if (attacking_attr > defending_attr) {
      round_result = 1
      round_end = TRUE
    } else if (attacking_attr < defending_attr) {
      round_result = 2
      round_end = TRUE
    } else if (attacking_attr == defending_attr) {
      
      # if at least one of the players doesn't have any more cards select 
      # different attribute for the same cards
      if ('cards_player' %>% 
          paste0(attacking_player) %>% 
          get() %>% 
          nrow() == 0 |
          'cards_player' %>% 
          paste0(defending_player) %>% 
          get() %>% 
          nrow() == 0) {
        
      } else {
        # add two additional cards if possible
        # if one or two players don't ave 2 cards left add just one
        new_cards_n = min(nrow(cards_player1), nrow(cards_player2), 2)
        
        # add cards to attacking stack
        attacking_stack = attacking_stack %>% 
          bind_rows('cards_player' %>% 
                      paste0(attacking_player) %>% 
                      get() %>% 
                      head(new_cards_n))
        
        # remove the cards from attacking player's main stack
        assign('cards_player' %>% 
                 paste0(attacking_player),
               'cards_player' %>% 
                 paste0(attacking_player) %>% 
                 get() %>% 
                 tail(-new_cards_n))
        
        # add cards to defending stack
        defending_stack = defending_stack %>% 
          bind_rows('cards_player' %>% 
                      paste0(defending_player) %>% 
                      get() %>% 
                      head(new_cards_n))
        
        # remove the cards from defending player's main stack
        assign('cards_player' %>% 
                 paste0(defending_player),
               'cards_player' %>% 
                 paste0(defending_player) %>% 
                 get() %>% 
                 tail(-new_cards_n))
      } 
      print('Draw!')
      
    } else {
      round_result = 'error'
      print(round_result)
    }
  }
  
  
  # Invert round results for inverted attributes
  if (round_result %in% c(1, 2) & selected_attr %in% inverted_attrs){
    round_result = setdiff(1:2, round_result)
  } 
  
  if (round_result == 1) {
    winning_player = attacking_player
  } else if (round_result == 2) {
    winning_player = defending_player
  } else {
    round_result = 'error'
    print(round_result)
  }
  
  paste0(paste0('player_', winning_player, '_name') %>% 
           get(), ' wins the round!') %>% 
    print()
  
  Sys.sleep(2)
  
  # winner take all cards that were used in the round
  winners_stack = attacking_stack %>% 
    bind_rows(defending_stack) %>% 
    slice(., sample(1:n()))
  
  if (winning_player == 1){
    cards_player1 = cards_player1 %>% 
      bind_rows(winners_stack)
  } else if (winning_player == 2){
    cards_player2 = cards_player2 %>% 
      bind_rows(winners_stack)
  }
  
  paste0(player_1_name, ' ', nrow(cards_player1), ':', nrow(cards_player2), ' ',
         player_2_name) %>% 
    print()
  print(' ')
  print(' ')
  
  attacking_player = winning_player
  
  if (nrow(cards_player1) == 0){
    paste0(player_2_name, ' wins the game!') %>% 
      print()
  } else if (nrow(cards_player2) == 0){
    paste0(player_1_name, ' wins the game!') %>% 
      print()
  }
}

