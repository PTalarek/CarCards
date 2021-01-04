#' Draw a card from player's deck
#'
#' @param player_role Character indicating player's role (either \code{attacking}
#'  or \code{defending})
#' @param n Number of cards to draw
#' @importFrom dplyr  %>%
#' @return A list with 2 updated player's data frames:
#' - attacking/defending stack (player's cards that are used in the current round)
#' - players cards (that are not used in the current round)
#' @export
draw_cards = function(player_role, n){


  assign(paste0(player_role, '_stack'),
         'cards_player' %>%
           paste0(
             paste0(player_role, '_player') %>%
               get()) %>%
           get() %>%
           head(n))

  # remove the card from attacking player's main stack
  assign('cards_player' %>%
           paste0(
             paste0(player_role, '_player') %>%
               get()),
         'cards_player' %>%
           paste0(
             paste0(player_role, '_player') %>%
               get()) %>%
           get() %>%
           tail(-1))

  return(list(paste0(player_role, '_stack') %>%
                get(),
              'cards_player' %>%
                paste0(
                  paste0(player_role, '_player') %>%
                    get()) %>%
                get()) %>%
           `names<-`(c(paste0(player_role, '_stack'),
                      'cards_player' %>%
                        paste0(
                          paste0(player_role, '_player') %>%
                            get()))))
}

#' Compile card image
#'
#' @param card 1-row data frame with card to be visualized
#' @importFrom dplyr  %>%
#' @importFrom magick image_annotate image_composite image_read
#' @return Compiled card image
#' @export
get_card_image = function(card){

  car_name = card$name
  card_frame = image_read('D:/R/CarCards/img/card_frame.png')
  card_image = image_read('D:/R/CarCards/img/pic1.png')
  attacking_card_image = image_composite(card_frame, card_image, gravity = 'North', offset = '+0+10') %>%
    image_annotate(car_name, font = 'Agency FB', size = 27, location = '+20+230') %>%

    image_annotate('Engine capacity [ccm]', font = 'Agency FB', size = 20, location = '+20+290') %>%
    image_annotate(card$capacity, font = 'Agency FB', size = 20, location = '+320+290') %>%

    image_annotate('Cylinders', font = 'Agency FB', size = 20, location = '+20+320') %>%
    image_annotate(card$cylinders, font = 'Agency FB', size = 20, location = '+320+320') %>%

    image_annotate('Engine type', font = 'Agency FB', size = 20, location = '+20+350') %>%
    image_annotate(card$engine, font = 'Agency FB', size = 20, location = '+320+350') %>%

    image_annotate('Power [HP]', font = 'Agency FB', size = 20, location = '+20+380') %>%
    image_annotate(card$power, font = 'Agency FB', size = 20, location = '+320+380') %>%

    image_annotate('Weight [kg]', font = 'Agency FB', size = 20, location = '+20+410') %>%
    image_annotate(card$weight, font = 'Agency FB', size = 20, location = '+320+410') %>%

    image_annotate('Acceleration 0-100 km/h [s]', font = 'Agency FB', size = 20, location = '+20+440') %>%
    image_annotate(card$acceleration, font = 'Agency FB', size = 20, location = '+320+440') %>%

    image_annotate('Top speed [km/h]', font = 'Agency FB', size = 20, location = '+20+470') %>%
    image_annotate(card$vmax, font = 'Agency FB', size = 20, location = '+320+470')
}
