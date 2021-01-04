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
