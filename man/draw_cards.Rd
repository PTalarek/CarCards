% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/play.R
\name{draw_cards}
\alias{draw_cards}
\title{Draw a card from player's deck}
\usage{
draw_cards(player_role, n)
}
\arguments{
\item{player_role}{Character indicating player's role (either \code{attacking}
or \code{defending})}

\item{n}{Number of cards to draw}
}
\value{
A list with 2 updated player's data frames:
\itemize{
\item attacking/defending stack (player's cards that are used in the current round)
\item players cards (that are not used in the current round)
}
}
\description{
Draw a card from player's deck
}
