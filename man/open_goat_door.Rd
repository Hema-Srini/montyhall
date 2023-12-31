% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Open a goat door.}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{game}{The current game setup.}

\item{a.pick}{The contestant's selected door.}
}
\value{
A number between 1 and 3 representing the opened door.
}
\description{
Open a goat door.
}
\examples{
open_goat_door(game = c("goat", "car", "goat"), a.pick = 1)

}
