% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jednostki_package.R
\name{temperatura_Re}
\alias{temperatura_Re}
\title{Konwersja temperatury (stopnie Reaumura)}
\usage{
temperatura_Re(x)
}
\arguments{
\item{x}{argument zawierajacy wartosc stopni Reaumura}
}
\value{
data frame z pozostalymi jednostkami temperatury i ich wartosci
}
\description{
Funkcja sluzaca do konwersji stopni Reaumura na pozostale jednostki temperatury
}
\examples{
temperatura_Re(20)
temperatura_Re(c(40, 95))
}
