% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jednostki_package.R
\name{cisnienie_mmHg}
\alias{cisnienie_mmHg}
\title{Konwersja cisnienia (milimetr slupa rteci)}
\usage{
cisnienie_mmHg(x)
}
\arguments{
\item{x}{argument zawierajacy wartosc milimetrow slupa rteci}
}
\value{
data frame z pozostalymi jednostkami cisnienia i ich wartosci
}
\description{
Funkcja sluzaca do konwersji milimetrow slupa rteci na pozostale jednostki cisnienia
}
\examples{
cisnienie_mmHg(20)
cisnienie_mmHg(c(40, 95))
}
