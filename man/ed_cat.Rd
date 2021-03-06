% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/ed_cat.r
\name{ed_cat}
\alias{ed_cat}
\title{Categorize a continuous education variable into common categories.}
\usage{
ed_cat(data, edvar, groups = "basic", factor = TRUE)
}
\arguments{
\item{data}{Data Frame with the education variable to categorize.}

\item{edvar}{Name of the education variable.}

\item{groups}{Categorization Scheme: more, basic, higher}

\item{factor}{Return a factor or not.  Defaults to TRUE}
}
\value{
A factor, unless otherwise specified, with recoded \code{edvar}.
}
\description{
Categorize a continuous education variable into common categories.
}
\examples{
ed_cat()
}
\keyword{education,}
\keyword{recode}

