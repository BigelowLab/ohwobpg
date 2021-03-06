% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{write_database}
\alias{write_database}
\title{Write a database to file as CSV (possible compressed)}
\usage{
write_database(x, path = ".", filename = "database.csv.gz")
}
\arguments{
\item{x}{data frame (tibble), the data to store}

\item{path}{character, the path to the database, by default "."}

\item{filename}{character, by default 'database.csv.gz' but it can be any other filename}
}
\description{
Write a database to file as CSV (possible compressed)
}
