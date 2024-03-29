% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment_functions.R
\name{multiple_coef}
\alias{multiple_coef}
\title{Runs a multiple experiment on artificial data in order to retrieve model coefficients.}
\usage{
multiple_coef(ds, L = 50, verbose = FALSE)
}
\arguments{
\item{ds}{A function generating a dataset, the features should be in columns 'x1', 'x2', ..., and the target variable should be in column 'y'}

\item{L}{Number of repetitions.}

\item{verbose}{A logical value indicating whether we want to print the string
or not.}
}
\value{
A dataframe with model coefficients
}
\description{
Runs a multiple experiment on artificial data in order to retrieve model coefficients.
}
