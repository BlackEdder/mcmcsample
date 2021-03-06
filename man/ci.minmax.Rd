% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/credible.R
\name{ci.minmax}
\alias{ci.minmax}
\title{Calculate samples outside credibility region using minmax method}
\usage{
ci.minmax(samples, max.outside = 1)
}
\arguments{
\item{samples}{Data frame holding the posterior samples. Each row is a sample, each column a parameter in the sample}

\item{max.outside}{Number of samples should lie outside}
}
\value{
A boolean vector, with true for samples inside the credibility region
}
\description{
Calculate which samples will fall outside a credibility region.
}

