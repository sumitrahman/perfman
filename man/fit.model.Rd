% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_model.R
\name{fit.model}
\alias{fit.model}
\title{Fitting the model}
\usage{
fit.model(dataset)
}
\arguments{
\item{dataset}{A data frame whose first column is an ordered numeric factor
containing the positive integers and whose subsequent columns are the
explanatory variables for modelling. For analysis
in this package all the explanatory variables must be factors.}
}
\value{
An object of class \code{polr}.  See documentation for the \code{polr}
 function in the \code{MASS} package for more details.
}
\description{
\code{fit.model} fits the ordered logistic model and sets up some useful 
variables for the other \code{perfman} functions.
}
\details{
The function fits the model using \code{MASS::polr}.  This is saved to
\code{fit}.

Then the vector \code{cuts} is created which are the cuts from the model
(these divide the cumulative density into the areas set for the ordered
outcomes), including \code{-Inf} for the zeroth cut and \code{Inf} for the end
point.  This makes it convenient for another \code{perfman} function to use.

Next two sets of tables are created which create the marginal distributions
from the dataset that is being analysed.  \code{counts.raw} gives you the
actual counts, while \code{proportions.raw} gives you the proportions.

Finally a vector of variables is created, again for the convenience of other 
functions in \code{perfman}.  This vector simply contains the names of the 
explanatory variables from the model.
}
\examples{
fit.model(test.data)
}

