% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{implied.proportion}
\alias{implied.proportion}
\title{Function for calculating implied proportions.}
\usage{
implied.proportion(X, outcome, variable)
}
\arguments{
\item{X}{A real number.  This is the number that is picking out one member of
the family of probability functions that are consistent with the fitted
logisitic model.}

\item{outcome}{An integer specifying a particular outcome.  This is one of the
values of the outcome variable (i.e. the dependent variable from the model)}

\item{variable}{A column name from the original data frame.  This will be the
variable one of whose values you are calculating the probability for.}
}
\value{
The calculated probability corresponding to the values you have
 specified.
}
\description{
This utility function calcuates the implied proportions in the population for
the specified outcome, making use of the probabilities for the various
categories of the chosen variable and the chosen value of X.
}
\details{
The real-valued X indexes a family of probability distributions for a given
explanatory variable.  Since we know the distribution of the values of the
chosen variable (from the original dataset, it is calculated in
\code{fit.model} and stored in \code{proportions.raw}), this information is
combined with the probability distributions for the different values of the 
variable to arrive at the implied probability for the chosen outcome.

For example, if we have a gender variable (taking values male and female), and
in our dataset there are equal numbers of males and females, then the implied
proportion for outcome 1 will be the mean of the male probability of outcome 1
and the female probability of outcome 1.  If there were twice as many females
as males, then the implied probability would be weighted towards the female
probability twice as heavily as towards the male.
}
\examples{
implied.proportion(-1.9,2,"gender")
}

