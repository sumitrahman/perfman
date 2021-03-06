% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{recommend}
\alias{recommend}
\title{Function for recommending the modelled probabilities to use.}
\usage{
recommend(variable, lower.bound = -100, upper.bound = 100)
}
\arguments{
\item{variable}{A column name from the original data frame.  This will be the
variable one of whose values you are calculating the probability for.}
}
\value{
A data frame called \code{optimised.probs} that gives the recommended
probabilities for each outcome and each value of the chosen variable, as well
as the implied distribution for outcomes given these recommended 
probabilities.
}
\description{
This function recommends the choice of modelled probabilities for the chosen
variable.
}
\details{
This function selects the member of the family of probability distributions
that are consistent with the model fitted by \code{fit.model} which is closest
to the overall observed distribution in the original dataset.  The resulting
probabilities are what we call \emph{corrected probabilities} and can be
thought of as the probability distributions for each value of the chosen
variable having corrected for the confounding effects of the other variables
in the model.

There is text output displaying the recommended value of X - this is the
value that is used to generate the specific probabilities from the family of
probability distributions that are consistent with the logisitc effects for 
the variable of interest in the fitted model.  Then a data frame is created 
(and displayed) that gives the probability distribution for each value of the
variable of interest, and the implied distribution of outcomes (which is the
mean of the various probability distributions, weighted by the relative
frequencies of each particular value of the variable in the original dataset).
}
\examples{
recommend("gender")

}

