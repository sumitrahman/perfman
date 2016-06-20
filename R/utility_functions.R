#'Coefficient lookup.
#'
#'This utility function is used by other functions to lookup coefficients that
#'have been estimated by \code{fit.model}.  The estimates are found in the 
#'\code{polr} class object \code{fit}.  Baseline categories return a value of
#'zero.
#'
#'The function checks that the variable is in the fitted model, and that the
#'value for that variable is one that has been included in the model.
#'
#'@param variable A column name from the original data frame.  This will be the
#'  variable one of whose values you are looking up the coefficient for.
#'
#'@param value A specific value of the variable under consideration.  This value
#'  must be one of the values actually used in the data frame for 
#'  \code{fit.model}.
#'
#'@return A numeric value, zero for a baseline category or the estimated
#'  coefficient from the fitted logistic model.  These estimates are logarithms
#'  of the cumulative logit (once you add the relevent cut).
#'
#'@examples
#'coef.lookup("gender","female")
coef.lookup<-function(variable,value){
    if (!(variable %in% variables)) {
        stop("variable not in model")
    } else if (!(value %in% fit$xlevels[[variable]])) {
        stop("value not observed for this variable")
    } else if (fit$xlevels[[variable]][[1]]==value) {
        return (0)
    } else
        return (fit$coefficients[[paste0(variable,value)]])
}


#'Probability function for perfman.
#'
#'This utility function calcuates the probability of a specific outcome for a
#'full set of values for the explanatory variables in the fitted model.
#'
#'This function is best thought of as a real-valued function of a single
#'variable X, with the function being specified by variable, value and outcome.
#'For a particular value of an explanatory variable, each value of X will
#'return a probability, and the same value of X will, for a different value of
#'the same variable, return a probability that is consistent with the set of
#'estimated coefficients from the logistic model that has been fitted.
#'(The value of the outcome variable is also taken account of too.)
#'
#'@param X A real number.  This is the number that is picking out one member of
#'  the family of probability functions that are consistent with the fitted
#'  logisitic model.
#'
#'@param outcome An integer specifying a particular outcome.  This is one of the
#'  values of the outcome variable (i.e. the dependent variable from the model)
#'
#'@param variable A column name from the original data frame.  This will be the
#'  variable one of whose values you are calculating the probability for.
#'
#'@param value A specific value of the variable under consideration.  This value
#'  must be one of the values actually used in the data frame for 
#'  \code{fit.model}.  This is the specific category you are calculating the
#'  probability for (for your chosen outcome).
#'
#'@return The calculated probability corresponding to the values you have
#'  specified.
#'@examples
#'P.X(-1.9,2,"gender","female")
#'
#'@export
P.X<-function(X,outcome,variable,value){
    (1+exp(X+coef.lookup(variable,value)-cuts[outcome+1]))^-1-
    (1+exp(X+coef.lookup(variable,value)-cuts[outcome]  ))^-1
}

#'Function for calculating implied proportions.
#'
#'This utility function calcuates the implied proportions in the population for
#'the specified outcome, making use of the probabilities for the various
#'categories of the chosen variable and the chosen value of X. 
#'
#'The real-valued X indexes a family of probability distributions for a given
#'explanatory variable.  Since we know the distribution of the values of the
#'chosen variable (from the original dataset, it is calculated in
#'\code{fit.model} and stored in \code{proportions.raw}), this information is
#'combined with the probability distributions for the different values of the 
#'variable to arrive at the implied probability for the chosen outcome.
#'
#'For example, if we have a gender variable (taking values male and female), and
#'in our dataset there are equal numbers of males and females, then the implied
#'proportion for outcome 1 will be the mean of the male probability of outcome 1
#'and the female probability of outcome 1.  If there were twice as many females
#'as males, then the implied probability would be weighted towards the female
#'probability twice as heavily as towards the male.
#'
#'@param X A real number.  This is the number that is picking out one member of
#'  the family of probability functions that are consistent with the fitted
#'  logisitic model.
#'
#'@param outcome An integer specifying a particular outcome.  This is one of the
#'  values of the outcome variable (i.e. the dependent variable from the model)
#'
#'@param variable A column name from the original data frame.  This will be the
#'  variable one of whose values you are calculating the probability for.
#'
#'@return The calculated probability corresponding to the values you have
#'  specified.
#'
#'@examples
#'implied.proportion(-1.9,2,"gender")
implied.proportion<-function(X,outcome,variable){
    implieds<-rep(0,length(fit$xlevels[[variable]]))
    for (i in fit$xlevels[[variable]]){
        implieds[i]<-P.X(X=X,outcome = outcome,variable = variable,value = i)*
            proportions.raw[[variable]][[i]]
    }
    return(sum(implieds))
}

#'Function for calculating the 'distance' between two distributions.
#'
#'This utility function calcuates the distance between the implied distribution
#'of outcomes for a specific value of X and a specific variable, and the
#'observed distribution of outcomes from the original data.  The distance
#'function being used is the sum of squared differences.
#'
#'The real-valued X indexes a family of probability distributions for a given
#'explanatory variable.  By specifying a variable, the function 
#'\code{implied.proportion} can produce implied probabilities, and these are
#'compared with the observed distribution for each outcome.
#'
#'The purpose of this function is to have a function of X that we can seek to 
#'minimise.  This is precisely what the \code{recommend} function does.
#'
#'@param X A real number.  This is the number that is picking out one member of
#'  the family of probability functions that are consistent with the fitted
#'  logisitic model.
#'
#'@param variable A column name from the original data frame.  This will be the
#'  variable one of whose values you are calculating the probability for.
#'
#'@return The sum of the squared differences between the observed distribution
#'of outcomes and the distribution of outcomes implied by your choice of X and
#'variable of interest.
#'
#'@examples
#'squared.error(-1.9,"gender")
squared.error<-function(X,variable){
    outcome.errors<-rep(0,length(fit$lev))
    for (i in 1:length(fit$lev)){
        outcome.errors[i]<-
            (implied.proportion(X=X,outcome = i,variable = variable)-
                 proportions.raw[[1]][[i]])^2
    }
    return(sum(outcome.errors))
}

#'Function for recommending the modelled probabilities to use.
#'
#'This function recommends the choice of modelled probabilities for the chosen
#'variable.
#'
#'This function selects the member of the family of probability distributions
#'that are consistent with the model fitted by \code{fit.model} which is closest
#'to the overall observed distribution in the original dataset.  The resulting
#'probabilities are what we call \emph{corrected probabilities} and can be
#'thought of as the probability distributions for each value of the chosen
#'variable having corrected for the confounding effects of the other variables
#'in the model.
#'
#'There is text output displaying the recommended value of X - this is the
#'value that is used to generate the specific probabilities from the family of
#'probability distributions that are consistent with the logisitc effects for 
#'the variable of interest in the fitted model.  Then a data frame is created 
#'(and displayed) that gives the probability distribution for each value of the
#'variable of interest, and the implied distribution of outcomes (which is the
#'mean of the various probability distributions, weighted by the relative
#'frequencies of each particular value of the variable in the original dataset).
#'
#'@param variable A column name from the original data frame.  This will be the
#'  variable one of whose values you are calculating the probability for.
#'
#'@return A data frame called \code{optimised.probs} that gives the recommended
#'probabilities for each outcome and each value of the chosen variable, as well
#'as the implied distribution for outcomes given these recommended 
#'probabilities.
#'
#'@examples
#'recommend("gender")
#'
#'@export
recommend<-function(variable, lower.bound=-100, upper.bound=100){
    if (!(variable %in% variables)) {
        stop("variable not in model")
    }
    optimised<-optimise(f = function(x){squared.error(x,variable)},
                        interval = c(min(lower.bound, upper.bound),max(lower.bound, upper.bound)))
    message("The recommended value of X is ",optimised$minimum)

#create data frame with outcomes along the top and variable values down the side
    optimised.probs<-data.frame(sapply(fit$xlevels[[variable]],
                                FUN=function(q){
                                    P.X(X=optimised$minimum,
                                        outcome = 1:length(fit$lev),
                                        variable = variable,
                                        value = q)}))
    optimised.probs<-cbind(1:length(fit$lev),
                           sapply(1:length(fit$lev), FUN=function(q){
                               implied.proportion(X=optimised$minimum,
                                                  outcome = q,
                                                  variable = variable)}),
                           optimised.probs)
    names(optimised.probs)[1]<-names(fit$model[1])
    names(optimised.probs)[2]<-"implied probability"

    optimised.probs
}
