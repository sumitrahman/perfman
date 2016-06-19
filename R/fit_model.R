#'Fitting the model
#'
#'\code{fit.model} fits the ordered logistic model and sets up some useful 
#'variables for the other \code{perfman} functions.
#'
#'The function fits the model using \code{MASS:polr}.  This is saved to
#'\code{fit}.
#'
#'Then the vector \code{cuts} is created which are the cuts from the model
#'(these divide the cumulative density into the areas set for the ordered
#'outcomes), including \code{-Inf} for the zeroth cut and \code{Inf} for the end
#'point.  This makes it convenient for another \code{perfman} function to use.
#'
#'Next two sets of tables are created which create the marginal distributions
#'from the dataset that is being analysed.  \code{counts.raw} gives you the
#'actual counts, while \code{proportions.raw} gives you the proportions.
#'
#'Finally a vector of variables is created, again for the convenience of other 
#'functions in \code{perfman}.  This vector simply contains the names of the 
#'explanatory variables from the model.
#'
#'@param dataset A data frame whose first column is an ordered numeric factor
#'  containing the positive integers and whose subsequent columns are the
#'  explanatory variables for modelling. For analysis
#'  in this package all the explanatory variables must be factors.
#'
#'@return An object of class \code{polr}.  See documentation for the \code{polr}
#'  function in the \code{MASS} package for more details.
#'
#'@examples
#'\dontrun{
#'fit.model(test.data)
#'}
fit.model<-function(dataset){

#fit the ordered logistic model
fit<-polr(data = dataset)

#create the cuts as a numeric vector
cuts<-numeric(length(fit$zeta)+2)
cuts[2:(length(fit$zeta)+1)]<-fit$zeta
cuts[c(1,length(cuts))]<-c(-Inf,Inf)

#get observed proportions of each variable in the data (including outcome)
counts.raw<-apply(dataset,2,table)
proportions.raw<-lapply(counts.raw,FUN = function(x){x/nrow(dataset)})

#create a vector of names of explanatory variables
variables<-attr(fit$terms,"term.labels")

}
