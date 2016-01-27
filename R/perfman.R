require(MASS)

# require(foreign)
# dat <- read.dta("http://www.ats.ucla.edu/stat/data/ologit.dta")

test.data <- read.csv("~/GitHub/perfman/test data.csv")
test.data$age <- factor(test.data$age, levels = c(1,2,3,4))
test.data$outcome <- factor(test.data$outcome, levels = c(1,2,3,4))


#ask for what to do with missing values
#default is to replace all missing values NA with "NA"

#fit the ordered logistic model
fit<-polr(data = test.data)

#create the cuts as a numeric vector
cuts<-numeric(length(fit$zeta)+2)
cuts[2:(length(fit$zeta)+1)]<-fit$zeta
cuts[c(1,length(cuts))]<-c(-Inf,Inf)

#create a vector of names of explanatory variables
variables<-attr(fit$terms,"term.labels")

#create the coefficient lookup function
#a convenient function for reading off the estimated coefficients for all
#the explanatory variables (including zero for baseline values)
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

#create function P.X(X, outcome, variable, value) that will be treated
#as a function of X and give a specific probability for given outcome k for 
#specific value of a specific variable
#what happens with invalid variables/values??
P.X<-function(X,outcome,variable,value){
    (1+exp(X+coef.lookup(variable,value)-cuts[outcome+1]))^-1-(1+exp(X+coef.lookup(variable,value)-cuts[outcome]))^-1
}

#get observed proportions of each variable in the data (including outcome)
counts.raw<-apply(test.data,2,table)
proportions.raw<-lapply(counts.raw,FUN = function(x){x/nrow(test.data)})


#create fucntion proportions.implied(X, outcome=k, variable=v)
#that will give the implied proportion for outcome k in BIS assuming X and v

#GIVEN VARIABLE e,g, GENDER, what is the implied proportion for outcome k?

implied.proportion<-function(X,outcome,variable){
    implieds<-rep(0,length(fit$xlevels[[variable]]))
    for (i in fit$xlevels[[variable]]){
        implieds[i]<-P.X(X=X,outcome = outcome,variable = variable,value = i)*
            proportions.raw[[variable]][[i]]
    }
    return(sum(implieds))
}

#GIVEN VARIABLE, find the sqaured error as a function of X

squared.error<-function(X,variable){
    outcome.errors<-rep(0,length(fit$lev))
    for (i in 1:length(fit$lev)){
        outcome.errors[i]<-
            (implied.proportion(X=X,outcome = i,variable = variable)-
                 proportions.raw[[1]][[i]])^2
    }
    return(sum(outcome.errors))
}

#find answer for given variable
recommend<-function(variable){
    if (!(variable %in% variables)) {
        stop("variable not in model")
    }
    optimised<-optimise(f = function(x){squared.error(x,variable)},
                        interval = c(-100,100))
    message("The recommended value of X is ",optimised$minimum)
#create data frame with outcomes along the top and variable values down the side
#    optimised.probs<-matrix(nrow = length(fit$lev),
 #                           ncol = length(fit$xlevels[[variable]]))
    
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
