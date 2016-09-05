#' Artificial dataset of performance outcomes and demographic variables.
#'
#' This dataset has been created to try out the \code{perfman} package
#' functions on.  It illustrates the format that the package currently requires
#' the data to be in to work straightforwardly.
#' 
#' Each row represents one person.  This person's outcome from the performance
#' management process is recorded: this is a whole number between 1 and 4 where
#' 1 represents the worst score and 4 the best.  Each person has observations
#' recorded of their gender, ethnicity, disability and age.  Ethnicity and
#' disability are self-reported and suffer from non-response, but the
#' non-response categories are coded for as separate categories (and not coded
#' using \code{NA} or similar).
#' 
#' It is important to note that the first column has to be called \code{outcome}
#' and has to be an ordered factor using integers starting from 1 in sequence.  
#' Note, too, that the other columns represent variables that will be fitted in
#' the ordered logistic model, and they need to be caegorical (and therefore 
#' represented using factors).
#'
#' @format A data frame with 200 rows and 5 variables.  Each row represents a 
#' person.  The first column is an ordered factor containing all the integers 
#' from 1 to 4.
#' \describe{
#'   \item{outcome}{Result from the performance management process for this 
#'   person, where 1 is the worst category and 4 the best.  There are four 
#'   possible outcomes.}
#'   \item{gender}{Gender of the person.  A variable with two factors
#'      \code{"female"} and \code{"male"}}
#'   \item{ethnicity}{Ethnicity of the person (self-reported).  A variable with 
#'      4 factors: \code{"BAME"} for Black, Asian or Minority Ethnicity; 
#'      \code{"WB"} for White British; \code{"PNS"} for people who declared they
#'      preferred not to say; and \code{"NK"} where no declaration has been made ('not 
#'      known').}
#'   \item{disability}{Disability status of the person (self-reported).  A 
#'      variable with 4 factors: \code{"disabled"}; \code{"not disabled"};
#'      \code{"PNS"} for people who declared they preferred not
#'      to say; and \code{"NK"} where no declaration has been made ('not 
#'      known').}
#'   \item{age}{Age of person, coded as \code{1} for 16-29 year-olds; \code{2} 
#'      for 30-39 year-olds; \code{3} for 40-49 year-olds; and \code{4} for 
#'      staff older than 49.}
#' }
#'
#' @docType data
#' @usage data(test.data)
"test.data"

