% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lagR.R
\name{lagR}
\alias{lagR}
\title{Compute the Lag of a Time Series (Wrap or Replace)}
\usage{
lagR(input, lag_val, method = "wrap", replacement_vector = FALSE, replace = FALSE)
}
\arguments{
\item{input}{A numerical vector of any length.}

\item{lag_val}{The number of lags (in units of observations).}

\item{method}{Takes either 'blank' or 'wrap' as values. The default is 'wrap.'
Wrap moves the last n values to the front of the time series–as determined by the lag_val.
Blank will shift the time series by lag_val positions and either have blank NaN values in the first lag_val places
or it will replace them with the values of your choice.}

\item{replacement_vector}{Vector of numerical values to replace NaNs for 'blank' method when 'replace' is TRUE. Must have the same number of elements as specified by lag_val (i.e., length(replacement_vector==lag_val))}

\item{replace}{Takes either TRUE or FALSE. TRUE will replace NaN values created by 'blank' method, whereas FALSE will leave them.
Replace cannot be used in the wrap method.}
}
\value{
A numerical vector lagged by lag_val (in units of observation).
}
\description{
lagR computes the lagged version of a time series or generic vector
by shifting the values back by a given number of observations. It then can
either 1) replace the lagged observations with an NaN or the inputs of
your choice (e.g., random values from a distribution) or 2) wrap the
last n values to the front of the time series–as determined by the
desired lag value. This package avoids the specific errors occasionally
caused by misuse of the stats::lag package, which is a generic function that
adds a tsp attribute via hasTsp to the time series by treating all inputs
and outputs as plain vectors. It also provides more functionality than
TSA::zlag by allowing users to wrap their time series or replace missing
lagged values in one function.
}
\details{
The original input is saved as a list output of the function as 'output$original_vector.' The input must be a single numerical vector.
}
\note{
Note that the sign of the lag_val matters. While both negative and positive values are acceptable, a negative value will move a time series up. Wrap is recommended with negative lags.
If you do not wrap the values, the first lag_val elements will be deleted, which may or may not be desireable. Regardless, the original vector is saved in output$original_vector.
}
\examples{

#' 
# Example #1: 'blank' method with no replacement
input<- rnorm(25)
lag_val <- 5
replacement_vector <- rnorm(lag_val,1,2)
output<-lagR(input,lag_val,method='blank', replace=TRUE, replacement_vector)
print(output)


# Example #2: 'blank' method with no replacement

input<- rnorm(25)
lag_val=5
output<-lagR(input,lag_val,method='blank', replace=FALSE)
print(output)

# Example #3: 'wrap' method
input<- rnorm(25)
lag_val=5
output<-lagR(input,lag_val,method='wrap')
print(output)



}
