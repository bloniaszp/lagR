#' Compute the Lag of a Time Series (Wrap or Replace)
#'
#'@description lagR computes the lagged version of a time series or generic vector 
#' by shifting the values back by a given number of observations. It then can 
#' either 1) replace the lagged observations with an NaN or the inputs of 
#' your choice (e.g., random values from a distribution) or 2) wrap the 
#' last n values to the front of the time series–as determined by the 
#' desired lag value. This package avoids the specific errors occasionally 
#' caused by misuse of the stats::lag package, which is a generic function that 
#' adds a tsp attribute via hasTsp to the time series by treating all inputs 
#' and outputs as plain vectors. It also provides more functionality than 
#' TSA::zlag by allowing users to wrap their time series or replace missing 
#' lagged values in one function. 
#' 
#' @usage 
#' lagR(input, lag_val, method = "wrap", replacement_vector = FALSE, replace = FALSE)
#' 
#' @details 
#' 
#' The original input is saved as a list output of the function as 'output$original_vector.' The input must be a single numerical vector.
#' 
#'
#' @param input A numerical vector of any length.
#' @param lag_val The number of lags (in units of observations).
#' @param method Takes either 'blank' or 'wrap' as values. The default is 'wrap.' 
#' Wrap moves the last n values to the front of the time series–as determined by the lag_val. 
#' Blank will shift the time series by lag_val positions and either have blank NaN values in the first lag_val places
#' or it will replace them with the values of your choice. 
#' @param replace Takes either TRUE or FALSE. TRUE will replace NaN values created by 'blank' method, whereas FALSE will leave them.
#' Replace cannot be used in the wrap method. 
#' @param replacement_vector Vector of numerical values to replace NaNs for 'blank' method when 'replace' is TRUE. Must have the same number of elements as specified by lag_val (i.e., length(replacement_vector==lag_val))
#' @note Note that the sign of the lag_val matters. While both negative and positive values are acceptable, a negative value will move a time series up. Wrap is recommended with negative lags. 
#' If you do not wrap the values, the first lag_val elements will be deleted, which may or may not be desireable. Regardless, the original vector is saved in output$original_vector.
#' @return A numerical vector lagged by lag_val (in units of observation). 
#' 
#' @examples 
#' 
#' #' 
#' # Example #1: 'blank' method with no replacement
#' input<- rnorm(25)
#' lag_val <- 5
#' replacement_vector <- rnorm(lag_val,1,2)
#' output<-lagR(input,lag_val,method='blank', replace=TRUE, replacement_vector)
#' print(output)
#' 
#' 
#' # Example #2: 'blank' method with no replacement
#' 
#' input<- rnorm(25)
#' lag_val=5
#' output<-lagR(input,lag_val,method='blank', replace=FALSE)
#' print(output)
#' 
#' # Example #3: 'wrap' method
#' input<- rnorm(25)
#' lag_val=5
#' output<-lagR(input,lag_val,method='wrap')
#' print(output)
#' 
#' 
#' 


#' @export
lagR <- function(input, lag_val, method = "wrap", replacement_vector = FALSE, replace = FALSE){
  num = lag_val
  
  if(num > 0) {
    run = "positive"
  } else {
    if(num == 0) {
      run = "zero"
    } else {
      run = "negative"
    }
  }
  
  
  if (num == "positive"){
    if(typeof(input)!="double" & typeof(input)!="numeric"){
      stop("Input can only take a single, numerical vector.")
    }
    
    else if (lag_val==length(input)){
      stop("You cannot make the lag value the same as the length of your input vector. The largest value you can  pick is ", length(input)-1,"for your useage.")
    }
    else if (lag_val>length(input)){
      stop("You cannot make the lag value larger than the length of your input vector. The largest value you can  pick is ", length(input)-1,"for your useage.")
    }
    
    if (method == "wrap"){
      if(replace == TRUE){
        stop("You cannot use 'replace' with the wrap function. Replace is only compatible with the 'blank' method to generated random values to be placed in the blank NaN indexes. Please either change method to 'blank' or change replace to False")
      }
      else if(replacement_vector != FALSE){
        stop("You cannot use 'replacement_vector' with the wrap function. 'Replacement_vector' is only compatible with the 'blank' method to generated random values to be placed in the blank NaN indexes. Please either change method to 'blank' or change 'replacement_vector' to NULL")
      }
      vector_length <- length(input)
      original_vector <- input
      lagged <- rep(NaN, abs(lag_val))
      manipulated_vector<- c(lagged, input)
      values_to_be_wrapped<-input[((length(input)-length(lagged))+1):length(input)]
      manipulated_vector[1:lag_val]<- values_to_be_wrapped
      manipulated_vector<-head(manipulated_vector,-lag_val)
      final_vector <-manipulated_vector
    }
    
    if (method == "blank"){
      if(typeof(replacement_vector)=="logical"){
        if(replace == TRUE & replacement_vector ==FALSE){
          stop("You cannot use 'replace' without specifying a replacement_vector (e.g., rnorm(lag_val,mean=0,sd=1)). Please adjust the argument.")
        }
      }
      else if(replace == FALSE & (typeof(replacement_vector)!="double")){
        stop("You cannot use replacement_vector without setting 'replace' to true. Please adjust the argument by either setting 'replacement_vector' to FALSE or making 'replace' TRUE.")
      }
      else if((typeof(replacement_vector)!="double") | (typeof(replacement_vector)==FALSE)){
        stop("A valid input for 'replacement_vector' is a vector of numbers corresponding to the number of NaNs being introduced or a replacement_vector (e.g., rnorm(lag_val,mean=0,sd=1))")
      }
      
      vector_length <- length(input)
      original_vector <- input
      lagged <- rep(NaN, abs(lag_val))
      manipulated_vector<- c(lagged, input)
      
      if(replace==TRUE & length(replacement_vector)==lag_val){
        manipulated_vector[1:lag_val]<-replacement_vector
      }
      else if(replace ==TRUE &length(replacement_vector)!=lag_val){
        stop("Replacement_vector length must be the same length as your log_val input.")
      }
      else if(replace == TRUE & as.character(sum(manipulated_vector))=="NaN"){
        stop("'Replace' was set to true, but there are still NaN values in the vector. Please either pick a different vector for replacement_vector or correct the replacement_vector arguments. Most likely, either the shape of the replacement_vector (e.g., the mean and sd) are incorrect or the lag_val in your replacement_vector does not match the specified lag_val earlier in the function.")
      }
      else if(replace==FALSE & typeof(replacement_vector)!="logical"){
        warning("Replacement_vector was not used, meaning it is redundant. Check to see if you meant to make 'replace' TRUE")
      }
      final_vector <-manipulated_vector
      
    }
    return(list(original_vector = original_vector, lagged = final_vector))
  }
  else if (num == "zero"){
    stop("A lag of 0 is just reproducing the same time series, please select a proper lag_val")
    
    
  }else{
    if(typeof(input)!="double" & typeof(input)!="numeric"){
      stop("Input can only take a single, numerical vector.")
    }
    
    else if (abs(lag_val)==length(input)){
      stop("You cannot make the lag value the same as the length of your input vector. The largest value you can  pick is ", length(input)-1,"for your useage.")
    }
    else if (abs(lag_val)>length(input)){
      stop("You cannot make the lag value larger than the length of your input vector. The largest value you can  pick is ", length(input)-1,"for your useage.")
    }
    
    if (method == "wrap"){
      if(replace == TRUE){
        stop("You cannot use 'replace' with the wrap function. Replace is only compatible with the 'blank' method to generated random values to be placed in the blank NaN indexes. Please either change method to 'blank' or change replace to False")
      }
      else if(replacement_vector != FALSE){
        stop("You cannot use 'replacement_vector' with the wrap function. 'Replacement_vector' is only compatible with the 'blank' method to generated random values to be placed in the blank NaN indexes. Please either change method to 'blank' or change 'replacement_vector' to NULL")
      }
      original_vector <- input
      vector_length <- length(input)
      original_vector <- input
      lagged <- rep(NaN, abs(lag_val))
      manipulated_vector<- c(input,lagged)
      manipulated_vector[(length(manipulated_vector)-abs(lag_val)+1):length(manipulated_vector)]<-manipulated_vector[1:abs(lag_val)]
      manipulated_vector<-tail(manipulated_vector,-abs(lag_val))
      final_vector <-manipulated_vector
      
    }
    
    if (method == "blank"){
      if(typeof(replacement_vector)=="logical"){
        if(replace == TRUE & replacement_vector ==FALSE){
          stop("You cannot use 'replace' without specifying a replacement_vector (e.g., rnorm(abs(lag_val),mean=0,sd=1)). Please adjust the argument.")
        }
      }
      else if(replace == FALSE & (typeof(replacement_vector)!="double")){
        stop("You cannot use replacement_vector without setting 'replace' to true. Please adjust the argument by either setting 'replacement_vector' to FALSE or making 'replace' TRUE.")
      }
      else if((typeof(replacement_vector)!="double") | (typeof(replacement_vector)==FALSE)){
        stop("A valid input for 'replacement_vector' is a vector of numbers corresponding to the number of NaNs being introduced or a replacement_vector (e.g., rnorm(lag_val,mean=0,sd=1))")
      }
      
      warning("Note that a negative lag_val with the 'blank' method will delete the first lag_val elements of the time series. 
        The original data is stored in  output$original_vector.")
      original_vector <- input
      vector_length <- length(input)
      original_vector <- input
      lagged <- rep(NaN, abs(lag_val))
      manipulated_vector<- c(input,lagged)
      manipulated_vector<-tail(manipulated_vector,-abs(lag_val))
      
      if(replace==TRUE & length(replacement_vector)==abs(lag_val)){
        manipulated_vector[(length(manipulated_vector)-abs(lag_val)+1):length(manipulated_vector)]<-replacement_vector
      }
      else if(replace == TRUE & length(replacement_vector)!=abs(lag_val)){
        stop("Replacement_vector length must be the same length as your log_val input.")
      }
      else if(replace == TRUE & as.character(sum(manipulated_vector))=="NaN"){
        stop("'Replace' was set to true, but there are still NaN values in the vector. Please either pick a different vector for replacement_vector or correct the replacement_vector arguments. Most likely, either the shape of the replacement_vector (e.g., the mean and sd) are incorrect or the lag_val in your replacement_vector does not match the specified lag_val earlier in the function.")
      }
      else if(replace==FALSE & typeof(replacement_vector)!="logical"){
        warning("Replacement_vector was not used, meaning it is redundant. Check to see if you meant to make 'replace' TRUE")
      }
      final_vector <-manipulated_vector
      
    }
    return(list(original_vector = original_vector, lagged = final_vector))
  }
}

