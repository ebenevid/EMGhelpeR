##' @title Moving Root Mean Square Filter
##' @description \code{moving_RMS} implements a moving root mean square filter on the input signal and returns the transform data. This function uses a sliding window method to compute the RMS in which a window of a specified length is moved over the data, sample by sample, and the RMS is calculated for each window. This function essentially acts as an R wrapper for the source moving RMS function written in C++. For a more user-friendly interface see the \code{autorms} function.   
##' @param input_data A numeric vector representing the EMG data.
##' @param intwind The size of the RMS moving window; units in samples.
##' @return This function returns a numeric vector that contains the RMS of the input EMG data. 
##' @examples
##' \dontrun{
##' # Example usage:
##' data(EMG_data)
##' 
##' RMS_data <- moving_RMS(input_data = EMG_data, intwind = (75*5))
##' }
##' @author Ethan Benevides
##' @export
##' @useDynLib EMGhelpeR
##' @importFrom Rcpp sourceCpp
##' 
moving_RMS <- function(input_data, intwind) {


    movingRMS_result <- movingRMS(input_data, intwind)


    return(movingRMS_result)
}
