##' @title Automatic Moving Root Mean Square Transformation
##' @description \code{autorms} is a user-friendly wrapper for the \code{moving_RMS} function. It returns the moving root mean square of the input signal using a sliding window method. It takes in additional arguments such as sampling rate, window size, and time units of the window size to make adjustments to the RMS filter more intuitive to beginners and non-experts. In addition, this function returns a plot of the output, allowing users to easily assess if their input parameters need any adjustments.  
##' @param input_data A numeric vector representing the EMG data.
##' @param fs Sampling rate of the EMG data.
##' @param window The size of the RMS sliding window.
##' @param time_units A character string specifying the desired time units for the moving RMS window. Options include: 'samples' (default), 'ms' (milliseconds), and 'sec' (seconds).
##' @param preview_plot Logical, if TRUE (default), running the function returns a plot of the RMS data. If set to FALSE, no figure is generated.
##' @return This function returns a numeric vector that contains the RMS of the input EMG data as well as a plot of the RMS transformed data.
##' @examples
##' \dontrun{
##' # Example usage:
##' data(EMG_data)
##' 
##' autorms_output <- autorms(
##'   input_data = EMG_data,
##'   fs = 5000,
##'   window = 50,
##'   time_units = 'ms',
##'   preview_plot = TRUE
##' )
##' }
##' @importFrom tibble as_tibble
##' @import ggplot2 
##' @importFrom dplyr %>%
##' @author Ethan Benevides 
##' @export
##' @useDynLib EMGhelpeR
##' @importFrom Rcpp sourceCpp

autorms <- function(input_data, fs, window, time_units = "samples", preview_plot = TRUE) {

    if (time_units == "ms") {
        tu = fs/1000
    } else if (time_units == "sec") {
        tu = fs
    } else if (time_units == "samples") {
        tu = 1
    } else {
        stop("Please chose a valide time unit. Options include: 'samples', 'sec', and 'ms'.")
    }

    int_wind = window * tu

    rms_array <- movingRMS(input_data = input_data, intwind = int_wind)

    if (preview_plot == TRUE) {
        xvals <- c(1:length(rms_array))
        plot_tib <- as_tibble(cbind(xvals, rms_array))

        sum_plot <- plot_tib %>%
            ggplot(mapping = aes(x = xvals, y = rms_array)) + geom_line() + theme_bw() + ylab("Diaphragm EMG activity (RMS)") + xlab("Samples")

        print(sum_plot)
    }

    return(rms_array)


}
