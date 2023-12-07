##' @title Rectify and Integrate EMG Data
##' @description The \code{recint} function rectifies and integrates electromyography data in a single function. The function takes the absolute value of the EMG trace and then applies a moving average filter to the rectified trace to integrate the signal.  Additionally, it returns a plot of the rectified and integrated EMG signal. 
##' @param input_data A numeric vector representing the EMG data.
##' @param window The size of the integration window.
##' @param fs Sampling rate of the EMG data.
##' @param time_units A character string specifying the time units for the window size. Options include: 'samples' (default), 'ms' (milliseconds), and 'sec' (seconds).
##' @param preview_plot Logical, indicating whether to display a preview plot of the rectified and integrated EMG data (default is TRUE).
##' @return A numeric vector representing the rectified and integrated EMG data as well as a plot of the rectified and integrated EMG data.
##' @examples
##' \dontrun{
##' # Example usage:
##' data(EMG_data)
##' 
##' recint_output <- recint(
##'   input_data = EMG_data,
##'   window = 25,
##'   fs = 5000,
##'   time_units = 'ms',
##'   preview_plot = TRUE
##' )
##' }
##' @importFrom tibble as_tibble
##' @import ggplot2 
##' @importFrom zoo rollapply
##' @importFrom caTools runmean
##' @importFrom dplyr %>%
##' @importFrom stats median
##' @author Ethan Benevides
##' @export

recint <- function(input_data, window, fs, time_units = "samples", preview_plot = TRUE) {


    if (time_units == "ms") {
        tu = fs/1000
    } else if (time_units == "sec") {
        tu = fs
    } else if (time_units == "samples") {
        tu = 1
    } else {
        stop("Please chose a valide time unit. Options include: 'samples', 'sec', and 'ms'.")
    }



    rec = abs(input_data)

    med_filt = zoo::rollapply(rec, width = window * tu, FUN = median, align = "right", fill = NA)
    smooth_data = caTools::runmean(med_filt, window * tu)

    if (preview_plot == TRUE) {
        xvals <- c(1:length(smooth_data))

        plot_tib <- as_tibble(cbind(xvals, smooth_data))


        sum_plot <- plot_tib %>%
            ggplot(mapping = aes(x = xvals, y = smooth_data)) + geom_line() + theme_bw() + ylab("Diaphragm EMG activity (AU)") + xlab("Samples")

        print(sum_plot)
    }
    return(smooth_data)

}
