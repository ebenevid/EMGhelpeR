##' @title Automatic EMG filtering
##' @description \code{autofilt} is an all-in-one EMG filtering function that designs and implements IIR filters in a single function call. The function designs and implements commonly used IIR filters including Butterworth, Chebyshev (type 1 and 2), and Elliptic filters. Additionally, the function returns a summary figure with the filtered EMG plotted on top of the raw EMG. 
##' @param input_data A numeric vector representing the raw EMG data.
##' @param filter_type A character string specifying the type of filter design. Options include: 'butter' (Butterworth), 'cheb1' (Chebyshev type 1), 'cheb2' (Chebyshev type 2), and 'ellip' (Elliptic Filter).
##' @param fs Sampling rate of the EMG data.
##' @param order Filter order, needs to be an integer scalar input.
##' @param ftype A character string specifying the filter type. Options include: 'high' (highpass), 'low' (lowpass), and 'pass' (bandpass).
##' @param high_cut High cut frequency for highpass and bandpass filters. Required argument if ftype is 'high' or 'band'.
##' @param low_cut Low cut frequency for lowpass and bandpass filters. Required argument if ftype is 'low' or 'band'.
##' @param rp Passband ripple for Chebyshev and Elliptic filters. 
##' @param rs Stopband attenuation for Elliptic filters.
##' @param preview_plot Logical, if TRUE (default), running the function returns a plot of the filtered data. If set to FALSE, no figure is generated.
##' @examples
##' \dontrun{
##' # Example usage:
##' data(EMG_data)
##' 
##' filt_butter <- autofilt(
##'   input_data = EMG_data,
##'   filter_type = 'butter',
##'   fs = 5000,
##'   order = 2,
##'   ftype = 'pass',
##'   high_cut = 1000,
##'   low_cut = 100
##' )
##'   
##' filt_cheby1 <- autofilt(
##'   input_data = EMG_data,
##'   filter_type = 'cheb1',
##'   fs = 5000,
##'   order = 2,
##'   ftype = 'pass',
##'   high_cut = 1000,
##'   low_cut = 100,
##'   rp = 0.5
##' )
##'   
##' filt_cheby2 <- autofilt(
##'   input_data = EMG_data,
##'   filter_type = 'cheb2',
##'   fs = 5000,
##'   order = 2,
##'   ftype = 'pass',
##'   high_cut = 1000,
##'   low_cut = 100,
##'   rp = 0.5
##' )
##'   
##' filt_ellip <- autofilt(
##'   input_data = EMG_data,
##'   filter_type = 'ellip',
##'   fs = 5000,
##'   order = 2,
##'   ftype = 'pass',
##'   high_cut = 1000,
##'   low_cut = 100,
##'   rp = 0.5,
##'   rs = 0.5
##' )
##' 
##' }
##' @return This function returns a numeric vector that contains the filtered EMG data as well as a plot of the filtered EMG data.
##' @importFrom signal butter cheby1 cheby2 ellip filtfilt
##' @importFrom tibble as_tibble
##' @import ggplot2 
##' @importFrom dplyr %>%
##' @author Ethan Benevides
##' @export

autofilt <- function(input_data, filter_type, fs, order, ftype, high_cut, low_cut, rp, rs, preview_plot = TRUE) {

    if (ftype == "pass") {

        freq = c(low_cut, high_cut)/(fs/2)

    } else if (ftype == "low") {

        freq = low_cut

    } else if (ftype == "high") {

        freq = high_cut

    } else {

        stop("Please chose a valid filter type. Options include: 'high', 'low', and 'pass'.")

    }



    if (filter_type == "butter") {

        filt_design <- signal::butter(n = order, W = freq, type = ftype)

    } else if (filter_type == "cheb1") {

        filt_design <- signal::cheby1(n = order, Rp = rp, W = freq, type = ftype)

    } else if (filter_type == "cheb2") {

        filt_design <- signal::cheby2(n = order, Rp = rp, W = freq, type = ftype)

    } else if (filter_type == "ellip") {

        filt_design <- signal::ellip(n = order, Rp = rp, Rs = rs, W = freq, type = ftype)

    } else {

        stop("Please chose a valid filter option. Options include: 'butter' (Butterworth), 'cheb1' (Chebyshev type 1), 'cheb2' (Chebyshev type 2), and 'ellip' (Elliptic Filter).")

    }


    filt_data <- signal::filtfilt(filt = filt_design$b, a = filt_design$a, x = input_data)

    if (preview_plot == TRUE) {
        xvals_filt <- c(1:length(filt_data))
        plot_tib_filt <- as_tibble(cbind(xvals_filt, filt_data))


        xvals_raw <- c(1:length(input_data))
        plot_tib_raw <- as_tibble(cbind(xvals_raw, input_data))

        colors <- c(`Raw Data` = "black", `Filtered Data` = "orange")

        sum_plot <- ggplot() + geom_line(data = plot_tib_raw, mapping = aes(x = xvals_raw, y = input_data, color = "Raw Data")) + geom_line(data = plot_tib_filt, mapping = aes(x = xvals_filt,
            y = filt_data, color = "Filtered Data")) + theme_bw() + labs(x = "Samples", y = "Diaphragm EMG activity (AU)", color = "Legend") + scale_color_manual(values = colors)



        print(sum_plot)
    }
    return(filt_data)

}
