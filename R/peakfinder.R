##' @title Find Local Maxima in EMG Data
##' @description The \code{peakfinder} function identifies local maxima (peaks) in electromyography (EMG) data. This function returns the values of the detected peaks and the corresponding sample indices. Additionally, a plot displaying the EMG data and detected peaks is automatically generated. 
##' @param input_data A numeric vector representing EMG data.
##' @param threshold A numeric value representing the minimum amplitude required for a point to be considered a peak (default is 0).
##' @param min_distance A numeric value representing the minimum distance (in samples) between consecutive peaks (default is 0).
##' @param preview_plot Logical, indicating whether to display a preview plot of EMG data with detected peaks (default is TRUE).
##' @return A list containing two numeric vectors: \code{peaks} representing the amplitudes of the detected peaks, and \code{locs} representing the corresponding sample indices.
##' @examples
##' \dontrun{
##' # Example usage:
##' data(EMG_data)
##' 
##' peak_out <- peakfinder(input_data = EMG_data, threshold = 0.1, min_distance = 2500)
##' }
##' @importFrom tibble as_tibble
##' @import ggplot2 
##' @importFrom dplyr %>%
##' @author Ethan Benevides
##' @export

peakfinder <- function(input_data, threshold = 0, min_distance = 0, preview_plot = TRUE) {

    peaks = c()
    locs = c()
    count = 0


    for (i in 2:(length(input_data) - 1)) {

        previous_point = input_data[i - 1]
        current_point = input_data[i]
        next_point = input_data[i + 1]

        if (any(is.na(c(previous_point, current_point, next_point)))) {

        } else if (current_point <= threshold) {

        } else if (current_point > previous_point & current_point > next_point) {

            count = count + 1
            peaks[count] = previous_point
            locs[count] = i
        } else {

        }


    }


    data_df <- data.frame(peaks, locs)


    data_df <- data_df[order(data_df$locs), ]

    if (nrow(data_df) > 1) {
        i <- 2
        while (i <= nrow(data_df)) {
            if (!is.na(data_df$locs[i]) && !is.na(data_df$locs[i - 1])) {
                time_difference <- data_df$locs[i] - data_df$locs[i - 1]
                if (time_difference < min_distance) {
                  if (data_df$peaks[i] > data_df$peaks[i - 1]) {
                    data_df <- data_df[-(i - 1), , drop = FALSE]
                  } else {
                    data_df <- data_df[-i, , drop = FALSE]
                  }

                  i <- i - 1
                }
            }
            i <- i + 1
        }
    }

    peaks <- data_df$peaks
    locs <- data_df$locs

    if (preview_plot == TRUE) {
        xvals <- c(1:length(input_data))

        plot_tib <- as_tibble(cbind(xvals, input_data))

        smooth_peaks <- as_tibble(cbind(locs, peaks))

        output_plot <- plot_tib %>%
            ggplot(mapping = aes(x = xvals, y = input_data)) + geom_line() + geom_point(data = smooth_peaks, aes(x = locs, y = peaks), color = "red", size = 3) + theme_bw() +
            ylab("Diaphragm EMG activity (AU)") + xlab("Samples")

        print(output_plot)
    }
    return(list(peaks = peaks, locs = locs))
}
