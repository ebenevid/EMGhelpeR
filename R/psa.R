##' @title Power Spectral Density Analysis
##' @description The \code{psa} function calculates the power spectral density of electromyography (EMG) data using the Fast Fourier Transform (FFT) algorithm. Additionally, it returns a plot of frequency vs power for the analyzed EMG trace.
##' @param input_data A numeric vector representing the EMG data.
##' @param fs Sampling rate of the EMG data.
##' @param preview_plot Logical, indicating whether to display a plot of frequency vs power (default is TRUE).
##' @return A list containing two numeric vectors: frequency vector (\code{freq_vec}) and corresponding power spectral density vector (\code{power_vec}).
##' @examples
##' \dontrun{
##' # Example usage:
##' data(EMG_data)
##' 
##' psa_output <- psa(input_data = EMG_data, fs = 5000)
##' }
##' @importFrom tibble as_tibble
##' @import ggplot2 
##' @importFrom stats fft
##' @author Ethan Benevides
##' @export

psa <- function(input_data, fs, preview_plot = TRUE) {

    T = 1/fs
    L = length(input_data)


    Y = (stats::fft(input_data, inverse = FALSE))/L

    P2 = abs(Y/L)
    P1 = P2[1:floor(L/2 + 1)]

    P1[2:length(P1) - 1] = 2 * P1[2:length(P1) - 1]
    power_vec = P1


    freq_vec = fs * (0:(L/2))/L

    if (preview_plot == TRUE) {
        plot_tib = as_tibble(cbind(power_vec, freq_vec))

        sum_plot <- ggplot(data = plot_tib) + geom_line(aes(x = freq_vec, y = power_vec)) + theme_bw() + labs(x = "f (Hz)", y = "|P1(f)|")

        print(sum_plot)
    }
    return(list(freq_vec, power_vec))

}

