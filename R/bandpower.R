##' @title Calculate the Power in Specific Frequency Bands
##' @description The \code{bandpower} function calculates the power of frequency components within a specified frequency band using the Fast Fourier Transform (FFT) algorithm.
##' @param input_data A numeric vector representing the EMG data.
##' @param fs Sampling rate of the EMG data.
##' @param freq_band Numeric vector specifying the frequency band of interest. It can either be a single frequency or a range defined by two values.
##' @return A numeric scalar value representing the average power of the specified frequency/frequency band.
##' @examples
##' \dontrun{
##' # Example usage:
##' data(EMG_data)
##' 
##' bandpower_output <- bandpower(input_data = EMG_data, fs = 5000, freq_band = c(250,450)))
##' }
##' @importFrom stats fft
##' @author Ethan Benevides
##' @export

bandpower <- function(input_data, fs, freq_band) {


    T = 1/fs
    L = length(input_data)
    t = (0:L - 1) * T

    Y = (stats::fft(input_data, inverse = FALSE))/L

    freq_vec = fs * (0:(L/2))/L

    P2 = abs(Y/L)
    P1 = P2[1:floor(L/2 + 1)]

    P1[2:length(P1) - 1] = 2 * P1[2:length(P1) - 1]
    power_vec = P1

    if (length(freq_band) == 1) {
        power_indices <- which(freq_vec == freq_band)
    } else if (length(freq_band) == 2) {
        low_band = min(freq_band)
        high_band = max(freq_band)

        power_indices <- which(freq_vec >= low_band & freq_vec <= high_band)

    } else {

        stop("Error: freq_band must consist of a scalar or a vector with two numbers.")
    }

    bandpower_output <- mean(power_vec[power_indices])

    return(bandpower_output)



}
