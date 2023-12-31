% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autorms.R
\name{autorms}
\alias{autorms}
\title{Automatic Moving Root Mean Square Transformation}
\usage{
autorms(input_data, fs, window, time_units = "samples", preview_plot = TRUE)
}
\arguments{
\item{input_data}{A numeric vector representing the EMG data.}

\item{fs}{Sampling rate of the EMG data.}

\item{window}{The size of the RMS sliding window.}

\item{time_units}{A character string specifying the desired time units for the moving RMS window. Options include: 'samples' (default), 'ms' (milliseconds), and 'sec' (seconds).}

\item{preview_plot}{Logical, if TRUE (default), running the function returns a plot of the RMS data. If set to FALSE, no figure is generated.}
}
\value{
This function returns a numeric vector that contains the RMS of the input EMG data as well as a plot of the RMS transformed data.
}
\description{
\code{autorms} is a user-friendly wrapper for the \code{moving_RMS} function. It returns the moving root mean square of the input signal using a sliding window method. It takes in additional arguments such as sampling rate, window size, and time units of the window size to make adjustments to the RMS filter more intuitive to beginners and non-experts. In addition, this function returns a plot of the output, allowing users to easily assess if their input parameters need any adjustments.
}
\examples{
\dontrun{
# Example usage:
data(EMG_data)

autorms_output <- autorms(
  input_data = EMG_data,
  fs = 5000,
  window = 50,
  time_units = 'ms',
  preview_plot = TRUE
)
}
}
\author{
Ethan Benevides
}
