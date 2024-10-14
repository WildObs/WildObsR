#' Manually correct date errors
#'
#' Function which asks for a specific placename, and the corrected date of the earliest image. It then calculates the difference and applies that to all dates within your placename.It will prompt you to do this one at a time.
#'
#' @param data This is the tabular data that contains all images with associated dates and times. This is the data where the manual corrections will be applied.
#' @return This function returns and updated data frame containing the same images as the input, but with the updated dates
#' @details This function requires the user to input accruate date values (in what format??), so this should be known before proceeding. Moreover, this will not work when knitting a Rmarkdown/quarto document
#' @examples
#' correct_dates_manually(obs)
#' @author Tom Bruce tom.bruce at uq.edu.au
#' @export
correct_dates_manually <- function(data) {

  # Check if the placenames and Photo.Date columns exist
  if (!("placename" %in% colnames(data)) || !("Photo.Date" %in% colnames(data))) {
    stop("Input data must have 'placename' and 'Photo.Date' columns.")
  }

  #Then ask for the placename that needs fixing
  cat("Enter the placename for which you want to correct dates: ")
  input_placename <- readline()

  # Check if the input placename exists in the data if not stop the function
  if (!any(data$placename == input_placename)) {
    stop("Provided placename not found in the data.")
  }

  #Then ask for the correct date in the format we have just amde above
  cat("Enter the correct date for the first record in placename",
      input_placename,
      "the earliest date is:",
      paste(min(data$Photo.Date[data$placename == input_placename], na.rm = TRUE),
            "(format: %Y-%m-%d): "))
  correct_date <- as.Date(readline(), format = "%Y-%m-%d")

  # Get the first record's date for the input placename
  first_date <- min(data$Photo.Date[data$placename == input_placename], na.rm = TRUE)

  # Calculate the number of days difference
  days_diff <- as.numeric(difftime(correct_date, first_date, units = "days"))

  # Offset all dates in the input placename by the calculated difference
  data$Photo.Date[data$placename == input_placename] <- data$Photo.Date[data$placename == input_placename] + days_diff

  return(data)
}


### NEED TO DO:
# Need to convert syntax into camtrap DP columns
# Need to think carefully about the use of an interactive function when we need to knit documents... doesnt make sense!
