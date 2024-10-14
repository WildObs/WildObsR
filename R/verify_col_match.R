#' Verify Column Matches Between Two Dataframes
#'
#' This function checks for mismatches between a specified column in two dataframes using the `setdiff()` function.
#' It provides detailed feedback on whether the column values match between the two dataframes, and highlights any discrepancies.
#'
#' @details
#' The function compares the values in a specified column from two dataframes:
#' \enumerate{
#'   \item If all values match, it prints a confirmation message that the dataframes are ready for further data wrangling.
#'   \item If there are mismatches, the function checks the number of mismatched values in each dataframe and provides details on where the discrepancies lie.
#'   \item If both dataframes have the same number of mismatches, it suggests the possibility of typos.
#' }
#'
#' @param data1 The first dataframe to compare.
#' @param data2 The second dataframe to compare.
#' @param col A string specifying the name of the column to compare. This column must exist in both dataframes.
#'
#' @return The function does not return any value, but prints messages about column matching and discrepancies to the console.
#'
#' @importFrom base setdiff
#'
#' @examples
#' # Example usage:
#' df1 <- data.frame(ID = c(1, 2, 3, 4), Name = c("Alice", "Bob", "Charlie", "David"))
#' df2 <- data.frame(ID = c(1, 2, 4, 5), Name = c("Alice", "Bob", "Charlie", "Eve"))
#'
#' # Verify column match for 'ID'
#' verify_col_match(df1, df2, col = "ID")
#'
#' # Verify column match for 'Name'
#' verify_col_match(df1, df2, col = "Name")
#'
#' @author Zachary Amir & ChatGPT
#' @export
verify_col_match <- function(data1, data2, col) {

  ## first, make sure the column is present in both dataframes
  if(!col %in% c(names(data1), names(data2))){
    stop(paste("The column you selected:", col, "is not present in one or both dataframes.\n",
               "Please ensure matching column names before using this function"))
  }

  ## check differences in data1
  vec1 = setdiff(data1[, col], data2[, col])

  ## check differences in data2
  vec2 = setdiff(data2[, col], data1[, col])

  ## Let us know if we are good to go.
  if(length(vec1) + length(vec2) == 0){

    print(paste("All values from the column:", col, "match! Proceed with further data wrangling"))

  }else{

    ## if they are both missing the same number of values, make a special condition!
    if(length(vec1) == length(vec2)){
      cat("The number of mis-matched values in the first and second dataframes are the same. Take special attention when ensuring a match, these could be the result of typos!\n\n",
          "The mis-matched values from the first dataframe are:",
          paste(vec1, collapse = ", "), "\n\n",
          "The mis-matched values from the second dataframe are:",
          paste(vec2, collapse = ", "))
    } # end equal but not zero condition

    ## if data1 has less options than data2, tell us
    if(length(vec1) < length(vec2)){
      cat("The number of mis-matched values in the second dataframe is greater than the first dataframe.\n\n",
          "The second dataframe has", length(vec2), "mis-matched values, and the first dataframe has", length(vec1), "mis-matched values.\n\n",
          "The mismatched values from the second dataframe are:",  paste(vec2, collapse = ", "))
    }# end less than condtion

    ## and do the reverse too
    if(length(vec1) > length(vec2)){
      cat("The number of mis-matched values in the first dataframe is greater than the second dataframe.\n\n",
          "The first dataframe has", length(vec1), "mis-matched values, and the second dataframe has", length(vec2), "mis-matched values.\n\n",
          "The mismatched values from the first dataframe are:",  paste(vec1, collapse = ", "))
    } # end reverse condition

  } # end else from 0 length

} # end function
