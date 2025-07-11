# This function either creates a new column with values and column name specified in the parameters, OR, if the dataframe does not exist, creates a new dataframe with that column.
# If the input does not exist (NULL) or is empty (""), the dataframe, df, is returned unchanged.
create_column_if_exists <- function(df, new_colname, source_df, input_val) {

  if (is.null(input_val) || input_val == "") {
    return(df)
  }

  new_vector <- source_df[[input_val]]

  if (ncol(df) == 0) {
    # Start new
    out <- tibble(!!sym(new_colname) := new_vector)
  } else {
    # Add a column
    out <- df %>% mutate(!!sym(new_colname) := new_vector)
  }

  return(out)
}