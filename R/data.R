#' Sample Dataset for quallmer app
#'
#' A sample dataset for demonstrating the quallmer app's validation functionality.
#' Contains movie review texts coded by multiple coders with a gold standard.
#'
#' @format A data frame with 20 rows and 6 variables:
#' \describe{
#'   \item{.row_id}{Unique identifier for each text}
#'   \item{text}{Movie review text}
#'   \item{gold_sentiment}{Gold standard sentiment label (positive, negative, neutral)}
#'   \item{coder1_sentiment}{Human coder's sentiment classification}
#'   \item{coder2_sentiment}{LLM coder's sentiment classification (moderate accuracy)}
#'   \item{coder3_sentiment}{Another LLM coder's sentiment classification (lower accuracy)}
#' }
#'
#' @details
#' This dataset is useful for:
#' \itemize{
#'   \item Testing inter-rater reliability calculations (using coder1, coder2, coder3)
#'   \item Testing gold-standard validation (using gold_sentiment as reference)
#'   \item Learning how to use the quallmer app
#'   \item Demonstrating nominal measurement level metrics
#' }
#'
#' @examples
#' \dontrun{
#' # Option 1: Use the pre-made sample file from the package
#' # Get the path to the sample data file
#' sample_file <- system.file("extdata", "sample_data.rds", package = "quallmer.app")
#'
#' # Launch the app and upload this file through the UI
#' qlm_app()
#'
#' # Option 2: Load the data and save your own copy
#' data(sample_data)
#' saveRDS(sample_data, "my_sample.rds")
#' # Then load my_sample.rds in qlm_app()
#' }
"sample_data"
