#' @title Log CPM Transformation
#' @description Transforms raw counts expression data into log1p(CPM) (Counts Per Million).
#' This is a common normalization method for gene expression data that accounts for
#' library size differences and applies a log transformation to reduce the effect of outliers.
#'
#' @param expression A data.frame containing raw counts expression data.
#' @return A data.frame containing log1p(CPM) transformed data.
#' @examples
#' # Create a sample expression matrix with raw counts
#' raw_counts <- data.frame(
#'   gene1 = c(100, 200, 300),
#'   gene2 = c(50, 100, 150),
#'   gene3 = c(10, 20, 30)
#' )
#'
#' # Transform to log CPM
#' log_cpm_data <- log_cpm(raw_counts)
#' print(log_cpm_data)
#' @export
log_cpm <- function(expression) {
  # Check if the input is a data frame or matrix
  if (!is.data.frame(expression) && !is.matrix(expression)) {
    stop("Input must be a data frame or matrix.", call. = FALSE)
  }

  # Check if the input has more than zero rows and columns
  if (nrow(expression) == 0 || ncol(expression) == 0) {
    stop("Input must have at least one row and one column.", call. = FALSE)
  }

  # Convert to matrix if not already
  expr_matrix <- as.matrix(expression)

  # Replace NAs with 0
  expr_matrix[is.na(expr_matrix)] <- 0

  # Replace negative values with 0
  expr_matrix[expr_matrix < 0] <- 0

  # Calculate library size (row sums)
  library_size <- rowSums(expr_matrix)

  # Calculate CPM
  cpm_matrix <- t(t(expr_matrix) / library_size) * 1e6

  # Apply log1p transformation
  log_cpm_matrix <- log1p(cpm_matrix)

  # Return as data frame with appropriate column names
  result <- as.data.frame(log_cpm_matrix)
  colnames(result) <- paste0(colnames(expression), "_cpm")

  return(result)
}

#' Extract Gene Expression Data from API Response
#'
#' @description Extracts and combines gene expression data from a complex API response,
#' with proper formatting and metadata association.
#'
#' @param parsed_content The parsed API response list
#' @param as_counts Logical, if FALSE, transforms the predicted expression counts into logCPM
#'        (default is TRUE, returning raw counts).
#' @return A list with two components:
#'         - metadata: tibble containing sample metadata
#'         - expression: tibble containing combined gene expression data
#'
#' @export
extract_expression_data <- function(parsed_content, as_counts = TRUE) {

  # Extract the expression matrices and combine them
  expression_list <- parsed_content$outputs$expression
  expression <- do.call(rbind, expression_list)

  # Get metadata dataframe
  metadata <- parsed_content$outputs$metadata

  # Expand metadata to match the expression data
  metadata_expanded <- metadata[rep(1:nrow(metadata),
                                    sapply(parsed_content$outputs$expression, nrow)), ]

  # Reset row names to be clean
  rownames(metadata_expanded) <- NULL

  # Add sample identifiers to match the expression data
  metadata_expanded <- data.frame(sample_id =
                                    paste0("sample_",
                                           rep(1:nrow(metadata_expanded))),
                                  metadata_expanded)

  # Add sample identifiers
  expression <- data.frame(sample_id = metadata_expanded$sample_id,
                           expression)

  # Set gene names as column names (excluding sample_id column)
  colnames(expression)[-1] <- parsed_content$gene_order

  # Apply log CPM transformation if requested
  if (!as_counts) {
    expression <- log_cpm(expression)
  }

  # Return both components
  return(list(
    metadata = metadata_expanded,
    expression = expression
  ))
}
