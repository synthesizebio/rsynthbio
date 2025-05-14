#' @title Log CPM Transformation
#' @description Transforms raw counts expression data into log1p(CPM) (Counts Per Million).
#' This is a common normalization method for gene expression data that accounts for
#' library size differences and applies a log transformation to reduce the effect of outliers.
#'
#' @param expression A data.frame containing raw counts expression data.
#' @return A data.frame containing log1p(CPM) transformed data.
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
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

  log_cpm_df <- expression %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(everything(), as.numeric),
           dplyr::across(everything(), ~ ifelse(is.na(.), 0, .)),
           dplyr::across(everything(), ~ ifelse(. < 0, 0, .)),
           library_size = rowSums(dplyr::across(where(is.numeric))),
           dplyr::across(dplyr::where(is.numeric) & !matches("library_size"),
                  ~ (. / library_size) * 1e6,
                  .names = "{.col}_cpm")) %>%
    dplyr::select(-dplyr::where(is.numeric) | dplyr::ends_with("_cpm")) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_cpm"), log1p))

  return(log_cpm_df)
}

#' Extract Gene Expression Data from API Response
#'
#' @description Extracts and combines gene expression data from a complex API response,
#' with proper formatting and metadata association.
#'
#' @param api_response The raw API response list
#' @param as_counts Logical, if FALSE, transforms the predicted expression counts into logCPM
#'        (default is TRUE, returning raw counts).
#' @importFrom purrr map_dfr set_names
#' @importFrom tibble as_tibble
#' @importFrom tidyr uncount
#' @importFrom dplyr bind_cols mutate across
#' @return A list with two components:
#'         - metadata: tibble containing sample metadata
#'         - expression: tibble containing combined gene expression data
#'
#' @export
extract_expression_data <- function(api_response, as_counts = TRUE) {
  # Extract gene names
  gene_order <- api_response$gene_order

  # Process expression data and metadata
  results <- purrr::map_dfr(seq_along(api_response$outputs$expression), function(group_idx) {
    # Get expression data for this group
    expr_data <- api_response$outputs$expression[[group_idx]]

    # Convert to tibble with gene names - explicitly set column names to avoid warnings
    expr_tibble <- expr_data %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      purrr::set_names(gene_order)

    # Get metadata for this group and repeat for each sample
    metadata_tibble <- api_response$outputs$metadata[group_idx, , drop = FALSE] %>%
      tibble::as_tibble() %>%
      tidyr::uncount(nrow(expr_tibble))

    # Return combined data with sample index
    dplyr::bind_cols(
      metadata_tibble,
      sample_group = group_idx,
      sample_index = seq_len(nrow(expr_tibble))
    )
  }, .id = "output_group") %>%
    # Create unique sample IDs
    dplyr::mutate(sample_id = paste0("sample_", dplyr::row_number()))

  # Separate metadata from sample indices
  metadata <- results %>%
    dplyr::select(-sample_group, -sample_index)

  # Process expression data
  expression <- purrr::map_dfr(seq_along(api_response$outputs$expression), function(group_idx) {
    api_response$outputs$expression[[group_idx]] %>%
      tibble::as_tibble(.name_repair = "minimal") %>%  # Add .name_repair parameter here
      purrr::set_names(gene_order)
  }) %>%
    # Convert all columns to integers
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))

  # Add sample IDs as row names (tidyverse approach typically avoids rownames)
  expression <- expression %>%
    dplyr::mutate(sample_id = metadata$sample_id) %>%
    tibble::column_to_rownames("sample_id")

  # Apply log CPM transformation if requested
  if (!as_counts) {
    expression <- log_cpm(expression)
  }

  # Return both components
  return(list(
    metadata = metadata,
    expression = expression
  ))
}

