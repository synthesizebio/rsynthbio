#' Extract Gene Expression Data from API Response
#' 
#' @description Extracts and combines gene expression data from a complex API response,
#' with proper formatting and metadata association.
#'
#' @param api_response The raw API response list
#' @param as_counts Logical, if FALSE, transforms the predicted expression counts into logCPM 
#'        (default is TRUE, returning raw counts).
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