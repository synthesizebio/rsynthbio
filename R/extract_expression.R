#' Extract Gene Expression Data from API Response
#' 
#' @description Extracts and combines gene expression data from a complex API response,
#' with proper formatting and metadata association.
#'
#' @param api_response The raw API response list
#' @param as_counts Logical, if FALSE, transforms the predicted expression counts into logCPM 
#'        (default is TRUE, returning raw counts).
#' @return A list with two components:
#'         - metadata: data frame containing sample metadata
#'         - expression: data frame containing combined gene expression data
#'         
#' @export
extract_expression_data <- function(api_response, as_counts = TRUE) {
  # Extract gene names
  gene_order <- api_response$gene_order
  
  # Initialize empty data frames for expression and metadata
  all_expression <- NULL
  all_metadata <- data.frame()
  
  # Process each output group
  for (i in seq_along(api_response$outputs$expression)) {
    # Get expression data for this group
    expr_data <- api_response$outputs$expression[[i]]
    
    # Convert to data frame and add column names (genes)
    expr_df <- as.data.frame(expr_data)
    colnames(expr_df) <- gene_order
    
    # Get metadata for this group
    group_metadata <- api_response$outputs$metadata[i, , drop = FALSE]
    
    # Repeat metadata for each sample in this group
    n_samples <- nrow(expr_df)
    repeated_metadata <- group_metadata[rep(1, n_samples), , drop = FALSE]
    rownames(repeated_metadata) <- NULL
    
    # Add sample IDs
    repeated_metadata$sample_id <- paste0("sample_", 
                                          ifelse(is.null(all_metadata), 1, nrow(all_metadata) + 1):
                                            (ifelse(is.null(all_metadata), 1, nrow(all_metadata) + 1) + n_samples - 1))
    
    # Combine with previous results
    if (is.null(all_expression)) {
      all_expression <- expr_df
    } else {
      all_expression <- rbind(all_expression, expr_df)
    }
    
    all_metadata <- rbind(all_metadata, repeated_metadata)
  }
  
  # Make sure all_expression is a data frame with integer values
  all_expression <- as.data.frame(lapply(all_expression, as.integer))
  
  # Add sample IDs as row names
  rownames(all_expression) <- all_metadata$sample_id
  
  # Apply log CPM transformation if requested
  if (!as_counts) {
    all_expression <- log_cpm(all_expression)
  }
  
  # Return both components
  return(list(
    metadata = all_metadata,
    expression = all_expression
  ))
}