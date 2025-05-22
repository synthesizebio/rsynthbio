## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE  # Set to FALSE since API calls require credentials
)

## ----installation, eval=FALSE-------------------------------------------------
# install.packages("rsynthbio")

## ----github-installation, eval=FALSE------------------------------------------
# if (!("remotes" %in% installed.packages())) {
#   install.packages("remotes")
# }
# remotes::install_github("synthesizebio/rsynthbio")

## -----------------------------------------------------------------------------
# library(rsynthbio)

## ----auth-secure, eval=FALSE--------------------------------------------------
# # Securely prompt for and store your API token
# # The token will not be visible in the console
# set_synthesize_token()
# 
# # You can also store the token in your system keyring for persistence
# # across R sessions (requires the 'keyring' package)
# set_synthesize_token(use_keyring = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # In future sessions, load the stored token
# load_synthesize_token_from_keyring()
# 
# # Check if a token is already set
# has_synthesize_token()

## ----clear-token, eval = FALSE------------------------------------------------
# # Clear token from current session
# clear_synthesize_token()
# 
# # Clear token from both session and keyring
# clear_synthesize_token(remove_from_keyring = TRUE)

## ----modalities---------------------------------------------------------------
# # Check available modalities
# get_valid_modalities()

## ----query--------------------------------------------------------------------
# # Get a sample query
# query <- get_valid_query()
# 
# # Inspect the query structure
# str(query)

## ----predict, eval=FALSE------------------------------------------------------
# # Request raw counts data
# result <- predict_query(query)

## ----modify-query-------------------------------------------------------------
# # Change output modality
# query$output_modality <- "bulk_rna-seq"
# 
# # Adjust number of samples
# query$inputs[[1]]$num_samples <- 10
# 
# # Modify cell line information
# query$inputs[[1]]$metadata$cell_line <- "MCF7"
# query$inputs[[1]]$metadata$perturbation <- "TP53"
# 
# # Add a new condition
# query$inputs[[3]] <- list(
#   metadata = list(
#     tissue = "lung",
#     disease = "adenocarcinoma",
#     sex = "male",
#     age = "57 years",
#     sample_type = "primary tissue"
#   ),
#   num_samples = 3
# )

