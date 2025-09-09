
RELEASE_NUMBER <- "v2.2"

#' @title API Base URL
#' @description Base URL for the Synthesize Bio API
#' @export
API_BASE_URL <- paste0("https://app.synthesize.bio/api/model/", RELEASE_NUMBER)

#' @title Model Modalities
#' @description A nested list containing supported modalities for different
#' model versions
#' + sra = this is bulk RNA-seq
#' @format A nested list with structure: model type > version > modalities
#' @export
MODEL_MODALITIES <- list()
MODEL_MODALITIES[[RELEASE_NUMBER]] <- list("bulk")

# Alternative approach using setNames():
# MODEL_MODALITIES <- setNames(list(list("bulk")), RELEASE_NUMBER)

# Now you can access it with:
# MODEL_MODALITIES$"v2.2"
# or
# MODEL_MODALITIES[["v2.2"]]
# or 
# MODEL_MODALITIES[[RELEASE_NUMBER]]

