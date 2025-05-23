#' @title Set Synthesize Bio API Token
#' @description Securely prompts for and stores the Synthesize Bio API token in the
#' environment. This function uses getPass to securely handle the token input without
#' displaying it in the console. The token is stored in the SYNTHESIZE_API_KEY
#' environment variable for the current R session.
#'
#' @param use_keyring Logical, whether to also store the token securely in the system
#'        keyring for future sessions. Defaults to FALSE.
#' @param token Character, optional. If provided, uses this token instead of prompting.
#'        This parameter should only be used in non-interactive scripts.
#'
#' @return Invisibly returns TRUE if successful.
#' @importFrom getPass getPass
#' @importFrom utils browseURL
#' @examples
#' # Interactive prompt for token
#' \dontrun{
#' set_synthesize_token()
#'
#' # Provide token directly (less secure, not recommended for interactive use)
#' set_synthesize_token(token = "your-token-here")
#'
#' # Store in system keyring for future sessions
#' set_synthesize_token(use_keyring = TRUE)
#' }
#' @export
set_synthesize_token <- function(use_keyring = FALSE, token = NULL) {
  if (is.null(token)) {
    message("Create an account at https://app.synthesize.bio/ then go to your profile.")
    message("Click create token then click the copy button in the corner.")

    browseURL("https://app.synthesize.bio/profile")
    token <- getPass::getPass(msg = paste(
      "Create an account at https://app.synthesize.bio/ then go to your profile.",
      "Click create token then copy it.",
      "Paste token here and press enter: "
    ))
  }

  # Store in environment
  Sys.setenv(SYNTHESIZE_API_KEY = token)

  # Optionally store in keyring if requested and available
  if (use_keyring) {
    if (requireNamespace("keyring", quietly = TRUE)) {
      tryCatch(
        {
          keyring::key_set_with_value(
            service = "rsynthbio",
            username = "api_token",
            password = token
          )
          message("API token stored in system keyring.")
        },
        error = function(e) {
          warning("Failed to store token in keyring: ", e$message)
        }
      )
    } else {
      warning("Package 'keyring' is not installed. Token not stored in keyring.")
      message("To store token in keyring, install with: install.packages('keyring')")
    }
  }

  message("API token set for current session.")
  invisible(TRUE)
}

#' @title Load Synthesize Bio API Token from Keyring
#' @description Loads the previously stored Synthesize Bio API token from the system
#' keyring and sets it in the environment for the current session.
#'
#' @return Invisibly returns TRUE if successful, FALSE if token not found in keyring.
#' @examples
#' \dontrun{
#' # Load token from keyring
#' load_synthesize_token_from_keyring()
#' }
#' @export
load_synthesize_token_from_keyring <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) {
    warning("Package 'keyring' is not installed. Cannot load token from keyring.")
    message("To use this feature, install with: install.packages('keyring')")
    return(invisible(FALSE))
  }

  tryCatch(
    {
      token <- keyring::key_get(service = "rsynthbio", username = "api_token")
      Sys.setenv(SYNTHESIZE_API_KEY = token)
      message("API token loaded from keyring and set for current session.")
      invisible(TRUE)
    },
    error = function(e) {
      warning("Failed to load token from keyring: ", e$message)
      invisible(FALSE)
    }
  )
}

#' @title Clear Synthesize Bio API Token
#' @description Clears the Synthesize Bio API token from the environment for the
#' current R session. This is useful for security purposes when you've finished
#' working with the API or when switching between different accounts.
#'
#' @param remove_from_keyring Logical, whether to also remove the token from the
#'        system keyring if it's stored there. Defaults to FALSE.
#'
#' @return Invisibly returns TRUE.
#' @examples
#' \dontrun{
#' # Clear token from current session only
#' clear_synthesize_token()
#'
#' # Clear token from both session and keyring
#' clear_synthesize_token(remove_from_keyring = TRUE)
#' }
#' @export
clear_synthesize_token <- function(remove_from_keyring = FALSE) {
  # Unset from environment
  if (Sys.getenv("SYNTHESIZE_API_KEY") != "") {
    Sys.unsetenv("SYNTHESIZE_API_KEY")
    message("API token cleared from current session.")
  } else {
    message("No API token was set in the current session.")
  }

  # Optionally remove from keyring
  if (remove_from_keyring) {
    if (requireNamespace("keyring", quietly = TRUE)) {
      tryCatch(
        {
          if (keyring::key_list(service = "rsynthbio")[1, "username"] == "api_token") {
            keyring::key_delete(service = "rsynthbio", username = "api_token")
            message("API token removed from system keyring.")
          } else {
            message("No API token was found in the keyring.")
          }
        },
        error = function(e) {
          # This might occur if no token exists or other keyring issues
          message("No API token was found in the keyring or could not access keyring.")
        }
      )
    } else {
      warning("Package 'keyring' is not installed. Cannot remove token from keyring.")
      message("To use this feature, install with: install.packages('keyring')")
    }
  }

  invisible(TRUE)
}

#' @title Check if Synthesize Bio API Token is Set
#' @description Checks whether a Synthesize Bio API token is currently set in the
#' environment. Useful for conditional code that requires an API token.
#'
#' @return Logical, TRUE if token is set, FALSE otherwise.
#' @examples  \dontrun{
#' # Check if token is set
#' if (!has_synthesize_token()) {
#'   # Prompt for token if not set
#'   set_synthesize_token()
#' }
#' }
#' @export
has_synthesize_token <- function() {
  Sys.getenv("SYNTHESIZE_API_KEY") != ""
}
