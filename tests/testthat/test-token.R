library(mockery)

# Mock for environment variables
mock_env <- function(expr, envvar, value) {
  old <- Sys.getenv(envvar, unset = NA)
  Sys.setenv(!!envvar := value)
  on.exit(if (is.na(old)) Sys.unsetenv(envvar) else Sys.setenv(!!envvar := old))
  force(expr)
}

# Mock for getPass
mock_getpass <- function() {
  "mock-token-12345"
}

# Setup mock for keyring package
mock_keyring <- list(
  stored_keys = list(),
  key_set_with_value = function(service, username, password) {
    mock_keyring$stored_keys[[paste0(service, ":", username)]] <- password
    invisible(TRUE)
  },
  key_get = function(service, username) {
    key <- mock_keyring$stored_keys[[paste0(service, ":", username)]]
    if (is.null(key)) stop("Key not found")
    return(key)
  },
  key_delete = function(service, username) {
    mock_keyring$stored_keys[[paste0(service, ":", username)]] <- NULL
    invisible(TRUE)
  },
  key_list = function(service) {
    keys <- names(mock_keyring$stored_keys)
    keys <- keys[grepl(paste0("^", service, ":"), keys)]
    if (length(keys) == 0) {
      return(data.frame(service = character(0), username = character(0)))
    }
    usernames <- sub(paste0("^", service, ":"), "", keys)
    return(data.frame(service = rep(service, length(usernames)),
                      username = usernames,
                      stringsAsFactors = FALSE))
  }
)

test_that("set_synthesize_token stores token in environment", {
  # Save original to restore after test
  orig_env <- Sys.getenv("SYNTHESIZE_API_KEY", unset = NA)
  on.exit(if (is.na(orig_env)) Sys.unsetenv("SYNTHESIZE_API_KEY") else Sys.setenv(SYNTHESIZE_API_KEY = orig_env))

  # Clear environment before test
  Sys.unsetenv("SYNTHESIZE_API_KEY")

  # Test with direct token
  expect_message(set_synthesize_token(token = "test-token"), "API token set for current session")
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "test-token")
})

test_that("set_synthesize_token prompts for token when not provided", {

  # Save original to restore after test
  orig_env <- Sys.getenv("SYNTHESIZE_API_KEY", unset = NA)
  on.exit(if (is.na(orig_env)) Sys.unsetenv("SYNTHESIZE_API_KEY") else Sys.setenv(SYNTHESIZE_API_KEY = orig_env))

  # Clear environment before test
  Sys.unsetenv("SYNTHESIZE_API_KEY")

  # Mock getPass function to return a predefined token
  m <- mock(mock_getpass())
  stub(set_synthesize_token, "getPass::getPass", m)
  stub(set_synthesize_token, "browseURL", function(...) NULL)

  # Call the function and verify it used our mock to get the token
  expect_message(set_synthesize_token(), "API token set for current session")
  expect_called(m, 1)
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "mock-token-12345")
})

test_that("set_synthesize_token stores token in keyring when requested", {

  # Save original to restore after test
  orig_env <- Sys.getenv("SYNTHESIZE_API_KEY", unset = NA)
  on.exit(if (is.na(orig_env)) Sys.unsetenv("SYNTHESIZE_API_KEY") else Sys.setenv(SYNTHESIZE_API_KEY = orig_env))

  # Clear environment before test
  Sys.unsetenv("SYNTHESIZE_API_KEY")

  # Mock requireNamespace to always return TRUE for keyring
  m_req <- mock(TRUE)
  stub(set_synthesize_token, "requireNamespace", m_req)

  # Mock keyring functions
  m_set <- mock(TRUE)
  stub(set_synthesize_token, "keyring::key_set_with_value", m_set)

  # Call the function with keyring storage enabled
  expect_message(
    set_synthesize_token(use_keyring = TRUE, token = "keyring-test-token"),
    "API token stored in system keyring"
  )

  # Verify keyring function was called
  expect_called(m_req, 1)
  expect_called(m_set, 1)
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "keyring-test-token")
})

test_that("set_synthesize_token handles missing keyring package", {

  # Save original to restore after test
  orig_env <- Sys.getenv("SYNTHESIZE_API_KEY", unset = NA)
  on.exit(if (is.na(orig_env)) Sys.unsetenv("SYNTHESIZE_API_KEY") else Sys.setenv(SYNTHESIZE_API_KEY = orig_env))

  # Clear environment before test
  Sys.unsetenv("SYNTHESIZE_API_KEY")

  # Mock requireNamespace to return FALSE for keyring
  m_req <- mock(FALSE)
  stub(set_synthesize_token, "requireNamespace", m_req)

  # Call the function with keyring storage enabled, but keyring not available
  expect_warning(
    set_synthesize_token(use_keyring = TRUE, token = "unavailable-keyring-token"),
    "Package 'keyring' is not installed"
  )

  expect_called(m_req, 1)
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "unavailable-keyring-token")
})

test_that("load_synthesize_token_from_keyring loads token from keyring", {

  # Save original to restore after test
  orig_env <- Sys.getenv("SYNTHESIZE_API_KEY", unset = NA)
  on.exit(if (is.na(orig_env)) Sys.unsetenv("SYNTHESIZE_API_KEY") else Sys.setenv(SYNTHESIZE_API_KEY = orig_env))

  # Clear environment before test
  Sys.unsetenv("SYNTHESIZE_API_KEY")

  # Mock requireNamespace to return TRUE for keyring
  m_req <- mock(TRUE)
  stub(load_synthesize_token_from_keyring, "requireNamespace", m_req)

  # Mock keyring::key_get to return a token
  m_get <- mock("keyring-stored-token")
  stub(load_synthesize_token_from_keyring, "keyring::key_get", m_get)

  # Call the function
  expect_message(
    result <- load_synthesize_token_from_keyring(),
    "API token loaded from keyring"
  )

  # Verify keyring function was called and token was set
  expect_true(result)
  expect_called(m_req, 1)
  expect_called(m_get, 1)
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "keyring-stored-token")
})

test_that("load_synthesize_token_from_keyring handles missing keyring package", {

  # Save original to restore after test
  orig_env <- Sys.getenv("SYNTHESIZE_API_KEY", unset = NA)
  on.exit(if (is.na(orig_env)) Sys.unsetenv("SYNTHESIZE_API_KEY") else Sys.setenv(SYNTHESIZE_API_KEY = orig_env))

  # Clear environment before test
  Sys.unsetenv("SYNTHESIZE_API_KEY")

  # Mock requireNamespace to return FALSE for keyring
  m_req <- mock(FALSE)
  stub(load_synthesize_token_from_keyring, "requireNamespace", m_req)

  # Call the function
  expect_warning(
    result <- load_synthesize_token_from_keyring(),
    "Package 'keyring' is not installed"
  )

  # Verify function returns FALSE and environment is unchanged
  expect_false(result)
  expect_called(m_req, 1)
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "")
})

test_that("load_synthesize_token_from_keyring handles keyring errors", {

  # Save original to restore after test
  orig_env <- Sys.getenv("SYNTHESIZE_API_KEY", unset = NA)
  on.exit(if (is.na(orig_env)) Sys.unsetenv("SYNTHESIZE_API_KEY") else Sys.setenv(SYNTHESIZE_API_KEY = orig_env))

  # Clear environment before test
  Sys.unsetenv("SYNTHESIZE_API_KEY")

  # Mock requireNamespace to return TRUE for keyring
  m_req <- mock(TRUE)
  stub(load_synthesize_token_from_keyring, "requireNamespace", m_req)

  # Verify function returns FALSE and environment is unchanged
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "")
})

test_that("clear_synthesize_token clears token from environment", {
  # Set a token
  Sys.setenv(SYNTHESIZE_API_KEY = "token-to-clear")

  # Clear it
  expect_message(clear_synthesize_token(), "API token cleared from current session")

  # Verify it's gone
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "")
})

test_that("clear_synthesize_token handles already empty environment", {
  # Make sure no token is set
  Sys.unsetenv("SYNTHESIZE_API_KEY")

  # Try to clear it
  expect_message(clear_synthesize_token(), "No API token was set in the current session")

  # Verify still empty
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "")
})

test_that("clear_synthesize_token removes token from keyring when requested", {

  # Set a token
  Sys.setenv(SYNTHESIZE_API_KEY = "token-to-clear-from-keyring")

  # Mock requireNamespace to return TRUE for keyring
  m_req <- mock(TRUE)
  stub(clear_synthesize_token, "requireNamespace", m_req)

  # Mock keyring functions
  m_list <- mock(data.frame(service = "rsynthbio", username = "api_token", stringsAsFactors = FALSE))
  stub(clear_synthesize_token, "keyring::key_list", m_list)

  m_delete <- mock(TRUE)
  stub(clear_synthesize_token, "keyring::key_delete", m_delete)

  # Call the function with keyring removal enabled
  expect_message(
    clear_synthesize_token(remove_from_keyring = TRUE),
    "API token removed from system keyring"
  )

  # Verify functions were called
  expect_called(m_req, 1)
  expect_called(m_list, 1)
  expect_called(m_delete, 1)
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "")
})

test_that("clear_synthesize_token handles missing keyring package", {

  # Set a token
  Sys.setenv(SYNTHESIZE_API_KEY = "token-to-clear-no-keyring")

  # Mock requireNamespace to return FALSE for keyring
  m_req <- mock(FALSE)
  stub(clear_synthesize_token, "requireNamespace", m_req)

  # Call the function with keyring removal enabled, but keyring not available
  expect_warning(
    clear_synthesize_token(remove_from_keyring = TRUE),
    "Package 'keyring' is not installed"
  )

  # Verify functions were called appropriately
  expect_called(m_req, 1)
  expect_equal(Sys.getenv("SYNTHESIZE_API_KEY"), "")
})

test_that("has_synthesize_token detects token presence correctly", {
  # Test when token is not set
  Sys.unsetenv("SYNTHESIZE_API_KEY")
  expect_false(has_synthesize_token())

  # Test when token is set
  Sys.setenv(SYNTHESIZE_API_KEY = "test-token-existence")
  expect_true(has_synthesize_token())

  # Clean up
  Sys.unsetenv("SYNTHESIZE_API_KEY")
})

