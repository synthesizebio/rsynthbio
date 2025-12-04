# Set Synthesize Bio API Token

Securely prompts for and stores the Synthesize Bio API token in the
environment. This function uses getPass to securely handle the token
input without displaying it in the console. The token is stored in the
SYNTHESIZE_API_KEY environment variable for the current R session.

## Usage

``` r
set_synthesize_token(use_keyring = FALSE, token = NULL)
```

## Arguments

- use_keyring:

  Logical, whether to also store the token securely in the system
  keyring for future sessions. Defaults to FALSE.

- token:

  Character, optional. If provided, uses this token instead of
  prompting. This parameter should only be used in non-interactive
  scripts.

## Value

Invisibly returns TRUE if successful.

## Examples

``` r
# Interactive prompt for token
if (FALSE) { # \dontrun{
set_synthesize_token()

# Provide token directly (less secure, not recommended for interactive use)
set_synthesize_token(token = "your-token-here")

# Store in system keyring for future sessions
set_synthesize_token(use_keyring = TRUE)
} # }
```
