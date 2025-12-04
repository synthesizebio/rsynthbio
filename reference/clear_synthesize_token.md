# Clear Synthesize Bio API Token

Clears the Synthesize Bio API token from the environment for the current
R session. This is useful for security purposes when you've finished
working with the API or when switching between different accounts.

## Usage

``` r
clear_synthesize_token(remove_from_keyring = FALSE)
```

## Arguments

- remove_from_keyring:

  Logical, whether to also remove the token from the system keyring if
  it's stored there. Defaults to FALSE.

## Value

Invisibly returns TRUE.

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear token from current session only
clear_synthesize_token()

# Clear token from both session and keyring
clear_synthesize_token(remove_from_keyring = TRUE)
} # }
```
