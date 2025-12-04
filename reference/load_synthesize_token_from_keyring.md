# Load Synthesize Bio API Token from Keyring

Loads the previously stored Synthesize Bio API token from the system
keyring and sets it in the environment for the current session.

## Usage

``` r
load_synthesize_token_from_keyring()
```

## Value

Invisibly returns TRUE if successful, FALSE if token not found in
keyring.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load token from keyring
load_synthesize_token_from_keyring()
} # }
```
