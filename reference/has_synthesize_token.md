# Check if Synthesize Bio API Token is Set

Checks whether a Synthesize Bio API token is currently set in the
environment. Useful for conditional code that requires an API token.

## Usage

``` r
has_synthesize_token()
```

## Value

Logical, TRUE if token is set, FALSE otherwise.

## Examples

``` r
 if (FALSE) { # \dontrun{
# Check if token is set
if (!has_synthesize_token()) {
  # Prompt for token if not set
  set_synthesize_token()
}
} # }
```
