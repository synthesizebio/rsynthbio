# Get Output Transformer for Model (Internal)

Looks up the appropriate output transformer for a given model ID.
Returns the standard transformer as the default if no specific
transformer is registered.

## Usage

``` r
get_output_transformer(model_id)
```

## Arguments

- model_id:

  Character string specifying the model ID

## Value

A transformer function that accepts (final_json) and returns a list with
metadata, expression, and optionally latents
