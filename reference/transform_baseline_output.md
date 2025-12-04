# Transform Standard Model Output (Internal)

Extracts and combines gene expression data from a standard API response
(e.g., gem-1-bulk, gem-1-sc models).

## Usage

``` r
transform_baseline_output(final_json)
```

## Arguments

- final_json:

  The parsed API response list

## Value

A list with: - metadata: data.frame containing sample metadata -
expression: data.frame containing combined gene expression data -
latents: data.frame containing embeddings (if requested)
