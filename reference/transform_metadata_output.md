# Transform Metadata Model Output (Internal)

Extracts metadata prediction outputs and converts them to data.frames
for metadata, latents, classifier_probs, and expression (from
decoder_sample counts).

Uses the same direct column-access pattern as transform_baseline_output,
since jsonlite with simplifyDataFrame=TRUE converts the outputs array
into a columnar structure.

## Usage

``` r
transform_metadata_output(final_json)
```

## Arguments

- final_json:

  The parsed API response list

## Value

A list with: - metadata: data.frame of predicted metadata - latents:
data.frame with biological/technical/perturbation columns -
classifier_probs: data.frame of per-category probability dicts -
expression: data.frame of decoder sample counts
