for (operation in c("predict-expression", "predict-expression-from-reference", "predict-metadata", "autoencode")) {
  for (modality in c("bulk", "sc")) {
    for (canonical in c(FALSE, TRUE)) {
      local({
        op <- operation; mod <- modality; canon <- canonical
        test_that(paste("ordinary GEM-2 call", op, mod, canon, "retains scientific results"), {
          row <- list(counts = c(16777217, 2), latents = list(generated = c(0.1, 0.2)))
          if (op == "predict-expression-from-reference") row$latents <- list(z_ref = 0.1, z_target = 0.2, z_displacement = 0.1)
          if (op == "predict-metadata") row <- list(
            predicted_metadata = list(tissue_ontology_id = "UBERON:0002107", age_years = NULL),
            classifier_probabilities = list(tissue_ontology_id = list("UBERON:0002107" = 0.75)),
            latents = list(encoded = c(0.3, 0.4)))
          if (op == "autoencode") row <- list(counts = c(16777217, 2), model_release_id = "qualified", vae_run_id = "same-vae")
          content <- list(outputs = list(row, row), gene_order = c("gene1", "gene2"), model_version = "qualified")
          # Match the ordinary HTTP JSON parsing path, including nested frames.
          content <- jsonlite::fromJSON(jsonlite::toJSON(content, auto_unbox = TRUE, null = "null", digits = NA), simplifyMatrix = FALSE)
          suffix <- switch(op, "predict-expression" = "baseline", "predict-expression-from-reference" = "reference-conditioning", "predict-metadata" = "predict-metadata", "autoencode" = "autoencode")
          model <- if (canon) "gem-2" else paste0("gem-2-", mod, "_", suffix)
          query <- if (canon) list(operation = op, inputs = list(modality = if (mod == "sc") "single-cell" else mod, items = list())) else list(inputs = list())
          fn <- predict_query
          start <- mockery::mock("id")
          mockery::stub(fn, "has_synthesize_token", function() TRUE)
          mockery::stub(fn, "start_model_query", start)
          mockery::stub(fn, "poll_model_query", function(...) list(status = "ready", payload = list(downloadUrl = "https://example.com/result")))
          mockery::stub(fn, "get_json", function(...) content)
          result <- fn(query, model_id = model, self_hosted = FALSE)
          expect_equal(mockery::mock_args(start)[[1]]$query$source, "rsynthbio")
          expect_equal(nrow(result$metadata), 2L)
          expect_equal(nrow(result$latents), 2L)
          if (op == "predict-metadata") {
            expect_equal(result$metadata$tissue_ontology_id, rep("UBERON:0002107", 2))
            expect_true(all(is.na(result$metadata$age_years)))
            expect_equal(result$classifier_probs$tissue_ontology_id[[1]], list("UBERON:0002107" = 0.75))
            expect_equal(dim(result$expression), c(2L, 0L))
          } else {
            expect_equal(as.numeric(result$expression[1, ]), c(16777217, 2))
            expect_equal(colnames(result$expression), c("gene1", "gene2"))
            if (op == "autoencode") {
              expect_equal(result$provenance$model_release_id, rep("qualified", 2))
              expect_equal(result$provenance$vae_run_id, rep("same-vae", 2))
            }
          }
        })
      })
    }
  }
}


test_that("GEM-2 retains nested molecule metadata in the formatted results", {
  molecules <- list(list(smiles = "CCO"), list(entity_ontology_id = "CHEMBL3261331"))
  row <- list(counts = c(16777217, 2), metadata = list(
    sample_type = "primary tissue", bioactive_entity_perturbations = molecules,
    gene_perturbations = list()), latents = list(generated = c(0.1, 0.2)))
  content <- jsonlite::fromJSON(jsonlite::toJSON(list(outputs = list(row, row),
    gene_order = c("gene1", "gene2")), auto_unbox = TRUE, digits = NA), simplifyMatrix = FALSE)
  result <- rsynthbio:::transform_gem2_output(content, "predict-expression")
  expect_equal(nrow(result$metadata), 2L)
  expect_equal(result$metadata$bioactive_entity_perturbations[[1]], molecules)
  expect_equal(result$metadata$bioactive_entity_perturbations[[2]], molecules)
  expect_equal(result$metadata$gene_perturbations[[1]], list())
  expect_equal(result$metadata$sample_type, rep("primary tissue", 2))
})
