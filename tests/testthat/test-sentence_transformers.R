test_that("we do not load a sentence_transformers model if suppling a bad id", {
  expect_error(sentence <- hf_load_sentence_model(model_id = "all-mpnet-base-xxl"))
})

test_that("we can load a sentence transformers model with encode method", {
  sentence <- hf_load_sentence_model(model_id = "all-mpnet-base-v2")
  expect_true(grepl("sentence_transformers", class(sentence)[[1]]))
  expect_true(any(grepl("encode", names(sentence))))
})

test_that("hf_sentence_encode works, and with 1 & 1 + texts",{
  sentence <- hf_load_sentence_model(model_id = "all-mpnet-base-v2")
  n_1 <- sentence %>%
    hf_sentence_encode("hello this is a test run")

  n_2 <- sentence %>%
    hf_sentence_encode(c("This is the first of two tests", "and this is the second"))

  expect_false(nrow(n_1) == nrow(n_2))
  expect_equal(ncol(n_1), ncol(n_2))
  expect_true(ncol(n_1) == 768)
})
