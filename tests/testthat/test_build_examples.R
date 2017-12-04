context("Build knn examples")

patterns <- rbind(2:1, 3:2, 4:3)
colnames(patterns) <- paste0("Lag", 1:2)
targets <- matrix(3:5, ncol = 1)
colnames(targets) <- "H1"

test_that("build_examples with one target", {
  expect_equal(build_examples(ts(1:5), 1:2),
               list(patterns = patterns, targets = targets))
})

patterns <- rbind(2:1, 3:2)
colnames(patterns) <- paste0("Lag", 1:2)
targets <- rbind(3:4, 4:5)
colnames(targets) <- paste0("H", 1:2)

test_that("build_examples with two targets", {
  expect_equal(build_examples(ts(1:5), 1:2, nt = 2),
               list(patterns = patterns, targets = targets))
})
