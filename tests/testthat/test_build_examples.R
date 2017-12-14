context("Build knn examples")

patterns <- rbind(1:2, 2:3, 3:4)
colnames(patterns) <- paste0("Lag", 2:1)
targets <- matrix(3:5, ncol = 1)
colnames(targets) <- "H1"

test_that("build_examples with one target", {
  expect_equal(build_examples(ts(1:5), 2:1),
               list(patterns = patterns, targets = targets))
})

patterns <- rbind(1:2, 2:3)
colnames(patterns) <- paste0("Lag", 2:1)
targets <- rbind(3:4, 4:5)
colnames(targets) <- paste0("H", 1:2)

test_that("build_examples with two targets", {
  expect_equal(build_examples(ts(1:5), 2:1, nt = 2),
               list(patterns = patterns, targets = targets))
})
