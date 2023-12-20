test_that("build_examples with one target", {
  patterns <- rbind(1:2, 2:3, 3:4)
  colnames(patterns) <- paste0("Lag", 2:1)
  targets <- matrix(3:5, ncol = 1)
  colnames(targets) <- "H1"
  targetsI <- 3:5
  result <- list(
    patterns = patterns,
    targets = targets,
    targetsI = targetsI
  )
  expect_equal(build_examples(ts(1:5), 2:1, transform = "none"), result)
})


test_that("build_examples with two targets", {
  patterns <- rbind(1:2, 2:3)
  colnames(patterns) <- paste0("Lag", 2:1)
  targets <- rbind(3:4, 4:5)
  colnames(targets) <- paste0("H", 1:2)
  targetsI <- 3:4
  result <- list(
    patterns = patterns,
    targets = targets,
    targetsI = targetsI
  )
  expect_equal(build_examples(ts(1:5), 2:1, nt = 2, transform = "none"), result)
})

test_that("build_examples with additive transformation", {
  patterns <- matrix(c(-1, 1, -2, 2, -1, 1, -0.5, 0.5), nrow = 4, byrow = TRUE)
  colnames(patterns) <- paste0("Lag", 2:1)
  targets <- matrix(c(5, 4, 2, 2.5), ncol = 1)
  colnames(targets) <- "H1"
  targetsI <- 3:6
  result <- list(
    patterns = patterns,
    targets = targets,
    targetsI = targetsI
  )
  expect_equal(build_examples(ts(c(1, 3, 7, 9, 10, 12)), 2:1, transform = "additive"), result)
})

test_that("build_examples with multiplicative transformation", {
  patterns <- matrix(c(0.5, 1.5, 0.6, 1.4, 0.875, 1.125, 0.94736842, 1.05263158), nrow = 4, byrow = TRUE)
  colnames(patterns) <- paste0("Lag", 2:1)
  targets <- matrix(c(3.5, 1.8, 1.25, 1.2631579), ncol = 1)
  colnames(targets) <- "H1"
  targetsI <- 3:6
  result <- list(
    patterns = patterns,
    targets = targets,
    targetsI = targetsI
  )
  expect_equal(build_examples(ts(c(1, 3, 7, 9, 10, 12)), 2:1, transform = "multiplicative"), result)
})
