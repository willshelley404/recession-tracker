# Source the project's modules for every test file. test_dir() runs with the
# working directory at tests/testthat, so the repo root is two levels up.
local({
  root <- normalizePath(file.path("..", ".."))
  for (f in sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))) {
    source(f, local = globalenv())
  }
})
