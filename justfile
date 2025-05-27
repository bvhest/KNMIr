# justfile to build, test and deploy my R packages.
#
# BvH, 2024-02-16, 2025-02-10, 2025-05-27

#default: build test check

# default action: show list of recipes
[private]
default: help

# List available recipes.
help:
  @just --list

# style package source code with R "tergo" package.
style:
  R -e 'library("tergo"); tergo::style_pkg()'

# lint package source code with R "lintr" package.
lint: style
  R -e 'library("lintr");lintr::lint_package()'

# generate package documentation in Markdown.
doc:
  R -e 'library("devtools"); document(roclets = c("rd", "collate", "namespace", "vignette"))'

# build package
build:
  R -e 'library("devtools"); build()'

# test package with R "tinytest" package.
test: build
  R -e 'library("tinytest");tinytest::test_package("KNMIr")'

# perform checks on generated package.
check: build
  R CMD check ../KNMIr_0.4.5.tar.gz

# perform CRAN-checks on generated package.
check4CRAN: build
  R CMD check --as-cran ../KNMIr_0.4.5.tar.gz

# deploy package (locally).
deploy: test check
  R -e 'library("devtools"); install(pkg = ".")'
