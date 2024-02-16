# justfile to build, test and deploy my R packages.
#
# BvH, 2024-02-16

default: build test check

lint:
  R -e 'library("styler");styler::style_pkg()'
  R -e 'library("lintr");lintr::lint_package()'

build:
  R -e 'library("devtools"); build()'

test: build
  R -e 'library("tinytest");tinytest::test_package("KNMIr")'

check: build
  R CMD check ../KNMIr_0.4.3.tar.gz

deploy: test
  R -e 'library("devtools"); install(pkg = ".")'
