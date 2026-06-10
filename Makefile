all:
	R CMD INSTALL .
test:
	Rscript -e 'tinytest::test_package("ConsumerTheory")'