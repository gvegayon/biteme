all:
	cd .. && \
	R CMD build biteme/ && \
	R CMD check --as-cran --use-valgrind biteme_*.tar.gz
