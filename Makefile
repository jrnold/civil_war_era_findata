PYTHON = python
R = Rscript
YAMLFILES = $(shell find sources/datapackage -type f -name '*.yaml')

DATA = data/bond_metadata.json
DATA += data/greenbacks.csv
DATA += data/greenbacks_fill.csv
DATA += data/bankers_magazine_govt_state_loans.csv

all: build

build: $(DATA) datapackage.json 
	@echo $(DATA)

datapackage.json: sources/datapackage.py $(YAMLFILES)
	$(PYTHON) $< sources/datapackage $@

data/bond_metadata.json: sources/bond_metadata.R
	$(R) $^ $@

data/greenbacks.csv: sources/greenbacks.R sources/data/greenbacks.csv
	$(R) $^ $@

data/greenbacks_fill.csv: sources/greenbacks_fill.R data/greenbacks.csv
	$(R) $^ $@

data/bankers_magazine_govt_state_loans.csv: sources/bankers_magazine_govt_state_loans.R sources/data/bankers_magazine_state_govt_loans.csv
	$(R) $^ $@

data/merchants_magazine_us_paper.csv: sources/merchants_magazine_us_paper.R sources/data/merchants_magazine_us_paper.csv
	$(R) $^ $@
