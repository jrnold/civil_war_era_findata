PYTHON = python3
R = Rscript
YAMLFILES = $(shell find sources/datapackage -type f -name '*.yaml')

vpath %.R sources/scripts/
vpath %.py sources/scripts/

DATA = data/bond_metadata.json
DATA += data/greenbacks.csv
DATA += data/greenbacks_fill.csv
DATA += data/bankers_magazine_govt_state_loans.csv
DATA += data/bankers_magazine_govt_state_loans_yields.csv
DATA += data/bankers_magazine_govt_state_loans_yields_2.csv
DATA += data/merchants_magazine_us_paper.csv
DATA += data/merchants_magazine_us_paper_yields.csv
DATA += data/merchants_magazine_us_paper_yields_2.csv
DATA += data/greenback_yields.csv


all: build

build: $(DATA) datapackage.json 

data/%.csv: %.R
	$(R) $^ $@

data/%.json: %.R
	$(R) $^ $@

datapackage.json: datapackage.py $(YAMLFILES)
	$(PYTHON)  $< sources/datapackage $@

data/bond_metadata.json: bond_metadata.R
data/greenbacks.csv: greenbacks.R sources/data/greenbacks.csv
data/greenbacks_fill.csv: greenbacks_fill.R data/greenbacks.csv
sources/scripts/greenbacks_fill.R: sources/scripts/R/finance.R
data/bankers_magazine_govt_state_loans.csv: bankers_magazine_govt_state_loans.R sources/data/bankers_magazine_state_govt_loans.csv
data/bankers_magazine_govt_state_loans_yields.csv: bankers_magazine_govt_state_loans_yields.R data/bankers_magazine_govt_state_loans.csv data/bond_metadata.json 
data/bankers_magazine_govt_state_loans_yields_2.csv: bankers_magazine_govt_state_loans_yields_2.R data/bankers_magazine_govt_state_loans_yields.csv
data/merchants_magazine_us_paper.csv: merchants_magazine_us_paper.R sources/data/merchants_magazine_us_paper.csv
data/merchants_magazine_us_paper_yields.csv: merchants_magazine_us_paper_yields.R data/merchants_magazine_us_paper.csv data/bond_metadata.json
data/merchants_magazine_us_paper_yields_2.csv: merchants_magazine_us_paper_yields_2.R data/merchants_magazine_us_paper_yields.csv
data/greenback_yields.csv: greenback_yields.R data/greenbacks.csv
