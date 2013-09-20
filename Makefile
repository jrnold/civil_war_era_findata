PYTHON = python
R = Rscript
YAMLFILES = $(shell find sources/datapackage -type f -name '*.yaml')

all: datapackage.json data/bond_metadata.json

datapackage.json: sources/datapackage.py $(YAMLFILES)
	$(PYTHON) $< sources/datapackage $@

data/bond_metadata.json: sources/bond_metadata.R
	$(R) $^ $@
