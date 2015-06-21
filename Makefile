PYTHON = python3
R = Rscript
YAMLFILES = $(shell find sources/datapackage -type f -name '*.yaml')

vpath %.R sources/scripts/
vpath %.py sources/scripts/

CSV_SRC = $(wildcard sources/csv/*.R)
CSV_DATA = $(addprefix data/,$(patsubst %.R,%.csv,$(notdir $(CSV_SRC))))
JSON_SRC = $(wildcard sources/json/*.R)
JSON_DATA = $(addprefix data/,$(patsubst %.R,%.json,$(notdir $(JSON_SRC))))

all: build

build: $(CSV_DATA) $(JSON_DATA) datapackage.json
	@echo $(CSV_DATA)
	@echo $(JSON_DATA)

# automatic dependency generation
sources/csv/%.mk: sources/csv/%.R
	$(R) depends.R $<

sources/json/%.mk: sources/json/%.R
	$(R) depends.R $<

data/%.csv: sources/csv/%.R
	$(R) $< $@

data/%.json: sources/json/%.R
	$(R) $< $@

datapackage.json: datapackage.py $(YAMLFILES)
	$(PYTHON)  $< sources/datapackage $@

include $(CSV_SRC:.R=.mk) $(JSON_SRC:.R=.mk)
