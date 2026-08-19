# Population Near Protected Areas (2000-2020)

This repository is the replication package for the paper *How many people live near protected areas in developing countries? Estimates from gridded population data (2000-2020)* by Florent Bédécarrats.

## Repository structure

The repository is organized into five main components:

- `code/`: R scripts used to reproduce the processed data and generate the tables and figures.
- `data/`: source data, processed datasets, and cache files.
- `manuscript/`: Quarto source files for the main manuscript and supplementary materials.
- `results/`: generated analytical objects, publication tables, figures, and diagnostics.
- `archive/`: legacy materials preserved for provenance.

## Reproduction workflow

The final analytical workflow is R-based.

1. Reproduce or refresh the processed country-level datasets.
2. Generate the analytical objects, figures, and tables.
3. Render the manuscript and supplementary materials.

The main scripts are:

- `code/01_reproduce_processed_data.R`
- `code/02_produce_tables_and_figures.R`

The manuscript sources are:

- `manuscript/main.qmd`
- `manuscript/supplementary.qmd`

The public webpage is published at `docs/index.html`. Supplementary HTML output is published at `docs/supplementary.html`. PDF and DOCX outputs remain in `manuscript/`.

Typical render sequence:

```bash
quarto render manuscript/main.qmd
quarto render manuscript/supplementary.qmd
```

After rendering, the HTML outputs are published automatically to `docs/`.

## Data contents

The `data/` directory is split into three parts:

- `data/raw/`: source inputs used by the analysis, including the World Bank income classification file, comparison material, the Word template, and the WDPA May 2021 spatial extracts.
- `data/processed/`: processed country-level outputs used by the final analysis, including the local reproduction dataset used by the manuscript.
- `data/cache/`: cache files used during local processing.

The final manuscript pipeline uses the processed local reproduction dataset in `data/processed/pa_population_local_reproduction/`.

## Results contents

The `results/` directory contains generated outputs:

- `results/figures/`: manuscript and supplementary figures.
- `results/tables/`: publication tables in `.rds`, `.html`, `.docx`, and `.tex` formats.
- `results/diagnostics/`: supplementary diagnostic files generated during the analysis.
- `results/pa_pop_refactored.rds`: the main serialized object loaded by the manuscript.

These outputs can be regenerated from the scripts in `code/`.

## Archive contents

The `archive/` directory contains legacy manuscript versions, prior rendered outputs, and review-related materials retained for provenance. These files are not required to reproduce the final manuscript.

## Data availability

The analysis relies on publicly available source data together with the processed outputs included in this repository.

External sources include:

- WorldPop Global Project Population Data: <https://developers.google.com/earth-engine/datasets/catalog/WorldPop_GP_100m_pop>
- WDPA polygons: <https://www.protectedplanet.net/>
- geoBoundaries ADM0 data: <https://www.geoboundaries.org/>
- World Bank historical country income classification: <https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups>

## Citation

Please cite the paper as:

Bédécarrats, Florent. 2026. "How Many People Live near Protected Areas in Developing Countries? Estimates from Gridded Population Data (2000-2020)." *Environmental Research Letters*. https://doi.org/10.1088/1748-9326/ae97f9.

```bibtex
@article{bedecarratsHowManyPeople2026,
	title = {How Many People Live near Protected Areas in Developing Countries? {Estimates} from Gridded Population Data (2000-2020)},
	shorttitle = {How Many People Live near Protected Areas in Developing Countries?},
	author = {B{\'e}d{\'e}carrats, Florent},
	year = {2026},
	journal = {Environmental Research Letters},
	doi = {10.1088/1748-9326/ae97f9},
	urldate = {2026-08-19}
}
```

## License

[![CC BY-ND 4.0](https://img.shields.io/badge/License-CC%20BY--ND%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nd/4.0/)

The manuscript and code in this repository are distributed under a [Creative Commons Attribution-NoDerivs 4.0 International License](https://creativecommons.org/licenses/by-nd/4.0/).
