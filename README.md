# Diagnosing early-onset neonatal sepsis in low-resource settings: development of a multivariable prediction model

Samuel R. Neal, MRes; Gwendoline Chimhini, MMED; Felicity Fitzgerald, PhD;
Simbarashe Chimhuya, MMED; Mario Cortina-Borja, PhD; Michelle Heys, MD(Res)

**Correspondence to:** Dr Michelle Heys, m.heys@ucl.ac.uk   
**Code queries to:** Dr Samuel Neal, samuel.neal.19@ucl.ac.uk

## Data

Data files are not included in this repo due to research ethics restrictions.
Sharing of deidentified individual participant data will be considered on a
case-by-case basis.

## Reports

The /Reports directory contains RMarkdown files (and their corresponding PDF
files) detailing model development in full. `SupplementaryMaterial_full.pdf`
is Additional File 2 of the published manuscript.

## Reproducibility

To reproduce our analyses:

1. Clone this repo then open the .Rproj file.
2. Create a /Data directory with a /_Raw_json subdirectory containing the .json
files to analyse.

We analysed Neotree files:

\#      | Admission forms                         | Outcome forms                                |
--------|-----------------------------------------|----------------------------------------------|
First   | `201812031408-NeoTree___Zimbabwe.json`  | `201812031408-NeoDischarge___Zimbabwe.json`  |
Last    | `202004021007-NeoTree___Zimbabwe.json`  | `202004021007-NeoDischarge___Zimbabwe.json`  |

*Note* `202002281200-NeoTree___Zimbabwe.json` was a corrupt file; for some reason
it was being read in as ?UCS-2 encoding. We opened this file as UTF-8 and deleted
the random characters before the correct JSON format started.
