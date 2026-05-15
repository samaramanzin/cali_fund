# Cali Fund Allocation Models
This repository contains all of the code and data to create reproducible workflow for modelling and exploring allocation scenarios for the Cali Fund established under the Convention on Biological Diversity by decision 16/2. The repository focuses on models that combine biodiversity, capacity, genetic resouces, and IPLC/TK indicators for consideration by the AHTEG.

This repository contains scripts, workflows, and supporting data processing steps used to develop allocation models for the Cali Fund. The project aims to provide a transparent and reproducible framework for exploring how funds could be distributed across countries under different weighting schemes and indicator selections.

---

## Overview

The workflow includes:

- Cleaning and harmonizing country-level datasets
- Standardizing indicators and country identifiers
- Exploring allocation methodologies
- Running allocation scenarios


## Allocation Formula
The Cali Fund allocation model distributes funding across countries using a weighted combination of biodiversity importance (C<sub>A</sub>), genetic resources (C<sub>B</sub>), capacity needs (C<sub>C</sub>), and IPLC/TK (C<sub>D</sub>) indicators. Each country receives an allocation score based on scaled indicator values, which is then converted into a proportional share of the total available funding.

$$
B + w_1 C_A + w_2 C_B + w_3 C_C + w_4 C_D
$$
Where:

- $B$ = baseline allocation
- $w_1$ – $w_4$ = weighting coefficients
- $C_A$ – $C_D$ = criterion scores

The criteria and weighting structure can be modified to explore alternative allocation scenarios and sensitivities.

---

## Allocation Scenarios
Two primary allocation approaches were explored within this modelling framework.

### Approach 1: Full Formula Allocation
In the first approach, the entire Cali Fund allocation was distributed according to the weighted scoring formula above. Under this approach, all available funds were allocated proportionally based on each country’s combined score across the selected criteria and weighting scheme.

### Approach 2: Base + Formula Allocation
An alternative scenario was also explored in which:

- **50% of the total funds** were divided equally among all eligible countries; and
- **50% of the total funds** were distributed proportionally using the weighted allocation formula above.

## Model Scenarios

1. Criterion A, C and D are equally weighted
2. Criterion A is weighted 50%, criterion C is weighted 25% and criterion D is weighted 25%
3. Criterion A, C, and D at 30% each and criterion B at 10%
* Henry's model: A=80%, C=5%, D=10%, B=5%
* Wilson's model: A=50%, C=40%, D=5%, B=5%
* Fuwei's model: A=40%, C=20%, D=10%, B=30%
* Gladman's model: A=35%, C=30%, D=30%, B=5%
* Paul's models:
  * model 1: A=30%, C=30%, D=30%, B=10%
  * model 2: A=50%, C=0%, D=25%, B=25%

---

## Data Cleaning and Harmonization

Input datasets originated from multiple sources and required harmonization before allocation modelling could be performed.

### Country Standardization

Country names were standardized using ISO3 country codes to ensure compatibility across datasets. The `countrycode` R package was used where possible.

### Missing Data

Missing values are currently being handeled as 0s in the model. This can be updated as needed.

### Scaling and Standardization

Indicators were rescaled between 0 and 1 prior to modelling to ensure comparability across datasets with different units and value ranges.
This was done using min-max normalization.

---

## Repository Structure

```text
cali_fund/
│
├── data_processed/         
│   ├── clean_data/         # Cleaned and harmonized datasets
│   └── outputs/            # Final model outputs and summaries
│  
├── data_raw/               # Original input datasets
│
├── scripts/
│   ├── data_cleaning/      # Scripts for preprocessing and harmonization
│   ├── modelling/          # Allocation model scripts
│   └── visualisation/      # Scripts to write figures and worked example
│
├── plots/                  # Figures
│
└── README.md
```

---







