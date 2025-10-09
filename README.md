μ-Opioid Modulation of Social Attention

Replication and Extension Study (Leknes Lab, University of Oslo)

This repository contains data and analysis scripts for the preregistered project:
“μ-opioid modulation of visual attention to faces and its relation to autistic traits and OPRM1 genotype”
(OSF preregistration)

The goal is to reproduce and extend prior analyses (Chelnokova et al., 2016) by including additional participants and new predictors (ASQ, OPRM1 genotype).

Folder Overview

data/ contains all datasets used in preprocessing and analysis.
**Primary analysis dataset: `data/analyses/fix_perc_ASQ_GEN.csv` → to be used for all statistical models in the preregistration.**

scripts/ contains all scripts for preprocessing, merging so far

Codebook for fix_perc_ASQ_GEN.csv

| Variable         | Type              | Description                                                  |
| ---------------- | ----------------- | ------------------------------------------------------------ |
| `Participant`    | int               | Unique subject ID (1–49).                                    |
| `Drug`           | factor (3 levels) | Drug condition: `Morphine`, `Naltrexone`, `Placebo`.         |
| `ASQ`            | numeric           | Autism Spectrum Quotient score (0–33).                       |
| `Genotype`       | factor (2 levels) | OPRM1 A118G genotype: `A/A`, `A/G`.                          |
| `AOI`            | factor (4 levels) | Area of Interest: `Eyes`, `Mouth`, `Forehead`, `Background`. |
| `FixPerc`        | numeric           | Percentage of total fixation time in AOI (0–1).              |
| `GazeDir`        | factor (2 levels) | `Direct` or `Averted`.                                       |
| `Gender`         | factor (2 levels) | Gender of the face stimulus.                                 |
| `Attractiveness` | factor (3 levels) | Rated attractiveness level.                                  |
| `StimOrder`      | numeric           | Presentation order within session.                           |
| `ImageSet`       | factor (6 levels) | Image subset identifier.                                     |
| `Session`        | factor (3 levels) | Experimental session number (1–3).                           |
