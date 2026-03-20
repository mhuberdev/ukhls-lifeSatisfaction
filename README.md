# Unemployment and Life Satisfaction: Evidence from UKHLS Panel Data

## Overview
This project examines how unemployment affects subjective well-being using UK Household Longitudinal Study (UKHLS) panel data. Code written in R.

## Research Question
How does unemployment affect life satisfaction over time?

## Data
The analysis uses UKHLS data. Raw data are not included in this repository because access is restricted through the UK Data Service.
https://ukdataservice.ac.uk/

## Methods
- Descriptive analysis
- Regression models (Two-way Fixed Effects (Staggered static DiD), Random Effects, Pooled OLS)
- Event-study design (Dynamic)

## Main Finding
Unemployment is associated with a substantial decline in life satisfaction.
Incomplete Adaptation: Unlike after other major life events (e.g., divorce, widowhood) people fail to adapt to job loss

## Repository Structure
- `R/`: analysis scripts
- `outputs/`: selected figures and tables, tbd
- `thesis_summary.Rmd`: narrative version of the analysis

## Reproducibility
The code is shared for transparency and workflow demonstration. Full replication requires authorized access to UKHLS data.

## Author
Michael Huber
