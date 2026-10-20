# Changelog

## Fixed
- Normalize tract identifiers to six-digit strings before joining. The supplied CSVs already preserve padding: 0 values changed and zero households failed the join. This prevents a numeric-import padding bug without claiming nonexistent source corruption.
- Reverse the source distance orientation for all 6,802 tracts; raw negative values denote PG&E. Retain original d and utility assignment.
- Treat 806,065 missing usage observations as closed-account months, not zero consumption or random recording gaps.
- Null 352 isolated spikes above 20 times household median use, requiring adjacent-month checks. Preserve persistent high-use accounts; flag 20 accounts with median use above 3,000 kWh/month.
- Convert the 2019 DiD coefficient to elasticity using log(0.22/0.25); the reduced-form coefficient itself is not elasticity.
- Separate exact revenue neutrality from profit neutrality; demand growth entails incremental supply cost.

## Added
- R estimates reproduce the prior numerical results to floating-point precision. Singleton fixed-effect groups remain in the estimation sample to preserve the stated observation counts and covariance correction.
- Validated household, tract, weather, and monthly keys; assert full panel coverage, positive observed usage, and complete joins.
- Exact historical tariff construction including CARE's marginal-only discount.
- Household and year-month fixed effects, county-clustered uncertainty, annual event study, pre-trend Wald test, balanced-panel and high-usage sensitivity, and pre-2020 sensitivity.
- Border local-linear validation at 0.10, 0.25, and 0.50 degrees, tract-clustered errors, income and CARE balance, and a fixed-charge placebo with a separate connection-status check.
- 2023 constant-elasticity simulation, explicit fixed-fee calibration, surplus integration, emissions and welfare accounting, and distributional tables by tier and CARE status.
- Replace 11 corrupted 2023 connected-month quantities with valid within-household 2023 medians for simulation only. Do not impute closed accounts or estimation outcomes.
- Native R analysis and R Markdown report using data.table, fixest, and ggplot2.
- Exact-fee reconciliation, cent-rounding residual, response/price sensitivities, input MD5 checksums, R session metadata, and self-contained HTML figures.
- Explicit county sandwich covariance with full fixed-effect degrees-of-freedom correction and normal confidence intervals.

## Known limitations
- Synthetic data support a controlled pricing analysis, not an estimate from actual PG&E billing records.
- Only two utility-level policy groups exist. County clustering and observed pre-trends cannot rule out utility-wide confounding. The 2021 placebo rejects zero and the 2019-only estimate is about half the main estimate; causal validation is incomplete.
- Border utility assignment changes both fixed and marginal prices, and narrow bandwidths contain few tracts. Income and CARE balance must be assessed before interpreting the usage discontinuity.
- Tract income proxies household resources; persistent large users may include multi-unit or commercial meters.
- Constant elasticity, no income effects, and fixed connection decisions are simulation assumptions. Higher fixed charges may change participation.
- Average CAMX emissions do not identify marginal dispatch emissions. Electrification benefits and network losses are not modeled.
- $190/t is the EPA 2023 report's 2020-emission central benchmark in 2020 dollars. Cost vintages are not harmonized; the private-cost residual is assumed, not audited.
- The unknown population expansion factor prevents utility-wide totals. Per-household annual-period figures include partial-year connections.
- Displayed cent-rounded fees need a true-up of $229.19 across the sample to reproduce the unrounded exact revenue target.
