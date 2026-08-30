# EVE Online - Ship Insurance Analysis

##### **Does insurance pricing that works in calm periods survive a sudden spike in claims?**

### Goal
This projects aims to test whether EVE Online's ship insurance system holds up financially during the game's largest wars.
The approach tracks the resulting loss ratio (modeled payouts against modeled premiums) over time, to see whether ship insurance system stayed sustainable or was overwhelmed during wartime.

### Scope
The analysis reconstructs modeled insurance premiums and payouts across two independent time windows, during which the game's most destructive conflicts occured:
- Window A (2011-2015): Fountain War, Bloodbath of B-R5RB
- Window B (2018-2022): World War Bee II - *The Casino War*, Massacre at M2-XFE

### Sources
All data is sourced from [EVE Ref](docs.everef.net):
- *Killmails*: every ship destroyed, used as the claims record
- *Market history*: historical mineral prices, used to estimate hull replacement cost
- *SDE (Static Data Export)*: ship blueprints and reference data
- *CCP Monthly Economic Report*: official economic indicators, available from 2016 onward
- *Wars*: formal war declarations, used as background context rather than as the source of the case-study war dates above

**Aggregated output**
| File | Contents |
|---|---|
| `daily_losses_window_[a/b].csv` | Ship losses per day, by ship type (from killmails) |
| `material_prices_window_[a/b].csv[.gz]` | Jita reference prices for every material type actually required by ship blueprints in that window (minerals and manufactured components) |
| `sde_blueprints_window_[a/b].csv` | Material inputs per ship blueprint, per SDE snapshot |
| `sde_types_window_[a/b].csv` | Ship type names and classification, filtered to types that actually appear in loss data |
| `wars_window_[a/b]_active_by_week.csv` | Weekly count of formally-declared corp wars active (background context — see limitations) |
| `wars_window_[a/b]_summary.csv` | Per-war lifetime destruction totals, where determinable within the study window |
| `mer_insurance_window_b.csv` | CCP's own published insurance faucet data (Window B only — CCP's Monthly Economic Report only exists from 2016 onward) |

**Model output**
| File | Contents |
|---|---|
| `hull_value_window_[a/b].csv` | Estimated ISK replacement cost of every ship lost per day |
| `excluded_ship_types_window_[a/b].csv` | Ship types dropped from the analysis along with how many losses each accounts for |
| `hull_value_completeness_by_ship.csv` | Per ship type share of the hull value estimate relying on priced materials vs unpriced materials |
| `loss_ratio_weekly_window_[a/b].csv` | Weekly modeled loss ratio |
| `loss_ratio_daily_window_[a/b].csv` | Daily modeled loss ratio |
| `war_case_studies_daily.csv` | Daily per-war summary for each of the four documented war windows |
| `war_case_studies_weekly.csv` | Weekly per-war summary for each of the four documented war windows |

Raw data or cleaned data is not committed to this repository due to size.
Running `R/download_data.R`, then `R/clean_data.R` reproduces it in full.

### Acknowledgments
I acknowledge the use of Gen-AI to assist with writing the data pull and cleaning scripts, and with debugging performance issues during development.