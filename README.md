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

Raw data is not committed to this repository due to size. Running `R/download_data.R` reproduces it in full.
Raw archive formats were inspected manually before writing the cleaning script.

### Acknowledgments
I acknowledge the use of Gen-AI to assist with writing the data pull and cleaning scripts, and with debugging performance issues during development.