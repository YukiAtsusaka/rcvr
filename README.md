# `rcvr`: A Tool for Ranked-Choice Voting Research with Cast Vote Records <img src="man/figures/logo.png" align="right" width="150"/>

## Overview

Cast vote records downloaded from the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/AMK8PJ) (Otis 2025) carry all their identifying information in the file name alone. `rcvr` automates the extraction of standardized election identifiers from those file names so researchers can augment CVR data with election-level attributes without manual coding.

**The problem:** When working with multiple CVR files, the only identifying information available in the data is typically the file name. Manually coding election identifiers across hundreds of files is error-prone and time-consuming.

**The solution:** `rcvr` matches each file name against a curated identifiers table and prepends a standardized `election_id` column to the data. This ID links directly to the [archive-rcv.com](https://www.archive-rcv.com) dataset of tabulated results from American RCV elections.

## Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("YukiAtsusaka/rcvr")
```

## Usage

``` r
library(rcvr)

# Read a CVR file — election_id is prepended automatically
dt <- read_cvr("Alaska_20221108_GovernorLieutenantGovernor.csv")

head(dt[, 1:3])
#> # A tibble: 6 × 3
#>   election_id                              Cast Vote Record  Precinct
#>   <chr>                                               <dbl> <chr>
#> 1 AK_2022_G_Alaska_At_Large_Governor                      1 ALDER
#> ...
```

### The `election_id` format

Election IDs follow a standardized pattern:

```         
{STATE}_{YEAR}_{TYPE}_{JURISDICTION}_{DISTRICT}_{OFFICE}
```

For example: `AK_2022_G_Alaska_At_Large_Governor`

| Component      | Description                                 |
|----------------|---------------------------------------------|
| `STATE`        | Two-letter state abbreviation               |
| `YEAR`         | Four-digit election year                    |
| `TYPE`         | `G` = general, `S` = special, `P` = primary |
| `JURISDICTION` | County or jurisdiction name                 |
| `DISTRICT`     | District identifier or `At_Large`           |
| `OFFICE`       | Office sought                               |

## Data sources

CVR files are available from the Otis archive on Harvard Dataverse:

> Otis, Deb, 2022, "Single winner ranked choice voting CVRs", <https://doi.org/10.7910/DVN/AMK8PJ>, Harvard Dataverse, V25, UNF:6:aSwg3y05mpHYUdcfKboLbw== [fileUNF]

Election-level tabulated results can be linked via `election_id` at [archive-rcv.com](https://www.archive-rcv.com).

> Atsusaka, Yuki and Holbrook, Jordan, 2025, "A Dataset of Tabulated Results from American Ranked-Choice Voting Elections", <https://osf.io/preprints/socarxiv/27hgx_v3>, Preprint
