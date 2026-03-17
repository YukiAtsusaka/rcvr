# code_identifiers.R
# Parse CVR file names into identifying variables and generate election_id.

library(dplyr)
library(stringr)
library(readr)

# ----------------------------------------------------------------------
# 1. Read file names
# ----------------------------------------------------------------------
file_names <- readRDS("cvr_file_names.rds")

# Working copy: lowercase, no extension; original kept
fn <- tolower(tools::file_path_sans_ext(file_names))
df <- tibble(source_file = file_names, fn = fn)

# ----------------------------------------------------------------------
# 2. Tease out identifying variables from file names
# ----------------------------------------------------------------------

## --- Year --------------------------------------------------------------
df <- df |>
  mutate(year = as.integer(str_extract(fn, "(200[0-9]|201[0-9]|202[0-9])(?=\\d{4}|\\b)")))

## --- Election type & primary party ------------------------------------
df <- df |>
  mutate(
    election_type = case_when(
      str_detect(fn, "special")                                                        ~ "special",
      str_detect(fn, "primary|_dem_|dem_|_rep_|rep_|_con_|con_|democratic|republican|nominee") ~ "primary",
      TRUE                                                                              ~ "general"
    ),
    prm_party = case_when(
      election_type != "primary"                             ~ NA_character_,
      str_detect(fn, "democratic|_dem_|dem_|nyc\\d+_dem|demnominee") ~ "DEM",
      str_detect(fn, "republican|_rep_|rep_|nyc\\d+_rep|repnominee") ~ "REP",
      str_detect(fn, "_con_|con_|nyc\\d+_con")               ~ "CON",
      TRUE                                                   ~ NA_character_
    )
  )

## --- Jurisdiction (determines state) ----------------------------------
df <- df |>
  mutate(juris = case_when(
    # Alaska (state-level)
    str_detect(fn, "alaska|^ak_|rcv-hd|rcv-sen|rcv-us")                        ~ "Alaska",
    # California cities
    str_detect(fn, "sanfran|san.fran|^d\\d+-pass|^rcv-d|^20\\d{6}_d|^d\\d+_detail|^da_detail|^defender_detail") ~ "San Francisco",
    str_detect(fn, "berkeley")                                                  ~ "Berkeley",
    str_detect(fn, "oakland")                                                   ~ "Oakland",
    str_detect(fn, "san.leandro|sanleandro")                                    ~ "San Leandro",
    # Minnesota cities
    str_detect(fn, "minneapolis|^mpls|_mpls_|cleaned_mpls|2017-ward|2017-park|2017-mayor|2017-board|2017_city_council|2021-council|2021-park|2021-mayor|minneapolis_2023") ~ "Minneapolis",
    str_detect(fn, "minnetonka")                                                ~ "Minnetonka",
    str_detect(fn, "bloomington")                                               ~ "Bloomington",
    str_detect(fn, "stlouispark|st.?louis.?park")                               ~ "St. Louis Park",
    # Maine jurisdictions
    str_detect(fn, "portland")                                                  ~ "Portland",
    str_detect(fn, "westbrook")                                                 ~ "Westbrook",
    str_detect(fn, "maine|main_repub|main_rcv")                                 ~ "Maine",
    # New York City boroughs
    str_detect(fn, "_bronx")                                                    ~ "Bronx",
    str_detect(fn, "_kings_")                                                   ~ "Kings",
    str_detect(fn, "_queens_")                                                  ~ "Queens",
    str_detect(fn, "_richmond_")                                                ~ "Richmond",
    str_detect(fn, "nyc|newyork|citywide")                                      ~ "New York City",
    # Washington
    str_detect(fn, "pierce")                                                    ~ "Pierce",
    # New Mexico
    str_detect(fn, "santafe|santa.?fe")                                         ~ "Santa Fe",
    str_detect(fn, "lascruces|las.?cruces|donaana")                             ~ "Donaana",
    # Colorado
    str_detect(fn, "boulder")                                                   ~ "Boulder",
    # Massachusetts
    str_detect(fn, "easthampton")                                               ~ "Easthampton",
    # Maryland
    str_detect(fn, "takoma")                                                    ~ "Takoma Park",
    # Utah
    str_detect(fn, "payson")                                                    ~ "Payson",
    str_detect(fn, "vineyard")                                                  ~ "Vineyard",
    str_detect(fn, "woodlandhills|woodland.hills")                              ~ "Woodland Hills",
    str_detect(fn, "springville")                                               ~ "Springville",
    str_detect(fn, "elkridge|elk.ridge")                                        ~ "Elk Ridge",
    # Oregon
    str_detect(fn, "covallis|corvallis")                                        ~ "Corvallis",
    # Vermont
    str_detect(fn, "burlington")                                                ~ "Burlington",
    # Michigan
    str_detect(fn, "eastpointe")                                                ~ "Eastpointe",
    # California (additional)
    str_detect(fn, "redondo")                                                   ~ "Redondo Beach",
    # US territories
    str_detect(fn, "usvirgin|virgin.island")                                    ~ "US Virgin Islands",
    TRUE ~ NA_character_
  ))

## --- State (additional coding: not in file names, inferred from juris) --
df <- df |>
  mutate(state = case_when(
    juris == "Alaska"                                                     ~ "AK",
    juris %in% c("San Francisco","Berkeley","Oakland","San Leandro","Redondo Beach") ~ "CA",
    juris %in% c("Minneapolis","Minnetonka","Bloomington","St. Louis Park") ~ "MN",
    juris %in% c("Maine","Portland","Westbrook")                          ~ "ME",
    juris %in% c("Bronx","Kings","Queens","Richmond","New York City")     ~ "NY",
    juris == "Pierce"                                                     ~ "WA",
    juris %in% c("Santa Fe","Donaana")                                    ~ "NM",
    juris == "Boulder"                                                    ~ "CO",
    juris == "Easthampton"                                                ~ "MA",
    juris == "Takoma Park"                                                ~ "MD",
    juris %in% c("Payson","Vineyard","Woodland Hills","Springville","Elk Ridge") ~ "UT",
    juris == "Corvallis"                                                  ~ "OR",
    juris == "Burlington"                                                 ~ "VT",
    juris == "Eastpointe"                                                 ~ "MI",
    juris == "US Virgin Islands"                                          ~ "VI",
    TRUE ~ NA_character_
  ))

## --- Office -----------------------------------------------------------
df <- df |>
  mutate(office = case_when(
    # Federal (check before state-level house/senate)
    str_detect(fn, "us.president|us_president|president")                             ~ "U.S. President",
    str_detect(fn, "us.senator|ussenator|us_senate|us.senate")                        ~ "U.S. Senate",
    str_detect(fn, "us.representative|usrepresentative|us_rep|ushouse|us.house|congressional|congress|rcv-usrep|cd\\d") ~ "U.S. House",
    # State executive
    str_detect(fn, "governor")                                                       ~ "Governor",
    # State legislature (check before generic "house"/"senate")
    str_detect(fn, "state.senate|statesenate|senate.district|senatedistrict|_senate_|sen[a-z]_|rcv-sen") ~ "State Senate",
    str_detect(fn, "state.house|state.representative|house.district|housedistrict|rcv-hd|_house_") ~ "State House",
    str_detect(fn, "state_representative|statehouse|staterep")                       ~ "State Representative",
    # Borough-level
    str_detect(fn, "borough.president|boroughpresident")                             ~ "Borough President",
    # County offices
    str_detect(fn, "county.executive|countyexecutive|county_exec")                   ~ "County Executive",
    str_detect(fn, "county.sheriff|county_sheriff")                                  ~ "County Sheriff",
    str_detect(fn, "county.council|county_council")                                  ~ "County Council",
    str_detect(fn, "assessor.treasurer|assessor_treasurer")                          ~ "County Assessor - Treasurer",
    # Municipal offices — specific before generic
    str_detect(fn, "district.attorney|districtattorney|dis.attorney|_da_|da_detail")  ~ "District Attorney",
    str_detect(fn, "city.attorney|cityattorney")                                     ~ "City Attorney",
    str_detect(fn, "public.advocate|publicadvocate|public_advocate")                  ~ "Public Advocate",
    str_detect(fn, "public.defend|defender")                                         ~ "Public Defender",
    str_detect(fn, "city.auditor|cityauditor|auditor")                               ~ "City Auditor",
    str_detect(fn, "comptroller")                                                    ~ "Comptroller",
    str_detect(fn, "treasurer")                                                      ~ "Treasurer",
    str_detect(fn, "assessor|recorder")                                              ~ "Recorder",
    str_detect(fn, "sheriff")                                                        ~ "Sheriff",
    str_detect(fn, "charter")                                                        ~ "Charter Commissioner",
    str_detect(fn, "school|schoolboard|school_director|schooldirector")              ~ "School Board",
    str_detect(fn, "park.board|park.district|_park_|parkboard")                      ~ "Park Board",
    str_detect(fn, "bet|board.of.estimate|board_of_estimate|tax.board")              ~ "Tax Board",
    str_detect(fn, "mayor")                                                          ~ "Mayor",
    str_detect(fn, "council|ward|bos|supervisor|_dis\\d|_d\\d+")                     ~ "City Council",
    TRUE ~ NA_character_
  ))

## --- District ---------------------------------------------------------
df <- df |>
  mutate(dist = case_when(
    # Explicit at-large markers
    str_detect(fn, "at.large|atlarge|citywide|at_large") ~ "At_Large",
    # "AL" suffix meaning at-large (e.g. CityCouncilAL)
    office == "City Council" & str_detect(fn, "al$")     ~ "At_Large",
    # Alaska has a single at-large congressional seat
    juris == "Alaska" & office == "U.S. House"           ~ "At_Large",
    # Eastpointe and Springville city council are at-large
    juris == "Eastpointe" & office == "City Council"     ~ "At_Large",
    juris == "Springville" & office == "City Council"    ~ "At_Large",
    # Offices that are inherently at-large
    office %in% c("Mayor","Governor","U.S. Senate","U.S. President",
                  "District Attorney","City Attorney","Public Defender",
                  "Public Advocate","City Auditor","Treasurer","Recorder",
                  "Sheriff","Comptroller","County Executive","County Sheriff",
                  "County Assessor - Treasurer")          ~ "At_Large",
    # Congressional district short code: CD2, CD_2
    str_detect(fn, "cd_?\\d+") ~ str_extract(fn, "cd_?(\\d+)") |> str_extract("\\d+"),
    # Alaska RCV short codes: RCV-HD40, RCV-SenF
    str_detect(fn, "rcv-hd\\d+")  ~ str_extract(fn, "(?<=rcv-hd)\\d+"),
    str_detect(fn, "rcv-sen[a-z]") ~ toupper(str_extract(fn, "(?<=rcv-sen)[a-z]")),
    # Alaska senate letter districts: senate_district_a, senate_districta, senatedistricta
    str_detect(fn, "senate_district_[a-z]\\b") ~
      toupper(str_extract(fn, "(?<=senate_district_)[a-z]")),
    str_detect(fn, "senate_district[a-z]\\b") ~
      toupper(str_extract(fn, "(?<=senate_district)[a-z]")),
    str_detect(fn, "senatedistrict[a-z]\\b") ~
      toupper(str_extract(fn, "(?<=senatedistrict)[a-z]")),
    # Alaska 2024 style: StateSenateB (no "District" word)
    str_detect(fn, "statesenate[a-z]\\b") ~
      toupper(str_extract(fn, "(?<=statesenate)[a-z]")),
    # NYC borough-name districts: BoroughPresidentBronx, etc.
    juris == "New York City" & str_detect(fn, "bronx")             ~ "Bronx",
    juris == "New York City" & str_detect(fn, "manhattan|newyork") ~ "Manhattan",
    juris == "New York City" & str_detect(fn, "kings|brooklyn")    ~ "Brooklyn",
    juris == "New York City" & str_detect(fn, "queens")            ~ "Queens",
    juris == "New York City" & str_detect(fn, "richmond|staten")   ~ "Staten Island",
    # Burlington character districts
    juris == "Burlington" & str_detect(fn, "central") ~ "Central",
    juris == "Burlington" & str_detect(fn, "east")    ~ "East",
    juris == "Burlington" & str_detect(fn, "north")   ~ "North",
    juris == "Burlington" & str_detect(fn, "south")   ~ "South",
    # Ward number at end of string: _w3 (Takoma Park) or CityCouncilW3 (Minneapolis)
    str_detect(fn, "w\\d+$") ~ str_extract(fn, "(?<=w)\\d+$"),
    # Generic: "district", "ward", "dis", "dist" followed by digits
    str_detect(fn, "(?:district|ward|_dis|dist[-_]?)\\d+") ~
      str_extract(fn, "(?:district|ward|_dis|dist[-_]?)(\\d+)") |> str_extract("\\d+"),
    # SF-style: _d3_, _d11_, d4_, d10_ (short district tokens)
    str_detect(fn, "(?<![a-z])d(\\d{1,2})(?![a-z0-9])") ~
      str_extract(fn, "(?<![a-z])d(\\d{1,2})(?![a-z0-9])") |> str_extract("\\d+"),
    # Trailing D+number: CouncilD36 → demcitycouncild36
    str_detect(fn, "d\\d+$") ~
      str_extract(fn, "(?<=d)\\d+$"),
    TRUE ~ NA_character_
  ))

# ----------------------------------------------------------------------
# 3. Drop working column
# ----------------------------------------------------------------------
df <- df |> select(-fn)

# ----------------------------------------------------------------------
# 4. Generate election_id
#    Format: STATE_YEAR_ET_JURIS_DIST_OFFICE[_PARTY]
#      ET    : G / P / S
#      JURIS : spaces → underscores
#      DIST  : numeric → zero-padded 2-digit; single letter → "0A"; At_Large as-is
#      OFFICE: abbreviated (see recode below)
#      PARTY : appended only for primaries
# ----------------------------------------------------------------------
et_code <- c(general = "G", primary = "P", special = "S")

pad_dist <- function(d) {
  case_when(
    is.na(d)                  ~ NA_character_,
    str_detect(d, "^\\d+$")  ~ sprintf("%02d", as.integer(d)),
    str_detect(d, "^[A-Za-z]$") ~ paste0("0", toupper(d)),
    TRUE                      ~ d
  )
}

office_abbrev <- function(o) {
  recode(o,
    "City Council"                = "Council",
    "State House"                 = "State_House",
    "State Senate"                = "State_Senate",
    "State Representative"        = "State_Representative",
    "U.S. House"                  = "US_Representative",
    "U.S. Senate"                 = "US_Senate",
    "U.S. President"              = "US_President",
    "Governor"                    = "Governor",
    "Mayor"                       = "Mayor",
    "Park Board"                  = "ParkBoard",
    "School Board"                = "SchoolBoard",
    "Tax Board"                   = "Tax_Board",
    "Borough President"           = "BoroughPresident",
    "County Council"              = "CountyCouncil",
    "County Executive"            = "CountyExecutive",
    "County Sheriff"              = "CountySheriff",
    "County Assessor - Treasurer" = "County_Assessor_Treasurer",
    "District Attorney"           = "DistrictAttorney",
    "City Attorney"               = "CityAttorney",
    "Public Defender"             = "PublicDefender",
    "Public Advocate"             = "PublicAdvocate",
    "City Auditor"                = "CityAuditor",
    "Comptroller"                 = "Comptroller",
    "Treasurer"                   = "Treasurer",
    "Recorder"                    = "Recorder",
    "Sheriff"                     = "Sheriff",
    "Charter Commissioner"        = "Charter_Commissioner",
    .default = str_replace_all(o, "\\s+", "_")
  )
}

df <- df |>
  mutate(
    .et    = et_code[election_type],
    .juris = str_replace_all(juris, "\\s+", "_"),
    .dist  = pad_dist(dist),
    .off   = office_abbrev(office),
    .party = if_else(!is.na(prm_party), paste0("_", prm_party), ""),
    election_id = if_else(
      !is.na(state) & !is.na(year) & !is.na(.et) &
        !is.na(.juris) & !is.na(.dist) & !is.na(.off),
      paste(state, year, .et, .juris, .dist, .off, sep = "_") |> paste0(.party),
      NA_character_
    )
  ) |>
  select(-.et, -.juris, -.dist, -.off, -.party)

# ----------------------------------------------------------------------
# 5. Save
# ----------------------------------------------------------------------
saveRDS(df, "cvr_identifiers.rds")
write_csv(df, "cvr_identifiers.csv")

cat(sprintf(
  "Total: %d | election_id generated: %d | missing: %d\n",
  nrow(df), sum(!is.na(df$election_id)), sum(is.na(df$election_id))
))



# ----------------------------------------------------------------------
# 5. Check
# ----------------------------------------------------------------------

not_coded <- df %>%
  filter(is.na(election_id))
