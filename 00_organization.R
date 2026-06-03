# 00_organization.R
# Purpose: Collect and clean seed datasets for all six categories of energy
#          policymaking actors (nonprofits, utilities, state legislatures,
#          environmental agencies, energy offices, PUCs).
# Output:  Policymaking Actors.csv

# ── Libraries ────────────────────────────────────────────────────────────────
library(tidyverse)
library(readxl)
library(httr)
library(rvest)
library(usdata)

# ── Set working directory to project root ────────────────────────────────────
# Edit this path to match your local project folder
DATA_DIR <- "data"

# ── Energy nonprofits (NCCS Core 2019, NTEE code C35) ────────────────────────
NP_df <- read.csv(file.path(DATA_DIR, "coreco.core2019pc.csv"))
NP_df_sub <- NP_df %>%
  filter(NTEECC == "C35") %>%
  select(NAME, STATE, CITY, ZIP5, NTEECC)
write.csv(NP_df_sub, file.path(DATA_DIR, "Energy-non-profits.csv"))

# ── Utilities (EIA Form 861, 2022) ───────────────────────────────────────────
U_df <- read_xlsx(file.path(DATA_DIR, "sales_ult_cust_2022.xlsx"),
                  range = cell_rows(3:8301), col_names = TRUE)
U_df_sub <- U_df %>%
  filter(Month == "12" & Ownership != "") %>%
  select(`Utility Name`, State, Ownership) %>%
  filter(Ownership %in% c("Investor Owned", "Cooperative", "Municipal"))
write.csv(U_df_sub, file.path(DATA_DIR, "Energy-utilities.csv"))

# ── State legislatures ───────────────────────────────────────────────────────
webpage      <- read_html(file.path(DATA_DIR, "html", "State legislature.html"))
html_text    <- html_text(webpage)
pattern      <- 'href="(.*?)"' 
urls         <- str_match_all(html_text, pattern)[[1]][, 2]
state_names  <- str_match_all(html_text, 'href="[^"]*">([^<]*)')[[1]][, 2]
state_legislature <- data.frame(state = state_names, hyperlink = urls)
write_csv(state_legislature, file.path(DATA_DIR, "state_legislature.csv"))

# ── State environmental agencies ─────────────────────────────────────────────
state_pattern <- paste(c("Alabama","Alaska","Arizona","Arkansas","California",
  "Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho",
  "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland",
  "Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana",
  "Nebraska","Nevada","New\\sHampshire","New\\sJersey","New\\sMexico",
  "New\\sYork","North\\sCarolina","North\\sDakota","Ohio","Oklahoma",
  "Oregon","Pennsylvania","Rhode\\sIsland","South\\sCarolina","South\\sDakota",
  "Tennessee","Texas","Utah","Vermont","Virginia","Washington",
  "West\\sVirginia","Wisconsin","Wyoming\\b"), collapse = "|")

webpage   <- read_html(file.path(DATA_DIR, "html", "state environmental agency.html"))
html_text <- html_text(webpage)
urls         <- str_match_all(html_text, 'href="(.*?)"')[[1]][, 2]
state_agency <- str_match_all(html_text, 'href="[^"]*">([^<]*)')[[1]][, 2]
state_agency_df <- data.frame(name = state_agency, hyperlink = urls) %>%
  mutate(state  = str_extract(name, state_pattern),
         health = as.integer(str_detect(name, "Health\\b")))
write.csv(state_agency_df, file.path(DATA_DIR, "state_environmental_agencies.csv"))

# ── State energy offices ─────────────────────────────────────────────────────
webpage   <- read_html(file.path(DATA_DIR, "html", "state_energy_office.html"))
html_text <- html_text(webpage)
urls       <- str_match_all(html_text, 'href="(.*?)"')[[1]][, 2]
state_name <- str_match_all(html_text, "<tr><td>(.*?)</td>")[[1]][, 2]
energy_office <- str_match_all(html_text, "<a[^>]*>(.*?)</a>")[[1]][, 2]
state_energy <- data.frame(name = energy_office, state = state_name, hyperlink = urls)
write.csv(state_energy, file.path(DATA_DIR, "state_energy_office.csv"))

# ── State PUCs ───────────────────────────────────────────────────────────────
webpage   <- read_html(file.path(DATA_DIR, "html", "PUC.html"))
html_text <- html_text(webpage)
urls  <- str_match_all(html_text, 'href="(.*?)"')[[1]][, 2]
puc   <- str_match_all(html_text, "<a[^>]*>(.*?)</a>")[[1]][, 2]
PUC_df <- data.frame(name = puc, hyperlink = urls) %>%
  mutate(state = str_extract(name, state_pattern))
write.csv(PUC_df, file.path(DATA_DIR, "PUClist.csv"))

# ── Combine all actor types into one master dataset ──────────────────────────
legislature_df <- state_legislature %>%
  mutate(orgtype = "state legislature",
         state   = state.abb[match(state, state.name)]) %>%
  select(state, name = state, hyperlink, orgtype) %>%
  na.omit()

puc_df <- PUC_df %>%
  mutate(orgtype = "state PUC",
         state   = state.abb[match(state, state.name)]) %>%
  select(state, name, hyperlink, orgtype)

agency_df <- state_agency_df %>%
  filter(str_detect(name, "Environmental|Resources|Environment|Conservation") &
           !is.na(state)) %>%
  mutate(orgtype = "public agency",
         state   = state.abb[match(state, state.name)]) %>%
  select(state, name, hyperlink, orgtype)

energy_df <- state_energy %>%
  mutate(orgtype = "state energy office",
         state   = state.abb[match(state, state.name)]) %>%
  select(state, name, hyperlink, orgtype) %>%
  na.omit()

nonprofit_df <- read.csv(file.path(DATA_DIR, "nonprofits.csv")) %>%
  rename(name = NAME, state = STATE, hyperlink = HOMEPAGE) %>%
  mutate(orgtype = "nonprofits") %>%
  select(state, name, hyperlink, orgtype) %>%
  na.omit()

utilities_df <- U_df_sub %>%
  rename(name = `Utility Name`) %>%
  mutate(orgtype = paste0("utilities-", Ownership),
         hyperlink = NA_character_) %>%
  select(state = State, name, hyperlink, orgtype) %>%
  na.omit()

Actor_policy <- bind_rows(legislature_df, puc_df, agency_df,
                          energy_df, nonprofit_df, utilities_df) %>%
  filter(grepl("^https?://", hyperlink)) %>%
  arrange(state, orgtype)

write.csv(Actor_policy, file.path(DATA_DIR, "Policymaking Actors.csv"))
