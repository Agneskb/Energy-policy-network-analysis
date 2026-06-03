# 01_hyperlink_collection.R
# Purpose: Scrape hyperlinks from the websites of all 50-state energy policy
#          actors using vosonSML, then save per-state .rda files.
# Input:   data/Policymaking Actors.csv
# Output:  data/state_hyperlinks/<STATE>_hyperlinks.rda  (one per state)
#          data/state_hyperlinks/state_network_hyperlinks.rda  (combined)

# ── Libraries ────────────────────────────────────────────────────────────────
library(remotes)
library(magrittr)
library(dplyr)
library(vosonSML)
library(igraph)
library(stringr)

# ── Paths ────────────────────────────────────────────────────────────────────
DATA_DIR      <- "data"
HYPERLINK_DIR <- file.path(DATA_DIR, "state_hyperlinks")
if (!dir.exists(HYPERLINK_DIR)) dir.create(HYPERLINK_DIR, recursive = TRUE)

# ── Load seed URLs ────────────────────────────────────────────────────────────
df_link <- read.csv(file.path(DATA_DIR, "Policymaking Actors.csv"))[, -1]

# ── Helper: build pages data frame for a given state ─────────────────────────
build_pages <- function(state_abb, link_type = "ext") {
  pages <- df_link$hyperlink[df_link$state == state_abb]
  data.frame(page = pages,
             type = rep(link_type, length(pages)),
             max_depth = rep(1, length(pages)))
}

# ── Helper: collect, optionally filter errors, and save ──────────────────────
collect_and_save <- function(state_abb, link_type = "ext", filter_errors = TRUE) {
  pages <- build_pages(state_abb, link_type)
  link  <- Authenticate("web") %>% Collect(pages, verbose = TRUE)
  if (filter_errors && "page_err" %in% names(link)) {
    link <- link[is.na(link$page_err), ]
  }
  assign(paste0(state_abb, "_link"),  link,  envir = .GlobalEnv)
  assign(paste0(state_abb, "_pages"), pages, envir = .GlobalEnv)
  save(list = c(paste0(state_abb, "_link"), paste0(state_abb, "_pages")),
       file = file.path(HYPERLINK_DIR, paste0(state_abb, "_hyperlinks.rda")))
  cat("Saved:", state_abb, "\n")
}

# ── Collect hyperlinks per state ─────────────────────────────────────────────
# "all" type collects both internal and external links; "ext" collects external only.
# States using "all": AL, DE, FL, GA, IN, KY, MA, MD, MI, MN, MO, NC, NH, NJ,
#                     NY, OH, PA, RI, TN, TX, VA, WI (legislature sites tend to
#                     block external-only crawling)
all_type_states <- c("AL", "DE", "FL", "GA", "IN", "KY", "MA", "MD", "MI",
                     "MN", "MO", "NC", "NH", "NJ", "NY", "OH", "PA", "RI",
                     "TN", "TX", "VA", "WI")

for (state_abb in state.abb) {
  link_type <- ifelse(state_abb %in% all_type_states, "all", "ext")
  tryCatch(
    collect_and_save(state_abb, link_type),
    error = function(e) cat("Error for", state_abb, ":", conditionMessage(e), "\n")
  )
}

# ── Combine all link objects and save ────────────────────────────────────────
rda_files <- list.files(HYPERLINK_DIR, pattern = "_hyperlinks\\.rda$", full.names = TRUE)
for (f in rda_files) load(f)

save(list = ls(pattern = "_link$"),
     file = file.path(HYPERLINK_DIR, "state_network_hyperlinks.rda"))
save(list = ls(pattern = "_pages$"),
     file = file.path(HYPERLINK_DIR, "state_seed_pages.rda"))

cat("All states collected and saved.\n")
