# 02_network_measures.R
# Purpose: Build igraph objects for all 50 states (page-grouping + pruning),
#          compute network measures, and export a summary CSV for regression.
# Input:   data/state_hyperlinks/state_network_hyperlinks.rda
#          data/state_hyperlinks/state_seed_pages.rda
# Output:  data/state_hyperlinks/state_igraph_objects.rda
#          data/network_analysis.csv

# ── Libraries ────────────────────────────────────────────────────────────────
library(magrittr)
library(dplyr)
library(vosonSML)
library(igraph)
library(stringr)

# ── Paths ────────────────────────────────────────────────────────────────────
DATA_DIR      <- "data"
HYPERLINK_DIR <- file.path(DATA_DIR, "state_hyperlinks")

# ── Load data ────────────────────────────────────────────────────────────────
load(file.path(HYPERLINK_DIR, "state_network_hyperlinks.rda"))
load(file.path(HYPERLINK_DIR, "state_seed_pages.rda"))

state_abbs <- state.abb

# ── Function: page-grouping and pruning ──────────────────────────────────────
preprocessing_state_hyperlink <- function(state_abb) {
  link_data <- get(paste0(state_abb, "_link"))

  actor_net <- Create(link_data, "actor")
  g_actor   <- actor_net %>% Graph()

  # Merge www.domain.com → domain.com
  www_sites <- V(g_actor)$name[grep("^www\\.", V(g_actor)$name)]
  t2 <- 1
  for (c in www_sites) {
    if (t2 %% 100 == 0)
      cat("Merging:", c, "(", t2, "of", length(www_sites), ")\n")
    ind_i <- grep(paste0("^", c, "$"), V(g_actor)$name)
    i     <- str_remove(c, "^www\\.")
    ind   <- grep(paste0("^", i, "$"), as.character(V(g_actor)$name))

    if (!length(ind)) {
      V(g_actor)$name[ind_i] <- i
      t2 <- t2 + 1
      next
    }
    ind   <- sort(c(ind_i, ind))
    map_i <- 1:ind[1]
    t     <- ind[1] + 1
    for (j in (ind[1] + 1):vcount(g_actor)) {
      if (j %in% ind) {
        map_i <- c(map_i, ind[1])
      } else {
        map_i <- c(map_i, t)
        t <- t + 1
      }
    }
    g_actor <- contract(g_actor, map_i, vertex.attr.comb = "first")
    V(g_actor)$name[ind[1]] <- i
    t2 <- t2 + 1
  }

  # Prune social-media and generic utility domains
  to_prune  <- c("google", "twitter", "instagram", "linkedin", "youtube",
                 "goo.gl", "facebook", "apple.com", "x.com", "outlook", "office365")
  pattern   <- paste(to_prune, collapse = "|")
  rm_vtx    <- grep(pattern, V(g_actor)$name, ignore.case = TRUE)
  if (length(rm_vtx) > 0) g_actor <- delete_vertices(g_actor, rm_vtx)

  return(g_actor)
}

# ── Function: extract and attach seed-page attribute ─────────────────────────
process_seed_pages <- function(state_abb) {
  df_pages   <- get(paste0(state_abb, "_pages"))
  seed_pages <- df_pages %>%
    mutate(page = str_remove(page, "^https?://"), seed = TRUE)
  seed_pages$page <- str_remove(seed_pages$page, "/$")
  a <- str_match(seed_pages$page, "(.+?)/")
  seed_pages$page <- ifelse(grepl("/", seed_pages$page), a[, 2], seed_pages$page)
  seed_pages %>% distinct(page, .keep_all = TRUE)
}

assign_seed_attribute <- function(graph, seed_pages) {
  V(graph)$seed <- seed_pages$page[match(V(graph)$name, seed_pages$page)]
  graph
}

# ── Build graph objects for all states ───────────────────────────────────────
for (state_abb in state_abbs) {
  g    <- preprocessing_state_hyperlink(state_abb)
  sp   <- process_seed_pages(state_abb)
  g    <- assign_seed_attribute(g, sp)
  assign(paste0(state_abb, "_g"), g)
  cat("Graph created:", state_abb, "\n")
}

save(list = ls(pattern = "_g$"),
     file = file.path(HYPERLINK_DIR, "state_igraph_objects.rda"))

# ── Compute network measures for all states ──────────────────────────────────
result_data <- data.frame(state = state_abbs, stringsAsFactors = FALSE)

# Average in-degree centrality
result_data$average_degree_centrality <- sapply(state_abbs, function(s) {
  mean(igraph::degree(get(paste0(s, "_g")), mode = "in"))
})

# Degree centralization (out-degree)
result_data$degree_centralization <- sapply(state_abbs, function(s) {
  centr_degree(get(paste0(s, "_g")), mode = "out", loops = FALSE)$centralization
})

# Average clustering coefficient
result_data$average_clustering <- sapply(state_abbs, function(s) {
  transitivity(get(paste0(s, "_g")), type = "average")
})

# Global clustering coefficient
result_data$global_clustering <- sapply(state_abbs, function(s) {
  transitivity(get(paste0(s, "_g")), type = "global")
})

# Network size (node count)
result_data$net_size <- sapply(state_abbs, function(s) {
  vcount(get(paste0(s, "_g")))
})

# Dyad count
result_data$dyad_cnt <- sapply(state_abbs, function(s) {
  length(cliques(get(paste0(s, "_g")), min = 2, max = 2))
})

# Triad count
result_data$triad_cnt <- sapply(state_abbs, function(s) {
  length(cliques(get(paste0(s, "_g")), min = 3, max = 3))
})

# Betweenness centrality (mean, normalised)
result_data$between_centrality <- sapply(state_abbs, function(s) {
  mean(igraph::betweenness(get(paste0(s, "_g")), normalized = TRUE))
})

write.csv(result_data, file.path(DATA_DIR, "network_analysis.csv"), row.names = FALSE)
cat("Network measures saved to data/network_analysis.csv\n")
