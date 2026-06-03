# 03_network_visualization.R
# Purpose: Produce and save PNG network plots for all 50 states.
# Input:   data/state_hyperlinks/state_igraph_objects.rda
# Output:  output/plots/<STATE>_plot.png

# ── Libraries ────────────────────────────────────────────────────────────────
library(igraph)

# ── Paths ────────────────────────────────────────────────────────────────────
HYPERLINK_DIR <- file.path("data", "state_hyperlinks")
PLOT_DIR      <- file.path("output", "plots")
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

# ── Load graph objects ────────────────────────────────────────────────────────
load(file.path(HYPERLINK_DIR, "state_igraph_objects.rda"))

# ── Plot all states ───────────────────────────────────────────────────────────
for (state_abb in state.abb) {
  g <- get(paste0(state_abb, "_g"))

  # Node colours: seed = blue, high in-degree = yellow, else grey
  V(g)$color <- ifelse(igraph::degree(g, mode = "in") > 1, "yellow", "grey")
  V(g)$color[which(V(g)$seed == TRUE)] <- "dodgerblue3"

  # Label colours
  V(g)$label.color <- "black"
  V(g)$label.color[which(V(g)$seed == TRUE)] <- "dodgerblue4"

  # Show labels only for seed nodes and high-in-degree nodes
  V(g)$label <- ifelse(
    (igraph::degree(g, mode = "in") > 1 | !is.na(V(g)$seed)),
    V(g)$name, NA
  )

  g <- simplify(g)

  png(file.path(PLOT_DIR, paste0(state_abb, "_plot.png")),
      width = 900, height = 600)
  set.seed(200)
  plot(g,
       layout          = layout_with_dh(g),
       vertex.label.cex = 1 - 0.5 * log(igraph::degree(g, mode = "in") + 1),
       vertex.size     = 3,
       edge.width      = 1,
       edge.arrow.size = 0.1,
       main            = paste(state_abb, "Energy Policy Hyperlink Network"))
  dev.off()
  cat("Saved plot:", state_abb, "\n")
}
