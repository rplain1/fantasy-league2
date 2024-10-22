library(reactable)
library(reactablefmtr)

group_column <- function(class = NULL, ...) {
  colDef(cell = format_pct, maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}

format_pct <- function(value) {
  if (value == 0) {
    "  \u2013 "
  } # en dash for 0%
  else if (value == 1) {
    "\u2713"
  } # checkmark for 100%
  else if (value < 0.01) {
    " <1%"
  } else if (value > 0.99) {
    ">99%"
  } else {
    formatC(paste0(round(value * 100), "%"), width = 4)
  }
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

rating_color <- make_color_pal(c("lightpink", "#f8fcf8", "lightgreen"), bias = 1.3)
off_rating_color <- make_color_pal(c("white", "lightblue", "#35b0ff", "#35f0ff"), bias = 0.6)
def_rating_color <- make_color_pal(c("white", "pink", "orangered", "#ff1010"), bias = 0.6)
knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
playoff_pct_color <- make_color_pal(c("#ffffff", "#ABBDFF", "#DA84bb", "#9471a4"), bias = 2)
loser_pct_column <- make_color_pal(c("#ffffff", "lightpink", "#fFA6BE"), bias = 2)
loser_pct_column <- make_color_pal(c("#ffffff", "#F0F0F0", "#888888"), bias = 2)

knockout_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color(value))
      }
    },
    ...
  )
}


playoff_column <- function(maxWidth = 70, class = NULL, borderLeft = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa", borderLeft = borderLeft)
      } else {
        list(color = "#111", background = knockout_pct_color(value), borderLeft = borderLeft)
      }
    },
    ...
  )
}

loser_column <- function(maxWidth = 70, class = NULL, borderRight = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa", borderRight = borderRight)
      } else {
        list(color = "#111", background = loser_pct_column(value), borderRight = borderRight) # loser_pct_column(value))
      }
    },
    ...
  )
}


rating_column <- function(maxWidth = 55, ...) {
  colDef(
    maxWidth = maxWidth,
    align = "center",
    class = "cell number",
    format = colFormat(digits = 0),
    ...
  )
}
