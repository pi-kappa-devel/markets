font_add_google("Libre Baskerville", "economist")
showtext::showtext_auto()
hexSticker::sticker(
  "rsc/logo-plot.png",
  package = "maRkets",
  p_size = 18,
  p_color = "#2c3e50",
  p_y = 1.45,
  p_family = "economist",
  h_fill = "#ced6e0",
  h_color = "#2c3e50",
  s_x = 1, s_y = 0.8,
  s_width = 0.75, s_height = 0.9,
  filename = "man/figures/logo.png"
)
