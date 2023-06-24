
add_title_to_cowplot <- function(cow_plot, title_string = "Default Plot Title", title_shift = 20) {
  title_obj  <- ggdraw() + 
    draw_label(
      title_string,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, title_shift)
    )
  
  plot_grid(
    title_obj, cow_plot,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
  
}

add_left_vertical_label_to_cowplot <- function(cow_plot, title_string = "Default Axis") {
  title_obj  <- ggdraw() + 
    draw_label(
      title_string,
      fontface = 'bold',
      x = 0,
      hjust = 0,
      angle = 90
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  plot_grid(
    title_obj, cow_plot,
    ncol = 2,
    # rel_heights values control vertical title margins
    rel_widths = c(0.03, 1)
  )
  
}

add_right_vertical_label_to_cowplot <- function(cow_plot, title_string = "Default Axis") {
  title_obj  <- ggdraw() + 
    draw_label(
      title_string,
      fontface = 'bold',
      x = 0,
      hjust = 0,
      angle = 90
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  plot_grid(
    cow_plot,
    title_obj,
    ncol = 2,
    # rel_heights values control vertical title margins
    rel_widths = c(1, 0.03)
  )
  
}

add_bottom_label_to_cowplot <- function(cow_plot, title_string = "") {
  title_obj  <- ggdraw() + 
    draw_label(
      title_string,
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 0)
    )
  
  plot_grid(
    cow_plot,
    title_obj,
    nrow = 2,
    # rel_heights values control vertical title margins
    rel_heights = c(1, 0.03)
  )
  
}

add_bottom_label_to_cowplot(figure_2.2, "")








