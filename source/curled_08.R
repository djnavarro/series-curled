# changes in this version:
#   - allows more variability in image parameters
#   - refactors code for the sake of my sanity

Rcpp::sourceCpp(here::here("source", "automaton_01.cpp"))

create_palette <- function(seed, n_shades) {
  set.seed(seed)
  palette_file <- here::here("source", "palette_01.csv")
  palette_data <- readr::read_csv(palette_file, show_col_types = FALSE)
  palette_base <- palette_data |>
    dplyr::slice_sample(n = 1) |>
    dplyr::select(-source) |>
    unlist()
  palette_func <- colorRampPalette(palette_base)
  return(palette_func(n_shades))
}

create_base_image <- function(seed,
                              n_rows,
                              n_cols,
                              n_shades,
                              iterations,
                              max_span) {
  set.seed(seed)
  dat <- automaton(n_rows, n_cols, iterations, max_span)
  dat <- as.vector(dat)
  dat <- (dat - min(dat)) / (max(dat) - min(dat))
  dat <- 1 + (n_shades - 1) * dat
  dat <- ceiling(dat)
  img <- tidyr::expand_grid(
    row = seq(0, 200, length.out = n_rows),
    col = seq(0, 200, length.out = n_cols),
    size = 1
  )
  palette <- create_palette(seed, n_shades)
  img$shade <- palette[dat]
  return(img)
}

show_base_image <- function(img) {
  ggplot2::ggplot(
    data = img,
    mapping = ggplot2::aes(x = col, y = row, fill = shade)
  ) +
    ggplot2::geom_raster(show.legend = FALSE) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}

curl_step <- function(data, iteration, scale, octaves, seed) {
  noise_data <- ambient::curl_noise(
    x = data$x,
    y = data$y,
    z = data$z,
    seed = seed,
    generator = ambient::fracture,
    noise = ambient::gen_simplex,
    fractal = ambient::billow,
    octaves = octaves
  )
  data$iteration <- iteration
  data$x <- data$x + noise_data$x * scale
  data$y <- data$y + noise_data$y * scale
  data$z <- data$z + noise_data$z * scale
  return(data)
}

curl_loop <- function(data, seed, iterations, scale, octaves) {
  data$iteration <- 1
  data$z <- 1
  state <- purrr::accumulate(
    .x = (1:iterations) + 1,
    .f = curl_step,
    .init = data,
    scale = scale,
    octaves = octaves,
    seed = seed
  )
  state <- dplyr::bind_rows(state)
  return(state)
}

create_curl_image <- function(data, seed, iterations, scale, octaves) {
  data |>
    dplyr::mutate(x = col * .01, y = row * .01) |>
    curl_loop(
      seed = seed,
      iterations = iterations,
      scale = scale,
      octaves = octaves
    ) |>
    dplyr::mutate(
      prop_complete = iteration / iterations,
      curl_strength = 1 - prop_complete
    )
}

compute_axis_limit <- function(data, column, border = .04) {
  values <- data[[column]][data$iteration == 1]
  range <- c(1 - max(values), 1 - min(values))
  limit <- range + c(1, -1) * border
  return(limit)
}

compute_dot_size <- function(raw_size, max_size, curl_strength) {
  raw_size * max_size * curl_strength
}

create_curl_plot <- function(data, max_dot_size) {
  ggplot2::ggplot(data) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = 1 - x,
        y = 1 - y,
        color = shade,
        size = compute_dot_size(size, max_dot_size, curl_strength)
      ),
      alpha = 1,
      stroke = 0,
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(
      xlim = compute_axis_limit(data, "x"),
      ylim = compute_axis_limit(data, "y")
    ) +
    ggplot2::scale_x_continuous(name = NULL, expand = c(0, 0), breaks = NULL) +
    ggplot2::scale_y_continuous(name = NULL, expand = c(0, 0), breaks = NULL) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_continuous(range = c(0, 1)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
}

create_directories <- function(dir, sizes, types) {
  if (!fs::dir_exists(dir)) fs::dir_create(dir)
  for (size in sizes) {
    for (type in types) {
        subdir <- fs::path(dir, type, size)
        if (!fs::dir_exists(subdir)) fs::dir_create(subdir)
    }
  }
}

file_path <- function(dir, type, size, file) {
  fs::path(dir, type, size, file)
}

file_name <- function(name, type) {
  paste0(name, ".", type)
}

create_image_file <- function(plot, size, type, dir, name) {
  file <- file_name(name, type)
  path <- file_path(dir, type, size, file)
  scaling <- 40 / 3
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = scaling,
    height = scaling,
    dpi = size / scaling
  )
}

create_extra_images <- function(image_path, sizes, types, dir, name) {
  image_size <- max(sizes)
  image_type <- "png"
  for (size in sizes) {
    for (type in types) {
      is_original_image <- size == image_size & type == image_type
      if (!is_original_image) {
        file <- file_name(name, type)
        path <- file_path(dir, type, size, file)
        img <- magick::image_read(image_path)
        img <- magick::image_resize(img, paste0(size, "x", size))
        magick::image_write(img, path)
        rm(img)
        gc()
      }
    }
  }
}

curled <- function(seed) {

  # system identification
  sys_id <- "08"
  sys_name <- "curled"

  # administrative set up
  message_stem <- paste0("image seed ", seed, ":")
  output_dir <- here::here("output", sys_id)
  output_name <- paste0(sys_name, "_", sys_id, "_", seed)
  output_sizes <- c(500, 2000)
  output_types <- c("png", "jpg")
  create_directories(output_dir, output_sizes, output_types)

  # parameters defining the image
  n_rows <- n_cols <- sample(50:200, 1)
  n_shades <- 1024
  base_iterations <- 100000
  base_max_span <- sample(1:8, 1)
  curl_scale <- runif(1, min = .00003, max = .0001)
  curl_octaves <- 10
  curl_iterations <- sample(80:300, 1)
  max_dot_size <- sample(10:20, 1)

  message(paste(message_stem, "making data for base image"))
  base_image_data <- create_base_image(
    seed = seed,
    n_rows = n_rows,
    n_cols = n_cols,
    n_shades = n_shades,
    iterations = base_iterations,
    max_span = base_max_span
  )

  message(paste(message_stem, "making data for curled image"))
  curl_image_data <- create_curl_image(
    data = base_image_data,
    seed = seed,
    iterations = curl_iterations,
    scale = curl_scale,
    octaves = curl_octaves
  )

  message(paste(message_stem, "making plot specification"))
  curl_plot <- create_curl_plot(curl_image_data, max_dot_size)

  message(paste(message_stem, "making primary image file"))
  image_path <- create_image_file(
    plot = curl_plot,
    size = max(output_sizes),
    type = "png",
    dir = output_dir,
    name = output_name
  )

  message(paste(message_stem, "making additional image files"))
  create_extra_images(
    image_path = image_path,
    sizes = output_sizes,
    types = output_types,
    dir = output_dir,
    name = output_name
  )
}

if (FALSE) for (s in 81:120) curled(s)
