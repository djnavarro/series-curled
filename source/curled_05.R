# changes in this version:
#  - more detailed curls than the last version
#  - diagonal patterns in output is caused by bug in create_base_image

sys_id <- "05"
output_dir <- here::here("output", sys_id)
if (!dir.exists(output_dir)) dir.create(output_dir)

create_base_image <- function(seed) {
  set.seed(seed)
  n_rows <- 200
  n_cols <- 200
  n_shades <- 1024
  dat <- matrix(
    data = sample(1024, n_cols * n_rows, replace = TRUE),
    nrow = n_rows,
    ncol = n_cols,
    byrow = TRUE
  )
  iterations <- 1000000
  for (i in 1:iterations) {
    r <- sample(2:(n_rows - 1), 1)
    c <- sample(2:(n_cols - 1), 1)
    sh <- sample(10, 1)
    sv <- sample(10, 1)
    h <- r + (-sh:sh)
    v <- c + (-sv:sv)
    h <- h[h >= 1 & h <= n_cols]
    v <- v[v >= 1 & v <= n_rows]
    dat[v, h] <- (dat[v, h] + dat[r, c]) / 2
  }
  dat <- as.vector(dat)
  dat <- (dat - min(dat)) / (max(dat) - min(dat))
  dat <- 1 + (n_shades - 1) * dat
  dat <- ceiling(dat)

  palettes_all <- readr::read_csv(
    here::here("source", "palette_01.csv"), 
    show_col_types = FALSE
  )
  ind <- sample(nrow(palettes_all), 1)
  palette_base <- unlist(palettes_all[ind, -1])
  shades <- (colorRampPalette(palette_base))(n_shades)

  ht <- tidyr::expand_grid(
    x = seq(0, 200, length.out = n_cols),
    y = seq(0, 200, length.out = n_cols),
    size = 1
  )
  ht$shade <- shades[dat]
  return(ht)
}

show_base_image <- function(ht) {
  ggplot2::ggplot(ht, aes(x, y, fill = shade)) +
    ggplot2::geom_raster(show.legend = FALSE) + 
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}

unfold <- function(
  data,
  iterations,
  scale,
  octaves,
  noise = NULL,
  fractal = NULL,
  ...
) {

  if (is.null(noise)) noise <- ambient::gen_simplex
  if (is.null(fractal)) fractal <- ambient::billow
  seed <- data$seed[1]
  data$iteration <- 1
  data$z <- 1

  do_step <- function(data, iter) {
    n <- nrow(data)
    noise <- ambient::curl_noise(
      x = data$x,
      y = data$y,
      z = data$z,
      seed = seed,
      generator = ambient::fracture,
      noise = noise,
      fractal = fractal,
      octaves = octaves,
      ...
    )
    data$iteration <- iter
    data$x <- data$x + noise$x * scale
    data$y <- data$y + noise$y * scale
    data$z <- data$z + noise$z * scale
    return(data)
  }
  state <- purrr::accumulate(
    .x = (1:iterations) + 1,
    .f = do_step,
    .init = data
  )
  state <- dplyr::bind_rows(state)
  return(state)
}

curled <- function(
  seed,
  resolutions =  c(500, 2000),
  types = c("png", "jpg")
) {

  message(paste("making base image", seed))
  ht <- create_base_image(seed)

  set.seed(seed)

  its <- 80
  dat <- ht |>
    dplyr::mutate(
      seed = seed,
      x = x * .01,
      y = y * .01
    ) |>
    unfold(
      iterations = its,
      scale = .0001,
      octaves = 10
    )

  compute_limit <- function(data, column, border = .04) {
    values <- data[[column]][data$iteration == 1]
    range <- c(1 - max(values), 1 - min(values))
    limit <- range + c(1, -1) * border
    return(limit)
  }

  pic <- ggplot2::ggplot(dat) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = 1 - x,
        y = 1 - y,
        color = shade,
        size = size * 10 * abs((its - iteration)/its)
      ),
      alpha = 1,
      stroke = 0,
      show.legend = FALSE
    ) +
    ggplot2::coord_cartesian(
      xlim = compute_limit(dat, "x"),
      ylim = compute_limit(dat, "y")
    ) +
    ggplot2::scale_x_continuous(name = NULL, expand = c(0, 0), breaks = NULL) +
    ggplot2::scale_y_continuous(name = NULL, expand = c(0, 0), breaks = NULL) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_alpha_continuous(range = c(0, 1)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))

  # ugh
  rm(ht, dat)
  gc()

  output <- paste0("curled_", sys_id, "_", seed)
  scaling <- 40 / 3

  for (size in resolutions) {
    for (type in types) {
      output_path <- fs::path(
        output_dir,
        type,
        size,
        paste0(output, ".", type)
      )
      message(paste("making", output_path))
      ggplot2::ggsave(
        filename = output_path,
        plot = pic,
        width = scaling,
        height = scaling,
        dpi = size / scaling
      )
    }
  }
}

# for(s in 21:40) curled(s)
