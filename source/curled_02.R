# changes in this version:
#   - curl noise is now defined in 3d space: (x, y, iteration)
#   - fractal reduces octaves from 10 to 3

sys_id <- "02"
output_dir <- here::here("output", sys_id)
if (!dir.exists(output_dir)) dir.create(output_dir)

import_halftone <- function(base, channel, x_sample, y_sample) {

  channel_index <- channel |>
    switch("red" = 1, "green" = 2, "blue" = 3)

  halftone_image <- base[, , channel_index] |>
    halftoner::halftone(x.samp = x_sample, y.samp = y_sample)

  dat <- tibble::tibble(
    x = halftone_image[[1]][,2],
    y = halftone_image[[1]][,1],
    size = halftone_image[[2]],
    shade = channel
  )
  return(dat)
}

import_image <- function(image, x_sample = 500, y_sample = 500) {

  base <- image |>
    jpeg::readJPEG() |>
    as.array() |>
    aperm(c(2, 1, 3))

  halftone_r <- import_halftone(base, "red", x_sample, y_sample)
  halftone_g <- import_halftone(base, "green", x_sample, y_sample)
  halftone_b <- import_halftone(base, "blue", x_sample, y_sample)

  merged_size <- (halftone_r$size + halftone_g$size + halftone_b$size)/3
  merged_shade <- rgb(
    red   = (1 - halftone_r$size),
    green = (1 - halftone_g$size),
    blue  = (1 - halftone_b$size),
    maxColorValue = 1.0
  )

  dat <- tibble::tibble(
    x = halftone_r$x / x_sample * 200,
    y = halftone_r$y / y_sample * 200,
    size = merged_size,
    shade = merged_shade
  )

  return(dat)
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
  image,
  seed = 4,
  resolutions =  c(500, 2000),
  types = c("png", "jpg")
) {

  image_path <- fs::path("input", paste0("input_", image, ".jpg"))
  message(paste("building from", image_path))

  ht <- import_image(here::here(image_path))

  min_size <- 0
  max_size <- .5

  set.seed(seed)

  ht <- ht |>
    dplyr::filter(size > min_size) |>
    dplyr::mutate(
      size = dplyr::if_else(size > max_size, .5, size),
      id = 1,
      seed = seed,
      ind = seq_len(dplyr::n()),
      type = "halftone"
    )

  its <- 80

  dat <- ht |>
    dplyr::mutate(
      x = x * .01,
      y = y * .01
    ) |>
    unfold(
      iterations = its,
      scale = .0001,
      octaves = 3
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
        size = size * 12 * abs((its - iteration)/its)
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

  output <- paste0("curled_", sys_id, "_", image, "-", seed)
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

inputs <- fs::dir_ls(here::here("input")) |>
  gsub(".*_", "", x = _) |>
  gsub("\\.jpg", "", x = _)

make_all <- FALSE
if (make_all) {
  for (input in inputs) curled(input)
}
