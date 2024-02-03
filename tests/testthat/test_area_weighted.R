context("calculate_area_intersection_weights")

test_that("area_intersection", {

  b1 <- sf::st_polygon(list(rbind(c(-1, -1), c(1, -1),
                             c(1, 1), c(-1, 1),
                             c(-1, -1))))
  b2 <- b1 + 2
  b3 <- b1 + c(-0.2, 2)
  b4 <- b1 + c(2.2, 0)
  b <- sf::st_sfc(b1, b2, b3, b4)
  a1 <- b1 * 0.8
  a2 <- a1 + c(1, 2)
  a3 <- a1 + c(-1, 2)
  a <- sf::st_sfc(a1, a2, a3)

  sf::st_crs(b) <- sf::st_crs(a) <- sf::st_crs(5070)

  b <- sf::st_sf(b, data.frame(idb = c(1, 2, 3, 4)))
  a <- sf::st_sf(a, data.frame(ida = c(1, 2, 3)))

  sf::st_agr(a) <- sf::st_agr(b) <- "constant"

  a_b <- calculate_area_intersection_weights(a, b, normalize = FALSE)
  b_a <- calculate_area_intersection_weights(b, a, normalize = FALSE)

  expect_equal(as.numeric(a_b[1, ]),
               c(1, 1, 1), info = "a1 is 100% covered by b1.")
  expect_equal(as.numeric(a_b[2, ]),
               c(2, 2, 0.5), info = "a2 is 50% covered by b2.")
  expect_equal(as.numeric(a_b[3, ]),
               c(2, 3, 0.375), info = "a2 is 37.5% covered by b3.")
  expect_equal(as.numeric(a_b[4, ]),
               c(3, 3, 0.625), info = "a3 is 62.5% covered by b3.")

  expect_equal(as.numeric(b_a[1, ]),
               c(1, 1, 0.64), info = "b1 is 64% covered by a1")
  expect_equal(as.numeric(b_a[2, ]),
               c(2, 2, 0.32), info = "b2 is 32% covered by a2")
  expect_equal(as.numeric(b_a[3, ]),
               c(3, 2, 0.24), info = "b3 is 24% covered by a2")
  expect_equal(as.numeric(b_a[4, ]),
               c(3, 3, 0.4), info = "b3 is 40% covered by a3")
  expect_equal(as.numeric(b_a[5, ]),
               c(4, NA, NA),
               info = "b4 is not covered")
})

test_that("extended_example", {
    
    x1 = sf::st_polygon(list(rbind(c(-1,-1), c(1,-1),
                                   c(1,1), c(-1,1),
                                   c(-1,-1))))
    x2 = x1 + 2
    x3 = x1 + c(0, 2)
    x4 = x1 + c(2, 0)
    x = sf::st_sfc(x1, x2, x3, x4)
    y1 = x1 * 0.75 + c(-.25, -.25)
    y2 = y1 + 1.5
    y3 = y1 + c(0, 1.5)
    y4 = y1 + c(1.5, 0)
    y = sf::st_sfc(y1,y2,y3, y4)
    
    # plot(x, border = 'red', lwd = 3)
    # plot(y, border = 'green', add = TRUE)
    
    sf::st_crs(x) <- sf::st_crs(y) <- sf::st_crs(5070)
    
    x <- sf::st_sf(x, data.frame(idx = c(1, 2, 3, 4)))
    y <- sf::st_sf(y, data.frame(idy = c("a", "b", "c", "d")))
    
    sf::st_agr(y) <- sf::st_agr(x) <- "constant"
    
    # say we have data from `x` that we want sampled to `y`.
    # this gives the percent of each `x` that intersects each `y`
    
    (x_y <- calculate_area_intersection_weights(x, y, normalize = FALSE))
    
    # note that `w` sums to 1 where `y` completely covers `x`.
    # w does not sum to 1 where `y` partially covers `x`
    expect_equal(dplyr::summarize(dplyr::group_by(x_y, idx), w = sum(w))$w, c(1, .25, .5, .5))
    
    # and to use these in an area weighted sum we need the area of each polygon in 
    # the source data (x)
    
    (z_x_y <- dplyr::tibble(idx = unique(x_y$idx),
                            val = c(1, 2, 3, 4)) |>
        dplyr::left_join(x_y, by = "idx") |>
        dplyr::mutate(val = ifelse(is.na(w), NA, val),
                      areasqkm = 2 ^ 2) |> # area of each polygon in `y`
        dplyr::group_by(idy) |> # group so we get one row per `y`
        # now we weight by the percent of the area from each polygon in `y` per polygon in `x`
        dplyr::mutate(new_val = sum( (val * w * areasqkm), na.rm = TRUE ) / sum(w * areasqkm)))
    
    # summarize to show the result
    (z_x_y <- dplyr::summarise(z_x_y, new_val = unique(new_val)))
    
    x$val <- c(1, 2, 3, 4)
    y$val <- z_x_y$new_val
    
    # plot(x["val"], border = 'red', lwd = 3, breaks = c(0, 1, 2, 3, 4))
    # 
    # plot(x["val"], border = 'red', lwd = 3, reset = FALSE, breaks = c(0, 1, 2, 3, 4))
    # plot(y["val"], border = 'green', add = TRUE, breaks = c(0, 1, 2, 3, 4))
    
    # we can go in reverse if we had data from x that we want sampled to y
    
    x <- dplyr::select(x, -val)
    y <- dplyr::select(y, -val)
    
    # if we reverse the above, we take data from y that we want to sample to x
    (y_x <- calculate_area_intersection_weights(y, x, normalize = FALSE))
    
    # note that `w` sums to 1 only where `x` completely covers `y`
    # in this case all x are vully covered by y
    
    expect_true(all(dplyr::summarize(dplyr::group_by(y_x, idy), w = sum(w))$w == 1))
    
    # and to use these in an area weighted sum we need the area of each polygon in 
    # the source data (x)
    
    (z_y_x <- dplyr::tibble(idy = unique(y_x$idy),
                            val = c(1, 2, 3, 4)) |>
        dplyr::left_join(y_x, by = "idy") |>
        dplyr::mutate(val = ifelse(is.na(w), NA, val),
                      areasqkm = 1.5 ^ 2) |> # area of each polygon in `x`
        dplyr::group_by(idx) |> # group so we get one row per `x`
        # now we weight by the percent of the area from each polygon in `x` per polygon in `y`
        dplyr::mutate(new_val = sum( (val * w * areasqkm), na.rm = TRUE ) / sum(w * areasqkm)))
    
    # summarize to show the result
    (z_y_x <- dplyr::summarise(z_y_x, new_val = unique(new_val)))
    
    x$val <- z_y_x$new_val
    y$val <- c(1, 2, 3, 4)
    
    # plot(x["val"], border = 'red', lwd = 3, breaks = c(0, 1, 2, 3, 4))
    # 
    # plot(x["val"], border = 'red', lwd = 3, reset = FALSE, breaks = c(0, 1, 2, 3, 4))
    # plot(y["val"], border = 'green', add = TRUE, breaks = c(0, 1, 2, 3, 4))
    
    x <- dplyr::select(x, -val)
    y <- dplyr::select(y, -val)
    
    # with `normalize = TRUE`, `w` will sum to 1 when the target
    # completely covers the source rather than when the source completly
    # covers the target.
    
    (x_y_norm <- calculate_area_intersection_weights(x, y, normalize = TRUE))
    
    expect_true(all(dplyr::summarize(dplyr::group_by(x_y_norm, idy), w = sum(w))$w == 1))
    
    # and to use these in an area weighted sum we need the area of each polygon in 
    # the source data (x)
    
    (z_x_y_norm <- dplyr::tibble(idx = unique(x_y_norm$idx),
                                 val = c(1, 2, 3, 4)) |>
        dplyr::left_join(x_y_norm, by = "idx") |>
        dplyr::mutate(val = ifelse(is.na(w), NA, val)) |> # area of each polygon in `y`
        dplyr::group_by(idy) |> # group so we get one row per `y`
        # now we weight by the percent of the area from each polygon in `y` per polygon in `x`
        dplyr::mutate(new_val = sum( (val * w), na.rm = TRUE )))
    
    # summarize to show the result
    (z_x_y_norm <- dplyr::summarise(z_x_y_norm, new_val = unique(new_val)))
    
    expect_equal(z_x_y, z_x_y_norm)
    
    x$val <- c(1, 2, 3, 4)
    y$val <- z_x_y_norm$new_val
    
    # plot(x["val"], border = 'red', lwd = 3, breaks = c(0, 1, 2, 3, 4))
    # 
    # plot(x["val"], border = 'red', lwd = 3, reset = FALSE, breaks = c(0, 1, 2, 3, 4))
    # plot(y["val"], border = 'green', add = TRUE, breaks = c(0, 1, 2, 3, 4))
    
    x <- dplyr::select(x, -val)
    y <- dplyr::select(y, -val)
    
    (y_x_norm <- calculate_area_intersection_weights(y, x, normalize = TRUE))
    
    expect_true(all(dplyr::summarize(dplyr::group_by(y_x_norm, idx), w = sum(w))$w == 1))
    
    # and to use these in an area weighted sum we need the area of each polygon in 
    # the source data (x)
    
    (z_y_x_norm <- dplyr::tibble(idy = unique(y_x_norm$idy),
                                 val = c(1, 2, 3, 4)) |>
        dplyr::left_join(y_x_norm, by = "idy") |>
        dplyr::mutate(val = ifelse(is.na(w), NA, val)) |>
        dplyr::group_by(idx) |> # group so we get one row per `x`
        # now we weight by the percent of the area from each polygon in `x` per polygon in `y`
        dplyr::mutate(new_val = sum( (val * w), na.rm = TRUE )))
    
    # summarize to show the result
    (z_y_x_norm <- dplyr::summarise(z_y_x_norm, new_val = unique(new_val)))
    
    x$val <- z_y_x_norm$new_val
    y$val <- c(1, 2, 3, 4)
    
    # plot(x["val"], border = 'red', lwd = 3, breaks = c(0, 1, 2, 3, 4))
    # 
    # plot(x["val"], border = 'red', lwd = 3, reset = FALSE, breaks = c(0, 1, 2, 3, 4))
    # plot(y["val"], border = 'green', add = TRUE, breaks = c(0, 1, 2, 3, 4))
    
    expect_equal(z_y_x$new_val, z_y_x_norm$new_val)
    
  
})
