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

  a_b <- calculate_area_intersection_weights(a, b)
  b_a <- calculate_area_intersection_weights(b, a)

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
  expect_equal(data.frame(b_a[5, ]),
               data.frame(tibble::tibble(idb = 4,
                                 ida = as.numeric(NA),
                                 w = as.numeric(NA))),
               info = "b4 is not covered")
})
