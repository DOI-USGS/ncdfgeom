context("create_cell_geometry")

test_that("basic", {
  Y_coords <- c(5, 4, 3, 2, 1)
  X_coords <- c(1, 2, 3, 4)
  prj <- "+init=epsg:5070"
  expect_warning(cells <- create_cell_geometry(X_coords, Y_coords, prj))

  expect_true(all(c("grid_ids", "X_ind", "Y_ind") %in% names(cells)))

  expect_equal(cells[cells$grid_ids == 1, ]$X_ind, 1)
  expect_equal(cells[cells$grid_ids == 1, ]$Y_ind, 1)

  expect_equal(cells[cells$grid_ids == (5*4), ]$X_ind, 4)
  expect_equal(cells[cells$grid_ids == (5*4), ]$Y_ind, 5)

  expect_s3_class(cells$geometry, "sfc_POLYGON")

  expect_true(st_crs(cells) == st_crs(prj))
})

test_that("daymet_subset", {
  X_coords <- c(1986250, 1987250, 1988250, 1989250, 1990250, 1991250, 1992250,
                1993250, 1994250, 1995250, 1996250, 1997250, 1998250, 1999250,
                2000250, 2001250, 2002250, 2003250, 2004250, 2005250, 2006250,
                2007250, 2008250, 2009250, 2010250, 2011250, 2012250, 2013250,
                2014250, 2015250, 2016250, 2017250, 2018250, 2019250, 2020250,
                2021250, 2022250, 2023250, 2024250, 2025250, 2026250, 2027250,
                2028250, 2029250, 2030250, 2031250, 2032250, 2033250, 2034250,
                2035250, 2036250, 2037250, 2038250, 2039250, 2040250, 2041250,
                2042250, 2043250, 2044250, 2045250, 2046250, 2047250, 2048250,
                2049250, 2050250, 2051250, 2052250, 2053250, 2054250, 2055250,
                2056250, 2057250, 2058250, 2059250, 2060250, 2061250, 2062250,
                2063250, 2064250)
  Y_coords <- c(-503500, -504500, -505500, -506500, -507500, -508500, -509500,
                -510500, -511500, -512500, -513500, -514500, -515500, -516500,
                -517500, -518500, -519500, -520500, -521500, -522500, -523500,
                -524500, -525500, -526500, -527500, -528500, -529500, -530500,
                -531500, -532500, -533500, -534500, -535500, -536500, -537500,
                -538500, -539500, -540500, -541500, -542500, -543500, -544500,
                -545500, -546500, -547500, -548500, -549500, -550500, -551500,
                -552500, -553500, -554500, -555500, -556500, -557500)

  prj <- "+proj=lcc +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +units=m +lat_0=42.5 +lon_0=-100 +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs"

  cells <- create_cell_geometry(X_coords, Y_coords, prj)

  expect_equal(cells[cells$grid_ids == 1, ]$X_ind, 1)
  expect_equal(cells[cells$grid_ids == 1, ]$Y_ind, 1)

  expect_equal(cells[cells$grid_ids == (length(X_coords) * length(Y_coords)), ]$X_ind, length(X_coords))
  expect_equal(cells[cells$grid_ids == (length(X_coords) * length(Y_coords)), ]$Y_ind, length(Y_coords))

  expect_s3_class(cells$geometry, "sfc_POLYGON")

  expect_true(st_crs(cells) == st_crs(prj))
})

test_that("crossing the date line works", {
  nc <- open.nc(list.files(pattern = "flxf06.gdas.1979.grb2.nc$", recursive = TRUE, full.names = TRUE))
  x <- var.get.nc(nc, "lon")
  y <- var.get.nc(nc, "lat")
  close.nc(nc)
  warn <- capture_warnings(cells <- create_cell_geometry(x, y, 4326, regularize = TRUE))
  expect_equal(nrow(cells), 17440)
  expect_true("Found longitude greater than 180. Converting to -180, 180" %in% warn)
  expect_true("Found longidude near international date line. Using 0-360 longitude." %in% warn)
})

test_that("crossing date line more", {
  skip_on_cran()
  
  geom <- matrix(c(-179, -170, 170, 179.9, -179,
                   51, 63, 63, 52, 51),
                 ncol = 2) %>%
    list() %>%
    st_polygon() %>%
    st_sfc(crs = 4326)

  x<- c(169, 169.5, 170, 170.5, 171, 171.5, 172, 172.5, 173,
        173.5, 174, 174.5, 175, 175.5, 176, 176.5, 177, 177.5, 178, 178.5,
        179, 179.5, 180, 180.5, 181, 181.5, 182, 182.5, 183, 183.5, 184,
        184.5, 185, 185.5, 186, 186.5, 187, 187.5, 188, 188.5, 189, 189.5,
        190, 190.5, 191, 191.5, 192, 192.5, 193, 193.5, 194, 194.5, 195,
        195.5, 196, 196.5, 197, 197.5, 198, 198.5, 199, 199.5, 200, 200.5,
        201, 201.5, 202, 202.5, 203, 203.5, 204, 204.5, 205, 205.5, 206,
        206.5, 207, 207.5, 208, 208.5, 209, 209.5, 210, 210.5, 211, 211.5,
        212, 212.5, 213, 213.5, 214, 214.5, 215, 215.5, 216, 216.5, 217,
        217.5, 218, 218.5, 219, 219.5, 220, 220.5, 221, 221.5, 222, 222.5,
        223, 223.5, 224, 224.5, 225, 225.5, 226, 226.5, 227, 227.5, 228,
        228.5, 229, 229.5, 230, 230.5, 231, 231.5, 232, 232.5, 233, 233.5,
        234, 234.5, 235, 235.5, 236, 236.5, 237)

  y <- c(73, 72.5, 72, 71.5, 71, 70.5, 70, 69.5, 69, 68.5,
         68, 67.5, 67, 66.5, 66, 65.5, 65, 64.5, 64, 63.5, 63, 62.5, 62,
         61.5, 61, 60.5, 60, 59.5, 59, 58.5, 58, 57.5, 57, 56.5, 56, 55.5,
         55, 54.5, 54, 53.5, 53, 52.5, 52, 51.5, 51, 50.5, 50, 49.5, 49)

  warn <- capture_warnings(cells <- create_cell_geometry(x, y, 4326, regularize = TRUE))

  expect_true(nrow(cells) == 6713)
  expect_true("Found longidude near international date line. Using 0-360 longitude." %in% warn)

  warn <- capture_warnings(cells <- create_cell_geometry(x, y, 4326, geom, regularize = TRUE))
  
  testthat::skip_on_os("mac")
  # this fails on mac but not windows or linux?
  expect_true(nrow(cells) == 936)
  
  expect_true("Found longidude near international date line. Using 0-360 longitude." %in% warn)

  geom <- matrix(c(-173.1318, -179.1185,
                   -179.1336, -179.1488, 179.226, 179.2137, 173.6277, 173.3859,
                   173.317, 173.3046, 173.2945, 172.3714, 172.3603, 172.3528, 172.3477,
                   172.3462, 172.3474, 172.3506, 172.3565, 172.3645, -173.1305,
                   -152.0446, -151.5654, -151.5436, -151.5136, -151.512, -151.5105,
                   -151.6557, -151.6715, -152.7324, -154.3472, -154.3605, -159.1322,
                   -159.1487, -159.1973, -159.21, -159.4758, -159.489, -159.506,
                   -169.2089, -169.2415, -173.1318, 51.95922, 51.15853, 51.15695,
                   51.1572, 51.29705, 51.29861, 52.29857, 52.33802, 52.35366, 52.35788,
                   52.36279, 52.88758, 52.89557, 52.90398, 52.91396, 52.92348, 52.93215,
                   52.93974, 52.94769, 52.95491, 60.73862, 64.41658, 64.37961, 64.37583,
                   64.35263, 64.34964, 64.31368, 63.02242, 62.93349, 60.74787, 58.45999,
                   58.44583, 54.87275, 54.85934, 54.82705, 54.82081, 54.71403, 54.70982,
                   54.70642, 52.71876, 52.7118, 51.95922), ncol = 2) %>%
    list() %>%
    st_polygon() %>%
    st_sfc(crs = 4326)

  warn <- capture_warnings(cells <- create_cell_geometry(x, y, 4326, geom, regularize = TRUE))
  expect_true(nrow(cells) == 1872)

})
