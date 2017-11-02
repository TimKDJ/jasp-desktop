expect_equal_plots <- function(image, reference, dir) {
  vdiffr::expect_doppelganger(paste(dir, reference, sep="-"), image, path=dir)
}
