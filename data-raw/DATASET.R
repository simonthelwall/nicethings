## code to prepare `DATASET` dataset goes here

solid <- read.csv("data-raw/SOLID_anthropometry.csv", header = TRUE,
                  stringsAsFactors = FALSE)
head(solid)
names(solid) <- tolower(names(solid))

usethis::use_data(solid, overwrite = TRUE)

smokers <- data.frame(
  smoker = c(rep(1L, (29961 + 39)), rep(0L, (59994 + 6))),
  lung_cancer = c(rep(1L, 39), rep(0L, 29961), rep(1L, 6), rep(0L, 59994))
  )

table(smokers$smoker, smokers$lung_cancer)
usethis::use_data(smokers, overwrite = TRUE)
