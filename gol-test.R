if (!require(testthat)) install.packages('testthat')

source("./gol.R")


# Canvas ----
test_that("canvas dimensions are ok", {
  expect_equal(
    canvas(rows=4, cols=5), 
    matrix(EMPTY, nrow=4, ncol=5)
  )
})

# Print Canvas ----
test_that("canvas is printed", {
  expect_output(
    display(canvas(cols=3, rows=2)),
    paste0(EMPTY_CHAR, EMPTY_CHAR, EMPTY_CHAR, "\n", 
           EMPTY_CHAR, EMPTY_CHAR, EMPTY_CHAR)
  )
})

# Seeding ----
test_that("canvas is randomly populated", {
  expect_equal(
    # count the number of "*"
    sum(populate.random(canvas(4, 7)) == ALIVE),
    5
  )
})

test_that("canvas is populated with a binomic distribution", {
  expect_gte(
    sum(populate.binomial(canvas(10, 10), 0.5) == ALIVE),
    30
  )
})

test_that("canvas is inhabited by specific population",
  expect_equal(
    populate(canvas(3, 2), c(2, 4, 6)),
    matrix(c(EMPTY, ALIVE, EMPTY, ALIVE, EMPTY, ALIVE), ncol=3, nrow=2)
  )
)

# Determine liveness ----
test_that("population alive", {
  expect_equal(
    population.alive(populate.random(canvas(2, 2))),
    TRUE
  )
})

test_that("population dead", {
  expect_equal(
    population.alive(canvas(2, 2)),
    FALSE
  )
})

# Simulation steps ----

test_that("simulation doesn't change canvas size", {
  expect_equal(
    length(population.evolve(canvas(4, 4))),
    16
  )
})
  
test_that("canvas survives 1 iteration", {
  expect_equal(
    # full canvas
     population.alive(
       population.evolve(
         populate.random(canvas(3, 3), 9)
       )
     ),
     TRUE
  )
})

test_that("cell dies in case of underpopulation", {
  expect_equal(
    population.alive(
      population.evolve(
        populate(canvas(3, 3), c(0, 0, 0, 0, 1, 0, 0, 0, 0))
      )
    ),
    FALSE
  )
})
