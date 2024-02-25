#' Write and read back, just as in the example:

#### Small example: ####
# Define dimension variables and variables:
single_target_dimVars <- list(
    ping_time = seq(as.POSIXct("1970-02-22 00:00:00", tz = "UTC"), by = 2, length.out = 2), 
    beam = c("Beam1", "Beam2")
)
single_target_vars <- list(
    single_target_identifier = structure(
        list( # ping_time
            structure(list(1:3, 1), names = single_target_dimVars$beam), # beam
            structure(list(2:8, 1:2), names = single_target_dimVars$beam) # beam
        ), 
        names = as.character(single_target_dimVars$ping_time)
    ), 
    single_target_detection_algorithm = 
        rep(list(
            rep(
                "testData", 
                length(single_target_dimVars$beam))), 
            length(single_target_dimVars$ping_time)
        ), 
    single_target_range = structure(
        list( # ping_time
            structure(
                list(
                    c(2, 4, 7), 
                    3
                ), 
                names = single_target_dimVars$beam
            ), # beam
            structure(
                list(
                    c(1, 8, 29, 55, 60, 60, 79), 
                    c(5, 35)
                ), 
                names = single_target_dimVars$beam
            ) # beam
        ), 
        names = as.character(single_target_dimVars$ping_time)
    )
)

# Write the data to SONAR-neCDF4:
filePath <- tempfile(fileext = "nc")
writeSTD_vars_dimVars(
    vars = single_target_vars, 
    dimVars = single_target_dimVars, 
    filePath = filePath
)

# Read back in:
dflat <- readSTD(filePath)

# Then try to write again from the flat table (long format):
filePath2 <- tempfile(fileext = "nc")
writeSTD(
    dflat, 
    filePath = filePath2
)

# The original and written data are identical:
dflat2 <- readSTD(filePath2)
expect_identical(dflat, dflat2)

#### A bigger example with TS(f): ####
# Define the dimension variables with frequency as the third dimension:
single_target_dimVars_TSf <- c(
    single_target_dimVars, 
    list(frequency = as.double(seq(36, 40) * 1e3))
)
nFreq <- length(single_target_dimVars_TSf$frequency)

# Generate sorted radom expoential values as TS(f):
single_target_vars_TSf <- c(
    single_target_vars, 
    list(
        uncompensated_TS  = structure(
            list( # ping_time
                structure(
                    list(
                        lapply(rep(3, nFreq),  stats::rexp), 
                        lapply(rep(1, nFreq),  stats::rexp)
                    ), 
                    names = single_target_dimVars$beam
                ), # beam
                structure(
                    list(
                        lapply(rep(7, nFreq),  stats::rexp), 
                        lapply(rep(2, nFreq),  stats::rexp)
                    ), 
                    names = single_target_dimVars$beam
                ) # beam
            ), 
            names = as.character(single_target_dimVars$ping_time)
        )
    )
)


# Write the data:
filePath3 <- tempfile(fileext = "nc")
writeSTD_vars_dimVars(
    vars = single_target_vars, 
    dimVars = single_target_dimVars, 
    filePath = filePath3
)

# Read back in to a flat table:
dflat3 <- readSTD(filePath3)

# Then try to write again:
filePath4 <- tempfile(fileext = "nc")
writeSTD(
    dflat3, 
    filePath = filePath4
)

# The original and written data are identical:
dflat4 <- readSTD(filePath4)
expect_identical(dflat3, dflat4)
