context("Julia version selection")

test_that("latest stable version uses semantic ordering", {
    versions <- list(
        "1.9.4" = list(stable = TRUE),
        "1.12.5" = list(stable = TRUE),
        "1.12.6-rc1" = list(stable = FALSE)
    )
    expect_equal(
        as.character(JuliaCall:::.latest_stable_julia_version_from_versions(versions)),
        "1.12.5"
    )
})

test_that("latest stable handles v-prefixed versions", {
    versions <- list(
        "v1.10.10" = list(stable = TRUE),
        "v1.12.5" = list(stable = TRUE)
    )
    expect_equal(
        as.character(JuliaCall:::.latest_stable_julia_version_from_versions(versions)),
        "1.12.5"
    )
})

test_that("latest lts prefers lts and falls back to latest stable", {
    versions_with_lts <- list(
        "1.12.5" = list(stable = TRUE),
        "1.10.10" = list(stable = TRUE, lts = TRUE)
    )
    expect_equal(
        as.character(JuliaCall:::.latest_lts_julia_version_from_versions(versions_with_lts)),
        "1.10.10"
    )

    versions_without_lts <- list(
        "1.11.4" = list(stable = TRUE),
        "1.12.5" = list(stable = TRUE)
    )
    expect_equal(
        as.character(JuliaCall:::.latest_lts_julia_version_from_versions(versions_without_lts)),
        "1.12.5"
    )
})
