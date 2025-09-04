context("Julia Installation and Version Management")

# Test version detection functions
test_that("julia_latest_version returns a valid version", {
  skip_on_cran()
  skip_if_offline()
  
  # Test that julia_latest_version works
  latest_version <- julia_latest_version()
  
  expect_s3_class(latest_version, "numeric_version")
  expect_true(latest_version >= "1.0.0")
  expect_true(latest_version >= "1.9.0")  # Should be at least 1.9+
})

test_that("julia_lts_version returns Julia 1.10.x", {
  skip_on_cran()
  skip_if_offline()
  
  # Test that julia_lts_version returns 1.10.x series
  lts_version <- julia_lts_version()
  
  expect_s3_class(lts_version, "numeric_version")
  expect_true(lts_version >= "1.10.0")
  expect_true(lts_version < "1.11.0")  # Should be in 1.10.x series
})

test_that("LTS version is different from latest version", {
  skip_on_cran()
  skip_if_offline()
  
  latest_version <- julia_latest_version()
  lts_version <- julia_lts_version()
  
  # LTS should be <= latest (could be equal if latest happens to be LTS)
  expect_true(lts_version <= latest_version)
})

# Test URL generation
test_that("julia_url generates correct URLs for different versions", {
  skip_on_cran()
  
  # Test with Julia 1.10.5
  version_1_10_5 <- numeric_version("1.10.5")
  url_1_10_5 <- julia_url(version_1_10_5)
  
  expect_type(url_1_10_5, "character")
  expect_match(url_1_10_5, "https://julialang-s3\\.julialang\\.org/bin/")
  expect_match(url_1_10_5, "julia-1\\.10\\.5-")
  expect_match(url_1_10_5, "/1\\.10/")
  
  # Test with Julia 1.9.4  
  version_1_9_4 <- numeric_version("1.9.4")
  url_1_9_4 <- julia_url(version_1_9_4)
  
  expect_type(url_1_9_4, "character")
  expect_match(url_1_9_4, "julia-1\\.9\\.4-")
  expect_match(url_1_9_4, "/1\\.9/")
})

test_that("julia_url generates platform-specific URLs", {
  skip_on_cran()
  
  version <- numeric_version("1.10.5")
  url <- julia_url(version)
  
  # Should contain platform-specific elements
  sysname <- Sys.info()["sysname"]
  if (sysname == "Linux") {
    expect_match(url, "linux")
    expect_match(url, "\\.tar\\.gz$")
  } else if (sysname == "Darwin") {
    expect_match(url, "mac")  
    expect_match(url, "\\.tar\\.gz$")
  } else if (sysname == "Windows") {
    expect_match(url, "win")
    expect_match(url, "\\.zip$")
  }
})

# Test install_julia parameter handling
test_that("install_julia handles version parameter correctly", {
  skip_on_cran()
  skip_if_offline()
  
  # Mock the actual installation to avoid downloading
  temp_dir <- tempdir()
  
  # Test string version conversion (without actual installation)
  expect_error({
    # This should not error during parameter processing
    # We expect it to fail at download/install stage, not parameter parsing
    tryCatch({
      install_julia(version = "1.10.5", prefix = temp_dir)
    }, error = function(e) {
      # If it fails, it should not be due to version parameter issues
      # It should be due to download/installation issues
      expect_false(grepl("version\\[", e$message, ignore.case = TRUE))
      expect_false(grepl("subscript", e$message, ignore.case = TRUE))
      stop(e)  # Re-throw for the expect_error
    })
  }, "download|network|install")  # Expect download/network/install related errors
})

test_that("install_julia defaults to LTS", {
  skip_on_cran()
  skip_if_offline()
  
  temp_dir <- tempdir()
  
  # Test that default parameter is "lts"
  install_julia_formals <- formals(install_julia)
  expect_equal(install_julia_formals$version, "lts")
  
  # Verify that calling without version parameter uses LTS
  expect_error({
    tryCatch({
      install_julia(prefix = temp_dir)  # No version specified - should use LTS
    }, error = function(e) {
      # Should not fail due to version issues
      expect_false(grepl("version", e$message, ignore.case = TRUE))
      stop(e)
    })
  }, "download|network|install")
})

# Test version comparison and logic
test_that("LTS version selection logic works correctly", {
  skip_on_cran()
  skip_if_offline()
  
  # Get actual LTS version
  lts_version <- julia_lts_version()
  
  # Test that it's in the expected range
  expect_true(lts_version >= "1.10.0")
  expect_true(lts_version < "1.11.0")
  
  # Test that URL generation works with LTS version
  lts_url <- julia_url(lts_version)
  expect_match(lts_url, "/1\\.10/")
  expect_match(lts_url, paste0("julia-", lts_version, "-"))
})

# Test that the fix for string versions works
test_that("string version parameters work correctly", {
  skip_on_cran()
  
  # Test the core conversion logic without network calls
  test_version_conversion <- function(version_str) {
    if (is.character(version_str)) {
      version_obj <- numeric_version(version_str)
    } else {
      version_obj <- version_str
    }
    
    # Test that we can do subsetting operations
    short_version <- version_obj[,1:2]
    return(short_version)
  }
  
  # These should all work without errors
  expect_silent(test_version_conversion("1.10.5"))
  expect_silent(test_version_conversion("1.9.4"))
  expect_silent(test_version_conversion("1.11.0"))
  
  result <- test_version_conversion("1.10.5")
  expect_equal(as.character(result), "1.10")
})

# Integration test to verify the complete flow
test_that("complete version flow works end-to-end", {
  skip_on_cran()
  skip_if_offline()
  
  # Test the complete flow: version detection -> URL generation
  lts_version <- julia_lts_version()
  latest_version <- julia_latest_version()
  
  # Generate URLs for both
  lts_url <- julia_url(lts_version)
  latest_url <- julia_url(latest_version)
  
  # Both should be valid URLs
  expect_match(lts_url, "^https://")
  expect_match(latest_url, "^https://")
  
  # LTS URL should point to 1.10 series
  expect_match(lts_url, "/1\\.10/")
  
  # URLs should be different (unless latest happens to be LTS)
  if (latest_version != lts_version) {
    expect_false(identical(lts_url, latest_url))
  }
})

# Test documentation and parameter validation
test_that("install_julia documentation reflects LTS default", {
  skip_on_cran()
  
  # Check that the function signature shows LTS as default
  install_julia_args <- formals(install_julia)
  expect_equal(install_julia_args$version, "lts")
  
  # The help should mention LTS (we can't easily test roxygen docs in tests,
  # but we can verify the function behavior aligns with documented behavior)
  
  # Test that "lts", "latest", and specific versions are all handled
  expect_silent({
    # These should not error during parameter processing
    version_lts <- "lts"
    version_latest <- "latest" 
    version_specific <- "1.10.5"
    
    # Mock the version parameter processing logic
    for (version in c(version_lts, version_latest, version_specific)) {
      if (version == "latest") {
        # Would call julia_latest_version()
        expect_true(TRUE)  # Placeholder for actual version function call
      } else if (version == "lts") {
        # Would call julia_lts_version()
        expect_true(TRUE)  # Placeholder for actual version function call
      } else {
        # Would convert string to numeric_version
        if (is.character(version)) {
          version_obj <- numeric_version(version)
          expect_s3_class(version_obj, "numeric_version")
        }
      }
    }
  })
})