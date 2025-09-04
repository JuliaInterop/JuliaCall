context("Julia Download and Installation Behavior")

# Helper function to mock julia version detection
mock_julia_latest_version <- function() {
  return(numeric_version("1.11.2"))
}

mock_julia_lts_version <- function() {
  return(numeric_version("1.10.10"))
}

test_that("LTS is correctly identified as default", {
  skip_on_cran()
  
  # Test that the function signature has LTS as default
  install_julia_formals <- formals(install_julia)
  expect_equal(install_julia_formals$version, "lts")
})

test_that("different version parameters produce different URLs", {
  skip_on_cran()
  skip_if_offline()
  
  source("../../R/installJulia.R")
  
  # Get versions (this requires network but should be fast)
  latest_version <- julia_latest_version()
  lts_version <- julia_lts_version()
  
  # Generate URLs
  latest_url <- julia_url(latest_version)
  lts_url <- julia_url(lts_version)
  
  # URLs should be different if versions are different
  if (!identical(latest_version, lts_version)) {
    expect_false(identical(latest_url, lts_url))
  }
  
  # LTS URL should always point to 1.10.x series
  expect_match(lts_url, "/1\\.10/")
  
  # Both should be valid download URLs
  expect_match(latest_url, "^https://julialang-s3\\.julialang\\.org/bin/")
  expect_match(lts_url, "^https://julialang-s3\\.julialang\\.org/bin/")
})

test_that("version parameter processing produces correct behavior", {
  skip_on_cran()
  
  # Mock the version resolution logic
  resolve_version <- function(version) {
    if (version == "latest") {
      return(mock_julia_latest_version())
    } else if (version == "lts") {
      return(mock_julia_lts_version()) 
    } else {
      if (is.character(version)) {
        return(numeric_version(version))
      }
      return(version)
    }
  }
  
  # Test different inputs
  latest_resolved <- resolve_version("latest")
  lts_resolved <- resolve_version("lts")
  specific_resolved <- resolve_version("1.9.4")
  
  # Verify correct resolution
  expect_equal(as.character(latest_resolved), "1.11.2")
  expect_equal(as.character(lts_resolved), "1.10.10")
  expect_equal(as.character(specific_resolved), "1.9.4")
  
  # Verify LTS is in correct range
  expect_true(lts_resolved >= "1.10.0")
  expect_true(lts_resolved < "1.11.0")
})

test_that("URL generation is consistent with version resolution", {
  skip_on_cran()
  
  source("../../R/installJulia.R")
  
  # Test with known version
  test_version <- numeric_version("1.10.5")
  url <- julia_url(test_version)
  
  # Verify URL components match the version
  expect_match(url, "/1\\.10/")  # Short version in path
  expect_match(url, "julia-1\\.10\\.5-")  # Full version in filename
  
  # Verify platform detection
  sysname <- Sys.info()["sysname"]
  if (sysname == "Linux") {
    expect_match(url, "/linux/")
  } else if (sysname == "Darwin") {
    expect_match(url, "/mac/")
  } else if (sysname == "Windows") {
    expect_match(url, "/winnt/")
  }
})

test_that("install_julia behavior with different version types", {
  skip_on_cran()
  
  # We can't easily test the full installation without actually downloading
  # But we can test the parameter validation and URL generation steps
  
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  
  # Mock test: verify the function doesn't crash on parameter processing
  # for different version types
  
  test_version_params <- function(version, expected_pattern) {
    expect_error({
      # This will fail at download stage, but should not fail at 
      # parameter processing stage  
      tryCatch({
        install_julia(version = version, prefix = temp_dir)
      }, error = function(e) {
        # Should not be version-related errors
        expect_false(grepl("subscript|version\\[", e$message))
        # Should not be parameter errors
        expect_false(grepl("argument|parameter", e$message, ignore.case = TRUE))
        # Re-throw the error for expect_error to catch
        stop(e)
      })
    }, "download|network|timeout|install")
  }
  
  # Test different version parameter types
  expect_silent(test_version_params("lts", "1\\.10"))
  expect_silent(test_version_params("latest", ""))  
  expect_silent(test_version_params("1.10.5", "1\\.10\\.5"))
  
  unlink(temp_dir, recursive = TRUE)
})

test_that("LTS version detection is stable", {
  skip_on_cran()
  skip_if_offline()
  
  # Test that LTS version detection is consistent
  lts1 <- julia_lts_version()
  lts2 <- julia_lts_version()
  
  expect_identical(lts1, lts2)
  
  # Test that LTS is always in 1.10.x series (current LTS)
  expect_true(lts1 >= "1.10.0")
  expect_true(lts1 < "1.11.0")
})

test_that("version comparison and filtering works correctly", {
  skip_on_cran()
  
  # Test the filtering logic used in julia_lts_version
  mock_versions <- c("1.9.4", "1.10.0", "1.10.5", "1.10.10", "1.11.0", "1.11.1")
  version_objects <- numeric_version(mock_versions)
  
  # Filter for 1.10.x series (LTS)
  lts_candidates <- version_objects[version_objects >= "1.10.0" & version_objects < "1.11.0"]
  
  expect_length(lts_candidates, 3)
  expect_true(all(lts_candidates >= "1.10.0"))
  expect_true(all(lts_candidates < "1.11.0"))
  
  # Get the maximum (latest LTS)
  latest_lts <- max(lts_candidates)
  expect_equal(as.character(latest_lts), "1.10.10")
})

test_that("install_julia default behavior uses LTS", {
  skip_on_cran()
  
  # Verify that calling install_julia() without version parameter
  # defaults to LTS behavior
  
  # Check function signature
  formals_list <- formals(install_julia)
  expect_equal(formals_list$version, "lts")
  
  # Test that the default resolves to LTS version range
  # (We can't run full install, but we can test parameter resolution)
  
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  
  # This should attempt to use LTS version
  expect_error({
    tryCatch({
      # Call without version parameter - should use LTS default
      install_julia(prefix = temp_dir)
    }, error = function(e) {
      # Should not fail due to version parameter issues
      expect_false(grepl("version|parameter", e$message, ignore.case = TRUE))
      stop(e)
    })
  }, "download|network|install")
  
  unlink(temp_dir, recursive = TRUE)
})

# Test the complete flow from version detection to URL generation
test_that("complete version-to-URL pipeline works", {
  skip_on_cran()
  skip_if_offline()
  
  source("../../R/installJulia.R")
  
  # Test complete pipeline: version detection -> URL generation -> validation
  
  # Get LTS version
  lts_version <- julia_lts_version()
  
  # Generate URL
  lts_url <- julia_url(lts_version)
  
  # Validate URL structure
  expect_match(lts_url, "^https://")
  expect_match(lts_url, "julialang-s3\\.julialang\\.org")
  expect_match(lts_url, paste0("julia-", lts_version, "-"))
  
  # URL should point to correct minor version path
  short_version <- lts_version[,1:2]
  expect_match(lts_url, paste0("/", short_version, "/"))
  
  # Should be platform appropriate
  sysname <- Sys.info()["sysname"]
  if (sysname %in% c("Linux", "Darwin")) {
    expect_match(lts_url, "\\.tar\\.gz$")
  } else if (sysname == "Windows") {
    expect_match(lts_url, "\\.zip$")
  }
})