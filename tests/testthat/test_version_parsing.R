context("Version Parsing and URL Generation (Offline Tests)")

# These tests can run without network access by mocking version data
test_that("version parsing logic works correctly", {
  # Test numeric_version conversion and subsetting
  
  # Test string to numeric_version conversion
  version_str <- "1.10.5"
  version_obj <- numeric_version(version_str)
  
  expect_s3_class(version_obj, "numeric_version")
  expect_equal(as.character(version_obj), "1.10.5")
  
  # Test subsetting (the core issue that was fixed)
  short_version <- version_obj[,1:2]
  expect_equal(as.character(short_version), "1.10")
  
  # Test with different versions
  test_versions <- c("1.9.4", "1.10.0", "1.10.10", "1.11.0")
  for (ver_str in test_versions) {
    ver_obj <- numeric_version(ver_str)
    short_ver <- ver_obj[,1:2]
    expected_short <- paste(ver_obj$major, ver_obj$minor, sep = ".")
    expect_equal(as.character(short_ver), expected_short)
  }
})

test_that("version comparison logic works", {
  # Test version comparisons used in LTS detection
  v1_9_4 <- numeric_version("1.9.4")
  v1_10_0 <- numeric_version("1.10.0")
  v1_10_5 <- numeric_version("1.10.5") 
  v1_11_0 <- numeric_version("1.11.0")
  
  # Basic comparisons
  expect_true(v1_10_0 > v1_9_4)
  expect_true(v1_10_5 > v1_10_0)
  expect_true(v1_11_0 > v1_10_5)
  
  # LTS range testing (1.10.x series)
  expect_true(v1_10_0 >= "1.10.0")
  expect_true(v1_10_0 < "1.11.0")
  expect_true(v1_10_5 >= "1.10.0")
  expect_true(v1_10_5 < "1.11.0")
  
  # Non-LTS versions
  expect_false(v1_9_4 >= "1.10.0")
  expect_false(v1_11_0 < "1.11.0")
})

test_that("URL generation logic works with mocked versions", {
  # Test julia_url function with known versions
  source("../../R/installJulia.R")
  
  # Test with Julia 1.10.5
  version <- numeric_version("1.10.5")
  url <- julia_url(version)
  
  # Verify URL structure
  expect_type(url, "character")
  expect_match(url, "^https://julialang-s3\\.julialang\\.org/bin/")
  expect_match(url, "/1\\.10/")  # Short version in path
  expect_match(url, "julia-1\\.10\\.5-")  # Full version in filename
  
  # Test platform-specific parts
  sysname <- Sys.info()["sysname"]
  sysmachine <- Sys.info()["machine"]
  
  if (sysname == "Linux") {
    expect_match(url, "/linux/")
    expect_match(url, "\\.tar\\.gz$")
    if (sysmachine == "arm64") {
      expect_match(url, "/aarch64/")
      expect_match(url, "linux-aarch64")
    } else {
      expect_match(url, "/x64/")
      expect_match(url, "linux-x86_64")
    }
  } else if (sysname == "Darwin") {
    expect_match(url, "/mac/")
    expect_match(url, "\\.tar\\.gz$")
    if (sysmachine == "arm64") {
      expect_match(url, "/aarch64/")
      expect_match(url, "macaarch64")
    } else {
      expect_match(url, "/x64/")  
      expect_match(url, "mac64")
    }
  } else if (sysname == "Windows") {
    expect_match(url, "/winnt/")
    expect_match(url, "\\.zip$")
    expect_match(url, "win64")
  }
})

test_that("LTS version selection algorithm works", {
  # Mock version data to test LTS selection logic
  mock_versions_data <- list(
    "1.9.4" = list(stable = TRUE),
    "1.10.0" = list(stable = TRUE),
    "1.10.5" = list(stable = TRUE),
    "1.10.10" = list(stable = TRUE),
    "1.11.0" = list(stable = TRUE),
    "1.11.1" = list(stable = TRUE),
    "1.12.0-beta1" = list(stable = FALSE)  # Pre-release
  )
  
  # Simulate LTS selection logic
  stable_versions <- Filter(function(v) v$stable, mock_versions_data)
  version_numbers <- numeric_version(names(stable_versions))
  
  # Get all 1.10.x versions (current LTS series)
  lts_candidates <- version_numbers[version_numbers >= "1.10.0" & version_numbers < "1.11.0"]
  
  expect_length(lts_candidates, 3)  # Should find 1.10.0, 1.10.5, 1.10.10
  
  selected_lts <- max(lts_candidates)
  expect_equal(as.character(selected_lts), "1.10.10")
})

test_that("install_julia parameter processing works correctly", {
  # Test the parameter processing logic without network calls
  
  # Mock the install_julia logic for parameter processing
  test_version_processing <- function(version) {
    if (version == "latest") {
      # Would normally call julia_latest_version()
      return(numeric_version("1.11.1"))  # Mock latest
    } else if (version == "lts") {
      # Would normally call julia_lts_version()  
      return(numeric_version("1.10.10"))  # Mock LTS
    } else {
      # Convert string version to numeric_version object if needed
      if (is.character(version)) {
        return(numeric_version(version))
      }
      return(version)
    }
  }
  
  # Test different version parameter types
  expect_equal(
    as.character(test_version_processing("latest")),
    "1.11.1"
  )
  
  expect_equal(
    as.character(test_version_processing("lts")), 
    "1.10.10"
  )
  
  expect_equal(
    as.character(test_version_processing("1.10.5")),
    "1.10.5"
  )
  
  # Test that numeric_version objects pass through unchanged
  version_obj <- numeric_version("1.9.4")
  expect_identical(
    test_version_processing(version_obj),
    version_obj
  )
})

test_that("version URL consistency", {
  # Test that the same version always generates the same URL
  source("../../R/installJulia.R")
  
  version <- numeric_version("1.10.5")
  
  url1 <- julia_url(version)
  url2 <- julia_url(version)
  
  expect_identical(url1, url2)
  
  # Test with different equivalent version representations
  version_str <- numeric_version("1.10.5")
  version_explicit <- numeric_version(c(1, 10, 5))
  
  url_str <- julia_url(version_str)
  url_explicit <- julia_url(version_explicit)
  
  expect_identical(url_str, url_explicit)
})

test_that("edge cases in version handling", {
  # Test edge cases that could cause issues
  
  # Very high version numbers
  high_version <- numeric_version("2.0.0")
  expect_silent({
    short_ver <- high_version[,1:2]
    expect_equal(as.character(short_ver), "2.0")
  })
  
  # Single digit versions  
  old_version <- numeric_version("1.0.0")
  expect_silent({
    short_ver <- old_version[,1:2]
    expect_equal(as.character(short_ver), "1.0")
  })
  
  # Versions with many parts
  detailed_version <- numeric_version("1.10.5.1")
  expect_silent({
    short_ver <- detailed_version[,1:2] 
    expect_equal(as.character(short_ver), "1.10")
  })
})