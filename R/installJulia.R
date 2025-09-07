julia_default_install_dir <- function(){
    if (exists("R_user_dir", asNamespace("tools"))) {
        R_user_dir <- get("R_user_dir", envir = asNamespace("tools"))
        return(file.path(R_user_dir("JuliaCall"), "julia"))
    }
    dir <- if (requireNamespace("rappdirs", quietly = TRUE)) {
        file.path(rappdirs::user_data_dir("JuliaCall"), "julia")
    } else {
        NULL
    }
    return(dir)
}

#' Get the latest stable Julia version.
#'
#' @return A numeric_version object representing the latest stable Julia version.
#' @export
julia_latest_version <- function(){
    url <- "https://julialang-s3.julialang.org/bin/versions.json"
    file <- tempfile()
    utils::download.file(url, file)
    versions <- rjson::fromJSON(file=file)

    max(numeric_version(names(Filter(function(v) v$stable, versions))))
}

#' Get the latest Long Term Support (LTS) Julia version.
#'
#' @return A numeric_version object representing the latest LTS Julia version.
#' @export
julia_lts_version <- function(){
    url <- "https://julialang-s3.julialang.org/bin/versions.json"
    file <- tempfile()
    utils::download.file(url, file)
    versions <- rjson::fromJSON(file=file)

    # Find the latest LTS version (currently 1.10.x series)
    lts_versions <- names(Filter(function(v) v$stable && isTRUE(v$lts), versions))
    if (length(lts_versions) > 0) {
        max(numeric_version(lts_versions))
    } else {
        # Fallback to latest stable if no LTS is marked
        julia_latest_version()
    }
}


julia_url <- function(version){
    sysmachine <- Sys.info()["machine"]
    arch <- if (sysmachine == "arm64") {
        "aarch64"
    } else if (.Machine$sizeof.pointer == 8) {
        "x64"
    } else {
        "x86"
    }
    short_version <- version[,1:2]
    sysname <- Sys.info()["sysname"]
    if (sysname == "Linux") {
        os <- "linux"
        slug <- "linux-x86_64"
        ext <- "tar.gz"
    } else if (sysname == "Darwin") {
        os <- "mac"
        slug <- ifelse(sysmachine == "arm64", "macaarch64", "mac64")
        ext <- "tar.gz"
    } else if (sysname == "Windows") {
        os <- "winnt"
        slug <- "win64"
        ext <- "zip"
    } else {
        stop("Unknown or unsupported OS")
    }

    sprintf(
        "https://julialang-s3.julialang.org/bin/%s/%s/%s/julia-%s-%s.%s",
        os, arch, short_version, version, slug, ext
    )
}

julia_default_depot <- function(){
    key <- if (Sys.info()["sysname"] == "Windows") {
        "USERPROFILE"
    } else {
        "HOME"
    }
    return(file.path(Sys.getenv(key), ".julia"))
}

julia_save_install_dir <- function(dir){
    depot <- Sys.getenv("JULIA_DEPOT_PATH", unset = julia_default_depot())
    prefs <- file.path(depot, "prefs")
    dir.create(prefs, recursive = TRUE, showWarnings = FALSE)
    cat(file.path(dir, "bin"), file = file.path(prefs, "JuliaCall"))
}

#' Install Julia.
#'
#' @param version The version of Julia to install (e.g. \code{"1.6.3"}).
#'                Defaults to \code{"lts"}, which will install the current
#'                Long Term Support release. Can also be \code{"latest"} for
#'                the most recent stable release.
#' @param prefix the directory where Julia will be installed.
#'     If not set, a default location will be determined by \code{rappdirs}
#'     if it is installed, otherwise an error will be raised.
#'
#' @export
install_julia <- function(version = "lts",
                          prefix = julia_default_install_dir()){
    if (is.null(prefix)) {
        stop("rappdirs is not installed and prefix was not provided")
    }

    if (version == "latest") {
        version <- julia_latest_version()
    } else if (version == "lts") {
        version <- julia_lts_version()
    }
    url <- julia_url(version)

    file <- tempfile()
    tryCatch({
        old_timeout = getOption("timeout")
        options(timeout = 300)
        utils::download.file(url, file)
        options(timeout = old_timeout)
    }, error = function(err) {
        stop(paste("There was an error downloading Julia. This could be due ",
                   "to network issues, and might be resolved by re-running ",
                   "`install_julia`.",
                   sep = ""))
    })

    dest <- file.path(prefix, version)
    if (dir.exists(dest)) {
      unlink(dest, recursive = TRUE)
    }

    sysname <- Sys.info()["sysname"]
    if (sysname == "Linux") {
      utils::untar(file, exdir=dest)
      subfolder <- paste("julia-", version, sep="")
    } else if (sysname == "Darwin") {
      utils::untar(file, exdir=dest)
      subfolder <- paste("julia-", version, sep="")
    } else if (sysname == "Windows") {
      utils::unzip(file, exdir = dest)
      subfolder <- paste("julia-", version, sep="")
    }
    dest <- file.path(dest, subfolder)

    julia_save_install_dir(dest)

    print(sprintf("Installed Julia to %s", dest))

    invisible(TRUE)
}
