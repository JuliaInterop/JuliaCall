# JuliaCall 0.17.5.9000

* Fix breakage with **knitr** 1.44 when using Plots in a `julia` chunk ([#209](https://github.com/Non-Contradiction/JuliaCall/issues/209)).

# JuliaCall 0.17.5

* Nineteenth release on CRAN.

# JuliaCall 0.17.4.9000

* Support keyword arguments for Callables. Fix #165.
* Add workaround for Julia 1.6+ on mac. Thank @JackDunnNZ.
* Use use knitr::sew() instead of deprecated knitr::wrap(). Thank Kevin Cazelles.
* Update install_julia to use official releases rather than Julia_jll. Thank @JackDunnNZ.
* Add support for macaarch64 to install_julia. Thank @JackDunnNZ.
* Fix warnings caused by pkg macro usage.

# JuliaCall 0.17.4

* Eighteenth release on CRAN.

# JuliaCall 0.17.3.9000

* White space stripping for string command in `julia_command` and `julia_eval`. Fix #161.
* Fix #163 and other problems related to the display system.
* Use `R_user_dir` to get default julia installation position on R 4.0.

# JuliaCall 0.17.2.9000

* Change the way to specific sysimage path in agree with pyjulia and julia -J/--sysimage.. command line. The new `sysimage_path` argument to the function `julia_setup` allows either an absolute path to the image, or a path relative to the current directory. (#155 by @JackDunnNZ).

# JuliaCall 0.17.2

* Seventeenth release on CRAN.

# JuliaCall 0.17.1.9000

* Fix bug for Julia plot in RMarkdown document caused by change of knitr #1717.
* Workaround for #99: `ERROR: could not load library` on Debian/Ubuntu and other systems which build Julia with `MULTIARCH_INSTALL=1` (#143 by @liskin).
* Add `install_julia` function for automatic installation of julia. (#135 by @christopher-dG).
* Add option "relative_sysimage_path" in function `julia_setup()` for custom sysimage loading (#140 by @dgrominski).
* Add option "installJulia" in function `julia_setup()` for automatic installation of Julia when Julia is not found. 
* Fix problem of dll searching on Windows (#150).


# JuliaCall 0.17.1

* Sixteenth release on CRAN.

# JuliaCall 0.17.0.9000

* Fix bug for not throwing error in RMarkdown document. Fix #122.
* Support julia chunk in RMarkdown notebook. Fix #98.
* Add `julia_markdown_setup` for setup of JuliaCall in RMarkdown document and notebook explicitly. Related to #98 and #132.

# JuliaCall 0.17.0

* Fifteenth release on CRAN.

# JuliaCall 0.16.7.9000

* Bug fixes and add CI tests for function `autowrap`.
* New features for automatic package wrapping: `julia_function`, `julia_pkg_import`, and `julia_pkg_hook`.
* `$.()` for `Julia` callable object syntax.

# JuliaCall 0.16.6.9000

* Fix bug about eval option in RMarkdown document, fix #109.
* Add a way to find Julia.exe on Windows when JULIA_HOME is not set. Thanks to @xiaodaigh.

# JuliaCall 0.16.6

* Fourteenth release on CRAN.

# JuliaCall 0.16.5.9000

* `julia_install_package` accepts URLs to install packages, fix #106.
* Do not let print in startup files to mess up with `julia_setup`, fix #105.

# JuliaCall 0.16.5

* Thirteenth release on CRAN.

# JuliaCall 0.16.4.9000

* Various improvements in documentation.
* Add a `rebuild` argument in the function `julia_setup`.
  The argument controls whether to rebuild `RCall.jl`, whose default value is `FALSE` to save startup time.
  If a new version of R is used, then this parameter needs to be set to `TRUE`.

# JuliaCall 0.16.4

* Twelfth release on CRAN.

# JuliaCall 0.16.3.9000

* Improvement for `JuliaObject`, which frees the `JuliaObject` on the `Julia` side after it's freed on the R side.

# JuliaCall 0.16.2.9000

* Respect `engine.path` option for `Julia` in RMarkdown document.
* Refactor `Julia` `knitr` engine to avoid use of `knitr` internal functions.

# JuliaCall 0.16.2

* Eleventh release on CRAN.

# JuliaCall 0.16.1.9000

* Capturing `Julia` stdout in RMarkdown documents.

# JuliaCall 0.16.1

* Tenth release on CRAN.

# JuliaCall 0.16.0.9000

* More deprecation warning fixed for `Julia` v0.7/1.0.

# JuliaCall 0.16.0

* Ninth release on CRAN.

# JuliaCall 0.15.1.9000

* More deprecation warning fixed for `Julia` v0.7.
* `JuliaCall` is now compatible with `RCall.jl` v0.11.0 on `Julia` v0.7.
* Fix the problem caused by the failure of `Pkg.build("RCall")`.
* Fix various small issues in `Julia` v1.0.
* Fix a segfault on Windows with `Julia` v0.7 and v1.0.
* `JuliaCall` now should be usable on `Julia` v0.7 and `Julia` v1.0 with released version of `RCall`.
* Fix issue #65 in `julia_source` on `Julia` v0.7 and v1.0.
* Remove `julia_check` which is deprecated a long time ago.

# JuliaCall 0.15.0.9000

* Some performance improvements.
* Add `install` argument in `julia_setup()`, setting it to `FALSE` can reduce startup time
  when no installation or checking of dependent `Julia` packages is needed.
* Improve `julia_console()`.

# JuliaCall 0.15.0

* Eighth release on CRAN.

# JuliaCall 0.14.3.9000

* Try to remove `R6` dependency to reduce overhead creating `JuliaObject`.
* Compatibility with `Julia` v0.7,
  currently need to use `RCall#e59a546` with `JuliaCall` on `Julia` v0.7.
* Deprecation fixing with `Julia` v0.7.
* Implementation of `diff.JuliaObject`.

# JuliaCall 0.14.2.9000

* Match the assignment behavior for `JuliaObject` to that in native R.
* Implementation of generics for `is.numeric` for `JuliaObject`.
* Bug correction for `c.JuliaObject`.

# JuliaCall 0.14.1.9000

* Make the error in `install_dependency` not muted.
* Various performance improvements in `julia_call` interface functions.
* Performance improvement for `sexp`, `rcopy` and creation of `JuliaObject`.
* Performance improvement in display systems.
* Have a `julia$simple_call` interface which is a simple and more performant
  "equivalent" of the `julia_call` interface.
* Various small bug fixes.

# JuliaCall 0.14.0

* Seventh release on CRAN.

# JuliaCall 0.13.2.9000

* Use more robust way to locate libjulia, fix #29 and #57.
* A simple interface to get access to `JuliaCall` from `Rcpp`.
* Bug correction for `as.vector.JuliaObject`.
* Bug correction for `as.double.JuliaObject`.
* Add error message that old version julia is not supported.
* Add error message when libjulia located is not a valid file.

# JuliaCall 0.13.1.9000

* Various small bug fixes.
* `JuliaObject` supports multiple index.
* Implementation of `mean`, `determinant` and `solve` generics for `JuliaObject`.
* Implementation of `c` and `t` generics for `JuliaObject`.
* Implementation of `aperm`, `dim<-` and `as.vector` generics for `JuliaObject`.

# JuliaCall 0.13.0.9000

* Important bug fixes.
* Reduce the number of messages from `julia_setup(verbose = TRUE)`.
* Add `need_return` argument to `julia_eval`, now there is a possibility to return
  the result as an `JuliaObject`, which is convenient for many use cases requiring
  an R object corresponding to a julia object.
* Bug fixing for unary operators on `JuliaObject`.
* Implement `rep.JuliaObject`.
* Important bug fix for assign of `JuliaObject`.
* New experimental `assign!` to match behavior for assign in R and use it for `JuliaObject`.
* Experimental `JuliaPlain` idea to alleviate the problem that R dispatches only on the first argument,
  make `ifelse` possible to work for `JuliaObject`.
* Fix display issue #54 when using `JuliaCall` from julia and `RCall.jl`.
* Speed up the loading a little.
* Array related methods `dim.JuliaObject`, `is.array.JuliaObject` and `is.matrix.JuliaObject`.

# JuliaCall 0.13.0

* Sixth release on CRAN.

# JuliaCall 0.12.4.9000

* Add `autowrap`, which can generates automatic wrappers for `julia` types.
* Fix bugs in passing arguments in `julia_docall` and `julia_call`.

# JuliaCall 0.12.3.9000

* Fix the issue in displaying `JuliaObject`, especially in `Rmd` documents. Fix #43.
* `x$name` could be used to get access to `field(x, name)` for `JuliaObject`.

# JuliaCall 0.12.2.9000

* Speed up `RCall` checking.
* Use some tricks to get around `julia` issue #14577.

# JuliaCall 0.12.2

* Fifth release on CRAN.

# JuliaCall 0.12.1.9000

* Clearer documentation for `JULIA_HOME`.
* More helpful error message for `julia_setup()`
* Bug fix for `julia` engine in `Rnw` files.
* Bug fix for `JuliaCall` in `rtichoke`.

# JuliaCall 0.12.1

* Fourth release on CRAN.

# JuliaCall 0.12.0.9000

* `JuliaCall` requires Julia 0.6 as Julia 0.5 is now officially unmaintained.
* Don't use inline to compile functions at `julia_setup()` any more,
  accelerate the startup time.
* Load juliarc in `julia_setup`.
* Fix the bug that `julia_setup()` fails to correctly load libjulia.dll
  if JULIA_HOME is not in path on windows.
* Get JULIA_HOME from environment variable.

# JuliaCall 0.11.1

* Third release on CRAN.

# JuliaCall 0.11.0.9000

* Remove deprecated `julia_eval_string`.
* Improve `JuliaCall` RMarkdown engine.
  The display system should work for all kinds of documents that RMarkdown supports.
* Should use older version of RCall with older version of R.

# JuliaCall 0.10.0.9000 - 0.10.6.9000

* Important bug fix in `JuliaObject`, for more detail, see github issue #15, issue #12 and #13 are related.
* Implement generics for `JuliaObject`, and fix many small bugs.
* Julia tuple converts to R S3 class of JuliaTuple based on list.
* Add `fields`, `field` and `field<-` function for JuliaObjects.
* Use R6 for the implementation of `JuliaObject`. It's lightweight, faster, and safer.

# JuliaCall 0.9.3.9000

* Performance improvements for dot notation function call.

# JuliaCall 0.9.3

* Second release on CRAN.

# JuliaCall 0.9.2.9000

* New Julia display system `plotsViewer()`, which integrates better into R.
* Fixes several bugs in `JuliaCall`, like removing deprecated functions.
* Improve display systems of RMarkdown and Jupyter R Notebook.

# JuliaCall 0.9.1.9000

* Improve `JuliaObject`, same Julia object could enter julia_object_dict only once.
  And the display of `JuliaObject` becomes better.
  Also every common types of Julia Object could be wrapped by `JuliaObject`.
* The users could now choose to have `JuliaObject` type return value instead of
  R Object.
* Dot notation in julia is now accepted through the `julia_call` interface.
* `julia_eval_string` is deprecated in favor of `julia_eval`.

# JuliaCall 0.9.0.9000

* Try to convert julia tuple.
* Add `JuliaObject`, which serves as a proxy in R for julia object,
  which is the automatic conversion target when other choices are not possible.
* `julia_setup()` doesn't need to be called first unless you want to force
  julia to restart or you need to set the julia path.

# JuliaCall 0.8.0.9000

* `julia.do_call` and `julia_call` now accept keyword arguments.
* `JuliaCall` works in Jupyter R notebook.
* `JuliaCall` works in R Markdown.
  The display system currently **only work for html document**.
  When there is no return from julia function and there is a need to display,
  a div with class=‘JuliaDisplay’ will be inserted into the html document
  with the corresponding content.
* Julia engine in R Markdown through `JuliaCall`.

# JuliaCall 0.7.5.9000

* Have a basic julia display system, now the plot functionality in Julia works (mostly).
* `JuliaCall` is more consistent with julia.

# JuliaCall 0.7.4.9000

* Added a `NEWS.md` file to track changes to the package.
* Add helpful error messages in libjulia DLL load and compilation.
* Add `julia_assign` which can assign a value to a name in julia with automatic type conversion.
* Give the option to set path for julia.
* Deprecate `julia_check`.
* `julia_console`. Now there is a fully functional julia repl in R terminal,
  and a usable julia console when you use IDE for R.
* In `julia_setup`, there is an option whether or not to use RCall.jl,
  RCall's R REPL mode and rgui will be set correctly.

# JuliaCall 0.7.3

* First release on CRAN.
* `julia_setup` for initial setup of `JuliaCall`.
* `julia_eval_string`, `julia_command` for executing commands in julia.
* `julia_do.call` and `julia_call` for calling functions in julia.
* Functions to deal with julia packages.
* Helper functions like `julia_help`.
