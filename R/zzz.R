.onAttach <- function(libname, pkgname) {
  packageStartupMessage("")
}

.onLoad <- function(libname, pkgname) {
  reset.load.fn()
}
