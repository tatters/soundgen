# .onLoad = function(libname, pkgname) {
# }

.onAttach = function(libname, pkgname) {
  mes = paste0(
    "Soundgen ", packageVersion('soundgen'),
    ". Tips & demos on project's homepage: http://cogsci.se/soundgen.html"
  )
  packageStartupMessage(mes)
}
