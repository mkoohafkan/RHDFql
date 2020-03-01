.onLoad = function(libname, pkgname) {
	set_paths()
}

.onAttach = function(libname, pkgname) {
	path_from_options(TRUE)
}

.onUnload = function(libpath) {
}

.onDetach = function(libpath) {
}
