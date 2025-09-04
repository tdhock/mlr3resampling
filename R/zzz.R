register_mlr3 = function() {
  mlr_resamplings = utils::getFromNamespace("mlr_resamplings", ns = "mlr3")
  mlr_resamplings$add("same_other_sizes_cv", ResamplingSameOtherSizesCV)
}

.onLoad = function(libname, pkgname) { # nolint
  # Configure Logger:
  assign("lg", lgr::get_logger("mlr3"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn") # nolint
  }
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  for(task_type in names(x$task_col_roles)){
    x$task_col_roles[[task_type]] = unique(c(x$task_col_roles[[task_type]], "subset"))
  }
  x$loaded_packages = c(x$loaded_packages, "mlr3resampling")
  mlr3misc::register_namespace_callback(pkgname, "mlr3", register_mlr3)
}

mlr3misc::leanify_package()
