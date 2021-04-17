# Utility functions --------------------------------------

#' Title
#'
#' @param logfile
#' @param start
#' @param ncores
#'
#' @return
#' @export
#'
#' @examples
out_session <- function(logfile = "logs/log_local.csv", start = NULL, ncores = 1) {

  # find the path (only works if script is sourced or run from cmd line)
  path <- this_file()

  if (is.null(path)) {
    cat("
    Running interactively, either use source or Rscript on the command line
    to save session info."
        )
  } else {

    out <- sessionInfo()
    attached <- lapply(out$otherPkgs, function(x) x["Version"])
    loaded <- lapply(out$loadedOnly, function(x) x["Version"])
    pkgs <- data.frame(packages = c(names(loaded), names(attached)),
                       version = c(unlist(loaded), unlist(attached)),
                       status = c(rep("loaded", length(loaded)),
                                  rep("attached", length(attached))))

    if (!is.null(start)) {
      jobtime <- as.numeric(Sys.time() - start, units = "mins")
    } else {
      jobtime <- NA
    }

    max_mem <- obj_size(sum(gc()[, ncol(gc())]))

    if (!is.null(start)) {
      jobtime <- timing(as.numeric(Sys.time() - start, units = "mins"))
    } else {
      jobtime <- NULL
    }

    # long format
    toappend <- data.frame(
      ran = timestamp(),
      timestamp = format(Sys.time(), "%Y%m%d_%H%M%S"),
      jobtime, max_mem, ncores,
      path, r_version = out$R.version$version.string,
      os = out$running, system = out$platform,
      timezone = Sys.timezone(), pkgs
    )

    if (file.exists(logfile)) {
      existing <- read.csv(logfile, stringsAsFactors = FALSE)
      towrite <- dplyr::bind_rows(existing, toappend)
      write.csv(towrite, logfile, row.names = FALSE)
    } else {
      write_create(toappend, logfile, write.csv, row.names = FALSE)
    }
  }
}



#' Title
#'
#' @param mb
#'
#' @return
#' @keywords internal
#'
#' @examples
obj_size <- function(mb) {
  ifelse(mb / 1000 > 1, paste(round(mb / 1000, 2), "Gb"), paste(round(mb, 2), "Mb"))
}

#' Title
#'
#' @param min
#'
#' @return
#' @keywords internal
#'
#' @examples

timing <- function(min) {
  if (min / 60 > 1) tim <- paste(round(min / 60, 2), "hr")
  if (min * 60 < 60 & min * 60 > 1) tim <- paste(round(min * 60, 2), "sec")
  if (min * 60 <= 1) tim <- paste(round(min * 60 * 1000, 2), "ms")
  if (min / 60 <= 1 & min * 60 >= 60) tim <- paste(round(min, 2), "min")

  tim
}

# From stack exchange:
# https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script
# I want the relative path!

#' Title
#'
#' @return
#' @keywords internal
#'
#' @examples
this_file <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  match <- sub("--file=", "", cmdArgs[grep("--file=", cmdArgs)])
  sysf <- sys.frames()[[1]]$ofile

  # Rscript
  if (length(match) > 0) {
    return(match)
  }

  # 'source'd via R console
  if (!is.null(sysf)) {
    return(trimws(sub(here::here(), "", path.expand(sysf)), whitespace = "/"))
  }

  if (is.null(sysf) & length(match) == 0) {
    return(NULL)
  }
}

#' Title
#'
#' @param obj
#' @param path
#' @param write_function
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write_create <- function(obj, path, write_function, ...) {
  dir_name <- dirname(path)

  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }

  if (is.character(write_function)) {
    get(write_function)(obj, path, ...)
  }

  if (is.function(write_function)) {
    write_function(obj, path, ...)
  }

  if (!is.function(write_function) & !is.character(write_function)) {
    print("Error: arg write_function is not a function or is not loaded!")
  }
}

# a ggsave function with arguments switched
#' Title
#'
#' @param plot
#' @param filename
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ggsave_it <- function(plot, filename, ...) {
  ggsave(filename, plot, ...)
}

# getting safe paths with here without using comma's
#' Title
#'
#' @param path
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
here_safe <- function(path, sep = "/") {
  path_list <- unlist(strsplit(path, sep))
  do.call(here::here, as.list(path_list))
}

# getting safe paths on cluster gpfs (for reading & writing)
# a linux system so "/" path separators are fine!
#' Title
#'
#' @param path
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
cl_safe <- function(path, dir = "/scratch/gpfs/mrajeev/mada_rt") {
  paste0(dir, "/", path)
}

#' Title
#'
#' @param slurm
#' @param type
#' @param local_logfile
#' @param cluster_logfile
#' @param mpi
#'
#' @return
#' @export
#'
#' @examples
setup_cl <- function(slurm = Sys.getenv("SLURM_JOB_ID") != "",
                     type = commandArgs(trailingOnly = TRUE)[1],
                     local_logfile = "logs/log_local.csv",
                     cluster_logfile = "logs/log_cluster.csv",
                     mpi = TRUE) {
  start <- Sys.time()

  if (!slurm) {

    logfile <- local_logfile

    if (type %in% "local") {
      # set up local cluster
      ncores <- parallel::detectCores() - 1
      make_cl <<- function(...) {
        parallel::makeCluster(...)
      }
      register_cl <<- doParallel::registerDoParallel
      close_cl <<- parallel::stopCluster
      cl_size <<- function(cl) {
        length(cl)
      }
    } else {
      # Look for the name of the script and how long it will take in the log
      ncores <- 1
      make_cl <<- function(...) {} # empty func (will return NULL)
      cl_size <<- function(...) {1}
      close_cl <<- register_cl <<- function(cl) {invisible(cl)} # dummy functions
    }
  }

  if (slurm) {
    ncores <- as.numeric(Sys.getenv("SLURM_NTASKS"))
    logfile <- cluster_logfile

    if (mpi == TRUE) {
      ncores <- ncores - 1
      make_cl <<- doMPI::startMPIcluster
      register_cl <<- doMPI::registerDoMPI
      cl_size <<- doMPI::clusterSize
      close_cl <<- function(cl) {
        doMPI::closeCluster(cl)
        Rmpi::mpi.quit()
      }
    }

    if (mpi == FALSE) {
      make_cl <<- parallel::makeCluster
      register_cl <<- doParallel::registerDoParallel
      cl_size <<- function(cl) {
        foreach::getDoParWorkers()
      }
      close_cl <<- parallel::stopCluster
    }
  }

  return(list(
    start = start, ncores = ncores, slurm = slurm,
    logfile = logfile
  ))
}
