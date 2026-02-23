\name{ekznwr_options}
\alias{ekznwr_options}
\title{Package Options for \pkg{ekznwr}}
\description{
    The \pkg{ekznwr} package uses the base R \code{\link[base]{options}} 
    mechanism to control certain aspects of its behaviour. 
    This help page documents the package-specific option 
    \code{extract_dir}.
}

\section{Package Options}{
    \describe{
        \item{\code{extract_dir}}{
        A character string giving the directory in which archive data 
        (for example ZIP files containing GIS layers or metadata) 
        are temporarily extracted during an R session.

        By default, \code{extract_dir} is set on package load to a 
        subdirectory of the session-specific temporary directory, e.g.:

        \preformatted{
            tempdir()/ekznw_dta
        }

        Users may override this default by setting 
        \code{options(extract_dir = "...")} in their \code{.Rprofile}, 
        in a script, or interactively. 
        
        Setting \code{extract_dir} to a persistent, non-temporary 
        location can be useful if the same archives are repeatedly 
        processed across sessions, as this avoids unnecessary 
        re-extraction and speeds up workflows—especially on network 
        drives commonly used within the organisation.
        }
    }
}

\section{Default Behaviour}{
    The default value for \code{extract_dir} is initialised in 
    \code{.onLoad()} when the package is attached. 
    If a user has already defined \code{extract_dir} in their 
    \code{.Rprofile}, that value will be respected and not overwritten.
}

\section{Usage}{
    To view the current value:
    \preformatted{
    getOption("extract_dir")
    }

    To set the extraction directory for the current session:
    \preformatted{
    options(extract_dir = "~/my_permanent_extracts")
    }

    To make this persistent across sessions, add the same line to 
    your \code{~/.Rprofile} (Ubuntu) or \code{Documents/.Rprofile} 
    (Windows).
}

    \seealso{
    \code{\link[base]{options}}, \code{\link[base]{tempdir}},
    \code{\link{ekznwr-package}}
    }

\keyword{datasets}