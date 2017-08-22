#' Read fatality analysis reporting system (fars) data files
#'
#' A simple function to read in the compressed .csv.bz2
#'    files that are made available by the US National Highway Traffic
#'    Safety Administration.
#' The files are read in as tbl data frame objects from the dplyr
#'    package.
#'
#' @param filename A character string giving the path to the file
#'    to open.
#'
#' @return A tbl data frame from the dplyr package.
#'    There are no side effects.
#'
#' @section Error:
#' If the file path is not valid (e.g., file does not exist),
#' an error will result.
#'
#' @section Imported functions:
#' read_csv() from readr and tbl_df() from dplyr
#'
#' @seealso \href{http://readr.tidyverse.org/reference/read_delim.html}{read_csv} for more about reading in csv's using dplyr
#'
#'  \href{https://www.rdocumentation.org/packages/dplyr/versions/0.5.0/topics/tbl_df}{tbl_df} for more about the tbl_df class
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#' Build filenames for fars data
#'
#' A simple function to build the name of compressed .csv.bz2
#'    files that are made available by the US National Highway
#'    Traffic Safety Administration.
#'
#' @param year A character, numeric, or logical object that is
#'    converted to an integer.
#'
#' @return The name of a .csv.bz2 file
#'
#' @section Warning:
#' If the file path is a chacter string that can't be converted to an
#'    integer.
#'
#' @section Imported functions:
#' None
#'
#' @examples
#' make_filename("2015")
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' Read in files using vector of year(s)
#'
#' A function to read in the compressed .csv.bz2
#'    files that are made available by the US National Highway
#'    Traffic Safety Administration. The user specifies a vector
#'    of years, and subsets of the data are read in to include
#'    only year and month columns.
#' The files are read in as tbl data frame objects from the dplyr
#'    package.
#'
#' @param years A vector of strings or numerics specifying the
#'    years of data to open.
#'
#' @return A list of tbl data frames. There are no side effects.
#'
#' @section Warning:
#' The only years of data that are available are 2013, 2014,
#'    and 2015. If any other year is provided, a warning is given
#'    naming the invalid year.
#'
#' @section Imported functions:
#' mutate() and select() from dplyr
#'
#' @seealso \href{https://cran.r-project.org/web/packages/dplyr/dplyr.pdf}{dplyr CRAN documentation} for more about dplyr
#'
#' @examples
#' fars_read_years(c("2013","2014","2015"))
#' fars_read_years(c("2013","2014",2015))
#' fars_read_years(c("2013"))
#' fars_read_years(2013)
#' fars_read_years(c("2013","2014","2050"))
#'
#' @export
fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year) %>%
                dplyr::select(MONTH, year)
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

#' Summarize fars data files by year and month
#'
#' A function to read in multiple years of files that are
#'    made available by the US National Highway Traffic
#'    Safety Administration.
#' The files are read in as a list, appended, and summarized
#'    using dplyr and tidyr functions.
#'
#' @param years A vector of strings or numerics specifying the
#'    years of data to open.
#'
#' @return A tbl data frame with counts of observations from
#'    every month and year. There are no side effects.
#'
#' @section Warning:
#' The only years of data that are available are 2013, 2014,
#'    and 2015. If any other year is provided, a warning is given
#'    naming the invalid year.
#'
#' @section Imported functions:
#' bind_rows(), group_by(), and summarize() from dplyr and spread()
#'    from tidyr.
#'
#' @seealso \href{https://cran.r-project.org/web/packages/dplyr/dplyr.pdf}{dplyr CRAN documentation} for more about dplyr
#'
#'  \href{hhttp://garrettgman.github.io/tidying/}{Data Science with R}
#'    to learn about data tidying
#'
#' @examples
#' fars_summarize_years(c(2013,2050))
#' fars_summarize_years(2013)
#' fars_summarize_years(c("2013","2014","2015"))
#'
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

#' Plot fatal accidents by state and year
#'
#' A function to plot fatal accidents with a state from files
#'    that are made available by the US National Highway Traffic
#'    Safety Administration.
#' The function returns a plot as a side effect.
#'
#' @param state.num A numeric, integer, or string specifying the
#'    state of interest.
#' @param year A numeric, integer, or string specifying the year
#'    of interest.
#'
#' @return A plot with points indicating fatal accidents within
#'    the state and year of interest.
#'
#' @section Errors:
#' The only years of data that are available are 2013, 2014,
#'    and 2015. If any other year is provided, an error is
#'    returned because the file does not exist.
#' If a number is provided that does not correspond to a state
#'    in the dataset, an error is returned.
#' If there are no accidents to plot in the state and year of
#'    interest, then an error message is returned.
#'
#' @section Imported functions:
#' map() from maps package and points() from graphics package
#'
#' @seealso \href{https://cran.r-project.org/web/packages/maps/maps.pdf}{maps CRAN documentation} for more about maps package
#'
#' @examples
#' fars_map_state(state.num = 6, year = 2015)
#' fars_map_state(6, 2015)
#' fars_map_state("6", "2015")
#' fars_map_state("6", "2014")
#'
#' @export
fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if(!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, STATE == state.num)
    if(nrow(data.sub) == 0L) {
        message("no accidents to plot")
        return(invisible(NULL))
    }
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                  xlim = range(LONGITUD, na.rm = TRUE))
        graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}
