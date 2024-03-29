\name{mfdb_queries}
\alias{mfdb_area_size}
\alias{mfdb_area_size_depth}
\alias{mfdb_temperature}
\alias{mfdb_survey_index_mean}
\alias{mfdb_survey_index_total}
\alias{mfdb_sample_count}
\alias{mfdb_sample_meanlength_stddev}
\alias{mfdb_sample_meanlength}
\alias{mfdb_sample_totalweight}
\alias{mfdb_sample_meanweight}
\alias{mfdb_sample_meanweight_stddev}
\alias{mfdb_sample_rawdata}
\alias{mfdb_sample_scaled}
\alias{mfdb_stomach_preycount}
\alias{mfdb_stomach_preymeanlength}
\alias{mfdb_stomach_preymeanweight}
\alias{mfdb_stomach_preyweightratio}
\alias{mfdb_stomach_presenceratio}

\title{MareFrame DB queries}

\description{
    Aggregate data from the database in a variety of ways
}

\usage{
mfdb_area_size(mdb, params)
mfdb_area_size_depth(mdb, params)
mfdb_temperature(mdb, params)
mfdb_survey_index_mean(mdb, cols, params, scale_index = NULL)
mfdb_survey_index_total(mdb, cols, params, scale_index = NULL)
mfdb_sample_count(mdb, cols, params, scale_index = NULL)
mfdb_sample_meanlength(mdb, cols, params, scale_index = NULL)
mfdb_sample_meanlength_stddev(mdb, cols, params, scale_index = NULL)
mfdb_sample_totalweight(mdb, cols, params, measurements = c('overall'))
mfdb_sample_meanweight(mdb, cols, params, scale_index = NULL,
                       measurements = c('overall'))
mfdb_sample_meanweight_stddev(mdb, cols, params, scale_index = NULL,
                              measurements = c('overall'))
mfdb_sample_rawdata(mdb, cols, params, scale_index = NULL)
mfdb_sample_scaled(mdb, cols, params, abundance_scale = NULL, scale = 'tow_length')
mfdb_stomach_preycount(mdb, cols, params)
mfdb_stomach_preymeanlength(mdb, cols, params)
mfdb_stomach_preymeanweight(mdb, cols, params)
mfdb_stomach_preyweightratio(mdb, cols, params)
mfdb_stomach_presenceratio(mdb, cols, params)
}

\arguments{
    \item{mdb}{An object created by \code{mfdb}()}
    \item{cols}{Any additonal columns to group by, see details.}
    \item{params}{A list of parameters, see details.}
    \item{scale_index}{Optional. survey_index used to scale results before aggregation, either "tow_length", "area_size" or from \link{mfdb_import_survey_index}}
    \item{abundance_scale}{Optional. Same as \code{scale_index}}
    \item{scale}{Optional. A scale to apply to the resulting values, e.g. 'tow_length'}
    \item{measurements}{Optional, default 'overall'. A vector of measurement names to use, one of overall, liver, gonad, stomach}
}

\details{
The items in the \code{params} list either restrict data that is returned, or
groups data if they are also in the \code{cols} vector, or are 'year',
'timestep', or 'area'.

If you are grouping by the column, \code{params} should contain one of the
following:

\describe{
    \item{NULL}{
        Don't do any grouping, instead put 'all' in the resulting column.
        For example, \code{age = NULL} results in "all".
    }
    \item{character / numeric vector}{
        Aggregate all samples together where they match.
        For example, \code{year = 1990:2000} results in 1990, ... , 2000.
    }
    \item{mfdb_unaggregated()}{
        Don't do any aggregation for this column, return all possible values.
    }
    \item{mfdb_group()}{
        Group several discrete items together.
        For example, \code{age = mfdb_group(young = 1:3, old = 4:5)}
        results in "young" and "old".
    }
    \item{mfdb_interval()}{
        Group irregular ranges together.
        For example, \code{length = mfdb_interval('len', c(0, 10, 100, 1000))}
        results in "len0", "len10", "len100" (1000 is the upper bound to len100).
    }
    \item{mfdb_step_interval()}{
        Group regular ranges together.
        For example, \code{length = mfdb_step_interval('len', to = 100, by = 10)}
        results in "len0", "len10", ... , "len90".
    }
}

In addition, \code{params} can contain other arguments to purely restrict the
data that is returned.

\describe{
    \item{institute}{A vector of institute names / countries, see mfdb::institute for possible values}
    \item{gear}{A vector of gear names, see mfdb::gear for possible values}
    \item{vessel}{A vector of vessel names, see mfdb::vessel for possible values}
    \item{sampling_type}{A vector of sampling_type names, see mfdb::sampling_type for possible values}
    \item{species}{A vector of species names, see mfdb::species for possible values}
    \item{sex}{A vector of sex names, see mfdb::sex for possible values}
}

To save specifying the same items repeatedly, you can use list
concatenation to keep some defaults, for example:

\preformatted{
defaults <- list(year = 1998:2000)
mfdb_sample_meanlength(mdb, c('age'), c(list(), defaults))
}

\code{scale_index} allows you to scale samples before aggregation. If 
it contains the name of a survey index (see \link{mfdb_import_survey_index}),
then any counts will be scaled by the value for that areacell before and used in
aggregation / weighted averages. As a special case, you can use "tow_length" to
to scale counts by the tow length.
}

\value{
    All will return a list of data.frame objects. If there was no bootstrapping
    requested, there will be only one. Otherwise, there will be one for each
    sample.

    The columns of these data frames depends on the function called.
    \describe{
        \item{mfdb_area_size}{
            Returns area, (total area) size
        }
        \item{mfdb_area_size_depth}{
            Returns area, (total area) size, mean depth, weighted by area size
        }
        \item{mfdb_temperature}{
            Returns year, step, area, (mean) temperature
        }
        \item{mfdb_survey_index_mean}{
            Returns year, step, area, (group cols), (mean) survey index
        }
        \item{mfdb_survey_index_total}{
            Returns year, step, area, (group cols), (sum) survey index
        }
        \item{mfdb_sample_count}{
            Returns year, step, area, (group cols), number (i.e sum of count)
        }
        \item{mfdb_sample_meanlength}{
            Return year, step, area, (group cols), number (i.e sum of count), mean (length)
        }
        \item{mfdb_sample_meanlength_stddev}{
            As mfdb_sample_meanlength, but also returns std. deviation.
        }
        \item{mfdb_sample_totalweight}{
            Returns year,step,area,(group cols),total (weight of group)
        }
        \item{mfdb_sample_meanweight}{
            Returns year, step, area, (group cols), number (i.e sum of count), mean (weight)
        }
        \item{mfdb_sample_meanweight_stddev}{
            As mfdb_sample_meanweight, but also returns std. deviation.
        }
        \item{mfdb_sample_rawdata}{
            Returns year,step,area,(group cols),number of samples, raw_weight and raw_length.

            NB: No grouping of results is performed, instead all matching table entries are returned
        }
        \item{mfdb_sample_scaled}{
            Returns year, step, area, (group cols), number (i.e. sum of count, scaled by tow_length), mean_weight (scaled by tow_length)
        }
        \item{mfdb_stomach_preycount}{
            Returns year, step, area, (group cols), number (of prey found in stomach)
        }
        \item{mfdb_stomach_preymeanlength}{
            Returns year, step, area, (group cols),
            number (of prey found in stomach),
            mean_length (of prey found in stomach).
            NB: Entries where count is NA (i.e. totals) are ignored with this function.
        }
        \item{mfdb_stomach_preymeanweight}{
            Returns year, step, area, (group cols),
            number (of unique stomachs in group),
            mean_weight (per unique stomach).
        }
        \item{mfdb_stomach_preyweightratio}{
            Returns year, step, area, (group cols), ratio (of selected prey in stomach to all prey by weight)
        }
        \item{mfdb_stomach_presenceratio}{
            Returns year, step, area, (group cols), ratio (of selected prey in stomach to all prey by count)
        }
    }
}

\examples{
mdb <- mfdb(tempfile(fileext = '.duckdb'))

# Define 2 areacells of equal size
mfdb_import_area(mdb, data.frame(name=c("divA", "divB"), size=1))

# Make up some samples
samples <- expand.grid(
    year = 1998,
    month = c(1:12),
    areacell = c("divA", "divB"),
    species = 'COD',
    age = c(1:5),
    length = c(0,40,80))
samples$count <- runif(nrow(samples), 20, 90)
mfdb_import_survey(mdb, data_source = "x", samples)

# Query numbers by age and length
agg_data <- mfdb_sample_count(mdb, c('age', 'length'), list(
    length = mfdb_interval("len", seq(0, 500, by = 30)),
    age = mfdb_group('young' = c(1,2), old = 3),
    year = c(1998)))
agg_data

# Use in a catchdistribution likelihood component
gadget_dir_write(gadget_directory(tempfile()), gadget_likelihood_component("catchdistribution",
        name = "cdist",
        weight = 0.9,
        data = agg_data[[1]],
        area = attr(agg_data[[1]], "area"),
        age = attr(agg_data[[1]], "age")))

mfdb_disconnect(mdb)
}
