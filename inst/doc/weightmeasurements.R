## ---- message=FALSE, echo=FALSE-----------------------------------------------
logging::setLevel('WARN')
library(unittest)
# Redirect ok() output to stderr
ok <- function(...) capture.output(unittest::ok(...), file = stderr())
library(mfdb)

# Remove our attributes from a dataframe - only used for testing
unattr <- function (obj) {
    attributes(obj) <- attributes(obj)[c('names', 'row.names', 'class')]
    obj
}

## -----------------------------------------------------------------------------
# Convert a string into a data.frame
table_string <- function (text, ...) read.table(
    text = text,
    blank.lines.skip = TRUE,
    header = TRUE,
    stringsAsFactors = FALSE,
    ...)

## -----------------------------------------------------------------------------
mdb <- mfdb(tempfile(fileext = '.duckdb'))
mfdb_import_area(mdb, table_string('
name  division size
45G01     divA   10
45G02     divA  200
45G03     divB  400
'))

## -----------------------------------------------------------------------------
mfdb_import_survey(mdb, data_source = "x",
    table_string('
year    month   areacell        species length  age     weight	liver_weight	gonad_weight	stomach_weight
2000    1       45G01           COD     21      2       210	93		82		72
2000    1       45G02           COD     34      3       220	28		93		99
2000    1       45G03           COD     44      4       230	93		85		72
    '))

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_totalweight(mdb, c('age'), list(
    age = mfdb_unaggregated(),
    null = NULL), measurements = c('overall', 'liver', 'gonad', 'stomach'))
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area age total_weight total_liver_weight total_gonad_weight total_stomach_weight
 all  all  all   2          210                 93                 82                   72
 all  all  all   3          220                 28                 93                   99
 all  all  all   4          230                 93                 85                   72
    ')), "measurements = c('overall', 'liver', 'gonad', 'stomach')")

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_totalweight(mdb, c('age'), list(
    age = mfdb_unaggregated(),
    null = NULL), measurements = c('stomach'))
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area age total_stomach_weight
 all  all  all   2                   72
 all  all  all   3                   99
 all  all  all   4                   72
    ')), "measurements = c('stomach')")

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_meanweight(mdb, c('age'), list(
    null = NULL), measurements = c('liver', 'stomach'))
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area age number mean_liver_weight mean_stomach_weight
 all  all  all all      3          71.33333                  81
    '), tolerance = 1e-7), "measurements = c('liver', 'stomach')")

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_meanweight_stddev(mdb, c('age'), list(
    null = NULL), measurements = c('overall', 'gonad'))
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area age number mean mean_gonad_weight stddev stddev_gonad_weight
 all  all  all all      3  220          86.66667     10            5.686241
    '), tolerance = 1e-7), "measurements = c('overall', 'gonad')")


## -----------------------------------------------------------------------------
mfdb_disconnect(mdb)

