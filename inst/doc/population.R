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
mfdb_import_population_taxonomy(mdb, table_string('
name 	description 				t_group
ns	"Northern Shrimp"			ns
ns_s	"Northern Shrimp in Skjalfandi"		ns
ns_a	"Northern Shrimp in Arnarfjordur"	ns
ns_i	"Northern Shrimp in Isafjardardjup"	ns
as	"Aesop Shrimp"				as
as_s	"Aesop Shrimp in Skjalfandi"		as
'))

## -----------------------------------------------------------------------------
mfdb_import_survey(mdb, data_source = "x",
table_string("
year    month   areacell   species population length  count
2019    1       45G01      PRA     ns_s    	10      285
2019    1       45G01      PRA     ns_s    	20      273

2019    1       45G01      PRA     ns_a    	10      299
2019    1       45G01      PRA     ns_a    	20      252

2019    1       45G01      PRA     ns_i    	10      193
2019    1       45G01      PRA     ns_i    	20      322
"))

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_count(mdb, c('population', 'length'), list(
        population = mfdb_group(ns_s = 'ns_s', ns = 'ns'),
        length = mfdb_unaggregated()))
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
# Can get aggregate ns group as well as ns_s
ok(ut_cmp_equal(unattr(agg_data[[1]]), table_string('
year step area population length number
all  all  all         ns     10    777
all  all  all         ns     20    847
all  all  all       ns_s     10    285
all  all  all       ns_s     20    273
    ')), "Group/filter by populations")

## -----------------------------------------------------------------------------
mfdb_disconnect(mdb)

