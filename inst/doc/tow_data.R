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
mfdb_import_tow_taxonomy(mdb, table_string('
name latitude longitude  depth length
   A     64.1    -23.15  98.82     10
   B     63.4    -20.18  44.90     10
   C     64.9    -13.55 140.91     20
   D     66.2    -18.55 122.61     20
'))

## -----------------------------------------------------------------------------
mfdb_import_survey(mdb, data_source = "tow_example_a",
    table_string("
year    month   areacell        species tow  length  age     weight
2000    1       45G01           COD     A    21      2       210
2000    1       45G02           COD     A    34      3       220
2000    1       45G03           COD     A    34      3       230
2000    1       45G01           COD     B    62      1       320
2000    1       45G02           COD     B    53      1       330
2000    1       45G03           COD     B    54      2       430
2000    1       45G01           COD     C    28      2       210
2000    1       45G02           COD     C    34      3       220
2000    1       45G03           COD     C    24      3       230
2000    1       45G01           COD     D    12      1       320
2000    1       45G02           COD     D    44      1       330
2000    1       45G03           COD     D    14      2       430
    "))

## -----------------------------------------------------------------------------
mfdb_import_bait_type_taxonomy(mdb, table_string('
name	description
b1	"Bait type 1"
b2	"Bait type 2"
b3	"Bait type 3"
'))

## -----------------------------------------------------------------------------
mfdb_import_tow_taxonomy(mdb, table_string('
name latitude longitude  length hook_count bait_type
 llA     63.4    -20.18	     11         30       b1
 llB     63.4    -20.18      12         30       b2
 llC     63.4    -20.18      13         30       b3
'))

## -----------------------------------------------------------------------------
mfdb_import_survey(mdb, data_source = 'longline', table_string('
year    month   areacell        species tow  length	age	weight
2000	1	45G01		COD	llA	12	15	236
2000	1	45G01		COD	llA	47	6	243
2000	1	45G01		COD	llA	92	11	118
2000	1	45G01		COD	llB	13	14	392
2000	1	45G01		COD	llB	15	4	169
2000	1	45G01		COD	llB	52	3	272
2000	1	45G01		COD	llC	85	10	132
2000	1	45G01		COD	llC	94	9	342
2000	1	45G01		COD	llC	71	12	375
    '))

## -----------------------------------------------------------------------------
mfdb_import_net_type_taxonomy(mdb, table_string('
name	description
black	Black
orange	Orange
white	White
'))

## -----------------------------------------------------------------------------
# Add more detailed information for gillnets
mfdb_import_tow_taxonomy(mdb, table_string('
name latitude longitude  length net_count net_type mesh_size
 gnA     63.4    -20.18      21         2  black         6
 gnB     63.4    -20.18      22         2  orange        7
 gnC     63.4    -20.18      23         2  white         8
'))

## -----------------------------------------------------------------------------
mfdb_import_survey(mdb, data_source = 'gillnet', table_string('
year    month   areacell        species tow  length	age	weight
2000	1	45G01		COD	gnA	34	10	314
2000	1	45G01		COD	gnA	45	14	255
2000	1	45G01		COD	gnA	48	5	322
2000	1	45G01		COD	gnB	24	8	170
2000	1	45G01		COD	gnB	83	7	122
2000	1	45G01		COD	gnB	15	4	152
2000	1	45G01		COD	gnC	33	14	311
2000	1	45G01		COD	gnC	79	6	373
2000	1	45G01		COD	gnC	57	5	186
    '))

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_meanlength(mdb, c('tow_depth'), list(
    step = mfdb_timestep_yearly,
    tow_depth = mfdb_interval('depth', c(0, 50, 100, 150)),
    null = NULL))
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
# Test expected output
ok(ut_cmp_equal(unattr(agg_data[[1]]), data.frame(
    year = c('all'),
    step = c('1'),
    area = c('all'),
    tow_depth = c('depth0', 'depth100', 'depth50'),
    number = c(3, 6, 3),
    mean = c(
        mean(c(62, 53, 54)),
        mean(c(28, 34, 24, 12, 44, 14)),
        mean(c(21, 34, 34)),
    NULL),
    stringsAsFactors = FALSE)), 'Grouping / filtering by tow_depth')

## -----------------------------------------------------------------------------
# Show all tow data unaggregated
agg_data <- mfdb_sample_meanlength(mdb, c('tow', 'tow_latitude', 'tow_longitude', 'tow_depth', 'tow_length'), list(
    step = mfdb_timestep_yearly,
    area = mfdb_group(divB = 'divB'),
    tow = mfdb_unaggregated(),
    tow_latitude = mfdb_unaggregated(),
    tow_longitude = mfdb_unaggregated(),
    tow_depth = mfdb_unaggregated(),
    tow_length = mfdb_unaggregated(),
    null = NULL))
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
ok(ut_cmp_equal(unattr(agg_data[[1]]), data.frame(
    year = c('all'),
    step = c('1'),
    area = c('divB'),
    tow = c('A', 'B', 'C', 'D'),
    tow_latitude = c(64.10000, 63.40000, 64.90000, 66.20000),
    tow_longitude = c(-23.15000, -20.18000, -13.55000, -18.55000),
    tow_depth = c(98.82, 44.90, 140.91, 122.61),
    tow_length = c(10, 10, 20, 20),
    number = c(1, 1, 1, 1),
    mean = c(34, 54, 24, 14),
    stringsAsFactors = FALSE), tolerance=1e-7), "Unaggregated tow data")

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_meanlength(mdb, c('tow'), list(
    step = mfdb_timestep_yearly,
    tow = mfdb_unaggregated(),
    null = NULL), scale_index = 'tow_length')
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
ok(ut_cmp_equal(unattr(agg_data[[1]]), data.frame(
    year = c('all'),
    step = c('1'),
    area = c('all'),
    tow = c('A', 'B', 'C', 'D', "gnA", "gnB", "gnC", "llA", "llB", "llC"),
    number = c(
        # A..D
        3 / 10, 3 / 10, 3 / 20, 3 / 20,
        # gnA..gnC
        3 / 21, 3 / 22, 3 / 23,
        # llA..llC
        3 / 11, 3 / 12, 3 / 13,
        NULL),
    mean = c(
        # A..D
        (21 * (1/10) + 34 * (1/10) + 34 * (1/10)) / (3/10),
        (62 * (1/10) + 53 * (1/10) + 54 * (1/10)) / (3/10),
        (28 * (1/20) + 34 * (1/20) + 24 * (1/20)) / (3/20),
        (12 * (1/20) + 44 * (1/20) + 14 * (1/20)) / (3/20),
        # gnA..gnC
        weighted.mean(c(34, 45, 48), c(1/21, 1/21, 1/21)),
        weighted.mean(c(24, 83, 15), c(1/22, 1/22, 1/22)),
        weighted.mean(c(33, 79, 57), c(1/23, 1/23, 1/23)),
        # llA..llC
        weighted.mean(c(12, 47, 92), c(1/11, 1/11, 1/11)),
        weighted.mean(c(13, 15, 52), c(1/12, 1/12, 1/12)),
        weighted.mean(c(85, 94, 71), c(1/13, 1/13, 1/13)),
        NULL),
    stringsAsFactors = FALSE), tolerance = 1e-5), "Using scale_index = 'tow_length'")

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_scaled(mdb, c('tow_depth'), list(
    step = mfdb_timestep_yearly,
    tow_depth = mfdb_interval('depth', c(0, 50, 100, 150)),
    null = NULL), scale = 'tow_length')
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
ok(ut_cmp_equal(unattr(agg_data[[1]]), data.frame(
    year = c('all'),
    step = c('1'),
    area = c('all'),
    tow_depth = c('depth0', 'depth100', 'depth50'),
    number = c(
        3 / sum(10, 10, 10),  # B
        6 / sum(20, 20, 20, 20, 20, 20), # C+D
        3 / sum(10, 10, 10),  # A
    NULL),
    mean_weight = c(
        mean(c(62, 53, 54)) / sum(10, 10, 10),  # B
        mean(c(28, 34, 24, 12, 44, 14)) / sum(20, 20, 20, 20, 20, 20), # C+D
        mean(c(21, 34, 34)) / sum(10, 10, 10),  # A
    NULL),
    stringsAsFactors = FALSE)), "scale = 'tow_length', grouping by tow_depth")

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_count(mdb, c('tow_bait_type'), list(
    tow_bait_type = mfdb_unaggregated()))
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
reorder_by_col <- function (df, col_name) {
  # NB: DuckDB 0.3.0--0.3.3 switches ORDER BY default to NULLS LAST, hide difference
  df <- df[order(df[[col_name]]),]  # Re-order by column
  rownames(df) <- NULL  # Strip rownames, which will reveal previous order
  return(df)
}
ok(ut_cmp_equal(reorder_by_col(unattr(agg_data[[1]]), 'tow_bait_type'), table_string('
year step area tow_bait_type number
 all  all  all            b1      3
 all  all  all            b2      3
 all  all  all            b3      3
 all  all  all            NA     21
    ')), "Group by tow_bait_type")

## -----------------------------------------------------------------------------
agg_data <- mfdb_sample_count(mdb, c('tow_net_type'), list(
    tow_net_type = mfdb_unaggregated()))
agg_data

## ---- message=FALSE, echo=FALSE-----------------------------------------------
ok(ut_cmp_equal(reorder_by_col(unattr(agg_data[[1]]), 'tow_net_type'), table_string('
year step area tow_net_type number
 all  all  all        black      3
 all  all  all       orange      3
 all  all  all        white      3
 all  all  all           NA     21
    ')), "Unaggregated tow_net_type")

## -----------------------------------------------------------------------------
mfdb_disconnect(mdb)

