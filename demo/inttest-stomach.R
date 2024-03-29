# This script demonstrates storing and retrieving stomach data
#
# ok and ok_group are there so we can run this code and verify it is correct,
# and not needed every-day use.
#
# NB: if you want to run this, you must be using mfdb-workspace, also be warned
# any data stored in your database will be destroyed.
library(unittest)
library(mfdb)
source('tests/utils/helpers.R')
source('tests/utils/inttest-helpers.R')

cmp_table <- function(tbls, expected) {
    ok(cmp(length(tbls), 1), "result only returned one data.frame")

    if (class(tbls[[1]]$number) == 'integer64') {
        tbls[[1]]$number <- as.numeric(tbls[[1]]$number)
    }
    if (class(tbls[[1]]$predator_count) == 'integer64') {
        tbls[[1]]$predator_count <- as.numeric(tbls[[1]]$predator_count)
    }

    cmp(tbls[[1]][names(tbls[[1]])], expected)
}

# Empty database & rebuild
if (exists("mdb")) mfdb_disconnect(mdb)
mfdb(gsub("inttest", "inttest-stomach", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, destroy_schema = TRUE)
mdb <- mfdb(gsub("inttest", "inttest-stomach", Sys.getenv('INTTEST_SCHEMA', 'inttest')), db_params = db_params, save_temp_tables = FALSE)

# Set-up areas/divisions
mfdb_import_area(mdb, data.frame(
    name = c('45G01', '45G02', '45G03'),
    division = c('divA', 'divA', 'divB'),
    size = c(10,200,400)))

ok_group("Stomach data", {
    # Import a stomach survey
    mfdb_import_stomach(mdb,
        data_source = "cod2000",
        predator_data = shuffle_df(table_string("
stomach_name	year	month	areacell	species	length	weight
A		2000	1	45G01		COD	21	210
B		2000	1	45G01		COD	34	220
C		2000	1	45G01		COD	34	230

D		2000	1	45G01		COD	62	320
E		2000	1	45G01		COD	33	330

G		2000	1	45G01		COD	34	430
        ")),
        prey_data = shuffle_df(table_string("
stomach_name	species	digestion_stage	length	weight	count
A		CAP		1	1	10	5
A		CAP		1	4	40	1
B		CAP		1	1	10	5
B		CAP		4	1	10	5
B		CAP		5	1	10	NA
B		CAP		5	1	10	NA
C		CLL		2	3.5	9.5	3

D		CAP		1	1.4	10	1
D		CLL		5	4	40	1
E		CAP		1	1.4	10	1
        ")))

     # Find out the ratio of capelin in stomachs
     ok(cmp_table(
         mfdb_stomach_presenceratio(mdb, c("predator_weight"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200', 'w300'),
             ratio = c(
                 # CAP in A, B out of A, B, C
                 2 / 3,
                 # CAP in D, E out of D, E
                 2 / 2,
                 NULL),
             stringsAsFactors = FALSE)), "Aggregated & got ratio")

     # Find out the ratio of capelin, grouped by digestion stage
     ok(cmp_table(
         mfdb_stomach_presenceratio(mdb, c("predator_weight", "digestion_stage"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             digestion_stage = mfdb_group(undigested = 1, digested = 2:5),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200', 'w200', 'w300'),
             digestion_stage = c('digested', 'undigested', 'undigested'),
             ratio = c(
                 # digested CAP in B (stages 4 & 5) out of A, B, C
                 1 / 3,
                 # undigested CAP in A, B out of A, B, C
                 2 / 3,
                 # undigested CAP in D, E out of D, E
                 2 / 2,
                 NULL),
             stringsAsFactors = FALSE)), "Aggregated & got ratio")

    # Find out the length distribution of all prey
    ok(cmp_table(
         mfdb_stomach_preycount(mdb, c("digestion_stage", "prey_length"), list(
             digestion_stage = mfdb_group(undigested = 1, digested = 2:5),
             prey_length = mfdb_step_interval("pl", to = 20, by = 1))),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             digestion_stage = c('digested', 'digested', 'digested', 'undigested', 'undigested'),
             prey_length = c('pl1', 'pl3', 'pl4', 'pl1', 'pl4'),
             number = c(
                 5 + 0 + 0,
                 3,
                 1,
                 5 + 5 + 1 + 1,
                 1,
             NULL),
             stringsAsFactors = FALSE)), "Length distribution of all prey")

    # Find out the mean length of all prey
    ok(cmp_table(
         mfdb_stomach_preymeanlength(mdb, c("digestion_stage"), list(
             digestion_stage = mfdb_group(undigested = 1, digested = 2:5))),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             digestion_stage = c('digested', 'undigested'),
             number = c(sum(5, 0, 0, 3, 1), sum(5, 1, 5, 1, 1)),
             mean_length = c(
                 weighted.mean(c(1, 1, 1, 3.5, 4),
                               c(5, 0, 0, 3,   1)),
                 weighted.mean(c(1, 4, 1, 1.4, 1.4),
                               c(5, 1, 5, 1,   1)),
             NULL),
             stringsAsFactors = FALSE)), "Mean length of all prey")

    # Find out the mean weight of all prey
    ok(cmp_table(
         mfdb_stomach_preymeanweight(mdb, c("digestion_stage"), list(
             digestion_stage = mfdb_group(undigested = 1, digested = 2:5))),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             digestion_stage = c('digested', 'undigested'),
             predator_count = c(6, 6),
             mean_weight = c(
                 (10*5 + 10*1 + 10*1 + 9.5*3 + 40*1) / 6,
                 (10*5 + 40*1 + 10*5 + 10*1 + 10*1) / 6,
             NULL),
             stringsAsFactors = FALSE)), "Mean weight of all prey")

     # Find out the ratio of capelin, grouped by digestion stage
     ok(cmp_table(
         mfdb_stomach_preyweightratio(mdb, c("predator_weight", "digestion_stage"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             digestion_stage = mfdb_group(undigested = 1, digested = 2:5),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200', 'w200', 'w300'),
             digestion_stage = c('digested', 'undigested', 'undigested'),
             ratio = c(
                 # digested CAP in B (stages 4 & 5) out of A, B, C
                 sum(10*5, 10*1, 10*1) / sum(10*5, 40*1, 10*5, 10*5, 10*1, 10*1, 9.5*3),
                 # undigested CAP in A, B out of A, B, C
                 sum(10*5, 40*1, 10*5) / sum(10*5, 40*1, 10*5, 10*5, 10*1, 10*1, 9.5*3),
                 # undigested CAP in D, E out of D, E
                 sum(10, 10) / sum(10, 40, 10),
                 NULL),
             stringsAsFactors = FALSE)), "Ratio of stomach content by weight")
})

ok_group("Stomach content likelihood compoment", {
     gd <- gadget_directory(tempfile())
     # Find out the ratio of capelin in stomachs
     res <- mfdb_stomach_presenceratio(mdb, c("predator_length", "prey_length"), list(
             predator_length = mfdb_interval("cod", c(20,30,40,50)),
             prey_length = mfdb_interval("cap", c(1,1.3,3,5)),
             prey_species = 'CAP'))

     gadget_dir_write(gd, gadget_likelihood_component(
         "stomachcontent",
         name = "cod-stomachs",
         prey_labels = list(
             "cod50" = "codmat",
             "cod" = c("codimm", "codother"),
             "cap3" = "capmat",
             "cap" = c("cap", "capimm"),
             "other"),
         prey_digestion_coefficients = 3:1,
         predator_names = c("cuthbert", "dibble"),
         data = res[[1]]))

     ok(cmp_file(gd, "likelihood",
         ver_string,
         "; ",
         "[component]",
         "name\tcod-stomachs",
         "weight\t0",
         "type\tstomachcontent",
         "function\tscsimple",
         "datafile\tData/stomachcontent.cod-stomachs.scsimple",
         "epsilon\t10",
         "areaaggfile\tAggfiles/stomachcontent.cod-stomachs.area.agg",
         "predatornames\tcuthbert\tdibble",
         "predatorlengths\t",
         "lenaggfile\tAggfiles/stomachcontent.cod-stomachs.len.agg",
         "preyaggfile\tAggfiles/stomachcontent.cod-stomachs.prey.agg",
         NULL), "likelihood file contains stomachcontent component")
    ok(cmp_file(gd, "Aggfiles/stomachcontent.cod-stomachs.prey.agg",
        ver_string,
        "; ",
        "cap1\t",
        "cap\tcapimm",
        "lengths\t1\t1.3",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cap1.3\t",
        "cap\tcapimm",
        "lengths\t1.3\t3",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cap3\t",
        "capmat\t",
        "lengths\t3\t5",
        "digestioncoefficients\t3\t2\t1",
        NULL), "prey aggregation file")
    ok(cmp_file(gd, "Aggfiles/stomachcontent.cod-stomachs.len.agg",
        ver_string,
        "cod20\t20\t30",
        "cod30\t30\t40",
        "cod40\t40\t50",
        NULL), "Predator length aggregation file")
})

ok_group("Stomach content likelihood compoment, multiple species", {
     gd <- gadget_directory(tempfile())
     # Find out the ratio of capelin in stomachs
     res <- mfdb_concatenate_results(
         mfdb_stomach_presenceratio(mdb, c("predator_length", "prey_length"), list(
             predator_length = mfdb_interval("cod", c(20,30,40,50)),
             prey_length = mfdb_interval("cap", c(1,1.3,3,5)),
             prey_species = 'CAP'))[[1]],
         mfdb_stomach_presenceratio(mdb, c("predator_length", "prey_length"), list(
             predator_length = mfdb_interval("cod", c(20,30,40,50)),
             prey_length = mfdb_interval("cll", c(1,2,3,4,5,6)),
             prey_species = 'CLL'))[[1]])

     gadget_dir_write(gd, gadget_likelihood_component(
         "stomachcontent",
         name = "cod-stomachs",
         prey_labels = list(
             "cod50" = "codmat",
             "cod" = c("codimm", "codother"),
             "cap3" = "capmat",
             "cap" = c("cap", "capimm"),
             "other"),
         prey_digestion_coefficients = 3:1,
         predator_names = c("cuthbert", "dibble"),
         data = res))

     ok(cmp_file(gd, "likelihood",
         ver_string,
         "; ",
         "[component]",
         "name\tcod-stomachs",
         "weight\t0",
         "type\tstomachcontent",
         "function\tscsimple",
         "datafile\tData/stomachcontent.cod-stomachs.scsimple",
         "epsilon\t10",
         "areaaggfile\tAggfiles/stomachcontent.cod-stomachs.area.agg",
         "predatornames\tcuthbert\tdibble",
         "predatorlengths\t",
         "lenaggfile\tAggfiles/stomachcontent.cod-stomachs.len.agg",
         "preyaggfile\tAggfiles/stomachcontent.cod-stomachs.prey.agg",
         NULL), "likelihood file contains stomachcontent component")
    ok(cmp_file(gd, "Aggfiles/stomachcontent.cod-stomachs.prey.agg",
        ver_string,
        "; ",
        "cap1\t",
        "cap\tcapimm",
        "lengths\t1\t1.3",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cap1.3\t",
        "cap\tcapimm",
        "lengths\t1.3\t3",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cap3\t",
        "capmat\t",
        "lengths\t3\t5",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cll1\t",
        "other\t",
        "lengths\t1\t2",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cll2\t",
        "other\t",
        "lengths\t2\t3",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cll3\t",
        "other\t",
        "lengths\t3\t4",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cll4\t",
        "other\t",
        "lengths\t4\t5",
        "digestioncoefficients\t3\t2\t1",
        "; ",
        "cll5\t",
        "other\t",
        "lengths\t5\t6",
        "digestioncoefficients\t3\t2\t1",
        NULL), "prey aggregation file")
    ok(cmp_file(gd, "Aggfiles/stomachcontent.cod-stomachs.len.agg",
        ver_string,
        "cod20\t20\t30",
        "cod30\t30\t40",
        "cod40\t40\t50",
        NULL), "Predator length aggregation file")
    ok(cmp_file(gd, "Data/stomachcontent.cod-stomachs.scsimple",
        ver_string,
        "; -- data --",
        "; year\tstep\tarea\tpredator_length\tprey_length\tratio",
        "all\tall\tall\tcod20\tcap1\t1",
        "all\tall\tall\tcod20\tcap3\t1",
        "all\tall\tall\tcod30\tcap1\t0.25",
        "all\tall\tall\tcod30\tcap1.3\t0.25",
        "all\tall\tall\tcod30\tcll3\t0.25",
        NULL), "Stomach data has both CAP and CLL")
})

ok_group("Stomach data with only weights", {
    # Import a stomach survey, only have total weights
    mfdb_import_stomach(mdb,
        data_source = "cod2000",
        predator_data = shuffle_df(table_string("
stomach_name	year	month	areacell	species	length	weight
A		2000	1	45G01		COD	21	210
B		2000	1	45G01		COD	34	220
C		2000	1	45G01		COD	34	230

D		2000	1	45G01		COD	62	320
E		2000	1	45G01		COD	33	330

G		2000	1	45G01		COD	34	430
        ")),
        prey_data = shuffle_df(table_string("
stomach_name	species	digestion_stage	weight_total
A		CAP		1	10
A		CAP		1	40
B		CAP		1	10
B		CAP		4	10
B		CAP		5	10
B		CAP		5	10
C		CLL		2	9.5

D		CAP		1	10
D		CLL		5	40
E		CAP		1	10
        ")))

     # Find out the ratio of capelin in stomachs
     ok(cmp_table(
         mfdb_stomach_presenceratio(mdb, c("predator_weight"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200', 'w300'),
             ratio = c(
                 # CAP in A, B out of A, B, C
                 2 / 3,
                 # CAP in D, E out of D, E
                 2 / 2,
                 NULL),
             stringsAsFactors = FALSE)), "Aggregated & got ratio, by weight")
})

ok_group("Predator/Prey mismatch", {
    ok(cmp_error(mfdb_import_stomach(mdb,
        data_source = "cod2000",
        predator_data = shuffle_df(table_string("
stomach_name	year	month	areacell	species	length	weight
AA		2000	1	45G02		COD	21	210
BB		2000	1	45G02		COD	34	220
CC		2000	1	45G02		COD	34	230
        ")),
        prey_data = shuffle_df(table_string("
stomach_name	species	digestion_stage	length	weight	count
AA		CAP		1			1	10	5
AA		CAP		1			4	40	1
BB		CAP		1			1	10	5
XX		CLL		2			3.5	9.5	3
BB		CAP		4			1	10	5
YY		CLL		2			3.5	9.5	3
        "))), "stomachs.*XX.*YY"), "Complained that XX and YY are unknown stomachs")

     ok(cmp_table(
         mfdb_stomach_presenceratio(mdb, c("predator_weight"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200', 'w300'),
             ratio = c(
                 # CAP in A, B out of A, B, C
                 2 / 3,
                 # CAP in D, E out of D, E
                 2 / 2,
                 NULL),
             stringsAsFactors = FALSE)), "Old data is still available in table")

    # Replace data, should modify the output of queries
    mfdb_import_stomach(mdb,
        data_source = "cod2000",
        predator_data = shuffle_df(table_string("
stomach_name	year	month	areacell	species	length	weight
AA		2000	1	45G02		COD	21	210
BB		2000	1	45G02		COD	34	220
CC		2000	1	45G02		COD	34	230
        ")),
        prey_data = shuffle_df(table_string("
stomach_name	species	digestion_stage	length	weight	count
AA		CAP		1			1	10	5
        ")))
     ok(cmp_table(
         mfdb_stomach_presenceratio(mdb, c("predator_weight"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             prey_species = 'CAP')),
         data.frame(
             year = 'all', step = 'all', area = 'all',
             predator_weight = c('w200'),
             ratio = c(
                 1 / 3,
                 NULL),
             stringsAsFactors = FALSE)), "Old data was replaced")

    # Remove data, shouldn't be able to find it again
    mfdb_import_stomach(mdb,
        data_source = "cod2000",
        predator_data = data.frame(),
        prey_data = data.frame())
     ok(cmp(
         unattr(mfdb_stomach_presenceratio(mdb, c("predator_weight"), list(
             predator_weight = mfdb_interval("w", c(200,300,400,500)),
             prey_species = 'CAP'))[['0.0.0.0']]),
         data.frame()), "cod2000 data removed")
})

mfdb_disconnect(mdb)
