# Print commands that will create schema
mfdb_show_schema <- function() {
    schema_from_0(NULL)
    invisible(NULL)
}

# Destroy everything in current schema
mfdb_destroy_schema <- function(mdb) mfdb_transaction(mdb, {
    if (mdb$schema == 'public') {
        for(t in c('prey', 'predator', 'sample', 'survey', 'division', 'survey_index', 'fleet', mfdb_taxonomy_tables, 'mfdb_schema')) {
            mdb$logger$info(paste("Removing table", t))
            if (mfdb_table_exists(mdb, t)) {
                mfdb_send(mdb, "DROP TABLE ", mdb$schema, ".", t, " CASCADE")
            }
        }
    } else {
        mfdb_send(mdb, "DROP SCHEMA ", mdb$schema, " CASCADE")
    }
    invisible(TRUE)
})

# Check to see if we need to update schema do it,
mfdb_update_schema <- function(
        mdb,
        schema_version = -1,
        target_version = package_major_version()) {
    while (TRUE) {
        # Find out existing schema version, if it's what we want return
        if (mfdb_table_exists(mdb, 'mfdb_schema')) {
            res <- mfdb_fetch(mdb, "SELECT MAX(version) FROM mfdb_schema")
            schema_version <- ifelse(nrow(res) == 0, 0, res[1, 1])
        } else {
            schema_version <- 0
        }

        if (schema_version > target_version) {
            stop("Cannot downgrade schema from ", schema_version, " to ", target_version)
        }

        fn <- tryCatch(get(paste0("schema_from_", schema_version)), error = function (e) {
            stop(paste(
                "DB Schema version", schema_version,
                "does not match package version", target_version,
                "& no upgrade step available.\n",
                "Call mfdb('", mdb$schema, "', destroy_schema = TRUE) first.",
                "Warning: This *will destroy* any existing data"))
            })
        fn(mdb, target_version)

        # NB: Do this afterwards so we at least run the "up-to-date" step
        if (schema_version == target_version) {
            break
        }
    }
}

# Create all tables required for schema
schema_create_tables <- function (mdb) mfdb_transaction(mdb, {
    mfdb_create_table(mdb, "mfdb_schema", "Table to keep track of schema version", cols = c(
        "version", "INT NOT NULL", "Version of MFDB schema"))
    mfdb_insert(mdb, "mfdb_schema", list(version = package_major_version()))

    # Create all required taxonomy tables
    for (t in c(mfdb_taxonomy_tables)) mfdb_create_taxonomy_table(mdb, t)

    for (t in names(mfdb_measurement_table_defs)) {
        mfdb_create_table(mdb, t,
            mfdb_measurement_table_defs[[t]][[1]],
            cols = mfdb_measurement_table_defs[[t]][['cols']])
    }
})

mfdb_measurement_table_defs <- list(
    survey_index = list("Indices used to modify surveys", cols = c(
        "survey_index_id", "SERIAL PRIMARY KEY", "",
        "data_source_id", "INT REFERENCES data_source(data_source_id)", "",
        "index_type_id", "INT REFERENCES index_type(index_type_id)", "",

        "areacell_id", "INT REFERENCES areacell(areacell_id)", "Areacell data relates to",
        "year", "INT NOT NULL", "Year sample was taken",
        "month", "INT NOT NULL", "Month sample was taken",
        "value", "REAL NOT NULL", "Value at this point in time"
    )),
    division = list("Grouping of area cells into divisions", cols = c(
        "division_id", "SERIAL PRIMARY KEY", "",

        "division", "VARCHAR(10) NOT NULL", "",
        "areacell_id", "INT REFERENCES areacell(areacell_id)", "Contained areacell"
    )),
    sample = list("Samples within a survey", cols = c(
        "sample_id", "SERIAL PRIMARY KEY", "",
        "data_source_id", "INT REFERENCES data_source(data_source_id)", "",

        "institute_id", "INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "gear_id", "INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id", "INT REFERENCES vessel(vessel_id)", "Vessel used",
        "trip_id", "INT REFERENCES trip(trip_id)", "Part of trip",
        "tow_id", "INT REFERENCES tow(tow_id)", "Tow used",
        "sampling_type_id", "INT REFERENCES sampling_type(sampling_type_id)", "Sampling type",

        "year", "INT NOT NULL", "Year sample was undertaken",
        "month", "INT NOT NULL CHECK(month BETWEEN 1 AND 12)", "Month sample was undertaken",
        "areacell_id", "INT REFERENCES areacell(areacell_id)", "Areacell data relates to",
        "species_id", "BIGINT REFERENCES species(species_id)", "",
        "age", "NUMERIC(10,5)", "Age (years)",
        "sex_id", "INT REFERENCES sex(sex_id)", "Sex ID",
        "maturity_stage_id", "INT REFERENCES maturity_stage(maturity_stage_id)", "Maturity Stage ID",
        "population_id", "INT REFERENCES population(population_id)", "Population sample is part of",

        "length", "REAL", "Length of fish / mean length of all fish",
        "length_var", "REAL", "Length variance of all fish (given aggregated data)",
        "length_min", "INT", "Minimum theoretical value in this group",
        "weight", "REAL", "Weight of fish / mean weight of all fish",
        "weight_var", "REAL", "Weight variance of all fish (given aggregated data)",

        "liver_weight", "REAL", "(mean) Weight of fish liver",
        "liver_weight_var", "REAL", "(mean) Weight variance of fish liver (given aggregated data)",
        "gonad_weight", "REAL", "(mean) Weight of fish gonads",
        "gonad_weight_var", "REAL", "(mean) Weight variance of fish gonads (given aggregated data)",
        "stomach_weight", "REAL", "(mean) Weight of fish stomach",
        "stomach_weight_var", "REAL", "(mean) Weight variance of fish stomach (given aggregated data)",
        "gutted_weight", "REAL", "(mean) Gutted weight of fish",
        "gutted_weight_var", "REAL", "(mean) Gutted weight variance of fish (given aggregated data)",

        "count", "DOUBLE PRECISION DEFAULT 1", "Number of fish meeting this criteria"
    )),
    predator = list("Predators in predator/prey sample", cols = c(
        "predator_id", "SERIAL PRIMARY KEY", "",
        "data_source_id", "INT REFERENCES data_source(data_source_id) NOT NULL", "",

        "institute_id", "INT REFERENCES institute(institute_id)", "Institute that undertook survey",
        "gear_id", "INT REFERENCES gear(gear_id)", "Gear used",
        "vessel_id", "INT REFERENCES vessel(vessel_id)", "Vessel used",
        "trip_id", "INT REFERENCES trip(trip_id)", "Part of trip",
        "tow_id", "INT REFERENCES tow(tow_id)", "Tow used",
        "sampling_type_id", "INT REFERENCES sampling_type(sampling_type_id)", "Sampling type",

        "year", "INT NOT NULL", "Year sample was undertaken",
        "month", "INT NOT NULL CHECK(month BETWEEN 1 AND 12)", "Month sample was undertaken",
        "areacell_id", "INT REFERENCES areacell(areacell_id)", "Areacell data relates to",

        "stomach_name", "VARCHAR(128) NOT NULL", "Stomach identifier",
        "species_id", "BIGINT REFERENCES species(species_id)", "",
        "age", "NUMERIC(10,5)", "Age (years)",
        "sex_id", "INT REFERENCES sex(sex_id)", "Sex ID",
        "maturity_stage_id", "INT REFERENCES maturity_stage(maturity_stage_id)", "Maturity Stage ID",
        "stomach_state_id", "INT REFERENCES stomach_state(stomach_state_id)", "Status of stomach when caught",

        "length", "REAL", "Length of predator",
        "weight", "REAL", "Weight of predator",
        NULL
    )),
    prey = list("Prey in predator/prey sample", cols = c(
        "prey_id", "SERIAL PRIMARY KEY", "",
        "predator_id", "INT NOT NULL REFERENCES predator(predator_id)", "The stomach this sample was found in",

        "species_id", "BIGINT REFERENCES species(species_id)", "",
        "digestion_stage_id", "INT REFERENCES digestion_stage(digestion_stage_id)", "Digestion stage",

        "length", "REAL", "Length of prey / mean length of all prey",
        "weight", "REAL", "Weight of prey / mean weight of all prey",
        "count", "DOUBLE PRECISION DEFAULT 1", "Number of prey meeting this criteria",
        NULL
    )))
mfdb_measurement_tables <- names(mfdb_measurement_table_defs)

mfdb_taxonomy_col_default <- c(
            "description", "VARCHAR(1024)", "Long description",
            NULL)
mfdb_taxonomy_table_defs <- list(
    institute = list("Institute (mfdb::institute)", cols = mfdb_taxonomy_col_default),
    bait_type = list("Bait type", cols = mfdb_taxonomy_col_default),
    net_type =  list("Net type", cols = mfdb_taxonomy_col_default),
    gear = list("Gear used for sample", cols = c(
            "description", "VARCHAR(1024)", "Long description",
            "mesh_size", "REAL", "Mesh size (mm)",
            "mesh_size_min", "REAL", "Minimum mesh size (mm)",
            "mesh_size_max", "REAL", "Maximum mesh size (mm)",
            NULL)),
    vessel_type = list("Vessel type (mfdb::vessel_type)", cols = mfdb_taxonomy_col_default),
    vessel_owner = list("Vessel owner", cols = c(
        "full_name", "TEXT", "Full name of vessel owner",
        NULL)),
    market_category = list("Market category", cols = mfdb_taxonomy_col_default),
    sex = list("M/F/X/N/U (mfdb::sex)", cols = mfdb_taxonomy_col_default),
    maturity_stage = list("1..5 (mfdb::maturity_stage)", cols = mfdb_taxonomy_col_default),
    species = list("Species (mfdb::species)", cols = mfdb_taxonomy_col_default),
    areacell = list("Areacell", cols = c(
            "size", "REAL", "Size of areacell",
            "depth", "REAL", "Depth of areacell",
            NULL)),
    stomach_state = list("1..5 (mfdb::stomach_state)", cols = mfdb_taxonomy_col_default),
    digestion_stage = list("1..5 (mfdb::digestion_stage)", cols = mfdb_taxonomy_col_default),
    sampling_type = list("Sampling type (e.g. 'RESearch')", cols = mfdb_taxonomy_col_default),
    population = list("Population (i.e. stock groupings)", cols = mfdb_taxonomy_col_default),
    data_source = list("Data source for sample", cols = mfdb_taxonomy_col_default),
    index_type = list("Abundance index type (e.g. acoustic)", cols = mfdb_taxonomy_col_default),
    port = list("Port", cols = c(
        "latitude", "REAL", "Latitude",
        "longitude", "REAL", "Longitude",
        "institute_id", "INT REFERENCES institute(institute_id)", "Institute (or country) responsible for port",
        NULL)),
    trip = list("", cols = c(
        "start_date", "TIMESTAMP", "Date/time trip started",
        "end_date", "TIMESTAMP", "Date/time trip ended",
        "crew", "INTEGER", "Number of staff on-board",
        "oil_consumption", "REAL", "Amount of oil consumed as part of trip",
        "start_port_id", "INT REFERENCES port(port_id)", "Starting port",
        "end_port_id", "INT REFERENCES port(port_id)", "Finishing port",
        NULL)),
    tow = list("Tow sample is part of", cols = c(
            "latitude", "REAL", "Latutide of sample",
            "longitude", "REAL", "Longitude of sample",
            "end_latitude", "REAL", "Latutide of sample end",
            "end_longitude", "REAL", "Longitude of sample end",
            "start", "TIMESTAMP WITH TIME ZONE", "Time tow started",
            "depth", "REAL", "Tow depth (m)",
            "length", "REAL", "Tow length (m)",
            "duration", "REAL", "Tow duration (hours)",
            "hook_count", "INT", "Hook count (e.g. for long lines)",
            "bait_type_id", "INT REFERENCES bait_type(bait_type_id)", "Bait type used (e.g. for long lines)",
            "net_count", "INT", "Number of nets used (e.g. for gillnets)",
            "net_type_id", "INT REFERENCES net_type(net_type_id)", "Net type used (e.g. for gillnets)",
            "mesh_size", "REAL", "Mesh size (mm) (e.g. for gillnets)",
            NULL)),
    vessel = list("Vessel performing sample", cols = c(
            "vessel_type_id", "INT REFERENCES vessel_type(vessel_type_id)", "Vessel type used",
            "vessel_owner_id", "INT REFERENCES vessel_owner(vessel_owner_id)", "Vessel owner",
            "full_name", "TEXT", "Full name of vessel",
            "length", "REAL", "Vessel length (m)",
            "power", "REAL", "Vessel engine power (KW)",
            "tonnage", "REAL", "Vessel gross tonnage",
            NULL)),
    case_study = list("Case study data is part of", cols = mfdb_taxonomy_col_default))
mfdb_taxonomy_tables <- names(mfdb_taxonomy_table_defs)

mfdb_get_taxonomy_extra_cols <- function (table_name, create_detail = FALSE) {
    extra_cols <- mfdb_taxonomy_table_defs[[table_name]]$cols
    if (!create_detail) {
        # Filter so we just get the column name
        extra_cols <- extra_cols[seq(1, length(extra_cols), 3)]
    }
    return(extra_cols)
}
mfdb_get_taxonomy_extra_cols('vessel', create_detail = TRUE)

mfdb_create_taxonomy_table <- function(mdb, table_name) {
    key_col <- paste0(table_name, "_id")
    key_type <- ifelse(table_name == "data_source", "SERIAL", ifelse(table_name == "species", "BIGINT", "INT"))
    mfdb_create_table(mdb, table_name, "", cols = c(
        key_col, paste0(key_type, " PRIMARY KEY"), "Numeric ID for this entry",
        "name", "VARCHAR(1024) UNIQUE NOT NULL", "Short name used in data files / output data (in ltree notation)",
        "t_group", paste0("VARCHAR(1024) NULL REFERENCES ", table_name, "(name)"), "Value grouping (short name)",
        mfdb_get_taxonomy_extra_cols(table_name, create_detail = TRUE),
        NULL
    ))
}

# Return the major version of the package
package_major_version <- function () gsub("\\..*", "", packageVersion("mfdb"))
