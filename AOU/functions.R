# Quiet joins
full_join_quiet <- function(...){ suppressMessages(full_join(...))}
inner_join_quiet <- function(...){ suppressMessages(inner_join(...))}
left_join_quiet <- function(...){ suppressMessages(left_join(...))}
right_join_quiet <- function(...){ suppressMessages(right_join(...))}
anti_join_quiet <- function(...){suppressMessages(anti_join(...))}

# Write csv to disk and bucket
write_to_bucket <- function(my_dataframe, destination_filename) {
    # store the dataframe in current workspace
    tic("Writing file")
    write_excel_csv(my_dataframe, destination_filename)
    toc()

    # Get the bucket name
    my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

    # Copy the file from current workspace to the bucket
    tic("Copying file to workspace bucket")
    system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/data/Feb2025/"), intern=T)
    toc()
}

# Read csv from disk or bucket
read_from_bucket <- function(name_of_file_in_bucket, skip_copy = FALSE) {
    # Get the bucket name
    my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
    
    # Copy the file from bucket to disk if requested, or if file doesn't exist on disk
    if (!skip_copy){
        # Copy the file from the bucket to the current workspace
        tic("Copying file from workspace bucket")
        system(paste0("gsutil cp ", my_bucket, "/data/Feb2025/", name_of_file_in_bucket, " ."), intern=T)
        toc()
    }

    # Load the file from disk
    tic("Reading file")
    my_dataframe  <- read_csv(name_of_file_in_bucket, show_col_types=FALSE, guess_max=Inf)
    # Otherwise drops some relevant values and throws warnings
    toc()
    return(my_dataframe) 
}

# Write RDATA to disk and bucket
write_RDATA_to_bucket <- function(object_list, destination_filename) {
    if (!grepl("RData$", destination_filename, ignore.case=T)) {stop("Must write to an .RData file")}
    
    # store the dataframe in current workspace
    save(list = object_list, file = destination_filename)

    # Get the bucket name
    my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

    # Copy the file from current workspace to the bucket
    system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/data/Feb2025/"), intern=T)    
}


# Load RData file from disk or bucket
load_RDATA_from_bucket <- function(filename, skip_copy = T) {
    if (!grepl("RData$", filename, ignore.case=T)) {stop("Must provide an .RData file")}
    
    # Get the bucket name
    my_bucket_dir <- paste0(Sys.getenv('WORKSPACE_BUCKET'), "/data/Feb2025/")

    # Copy the file from bucket to disk if requested, or if file doesn't exist on disk
    if (!skip_copy | !file.exists(filename)){
        tic("Copying file from workspace bucket")
        system(paste0("gsutil cp ", my_bucket_dir, filename, " ."), intern=T) 
	toc()
    }
    
    # store the dataframe in current workspace
    load(filename, envir=.GlobalEnv, verbose=T)
}

# Read only certain columns from disk or bucket with data.table::fread
read_cols <- function(name_of_file_in_bucket, select, nrows = Inf, skip_copy = FALSE){
    library(data.table)
    # Get the bucket name
    my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
    
    if (!skip_copy) {
        # Copy the file from current workspace to the bucket
        system(paste0("gsutil cp ", my_bucket, "/data/Feb2025/", name_of_file_in_bucket, " ."), intern=T)
    }

    # Load the file into a dataframe
    my_datatable  <- fread(name_of_file_in_bucket, select = select, nrows = nrows)
    return(my_datatable %>% data.frame())
}

# delete file from bucket
delete_file <- function(filename){
    if(is.null(filename) | is.na(filename) | filename == "" | filename == "." | nchar(filename) <= 5) {
        stop("Must specify a filename")
    }
    # include full path after my_bucket
    my_bucket_dir <- Sys.getenv('WORKSPACE_BUCKET')
    system(paste0("gsutil rm ", my_bucket, "/data/Feb2025/", filename))
}

# CCI - Function to slip in the first variable
slip_condition_first <- function(condition_name) {
    better_name <- gsub(" ", "_", condition_name)
    
    dat_slip <- CCI_first %>% filter(condition == condition_name) %>%
        mutate(year = decimal_date(first_occurrence) - decimal_date(ogn),
              condition_status = 1)
    
    tdept <- tmerge(tindpt, dat_slip, id=person_id, newvar = tdc(year, condition_status))
    names(tdept)[length(names(tdept))] <- better_name
    
    tdept <- tmerge(tdept, dat_slip, id=person_id, newvar = tdc(year, weight))
    names(tdept)[length(names(tdept))] <- paste0(better_name, "_weight")
    
    return(tdept)
}

# CCI - Function to slip in all subsequent variables
slip_condition <- function(condition_name) {
    better_name <- gsub(" ", "_", condition_name)
    
    dat_slip <- CCI_first %>% filter(condition == condition_name) %>%
        mutate(year = decimal_date(first_occurrence) - decimal_date(ogn),
              condition_status = 1)
    
    tdept <- tmerge(tdept, dat_slip, id=person_id, newvar = tdc(year, condition_status))
    names(tdept)[length(names(tdept))] <- better_name
    
    tdept <- tmerge(tdept, dat_slip, id=person_id, newvar = tdc(year, weight))
    names(tdept)[length(names(tdept))] <- paste0(better_name, "_weight")
    
    return(tdept)
}

# Define a function for extractiing conditions data from ICD9 and ICD10 'like' patterns, 
# which is easier than generating a new concept set for each condition. Extracts both 
# the longitudinal data and the summary of the first occurrence. 
extract_condition <- function(where_ICD9_like, where_ICD10_like, name = "condition") {
    tic()
    condition_sql <- paste(
        "SELECT DISTINCT
            c_occurrence.person_id,
            c_occurrence.condition_start_datetime,
            c_source_concept.concept_code as source_concept_code,
            c_source_concept.concept_name as source_concept_name
        FROM `condition_occurrence` c_occurrence
        INNER JOIN `concept` c_source_concept
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id
        WHERE (
            (c_source_concept.vocabulary_id = 'ICD9CM' AND  (
                c_source_concept.concept_code LIKE ", 
                    paste0(where_ICD9_like, collapse = "\n OR c_source_concept.concept_code LIKE"), "))",
        "
        OR 
            (c_source_concept.vocabulary_id = 'ICD10CM' AND (
                c_source_concept.concept_code LIKE ", 
                    paste0(where_ICD10_like, collapse = "\n OR c_source_concept.concept_code LIKE"), "))",
        ")" )
    
    cat(condition_sql)
    
    condition_path <- file.path(
      Sys.getenv("WORKSPACE_BUCKET"),
      "bq_exports",
      Sys.getenv("OWNER_EMAIL"),
      "condition_tmp",
      "condition_tmp_*.csv")

    bq_table_save(
      bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
      condition_path,
      destination_format = "CSV")
  
    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(person_id = col_integer(), condition_start_datetime = col_datetime())
        bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))}
    
    dataset_df <- read_bq_export_from_workspace_bucket(condition_path) %>%
        mutate(condition_start_datetime = as.Date(condition_start_datetime)) %>% 
        distinct() %>%
        select(person_id, condition_start_datetime, source_concept_code, source_concept_name)
    
    cat(paste0("\n", nrow(dataset_df), " rows, ", length(unique(dataset_df$person_id)), " patients", "\n"))
    
    dataset_first <- dataset_df %>% group_by(person_id) %>%
        slice_min(condition_start_datetime) %>%
        distinct(person_id, condition_start_datetime, .keep_all = TRUE) %>%
        mutate(condition = 1) %>% arrange(person_id)
    
    names(dataset_first)[2:5] <- c(paste0("first_", name, "_date"), paste0(name, "_concept_source_code"), 
                                   paste0(name, "_concept_name"), name)
    names(dataset_df)[2:4] <- paste0(name, c("_date", "_concept_source_code", "_concept_name"))
    
    system(paste0("gsutil rm ", condition_path), intern=T)
    
    toc()
    
    return(list("all" = dataset_df, "first" = dataset_first))
}

extract_procedure <- function(code_in, name = "procedure") {
    tic()
    procedure_sql <- paste(
        "SELECT DISTINCT
            p_occurrence.person_id,
            p_occurrence.procedure_datetime,
            p_source_concept.concept_code as source_concept_code,
            p_source_concept.concept_name as source_concept_name
        FROM `procedure_occurrence` p_occurrence
        INNER JOIN `concept` p_source_concept
            ON p_occurrence.procedure_source_concept_id = p_source_concept.concept_id
        WHERE p_source_concept.concept_code IN (", 
                    paste0(code_in, collapse = ","), ")"    
        )
     
    cat(procedure_sql)
    procedure_path <- file.path(
      Sys.getenv("WORKSPACE_BUCKET"),
      "bq_exports",
      Sys.getenv("OWNER_EMAIL"),
      "procedure_tmp",
      "procedure_tmp_*.csv")

    bq_table_save(
      bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), procedure_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
      procedure_path,
      destination_format = "CSV")
  
    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(person_id = col_integer(), procedure_datetime = col_datetime())
        bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))
    }
    
    dataset_df <- read_bq_export_from_workspace_bucket(procedure_path) %>%
        mutate(procedure_datetime = as.Date(procedure_datetime)) %>% distinct() %>%
        select(person_id, procedure_datetime, source_concept_code, source_concept_name)
    
    cat(paste0("\n", nrow(dataset_df), " rows, ", length(unique(dataset_df$person_id)), " patients", "\n"))
    
    dataset_first <- dataset_df %>% group_by(person_id) %>%
        slice_min(procedure_datetime) %>%
        distinct(person_id, procedure_datetime, .keep_all = T) %>%
        mutate(procedure = 1) %>% arrange(person_id)
    
    names(dataset_first)[2:5] <- c(paste0("first_", name, "_date"), paste0(name, "_concept_source_code"), 
                                   paste0(name, "_concept_name"), name)
    names(dataset_df)[2:4] <- paste0(name, c("_date", "_concept_source_code", "_concept_name"))
    
    system(paste0("gsutil rm ", procedure_path), intern=T)
    
    toc()
    
    return(list("all" = dataset_df, "first" = dataset_first))
}



# DCSI - These functions are a little different than for CCI
slip_condition_first_DCSI <- function(condition_name) {
    dat_slip <- get(condition_name)$first 
    names(dat_slip)[2] <- "first_occurrence" 
    dat_slip <- dat_slip %>% 
        select(person_id, first_occurrence) %>%
        mutate(year = decimal_date(first_occurrence) - decimal_date(ogn),
              condition_status = 1)
    
    tdept <- tmerge(tindpt, dat_slip, id=person_id, newvar = tdc(year, condition_status))
    names(tdept)[length(names(tdept))] <- gsub("dcsi.", "", condition_name)
    
    return(tdept)
}

# DCSI - Function to slip in all subsequent variables
slip_condition_DCSI <- function(condition_name) {   
    dat_slip <- get(condition_name)$first 
    names(dat_slip)[2] <- "first_occurrence" 
    dat_slip <- dat_slip %>% 
        mutate(year = decimal_date(first_occurrence) - decimal_date(ogn),
              condition_status = 1)
    
    tdept <- tmerge(tdept, dat_slip, id=person_id, newvar = tdc(year, condition_status))
    names(tdept)[length(names(tdept))] <- gsub("dcsi.", "", condition_name)
    
    return(tdept)
}


# Extract medication records from ancestor concept IDs
extract_medication <- function(concept_ids, name) {
    tic(paste0("Pulling ", name))
    concept_id_pattern <- paste0(concept_ids, collapse = ", ")
    
    med_sql <- paste("
    SELECT
        d_exposure.person_id,
        d_exposure.drug_concept_id,
        d_exposure.drug_exposure_start_datetime,
        d_exposure.days_supply,
        d_exposure.refills,
        d_exposure.quantity,
        d_type.concept_name as drug_type_concept_name,
        d_standard_concept.concept_name as standard_concept_name, 
        d_route.concept_name as route_concept_name,
        d_visit.concept_name as visit_occurrence_concept_name
    FROM
        ( SELECT
            * 
        from
            `drug_exposure` d_exposure 
        WHERE
            (
                drug_concept_id IN  (
                    SELECT
                        DISTINCT ca.descendant_id 
                    FROM
                        `cb_criteria_ancestor` ca 
                    JOIN
                        (
                            select
                                distinct c.concept_id 
                            FROM
                                `cb_criteria` c 
                            JOIN
                                (
                                    select
                                        cast(cr.id as string) as id 
                                    FROM
                                        `cb_criteria` cr 
                                    WHERE
                                        concept_id IN ( ", 
                     concept_id_pattern, 
                     " ) 
                                        AND full_text LIKE '%_rank1]%'
                                ) a 
                                    ON (
                                        c.path LIKE CONCAT('%.',
                                    a.id,
                                    '.%') 
                                    OR c.path LIKE CONCAT('%.',
                                    a.id) 
                                    OR c.path LIKE CONCAT(a.id,
                                    '.%') 
                                    OR c.path = a.id) 
                                WHERE
                                    is_standard = 1 
                                    AND is_selectable = 1
                                ) b 
                                    ON (
                                        ca.ancestor_id = b.concept_id
                                    )
                            )
                        )
                ) d_exposure 
        LEFT JOIN
            `concept` d_type 
                on d_exposure.drug_type_concept_id = d_type.concept_id 
        left join
            `concept` d_standard_concept 
                on d_exposure.drug_concept_id = d_standard_concept.concept_id
        LEFT JOIN
            `concept` d_route 
                ON d_exposure.route_concept_id = d_route.concept_id 
        LEFT JOIN
            `visit_occurrence` v 
                ON d_exposure.visit_occurrence_id = v.visit_occurrence_id 
        LEFT JOIN
            `concept` d_visit 
                ON v.visit_concept_id = d_visit.concept_id", sep="")

    # Formulate a Cloud Storage destination path for the data exported from BigQuery.
    # NOTE: By default data exported multiple times on the same day will overwrite older copies.
    #       But data exported on a different days will write to a new location so that historical
    #       copies can be kept as the dataset definition is changed.
    med_path <- file.path(
      Sys.getenv("WORKSPACE_BUCKET"),
      "bq_exports",
      Sys.getenv("OWNER_EMAIL"),
      # strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
      name,
      paste0(name, "_*.csv"))
    message(str_glue('The data will be written to {med_path}. Use this path when reading ',
                     'the data into your notebooks in the future.'))

    # Perform the query and export the dataset to Cloud Storage as CSV files.
    # NOTE: You only need to run `bq_table_save` once. After that, you can
    #       just read data from the CSVs in Cloud Storage.
    bq_table_save(
      bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), med_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
      med_path,
      destination_format = "CSV")

    # Read the data directly from Cloud Storage into memory.
    # NOTE: Alternatively you can `gsutil -m cp {med_path}` to copy these files
    #       to the Jupyter disk.
    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), drug_type_concept_name = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))
    }
    med_df <- read_bq_export_from_workspace_bucket(med_path)
    toc()
    return(med_df)
}


# process biomarker units
process_units <- function(x) {
    # x is a vector of units
    x[x %in% c("No matching concept", "no value")] <- NA
    x <- tolower(x)
    x <- gsub(" per ", "/", x)
    x <- gsub("nano", "n", x)
    x[!grepl("million", x)] <- gsub("milli", "m", x[!grepl("million", x)] )
    x <- gsub("centi", "c", x)
    x <- gsub("deci", "d", x)
    x <- gsub("gram", "g", x)
    x <- gsub("liter", "l", x)
    x <- gsub("meter", "m", x)
 return(x)
}

# generic function to pull a lab using ancestor concept ids
pull_lab <- function(concept_ids_string = "xxxx", name = "biomarker", read_existing = FALSE, remove_NA = TRUE) {
    # example string: "37047736, 4146380"

    if (name == "biomarker") {stop("Provide a name for the biomarker")}
    
    biomarker_path <- file.path(
      Sys.getenv("WORKSPACE_BUCKET"),
      "bq_exports",
      Sys.getenv("OWNER_EMAIL"),
      paste0("measurement_", name, "_*.csv") )
    
  if (!read_existing) {
    tic(paste("Pulling data for ", name))
    
    biomarker_sql <- paste("
        SELECT DISTINCT
            measurement.person_id,
            measurement.measurement_concept_id,
            m_standard_concept.concept_name as standard_concept_name,
            measurement.measurement_datetime as measurement_date,
            measurement.value_as_number,
            m_unit.concept_name as unit_concept_name,

            m_visit.concept_name as visit_occurrence_concept_name 
        FROM
            ( SELECT
                * 
            FROM
                `measurement` measurement 
            WHERE
                (
                    measurement_concept_id IN  (
                        SELECT
                            DISTINCT c.concept_id 
                        FROM
                            `cb_criteria` c 
                        JOIN
                            (
                                select
                                    cast(cr.id as string) as id 
                                FROM
                                    `cb_criteria` cr 
                                WHERE
                                    concept_id IN ( ",
                                        concept_ids_string,
                                   " ) 
                                    AND full_text LIKE '%_rank1]%'
                            ) a 
                                ON (
                                    c.path LIKE CONCAT('%.',
                                a.id,
                                '.%') 
                                OR c.path LIKE CONCAT('%.',
                                a.id) 
                                OR c.path LIKE CONCAT(a.id,
                                '.%') 
                                OR c.path = a.id) 
                            WHERE
                                is_standard = 1 
                                AND is_selectable = 1
                            )
                    )
                ) measurement 
            LEFT JOIN
                `concept` m_standard_concept 
                    ON measurement.measurement_concept_id = m_standard_concept.concept_id 
            LEFT JOIN
                `concept` m_unit 
                    ON measurement.unit_concept_id = m_unit.concept_id 
            LEFT JOIn
                `visit_occurrence` v 
                    ON measurement.visit_occurrence_id = v.visit_occurrence_id 
            LEFT JOIN
                `concept` m_visit 
                    ON v.visit_concept_id = m_visit.concept_id", sep="")

    bq_table_save(
      bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), biomarker_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
      biomarker_path,
      destination_format = "CSV")
      
   } else { tic(paste("Reading existing pulled data for ", name))}

    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), unit_concept_name = col_character(), 
                        visit_occurrence_concept_name = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              # message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))}
    
    biomarker_df <- suppressWarnings(read_bq_export_from_workspace_bucket(biomarker_path)) 
    
    if (remove_NA) {
        biomarker_df <- biomarker_df %>%
            filter(!is.na(value_as_number)) 
    }
        
    biomarker_df <- biomarker_df %>%
        mutate(measurement_date = as.Date(measurement_date)) %>%
        mutate(unit_concept_name = process_units(unit_concept_name)) 
    
    biomarker_df$visit_occurrence_concept_name[biomarker_df$visit_occurrence_concept_name %in% c("No matching concept", "NA")] <- NA
    
    toc()
    
    return(biomarker_df)
}


# generic function to pull an observation from concept ids, including value as number/string (for e.g. biomarkers, like Pain)
pull_observation_with_value <- function(concept_ids_string = "xxxx", name = "observation", read_existing = FALSE) {
    # example string: "37047736, 4146380"
    if (name == "observation" | is.null(name) | is.na(name)) {stop("Provide a name for the observation")}
    
    observation_path <- file.path(
      Sys.getenv("WORKSPACE_BUCKET"),
      "bq_exports",
      Sys.getenv("OWNER_EMAIL"),
      paste0("observation_", name, "_*.csv") )
    
    if (!read_existing) {

        # Delete files if they already exist
        system(paste0("gsutil rm ", observation_path))

    tic(paste("Pulling data for ", name))
    
    observation_sql <- paste("
        SELECT DISTINCT
            observation.person_id,
            observation.observation_concept_id,
            m_standard_concept.concept_name as standard_concept_name,
            observation.observation_datetime as measurement_date,
            observation.value_as_number,
            observation.value_as_string,
            m_unit.concept_name as unit_concept_name,
            m_visit.concept_name as visit_occurrence_concept_name, 
            o_type.concept_name as observation_type_concept_name,
            o_qualifier.concept_name as qualifier_concept_name,
        FROM
            ( SELECT
                * 
            FROM
                `observation` observation 
            WHERE
                (observation_concept_id IN  (", concept_ids_string, "))

                ) observation 
            LEFT JOIN
                `concept` m_standard_concept 
                    ON observation.observation_concept_id = m_standard_concept.concept_id 
            LEFT JOIN
                `concept` m_unit 
                    ON observation.unit_concept_id = m_unit.concept_id 
            LEFT JOIn
                `visit_occurrence` v 
                    ON observation.visit_occurrence_id = v.visit_occurrence_id 
            LEFT JOIN
                `concept` m_visit 
                    ON v.visit_concept_id = m_visit.concept_id
            LEFT JOIN
                `concept` o_qualifier 
                    ON observation.qualifier_concept_id = o_qualifier.concept_id 
            LEFT JOIN
                `concept` o_type 
                    ON observation.observation_type_concept_id = o_type.concept_id ", sep="")

    bq_table_save(
      bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), observation_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
      observation_path,
      destination_format = "CSV")
      
   } else { tic(paste("Reading existing pulled data for ", name))}

    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), unit_concept_name = col_character(), 
                        visit_occurrence_concept_name = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              # message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))}
    
   observation_df <- suppressWarnings(read_bq_export_from_workspace_bucket(observation_path)) %>%
        mutate(measurement_date = as.Date(measurement_date)) 
    
    observation_df$visit_occurrence_concept_name[observation_df$visit_occurrence_concept_name %in% 
                                                 c("No matching concept", "NA")] <- NA
    
    toc()
    
    return(observation_df)
}


# generic function to pull an observation from concept ids, without values
pull_observation <- function(concept_ids_string = "xxxx", name = "observation", read_only=FALSE){
    
    if (name == "observation" | is.null(name) | is.na(name)) {stop("Provide a name for the observation")}
    
    observation_path <- file.path(
      Sys.getenv("WORKSPACE_BUCKET"),
      "bq_exports",
      Sys.getenv("OWNER_EMAIL"),
      paste0("observation_", name, "_*.csv"))
     
    if (!read_only) {
        
        # Delete files if they already exist
        system(paste0("gsutil rm ", observation_path))

        tic(paste("Pulling data for ", name))

        observation_sql <- paste("
            SELECT
                observation.person_id,
                observation.observation_concept_id,
                o_standard_concept.concept_name as standard_concept_name,
                observation.observation_datetime AS observation_date,
                o_type.concept_name as observation_type_concept_name,
                o_visit.concept_name as visit_occurrence_concept_name,
                o_source_concept.concept_name as source_concept_name,
                o_source_concept.concept_code as source_concept_code, 
                o_source_concept.vocabulary_id as source_vocabulary
            FROM
                ( SELECT
                    * 
                FROM
                    `observation` observation 
                WHERE
                    (
                        observation_concept_id IN (", concept_ids_string, ")
                    )) observation 
            LEFT JOIN
                `concept` o_standard_concept 
                    ON observation.observation_concept_id = o_standard_concept.concept_id 
            LEFT JOIN
                `concept` o_type 
                    ON observation.observation_type_concept_id = o_type.concept_id 
            LEFT JOIN
                `visit_occurrence` v 
                    ON observation.visit_occurrence_id = v.visit_occurrence_id 
            LEFT JOIN
                `concept` o_visit 
                    ON v.visit_concept_id = o_visit.concept_id 
            LEFT JOIN
                `concept` o_source_concept 
                    ON observation.observation_source_concept_id = o_source_concept.concept_id", sep="")

        bq_table_save(
          bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), observation_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
          observation_path,
          destination_format = "CSV")
        }

    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), observation_type_concept_name = col_character(), 
                        visit_occurrence_concept_name = col_character(), source_concept_name = col_character(), 
                        source_concept_code = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))
    }
    observation_df <- read_bq_export_from_workspace_bucket(observation_path) %>%
        mutate(observation_date = as.Date(observation_date))

    dim(observation_df)
    
    return(observation_df)

}


# functions for processing biomarkers faster
dedup_records <- function(data){
    tic("De-duplicating records")
    # First deal with duplicated records (i.e. same value on same date)
    suppressMessages(dups_only <- data %>% 
        group_by(person_id, measurement_date, value_as_number) %>%
        filter(n() > 1))
    # discard copies of same record mapped to multiple measurement_concept_ids
    fixed_dups <- dups_only %>% 
        tidyr::fill(visit_occurrence_concept_name, unit_concept_name, .direction = "downup") %>%
        distinct(person_id, measurement_date, value_as_number, .keep_all = TRUE) 
    suppressMessages(deduped <- data %>%
        anti_join(dups_only) %>%
        full_join(fixed_dups))
    toc()
    return(deduped)
 }

# Summarize by date
dedup_median <- function(data) {
    tic("Summarizing biomarker by date")
    # After cleaning units and filtering range, summarize deduped data by date
    
    # Allow max of 10 real records per date
    m10 <- data %>% group_by(person_id, measurement_date) %>%
            filter(n() <= 10) 
    
    # Don't process dates with only one record, to save time
    m1 <- m10 %>% filter(n() == 1)
    
    # these dates have between 2 and 10 records
    suppressMessages(m10 <- m10 %>% anti_join(m1) )
    
    # summarize the values
    suppressMessages(
        m10_summ1 <- m10 %>% summarize(
                    summarized_from_n = n(),
                    value_as_number = median(value_as_number)) )
    
    # Summarize the auxiliary info, where it is all consistent
    chk <-  m10 %>% 
        fill(visit_occurrence_concept_name, .direction = "downup") %>% 
        distinct(measurement_concept_id, standard_concept_name, unit_concept_name, visit_occurrence_concept_name)
    m10_consistent <- chk %>% filter(n() == 1)
    
    # summarize auxiliary info where there are inconsistencies
    suppressMessages(
        m10_multiple <- anti_join(chk, m10_consistent))
    
        # Due to multiple concept ids
        m10m_diffconcept <- m10_multiple %>% filter(length(unique(measurement_concept_id)) > 1)
        # Due to other some other field
        suppressMessages(m10m_sameconcept <- m10_multiple %>% anti_join(m10m_diffconcept))

        if (nrow(m10m_diffconcept) > 0) {
            warning(paste0("Summarizing across multiple concept ids for ", nrow(m10m_diffconcept) , " records"))
            suppressMessages( 
                m10m_summ1 <- m10m_diffconcept %>% 
                    summarize(
                        # This will produce a separate, character type field if there are multiple 
                        summarized_concept_ids = paste0(unique(measurement_concept_id), collapse = " / "),
                        visit_occurrence_concept_name = paste0(na.omit(unique(visit_occurrence_concept_name)), collapse = " / "), 
                        unit_concept_name = paste0(na.omit(unique(unit_concept_name)), collapse = " / "), 
                        standard_concept_name = paste0(unique(standard_concept_name), collapse = " / "))  %>%
            ungroup() %>% mutate(mult_type_summary_flag = 1) %>%
            mutate(unit_concept_name = ifelse(unit_concept_name == "", NA, unit_concept_name),
                 visit_occurrence_concept_name = ifelse(visit_occurrence_concept_name == "", 
                                                        NA, visit_occurrence_concept_name))
            )
                            
        }

        if (nrow(m10m_sameconcept) > 0) {
            warning(paste0("Summarizing across multiple units or visit types for ", nrow(m10m_sameconcept) , " records"))
            suppressMessages(
               m10m_summ2 <- m10m_sameconcept %>% group_by(person_id, measurement_date, measurement_concept_id, standard_concept_name) %>%
                    summarize(
                        visit_occurrence_concept_name = paste0(na.omit(unique(visit_occurrence_concept_name)), collapse = " / "), 
                        unit_concept_name = paste0(na.omit(unique(unit_concept_name)), collapse = " / "))  %>%
            ungroup() %>% mutate(mult_type_summary_flag = 1) %>%
            mutate(unit_concept_name = ifelse(unit_concept_name == "", NA, unit_concept_name),
                 visit_occurrence_concept_name = ifelse(visit_occurrence_concept_name == "", 
                                                        NA, visit_occurrence_concept_name))
            )
        }

    
    if (exists("m10m_summ1") & exists("m10m_summ2")) {
        suppressMessages(
            m10_all <-  full_join(m10m_summ1, m10m_summ2) %>%
                full_join(m10_consistent) %>% 
                full_join(m10_summ1) )
    } else if (exists("m10m_summ1")){
        suppressMessages(
            m10_all <-  full_join(m10m_summ1, m10_consistent) %>% 
                full_join(m10_summ1) )
    } else if (exists("m10m_summ2")) {
        suppressMessages(
            m10_all <-  full_join(m10m_summ2, m10_consistent) %>% 
                full_join(m10_summ1) )
    } else {
        suppressMessages(
            m10_all <-  full_join(m10_consistent, m10_summ1) )
    }

    suppressMessages(
        alldata <- full_join(m1, m10_all) %>% 
            select(any_of(c("person_id", "measurement_date", "value_as_number", "unit_concept_name", "standard_concept_name", 
                   "measurement_concept_id", "summarized_concept_ids", "visit_occurrence_concept_name",  
                   "mult_type_summary_flag", "summarized_from_n"))) %>% 
            ungroup() %>%
            mutate(summarized_from_n = coalesce(summarized_from_n, 1))
            )
    toc()
    return(alldata)
}


# Pull conditions using concept ids rather than icd patterns, including descendant concept ids
pull_condition <- function(concept_ids_string = "xxxx", name = "condition", read_only = FALSE) {
    
    if (name == "condition" | is.null(name) | is.na(name)) {stop("Provide a name for the condition")}
    
    condition_path <- file.path(
          Sys.getenv("WORKSPACE_BUCKET"),
          "bq_exports",
          Sys.getenv("OWNER_EMAIL"),
          paste0("condition_", name, "_*.csv") )
     
    if (!read_only) {
        
        # Delete files if they already exist
        system(paste0("gsutil rm ", condition_path))
        
        tic(paste("Pulling data for ", name))
    
        condition_sql <- paste("
                SELECT DISTINCT
                c_occurrence.person_id,
                c_occurrence.condition_start_datetime condition_start_date,
                c_occurrence.condition_concept_id,         
                c_standard_concept.concept_name as standard_concept_name,
                c_source_concept.vocabulary_id as source_vocabulary,
                c_source_concept.concept_code as source_concept_code, 
                c_source_concept.concept_name as source_concept_name,
                m_visit.concept_name as visit_occurrence_concept_name 
            FROM
                ( SELECT
                    * 
                from
                    `condition_occurrence` c_occurrence 
                WHERE
                    (
                        condition_concept_id IN  (
                            SELECT
                                DISTINCT c.concept_id 
                            FROM
                                `cb_criteria` c 
                            JOIN
                                (
                                    select
                                        cast(cr.id as string) as id 
                                    FROM
                                        `cb_criteria` cr 
                                    WHERE
                                        concept_id IN (",
                               concept_ids_string,
                                ") 
                                    AND full_text LIKE '%_rank1]%'
                            ) a 
                                ON (
                                    c.path LIKE CONCAT('%.',
                                a.id,
                                '.%') 
                                OR c.path LIKE CONCAT('%.',
                                a.id) 
                                OR c.path LIKE CONCAT(a.id,
                                '.%') 
                                OR c.path = a.id) 
                            WHERE
                                is_standard = 1 
                                AND is_selectable = 1
                            )
                    )
                ) c_occurrence 
            left join
                `concept` c_standard_concept 
                    on c_occurrence.condition_concept_id = c_standard_concept.CONCEPT_ID 
            left join
                `concept` c_source_concept 
                    on c_occurrence.CONDITION_SOURCE_CONCEPT_ID = c_source_concept.CONCEPT_ID
            left join
                    `visit_occurrence` v 
                        ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
            left join
                    `concept` m_visit 
                        ON v.visit_concept_id = m_visit.concept_id", sep="")

        bq_table_save(
          bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
          condition_path,
          destination_format = "CSV")
        
        } else { tic(paste0("Reading in existing pull for ", name))}

    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), unit_concept_name = col_character(), 
                        visit_occurrence_concept_name = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              # message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))}
    
    condition_df <- suppressWarnings(read_bq_export_from_workspace_bucket(condition_path)) %>%
        mutate(condition_start_date = as.Date(condition_start_date)) 
    
    condition_df$visit_occurrence_concept_name[condition_df$visit_occurrence_concept_name %in% c("No matching concept", "NA")] <- NA
    
    toc()
    
    return(condition_df)
                           
}

# Pull conditions by concept id, not as ancestor concept
pull_condition_without_descendants <- function(concept_ids_string = "xxxx", name = "condition", read_only = FALSE) {
    
    if (name == "condition" | is.null(name) | is.na(name)) {stop("Provide a name for the condition")}
    
    condition_path <- file.path(
          Sys.getenv("WORKSPACE_BUCKET"),
          "bq_exports",
          Sys.getenv("OWNER_EMAIL"),
          paste0("condition_", name, "_noDesc_*.csv") )
     
    if (!read_only) {
        
        # Delete files if they already exist
        system(paste0("gsutil rm ", condition_path))
        
        tic(paste("Pulling data for ", name))
    
        condition_sql <- paste("
                SELECT DISTINCT
                c_occurrence.person_id,
                c_occurrence.condition_start_datetime condition_start_date,
                c_occurrence.condition_concept_id,         
                c_standard_concept.concept_name as standard_concept_name,
                c_source_concept.vocabulary_id as source_vocabulary,
                c_source_concept.concept_code as source_concept_code, 
                c_source_concept.concept_name as source_concept_name,
                m_visit.concept_name as visit_occurrence_concept_name 
            FROM
                 `condition_occurrence` c_occurrence 
            left join
                `concept` c_standard_concept 
                    on c_occurrence.condition_concept_id = c_standard_concept.CONCEPT_ID 
            left join
                `concept` c_source_concept 
                    on c_occurrence.CONDITION_SOURCE_CONCEPT_ID = c_source_concept.CONCEPT_ID
            left join
                    `visit_occurrence` v 
                        ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
            left join
                    `concept` m_visit 
                        ON v.visit_concept_id = m_visit.concept_id
            WHERE
                condition_concept_id IN (",  concept_ids_string,  ")", sep="")

        bq_table_save(
          bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
          condition_path,
          destination_format = "CSV")
        
        } else { tic(paste0("Reading in existing pull for ", name))}

    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), unit_concept_name = col_character(), 
                        visit_occurrence_concept_name = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              # message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))}
    
    condition_df <- suppressWarnings(read_bq_export_from_workspace_bucket(condition_path)) %>%
        mutate(condition_start_date = as.Date(condition_start_date)) 
    
    condition_df$visit_occurrence_concept_name[condition_df$visit_occurrence_concept_name %in% c("No matching concept", "NA")] <- NA
    
    toc()
    
    return(condition_df)
                           
}

# Generic function to pull procedure records
pull_procedure <- function(concept_ids_string = "xxxx", name = "procedure", require_standard = TRUE){
    
    if (name == "procedure") {stop("Provide a name for the procedure")}
    
    procedure_path <- file.path(
          Sys.getenv("WORKSPACE_BUCKET"),
          "bq_exports",
          Sys.getenv("OWNER_EMAIL"),
          paste0("procedure_", name, "_*.csv") )
    
    # Delete files if they already exist
    system(paste0("gsutil rm ", procedure_path))
     
    tic(paste("Pulling data for ", name))
    
    procedure_sql <- paste( "
        SELECT DISTINCT
            procedure.person_id,
            procedure.procedure_date,
            procedure.procedure_concept_id,
            p_standard_concept.concept_name as standard_concept_name,
            p_source_concept.vocabulary_id as source_vocabulary,
            p_source_concept.concept_code as source_concept_code,    
            p_source_concept.concept_name as source_concept_name,
            m_visit.concept_name as visit_occurrence_concept_name 
        FROM
            ( SELECT
                * 
            from
                `procedure_occurrence` procedure 
            WHERE
                (
                    procedure_concept_id IN  (
                        SELECT
                            DISTINCT c.concept_id 
                        FROM
                            `cb_criteria` c 
                        JOIN
                            (
                                select
                                    cast(cr.id as string) as id 
                                FROM
                                    `cb_criteria` cr 
                                WHERE
                                    concept_id IN (",
            concept_ids_string, 
             ") 
                                        AND full_text LIKE '%_rank1]%'
                                ) a 
                                    ON (
                                        c.path LIKE CONCAT('%.',
                                    a.id,
                                    '.%') 
                                    OR c.path LIKE CONCAT('%.',
                                    a.id) 
                                    OR c.path LIKE CONCAT(a.id,
                                    '.%') 
                                    OR c.path = a.id) 
                                WHERE ",
                                   ifelse(require_standard, "is_standard = 1 AND ", ""),                                
                                   " is_selectable = 1
                                )
                        )
                    ) procedure 
                LEFT JOIN
                    `concept` p_standard_concept 
                        on procedure.procedure_concept_id = p_standard_concept.CONCEPT_ID 
                LEFT JOIN
                    `concept` p_source_concept 
                        on procedure.PROCEDURE_SOURCE_CONCEPT_ID = p_source_concept.CONCEPT_ID
                left join
                        `visit_occurrence` v 
                            ON procedure.visit_occurrence_id = v.visit_occurrence_id 
                left join
                        `concept` m_visit 
                            ON v.visit_concept_id = m_visit.concept_id", sep = "")  

 

    bq_table_save(
      bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), procedure_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
      procedure_path,
      destination_format = "CSV")

    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), source_concept_code = col_character(), 
                        source_vocabulary = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))
    }
    
    procedure_df <- suppressWarnings(read_bq_export_from_workspace_bucket(procedure_path)) %>%
            mutate(procedure_date = as.Date(procedure_date))
     
    procedure_df$visit_occurrence_concept_name[procedure_df$visit_occurrence_concept_name %in% 
                                               c("No matching concept", "NA")] <- NA
    
    toc()
    
    return(procedure_df)
    
}       

# Get concept names from concept ids, for convenience
getConceptNames <- function(concept_ids_string){
    concept_path <- file.path(
          Sys.getenv("WORKSPACE_BUCKET"),
          "bq_exports",
          Sys.getenv("OWNER_EMAIL"),
          paste0("tmp_concept*.csv") )
    
    # Delete the files there if they already exist
    system(paste0("gsutil rm ", concept_path))
    
    concept_sql <- paste0(
        "SELECT
            standard_concept.concept_id as concept_id,
            standard_concept.concept_name as standard_concept_name
        FROM `concept` standard_concept 
        WHERE concept_id IN (", concept_ids_string, ")")  

    bq_table_save(
      bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), concept_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
      concept_path,
      destination_format = "CSV")

    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), source_concept_code = col_character(), 
                        source_vocabulary = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))
    }
    
    concept_df <- suppressWarnings(read_bq_export_from_workspace_bucket(concept_path))
    
    return(concept_df)
}


# Get all concept info from concept ids, for convenience
getConceptInfo <- function(concept_ids_string){
    concept_path <- file.path(
          Sys.getenv("WORKSPACE_BUCKET"),
          "bq_exports",
          Sys.getenv("OWNER_EMAIL"),
          paste0("tmp_concept*.csv")) 
        
    # Delete files if they already exist    
    system(paste0("gsutil rm ", concept_path), intern=T)
        
    concept_sql <- paste0(
        "SELECT *
        FROM `concept` standard_concept 
        WHERE concept_id IN (", concept_ids_string, ")")  

    bq_table_save(
      bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), concept_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
      concept_path,
      destination_format = "CSV")

    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), source_concept_code = col_character(), 
                        source_vocabulary = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))
    }
    
    concept_df <- suppressWarnings(read_bq_export_from_workspace_bucket(concept_path))

    return(concept_df)
}

# Generic function to perform a query, save the results, and read them in
getQuery <- function(query){
    save_path <- file.path(
      Sys.getenv("WORKSPACE_BUCKET"),
      "bq_exports",
      Sys.getenv("OWNER_EMAIL"),
      paste0("tmp_query_*.csv") )
    
    # delete files if they already exist
    system(paste0("gsutil rm ", save_path), intern=T)

    bq_table_save(
      bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), query, billing = Sys.getenv("GOOGLE_PROJECT")),
      save_path,
      destination_format = "CSV")

    read_bq_export_from_workspace_bucket <- function(export_path) {
      col_types <- cols(standard_concept_name = col_character(), source_concept_code = col_character(), 
                        source_vocabulary = col_character())
      bind_rows(
        map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
            function(csv) {
              message(str_glue('Loading {csv}.'))
              chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
              if (is.null(col_types)) {
                col_types <- spec(chunk)
              }
              chunk
            }))
    }
    
    query_df <- suppressWarnings(read_bq_export_from_workspace_bucket(save_path))

    return(query_df)
}

# Generic function to perform a query, save the results to a temporary table, and read them in
getQuery2 <- function(query){
    # store result to temporary sql table
    tb <- bq_project_query(Sys.getenv('GOOGLE_PROJECT'), query = query, default_dataset = Sys.getenv('WORKSPACE_CDR'))
    # download from db to R
    df <- bq_table_download(tb)
    return(df)
}

# Get all descendant concept ids from concept ids, for convenience
getDescendants <- function(concept_ids_string) {
    qry <- paste0("SELECT
            standard_concept.concept_id as concept_id,
            standard_concept.concept_name as standard_concept_name,
            ancestor_concept.descendant_concept_id,
            descendant_concept.concept_name as descendant_concept_name
        FROM `concept` standard_concept 
        JOIN `concept_ancestor` ancestor_concept
            on standard_concept.concept_id = ancestor_concept_id
        JOIN `concept` descendant_concept
            on descendant_concept_id = descendant_concept.concept_id
        WHERE ancestor_concept.ancestor_concept_id IN (", concept_ids_string, ")")  
    descs <-  getQuery2(qry)
}

# Get all descendant concept ids from concept ids, excluding duplicates, for convenience
getUniqueDescendants <- function(concept_ids_string) {
    tmp <- getDescendants(concept_ids_string)
    rt <- tmp %>% filter(concept_id != descendant_concept_id) %>%
        select(-concept_id, -standard_concept_name) %>%
        dplyr::rename(concept_id = descendant_concept_id, standard_concept_name = descendant_concept_name) %>%
        distinct()
    return(rt)
}

# Get all descendant concept ids from concept ids, excluding duplicates and provided concepts, for convenience
getUniqueDescendantsMulti <- function(concept_ids_string) {
    tmp <- getUniqueDescendants(concept_ids_string)
    CIDs <- as.numeric(unlist(strsplit(concept_ids_string, ",\\s*")))
    rt <- tmp %>% filter(!(concept_id %in% CIDs ))
}

# Define qualifying sequences of codes within relevant time frame, for outcomes (i.e. ESRD codes)
code_sequence <- function(x, min_sequence_days, new_sequence_days, min_n_codes, prefix = "seq") {
    seqs <- x |> distinct() |>
        group_by(person_id) |>
        filter(n() >= min_n_codes) |>
        arrange(person_id, event_date) |>
        mutate(days_since_last_dt = as.numeric(event_date - lag(event_date)), 
              new_seq = days_since_last_dt > new_sequence_days | is.na(days_since_last_dt),
              seqno = cumsum(new_seq)) |>
    	group_by(person_id, seqno) %>%
        mutate(seq_qual = as.numeric(max(event_date) - min(event_date) ) >= min_sequence_days) |>
        mutate(any_seq_qual = any(seq_qual)) %>% 
        filter(any_seq_qual == T) %>%
        mutate(first_seq_qual = min(seqno[which(seq_qual)]),
              first_seq_qual = ifelse(is.infinite(first_seq_qual), NA, first_seq_qual))
    
    rtn <- seqs |>
        filter(seq_qual & seqno == first_seq_qual) |>
        group_by(person_id) |>
        summarise(first_seq = min(event_date))
    names(rtn)[2] <- paste0("first_", prefix, ".", min_n_codes, ".", min_sequence_days, ".", new_sequence_days)
    return(rtn)
}

# Convert dates to the number of years since DMDx and filter to the relevant time frame.
align_date_dm <- function(data, marker_names = "value_as_number", date_name = "measurement_date", years_prior = 2) {
    data <- data %>% dplyr::rename(marker_date = date_name) %>%
        select(person_id, marker_date, all_of(marker_names)) %>%
        filter(!is.na(!!sym(marker_names[1])))
    marker_time <- data %>%
            filter(!is.na(marker_names[1])) %>%
            right_join_quiet(DMcohort %>% dplyr::select(person_id, DmDx_first, DeathDateTime)) %>%
            mutate(t = marker_date - DmDx_first) %>%
            filter(t >= (-years_prior) & (marker_date <= DeathDateTime | is.na(DeathDateTime))) %>%
            mutate(t = round(t, 1)) %>%
            dplyr::select(person_id, t, all_of(marker_names)) %>%
            arrange(person_id, t) %>%
            distinct_at(all_of(c("person_id", "t", marker_names)), .keep_all = T) %>%
            inner_join_quiet(DMcohort %>% select(person_id))
    return(marker_time)
}

# Process meds
med.gap <- function(data, min_gap_days = 30, years_prior = 0, mult.by.refills = TRUE) {
    meds_time <- data %>%
        right_join_quiet(DMcohort %>% dplyr::select(person_id, DmDx_first, DeathDateTime)) %>%
        mutate(tstart = drug_exposure_start_datetime - DmDx_first,
            tstop = drug_exposure_start_datetime + (DaysSupply/365.25)*(ifelse(mult.by.refills, refills+1, 1)) - DmDx_first) %>%
        filter(tstart >= -(years_prior) & tstop > tstart & 
               (drug_exposure_start_datetime <= DeathDateTime | is.na(DeathDateTime))) %>%
        distinct(person_id, tstart, tstop) %>%
        arrange(person_id, tstart) %>%
        group_by(person_id) %>%
        # collapse overlapping date ranges
        mutate(indx = c(0, cumsum(as.numeric(lead(tstart)) > cummax(as.numeric(tstop)))[-n()])) %>%
        group_by(person_id, indx) %>%
        summarise(tstart = min(tstart), tstop = max(tstop), .groups = "drop") %>%
        group_by(person_id) %>%
        mutate(med = 1, 
               gap_tstart = ifelse(lead(tstart) > tstop, tstop, NA),
                gap_tstop = ifelse(lead(tstart) > tstop, lead(tstart), NA))
    meds_gaps <- meds_time %>%
        select(person_id, gap_tstart, gap_tstop) %>%
        filter(!is.na(gap_tstart) & !is.na(gap_tstop)) %>%
        mutate(med = 0, gap_length_days =  (gap_tstop - gap_tstart)*365.25)
    meds_gaps <- meds_gaps %>%
        filter(gap_length_days > min_gap_days) %>%
        dplyr::distinct(person_id, tstart = gap_tstart, med)
    meds_time <- full_join_quiet(meds_time, meds_gaps) %>%
        arrange(person_id, tstart) %>%
        distinct(person_id, tstart, med) %>%
        group_by(person_id) %>%
        filter(row_number() == 1 | !(med == lag(med))) %>%
        mutate(tstart = round(tstart, 1)) %>%
        distinct(person_id, tstart, med) %>%
        group_by(person_id, tstart) %>%
        summarize(med = max(med), .groups = "drop") %>%
        ungroup()
    meds_time <- meds_time %>% inner_join_quiet(DMcohort %>% select(person_id)) %>%
        dplyr::rename(t = tstart)
    return(meds_time)
}

# Transformations
standardize <- function(x) { (x - mean(x, na.rm=T))/sd(x, na.rm=T) }
invnorm <- function(x) { y = qnorm(( rank(x, na.last = "keep") - 0.5)/sum(!is.na(x)));  return(y) }

# Detect binary variables and convert to factor
factorize.i <- function(x) { 
    if (identical(sort(unique(x)), c(0,1)) | identical(sort(unique(x)), c(0)) | identical(sort(unique(x)), c(1)) |
       identical(sort(unique(x)), c(0L,1L)) | identical(sort(unique(x)), c(0L)) | identical(sort(unique(x)), c(1L))
       
       ) { 
        x <- factor(x, levels = c(0,1))
    } 
    return(x)
}

# Get a snapshot of most recent covariates at a given landmark time, from tdept
landmark <- function(data, t) {
    data %>% filter(tstart <= t) %>%
        group_by(person_id) %>%
        slice_max(tstart) %>%
        ungroup()
}

# Quantiles
q25 <- function(x) { signif(quantile(x, 0.25, na.rm=T), 3)}
q75 <- function(x) { signif(quantile(x, 0.75, na.rm=T), 3)}
IQR <- function(x) { paste0( signif(median(x, na.rm=T), 3), " (", q25(x), " - ", q75(x), ")" )}


# Function to round to the nearest 0.1 year, taking the most recent measure in this time frame
yr_round <- function(x) {
    x %>% mutate(measurement_date = decimal_date(measurement_date) - 2000) %>%
        inner_join_quiet(DMcohort %>% select(person_id, DeathDateTime, DmDx_first)) %>%
        filter(measurement_date <= DeathDateTime | is.na(DeathDateTime)) %>%
        mutate(yrs = measurement_date - DmDx_first,
              yrs_rounded = round(yrs, 1)) %>%
        filter(yrs >= -2) %>%
        group_by(person_id, yrs_rounded) %>%
        slice_max(yrs, with_ties = F) %>%
        select(-yrs) %>%
        ungroup() %>%
        dplyr::rename(yrs = yrs_rounded) %>% 
        select(person_id, yrs, value_as_number)     
}


# Mask counts under 20 in summary stats - NAs
summcat_mask20_NAs <- function(x){
    outpt <- paste0(sum(is.na(x)), " (", signif(mean(is.na(x))*100, 3), "%)")
    if (sum(is.na(x)) < 20 & sum(is.na(x)) != 0) { 
        outpt <-  paste0("<=20 (<=", signif((20/length(x))*100, 3), "%)")
    }
    return(outpt)
}


# Number of repeated measures in time frame
get_n_measures <- function(object_name){
    d <- get(object_name) %>%
        right_join_quiet(table1_start %>% distinct(person_id)) %>%
        mutate(pre_y1 = (yrs <= 1) ) %>%
        group_by(person_id, pre_y1) %>%
        count() %>% ungroup() %>%
        mutate(n_adj = ifelse(is.na(pre_y1), 0, 
                              ifelse(pre_y1, pmin(n, 1, na.rm=T), 
                                     n ))) %>%
        group_by(person_id) %>%
        summarize(repeated_measures = sum(n_adj, na.rm=T))
    names(d)[2] <- object_name
    return(d)
}

# Number of repeated measures in time frame by gender/sex
get_n_measures_sex <- function(object_name){
    d <- get(object_name) %>%
        right_join(table1_start %>% distinct(person_id, Gender)) %>%
        mutate(pre_y1 = (yrs <= 1) ) %>%
        group_by(person_id, Gender, pre_y1) %>%
        count() %>% ungroup() %>%
        mutate(n_adj = ifelse(is.na(pre_y1), 0, 
                              ifelse(pre_y1, pmin(n, 1, na.rm=T), n))) %>%
        group_by(person_id, Gender) %>%
        summarize(repeated_measures = sum(n_adj))
    names(d)[3] <- object_name
    return(d)
}


# Generate predicted probabilities using fastCrr for 1 horizon
predict_prob_calibrated <- function(ftime, fstatus, horizon, betax){
    
    recalibLP <- fastCrr(Crisk(ftime,fstatus) ~ betax, 
                  variance = FALSE, returnDataFrame = T)
      
    if (!(horizon %in% recalibLP$uftime)) {
        horizon_use <- max(recalibLP$uftime[recalibLP$uftime < horizon])
        warning(paste0("Htime ", horizon, " did not exist, using htime ", horizon_use, " instead"))
    } else {horizon_use <- horizon}

    CIF0.recalibLP <- predict(recalibLP, newdata = mean(betax))
    CIF0.t.recalibLP <- CIF0.recalibLP$CIF[CIF0.recalibLP$ftime == horizon_use]       
    s0.t.recalibLP <- 1 - CIF0.t.recalibLP    
    surv.t.recalibLP <- s0.t.recalibLP^(exp(betax - mean(betax)))             
    pred.t.recalibLP <- 1 - surv.t.recalibLP   
    return(pred.t.recalibLP)
}

# Generate predicted probabilities using fastCrr for all horizons
predict_prob_calibrated_allH <- function(ftime, fstatus, horizons, betax) {
    init <- data.frame(betaX = betax)
    for (horizon in horizons){
        prd <- predict_prob_calibrated(ftime, fstatus, horizon, betax)
        init <- init %>% mutate(prd)
        names(init)[length(names(init))] <- paste0("pred", horizon)
    }
    rt <- init %>% select(-betaX)
    return(rt)
}

# Calibration by quantiles, using competing risk
man_cal_quantiles <- function(pred, truth, htime, conf_level = 0.95, ntile = 4) { 
    if (max(truth$CR_tte_landmark) >= htime) {
      if (!is.null(pred) & length(pred) > 0) {  
        # Use pseudovalues
        f=prodlim(Hist(CR_tte_landmark, event_CR)~1, data=truth)
        pseudovalues <- jackknife(f, times = htime, cause = 1) %>% data.frame ()
        names (pseudovalues) <- "pseudo.t"
        brierScore <- mean((pseudovalues$pseudo.t - pred)^2)
        tmp <- data.frame("pseudo.t" = pseudovalues$pseudo.t, "pred" = pred)
        lmod <- lm(formula = I(pseudo.t) ~ 0 + pred, data=tmp)
        slp_SE <- signif( summary(lmod)$coefficients[,2] ) 
        slp <- signif(lmod$coefficients[[1]], 3)
        binned <- tmp %>%
            mutate(pred_grp = ntile(pred, ntile) ) %>%
            group_by(pred_grp) %>%
            dplyr::summarize (event_rt = mean(pseudo.t),
                                bin_meanpt = mean(pred),
                                total = n(), events = sum(pseudo.t) ) %>%
            ungroup() %>%
            filter(events >=0) %>% # rare issue with pseudovalues in very small bins - remove point
            filter(events <= total) %>% # rare issue in very small bins
            rowwise() %>%
            mutate(lower = prop.test(events, total, conf.level = conf_level)$conf.int[[1]],
            upper = prop.test(events, total, conf.level = conf_level) $conf.int [[2]]) %>%
            ungroup( ) %>%
            mutate(Slope = slp, Slope_SE = slp_SE, BrierScore = brierScore)
       } else {binned <- data.frame(Note = "Predictions not provided")}
        return(binned %>% mutate(htime) )
    } else {
        warning ("The requested time is later than exists in the data: returning NA")
        return(data.frame(htime) %>% mutate(Note = "The requested horizon is later than occurrs in the data") )
    }
}


# bootstrap function for AUC - default 1000 bootstraps
AUC_bootstrap <- function(truth, pred, cause=1, htimes, n_bootstrap = 1000 ) {
    res_init <- data.frame(horiz = NA) %>% filter(!is.na(horiz))
    for (htime in htimes){
        bootstrap_init <- data.frame(boot_n = 0) %>% filter(is.na(boot_n))
        main_troc <- timeROC(truth$CR_tte_landmark, truth$event_CR, pred, cause = 1, times = htime, ROC = F, iid = F)
        if (!is.null(main_troc$AUC_1)) {
           main_auc <- cbind("AUC_1" = main_troc$AUC_1[2], "AUC_2" = main_troc$AUC_2[2]) %>% data.frame() 
        } else {
            # If there are no competing event occurrences, it changes to just "AUC" not AUC_1 and AUC_2
           main_auc <- cbind("AUC_1" = main_troc$AUC[2], "AUC_2" = main_troc$AUC[2]) %>% data.frame() 
        }

        for (i in 1:n_bootstrap) {
            indices_boot <- sample(1:nrow(truth), nrow(truth), replace=TRUE )
            truth_boot <- truth[indices_boot, ]
            pred_boot <- pred[indices_boot]
            troc_boot <- timeROC(truth_boot$CR_tte_landmark, truth_boot$event_CR, pred_boot, 
                                   cause=1, time = htime, ROC=F, iid=F) 
            if (!is.null(troc_boot$AUC_1)) {
                aucdat_boot <- cbind("AUC_1" = troc_boot$AUC_1[2],
                                     "AUC_2" = troc_boot$AUC_2[2]) %>% data.frame() %>% mutate(boot_n = i)
            } else {
                aucdat_boot <- cbind("AUC_1" = troc_boot$AUC[2],
                                     "AUC_2" = troc_boot$AUC[2]) %>% data.frame() %>% mutate(boot_n = i)
            }
            suppressMessages(bootstrap_init <- full_join(bootstrap_init, aucdat_boot))
        }

        bootstrap_CI_sd <- bootstrap_init %>%
            dplyr::summarize(AUC_1_lower = quantile(AUC_1, 0.025, na.rm=T),
                            AUC_1_upper = quantile(AUC_1, 0.975, na.rm=T),
                             AUC_2_lower = quantile(AUC_2, 0.025, na.rm=T),
                            AUC_2_upper = quantile(AUC_2, 0.975, na.rm=T),
                            AUC_1_sd = sd(AUC_1, na.rm=T),
                            AUC_2_sd = sd(AUC_2, na.rm=T)) %>% 
        mutate(horiz = htime)
        res <- cbind(main_auc, bootstrap_CI_sd)
        suppressMessages(res_init <- full_join(res_init, res))
    }
    return(res_init)
}

# AUPRC basic functions
CR_get_PRC <- function (pred, truth, htime, cause = 1) {
    # Get precision-recall curve values for a given single time and predictor
    pred <- signif(pred, 2) # Don't need too many points
    init <- data.frame(cutpoint=0, precision=0, recall=0) 
    for (cutp in unique(pred)){
        sspn_dat <- timeROC::SeSpPPVNPV(cutpoint=cutp, truth$CR_tte_landmark, truth$event_CR, pred, cause = cause, times=htime)
        suppressMessages(
            r_cut <- data.frame(cutpoint = cutp, precision = sspn_dat$PPV, recall = sspn_dat$TP) %>%
                cbind(sspn_dat$Stats %>% data.frame()) %>% .[2, ] )
        suppressMessages (init <- full_join(init, r_cut) )
    }
    return(init)
}

CR_get_AUPRC <- function(pred, truth, htime, cause = 1) {
    PRC <- CR_get_PRC(pred, truth, htime, cause)
    AUPRC <- PRC %>% filter(!is.nan(precision)) %>%
        arrange(recall) %>% # must be arranged by recall first
        dplyr::summarize(AUPRC = pracma::trapz(recall, precision) )
    real_cif <- cmprsk::cuminc(truth$CR_tte_landmark, truth$event_CR)
    cif_time <- timepoints(real_cif, times = htime)
    AUPRC <- AUPRC %>% mutate(cif_est_prob = cif_time$est[1, ])
    return(list("PRC" = PRC, "AUPRC" = AUPRC) )
}


# bootstrap function for AUPRC - by default no bootstrapping
AUPRC_bootstrap <- function(truth, pred, cause=1, htimes, n_bootstrap = 0 ) {
    res_init <- data.frame(horiz = NA) %>% filter(!is.na(horiz))
    for (htime in htimes){

        main_auprc <- CR_get_AUPRC(pred, truth, htime, cause)$AUPRC
        
        if (n_bootstrap > 1) {
            bootstrap_init <- data.frame(boot_n = 0) %>% filter(is.na(boot_n))
            for (i in 1:n_bootstrap) {
                indices_boot <- sample(1:nrow(truth), nrow(truth), replace=TRUE )
                truth_boot <- truth[indices_boot, ]
                pred_boot <- pred[indices_boot]
                auprcdat_boot <- CR_get_AUPRC(pred_boot, truth_boot, htime, cause)$AUPRC %>%
                    mutate(boot_n = i)
                suppressMessages(bootstrap_init <- full_join_quiet(bootstrap_init, auprcdat_boot))
            }
            bootstrap_CI_sd <- bootstrap_init %>%
                dplyr::summarize(AUPRC_lower = quantile(AUPRC, 0.025, na.rm=T),
                                AUPRC_upper = quantile(AUPRC, 0.975, na.rm=T),
                                AUPRC_sd = sd(AUPRC, na.rm=T),
                                cif_est_prob_lower = quantile(cif_est_prob, 0.025, na.rm=T),
                                cif_est_prob_upper = quantile(cif_est_prob, 0.975, na.rm=T),
                                cif_est_prob_sd = sd(cif_est_prob, na.rm=T)
                                ) 
            res <- cbind(main_auprc, bootstrap_CI_sd)
        } else {
            res <- main_auprc
        }
        res <- res %>% mutate(horiz = htime)
        suppressMessages(res_init <- full_join_quiet(res_init, res))
    }
    return(res_init)
}

# function to define subsets
subset_group <- function(data, subgroup, EGFRdat) {
    if (subgroup == "Black/African American") {
        data <- data %>% filter(Race == "Black or African American")
    } else if (subgroup == "White/Caucasian") {
        data <- data %>% filter(Race == "White")
    } else if (subgroup == "Male") {
        data <- data %>% filter(Gender == "Male")
    } else if (subgroup == "Female") {
        data <- data %>% filter(Gender == "Female")
    } else if (subgroup == "Hispanic or Latino") {
        data <- data %>% filter(Ethnicity == "Hispanic or Latino")
    } else if (subgroup == "Non-Hispanic") {
        data <- data %>% filter(Ethnicity == "Not Hispanic or Latino")
    } else if (subgroup == "DX2000") {
        data <- data %>% filter(DmDx_first >= 0 & DmDx_first < 10)
    } else if (subgroup == "DX2010") {
        data <- data %>% filter(DmDx_first >= 10 & DmDx_first < 20)
    } else if (subgroup == "DXUNDER65") {
        data <- data %>% 
            left_join_quiet(ydat1 %>%  
                              select(person_id, Age_DmDx_raw = Age_DmDx)) %>%
            filter(Age_DmDx_raw < 65) %>% select(-Age_DmDx_raw)
    } else if (subgroup == "DXSENIOR") {
        data <- data %>% 
            left_join_quiet(ydat1 %>%  
                              select(person_id, Age_DmDx_raw = Age_DmDx)) %>%
            filter(Age_DmDx_raw < 65) %>% select(-Age_DmDx_raw)
    } else if (subgroup == "EGFRlt60") {
        data <- data %>% 
            left_join_quiet(EGFRdat %>% 
                              select(person_id, EGFR_raw = EGFR)) %>%
            filter(EGFR_raw < 60) %>% select(-EGFR_raw)
    } else if (subgroup == "EGFRgt60") {
        data <- data %>% 
            left_join_quiet(EGFRdat %>% 
                              select(person_id, EGFR_raw = EGFR)) %>%
            filter(EGFR_raw >= 60) %>% select(-EGFR_raw)
    } else if (subgroup == "UACRNA"){
        data <- data %>% filter(UACR_NA == 1)
    } else if (subgroup == "UACRnotNA") {
        data <- data %>% filter(UACR_NA == 0)
    } else (stop(paste("subgroup ", subgroup, " not valid")))
}

# Generic mask values < 20 (low group counts)
mask20 <- function(x){
    outpt <- as.character(x)
    outpt[x < 20 & !is.na(x)] <- "<=20"
    return(outpt)
}

# Calculate the RECODe risk score from input landmark data
calc_RECODe_riskscore <- function(landmark_data) {
    coefs <- data.frame(predictor = c("age_td", 
                                      "GenderF", "RaceBLACK.OR.AFRICAN.AMERICAN",
                                     "EthnicityHISPANIC.OR.LATINO", "SmokingFactorCURRENT.SMOKER", "SBP",
                                       "CVD2", 
                                      "bp_lowering_drugs", 
                                      "oraldmdrug", 
                                      "Anticoagulants",
                                      "A1C", "TotChol", "HDLC", 
                                      "Creat_BSP",  
                                      "UACR"),
                        coef = c(-0.01938, -0.01129, 0.08812, 0.23380, 0.14830, 0.00303, -0.02164,
                                -0.07952, -0.12560, 0.03199, 0.13690, -0.00111, 0.00629, 0.86090, 0.00036))
    data <- landmark_data %>%
        mutate(EthnicityHISPANIC.OR.LATINO = as.numeric(Ethnicity == 'Hispanic or Latino'),
              GenderF = as.numeric(Gender == "Female"),
              RaceBLACK.OR.AFRICAN.AMERICAN = as.numeric(Race == "Black or African American"),
              SmokingFactorCURRENT.SMOKER = as.numeric(SmokingFactor == "Current Smoker")) %>% 
        mutate_if(is.factor, function(x) {as.numeric(as.character(x))} ) %>%
        mutate(UACR = coalesce(UACR, 7.5),
              bp_lowering_drugs = ACEInhibitors|AngiotensinIIRB|Diuretics|BetaBlockers|CChannelBlockers,
              oraldmdrug = DPP4|SGLT2|Sulfonylurea|Thiazoladinedione|Other_oraldm) # metformin not included
        
    data_long <- data %>% select(person_id, all_of(unique(coefs$predictor))) %>%
            pivot_longer(!person_id, names_to = "predictor", values_to = "value") %>%
            full_join(coefs) %>%
            mutate(score_component = value * coef)
    scores <- data_long %>% group_by(person_id) %>%
        summarize(RECODe_score = sum(score_component))
    
    return(scores)                  
}

# Get recalibrated predicted probabilities, ignoring competing risk. Use cox regression for S0
predict_prob_calibrated_cox <- function(ftime, fstatus, horizons, betax){
    
    # Create the input datset for the model to be recalibrated
    tmpdatIN <- data.frame(ftime, fstatus, betax) %>% 
         mutate(event_ESRD = as.numeric(fstatus == 1)) %>% # convert 0/1/2 to 0/1
        select(-fstatus)
    
    # Fit the model using the whole available followup time and the defined risk score
    recalibLP_cox <- coxph(Surv(ftime, event_ESRD) ~ betax, data = tmpdatIN,
                      y = T, x = T)

    # Get risk at horizon times h
    pred.t.recalibLP <- riskRegression::predictRisk(recalibLP_cox, newdata = tmpdatIN, times = horizons)
    pred.t.recalibLP <- data.frame(pred.t.recalibLP)
    names(pred.t.recalibLP) <- paste0("pred", horizons)
    
    # This provides calibration/performance measures at each horizon - also run and store this object
    rrS <- riskRegression::Score(object = list("myModel" = recalibLP_cox),
             formula = Surv(ftime,event_ESRD)~1,
             data= tmpdatIN,
            metrics = c("auc", "brier"),
            summary = c("risks","IPA","riskQuantile","ibs"),
            plots = c("ROC", "Calibration", "boxplot"),
            times = horizons,
            cens.method = "pseudo", # "ipcw"
            cens.model = "km", # "cox" kills kernel
            seed = 12345
            ) 
    # Note that pseudovalues are only used for calibration curve.  Brier Score uses ipcw and km. 
    # It is expected that this Brier score will differ from what we would get using pseudovalues. 
    
    return(list(pred = pred.t.recalibLP,
               model = recalibLP_cox,
               data = tmpdatIN, 
               ScoreObj = rrS,
               horizons = horizons))    
}

# use with cox recalibration - alternative calibration metrics
get_Calib_quantiles_riskReg <- function(obj){
    rt1 <- obj$ScoreObj$Calibration$plotframe %>%
        group_by(times) %>% # horizon
        mutate(pred_grp = ntile(risk, 4)) %>%
        group_by(times, pred_grp) %>%
        summarise(event_rt = mean(pseudovalue),
                  bin_meanpt = mean(risk),
                  total = n(), 
                  events = sum(pseudovalue),
                 .groups = "drop") %>%
        dplyr::rename(htime = times) 
    
    rt2 <- obj$ScoreObj$Brier$score %>% 
        filter(model == "myModel") %>% select(-model) %>%
        dplyr::rename(htime = times, 
                     BrierScore_RiskReg = Brier,
                     BrierScoreLower_RiskReg=lower,
                     BrierScoreUpper_RiskReg=upper,
                     BrierScoreSE_RiskReg=se) 
    
    rt3 <- obj$ScoreObj$AUC$score %>% filter(model == "myModel") %>% 
        select(-model) %>%
        dplyr::rename(htime = times,
                     AUC_RiskReg = AUC,
                     AUCLower_RiskReg = lower,
                     AUCUpper_RiskReg = upper,
                     AUCSE_RiskReg=se)
    rt <- full_join_quiet(rt1, rt2) %>%
        full_join_quiet(rt3)
    return(rt)
}

# Calibration by quantiles, ignoring competing risk
man_cal_quantiles_nocmprsk <- function(pred, truth, htime, conf_level = 0.95, ntile = 4) { 
    truth <- truth %>% mutate(event_CR = as.numeric(event_CR == 1))
    
    if (max(truth$CR_tte_landmark) >= htime) {
      if (!is.null(pred) & length(pred) > 0) {  
        # Use pseudovalues
        f=prodlim(Hist(CR_tte_landmark, event_CR)~1, data=truth)
        pseudovalues <- jackknife(f, times = htime, cause = 1) %>% data.frame() 
        names(pseudovalues) <- "pseudo.t"
        # Convert from survival to risk - different from the case of Cometing risks where CIF is returned
        pseudovalues$pseudo.t <- 1 - pseudovalues$pseudo.t
        brierScore <- mean( (pseudovalues$pseudo.t - pred)^2)
        tmp <- data.frame("pseudo.t" = pseudovalues$pseudo.t, "pred" = pred)
        lmod <- lm(formula = I(pseudo.t) ~ 0 + pred, data=tmp)
        slp <- signif(lmod$coefficients[[1]], 3)
        slp_SE <- signif( summary(lmod)$coefficients[,2] )
        binned <- tmp %>%
            mutate(pred_grp = ntile(pred, ntile) ) %>%
            group_by(pred_grp) %>%
            dplyr::summarize (event_rt = mean(pseudo.t),
                                bin_meanpt = mean(pred),
                                total = n(), events = sum(pseudo.t) ) %>%
            ungroup() %>%
            filter(events >=0) %>% 
            filter(events <= total) %>% 
            rowwise() %>%
            mutate(lower = prop.test(events, total, conf.level = conf_level)$conf.int[[1]],
            upper = prop.test(events, total, conf.level = conf_level) $conf.int[[2]]) %>%
            ungroup() %>%
            mutate(Slope = slp, Slope_SE = slp_SE, BrierScore = brierScore)
       } else {binned <- data.frame(Note = "Predictions not provided")}
        return(binned %>% mutate(htime) )
    } else {
        warning ("The requested time is later than exists in the data: returning NA")
        return(data.frame(htime) %>% mutate(Note = "The requested horizon is later than occurrs in the data") )
    }
}

