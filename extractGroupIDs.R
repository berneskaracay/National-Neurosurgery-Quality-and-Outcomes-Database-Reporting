mapIdToPractice <- function(dagInfo, dag_name) {
    ## the old style record identifier is of the form SITE_XXYYYY or
    ## SITE*GROUP_XX0000 where SITE is the name of the practice, GROUP is the
    ## name of the practice sub group, XX is either LP, CP, CV and YYYY is the record
    ## number for that practice.
    ## the new style record identifier is of the form XXXXX-YYYY where XXXXX
    ## is the practice DAG number and YYYY is the record number for that
    ## practice.

    ## Extract the practice values from dag labels.
    dagInfo$practice <- sub("^([^*]+)(?:\\*(.+))?$", '\\1', dagInfo$dag_label)
    
    ## Preallocate the practice vector
    practice <- character(length(dag_name))

    ## Extract the practice name from the old style record identifier
    practice <- dagInfo$practice[match(dag_name, dagInfo$dag_name)]
    ## Set BLANK or NA dag_name values to BLANK in practice
    practice[is.na(dag_name) | dag_name == ""] <- ""
    
    ## Error if there are Blank in practice
    if (any(is.na(practice))) stop("Unable to resolve dag_id to practice name")
    
    return(practice)
}

 