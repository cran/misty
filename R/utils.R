#_______________________________________________________________________________
#
# Internatl Functions
#
# Colletion of internal function used within functions of the misty package

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Extract Variable Names Specified in the ... Argument  ------------------------

.var.names <- function(..., data, group = NULL, split = NULL, cluster = NULL,
                       id = NULL, obs = NULL, day = NULL, time = NULL,
                       check.chr = NULL) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Elements in the '...' Argument ####

  var.names <- sapply(substitute(list(...)), as.character)

  #...................
  ### Check for ! operators ####

  var.names.excl <- sapply(var.names, function(y) any(y %in% "!"))

  if (isTRUE(any(var.names.excl))) {

    for (i in which(var.names.excl)) {

      var.names.i <- unlist(strsplit(var.names[[i]], ""))

      if (isTRUE("+" %in% var.names.i)) {

         var.names[[i]] <- unlist(strsplit(var.names[[i]], "(?=[/+])", perl = TRUE))

      } else if (isTRUE("-" %in% var.names.i)) {

        var.names[[i]] <- unlist(strsplit(var.names[[i]], "(?=[/-])", perl = TRUE))

      } else if (isTRUE("~" %in% var.names.i)) {

        var.names[[i]] <- unlist(strsplit(var.names[[i]], "(?=[/~])", perl = TRUE))

      } else if (isTRUE(sum(var.names.i %in% ":") == 1L)) {

        var.names[[i]] <- c("!", ":", unlist(strsplit(var.names[[i]], ":"))[2L:3L])

      } else if (isTRUE(sum(var.names.i %in% ":") == 2L)) {

        var.names[[i]] <- c("!", "::", unlist(strsplit(var.names[[i]], "::"))[2L:3L])

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Variables ####

  var.exclude <- NULL

  #...................
  ### Starts with a prefix: List elements with + ####

  var.plus <- sapply(var.names, function(y) any(y == "+"))

  if (isTRUE(any(var.plus))) {

    # List elements with +
    for (i in which(var.plus)) {

      # Variable names
      var.i <- misty::chr.omit(var.names[[i]], omit = "+", check = FALSE)

      # No complement !
      if (isTRUE(!"!" %in% var.i)) {

        var.names[[i]] <- colnames(data)[which(substr(colnames(data), start = 1L, stop = nchar(var.i)) == var.i)]

      # Complement !
      } else {

        var.i <- misty::chr.omit(var.i, omit = "!", check = FALSE)

        var.exclude <- c(var.exclude, colnames(data)[which(substr(colnames(data), start = 1L, stop = nchar(var.i)) == var.i)])

        var.names[[i]] <- ""

      }

    }

  }

  #...................
  ### Ends with a suffix: List elements with - ####

  var.minus <- sapply(var.names, function(y) any(y == "-"))

  if (isTRUE(any(var.minus))) {

    # List elements with -
    for (i in which(var.minus)) {

      # Variable names
      var.i <- misty::chr.omit(var.names[[i]], omit = "-", check = FALSE)

      # No complement !
      if (isTRUE(!"!" %in% var.i)) {

        var.names[[i]] <- colnames(data)[which(substr(colnames(data), start = nchar(colnames(data)) - nchar(var.i) + 1L, stop = nchar(colnames(data))) == var.i)]

      # Complement !
      } else {

        var.i <- misty::chr.omit(var.i, omit = "!", check = FALSE)

        var.exclude <- c(var.exclude, colnames(data)[which(substr(colnames(data), start = nchar(colnames(data)) - nchar(var.i) + 1L, stop = nchar(colnames(data))) == var.i)])

        var.names[[i]] <- ""

      }

    }

  }

  #...................
  ### Contains a literal string: List elements with ~ ####

  var.tilde <- sapply(var.names, function(y) any(y == "~"))

  if (isTRUE(any(var.tilde))) {

    # List elements with ~
    for (i in which(var.tilde)) {

      # Variable names
      var.i <- misty::chr.omit(var.names[[i]], omit = "~", check = FALSE)

      # No complement !
      if (isTRUE(!"!" %in% var.i)) {

        var.names[[i]] <- grep(var.i, colnames(data), value = TRUE)

      # Complement !
      } else {

        var.exclude <- c(var.exclude, grep(misty::chr.omit(var.i, omit = "!", check = FALSE), colnames(data), value = TRUE))

        var.names[[i]] <- ""

      }

    }

  }

  #...................
  ### Consecutive variables: List elements with : ####

  var.colon <- sapply(var.names, function(y) any(y == ":"))

  if (isTRUE(any(var.colon))) {

    # List elements with :
    for (i in which(var.colon)) {

      # Variable names
      var.i <- misty::chr.omit(var.names[[i]], omit = ":", check = FALSE)

      check.var.i <- setdiff(misty::chr.omit(var.i, omit = "!", check = FALSE), colnames(data))
      if (isTRUE(length(check.var.i) != 0L)) {

        stop(paste0(ifelse(length(check.var.i) == 1L, "Variable name involved in the : operator was not found in 'data': ", "Variable names involved in the : operator were not found in 'data': "), paste0(check.var.i, collapse = ", ")), call. = FALSE)

      }

      # No complement !
      if (isTRUE(!"!" %in% var.i)) {

        var.names[[i]] <- colnames(data)[which(colnames(data) == var.i[1L]):which(colnames(data) == var.i[2L])]

      # Complement !
      } else {

        var.exclude <- c(var.exclude, colnames(data)[which(colnames(data) == var.i[2L]):which(colnames(data) == var.i[3L])])

        var.names[[i]] <- ""

      }

    }

  }

  #...................
  ### Numerical range: List elements with :: ####

  var.dcolon <- sapply(var.names, function(y) any(y == "::"))
  if (isTRUE(any(var.dcolon))) {

    # List elements with ::
    for (i in which(var.dcolon)) {

      # Variable names
      var.i <- misty::chr.omit(var.names[[i]], omit = "::", check = FALSE)

      var.j <- misty::chr.omit(var.i, omit = "!", check = FALSE)

      # Check if more than two variables were specified in the : operator
      if (isTRUE(length(misty::chr.omit(unlist(strsplit(var.j, ":")), omit = "", check = FALSE)) > 2L)) { stop("More than two variables specified in the : operator.", call. = FALSE) }

      # Split starting variable
      var.i1.split <- unlist(strsplit(var.j[1L], "", fixed = TRUE))
      # Split ending variable
      var.i2.split <- unlist(strsplit(var.j[2L], "", fixed = TRUE))

      # Numeric values starting variable
      var.i1.log <- sapply(var.i1.split, function(y) y %in% as.character(0:9))
      # Numeric values ending variable
      var.i2.log <- sapply(var.i2.split, function(y) y %in% as.character(0:9))

      # Variable name root starting variable
      var.i1.root <- paste0(var.i1.split[-((max(which(var.i1.log == FALSE)) + 1L):length(var.i1.split))], collapse = "")
      # Variable name root ending variable
      var.i2.root <- paste0(var.i2.split[-((max(which(var.i2.log == FALSE)) + 1L):length(var.i2.split))], collapse = "")

      # Check if variable names match
      if (var.i1.root != var.i2.root) { stop(paste0("Variable names involvd in the :: operator do not match: ", paste(var.i1.root, "vs.", var.i2.root)), call. = FALSE) }

      # No complement !
      if (isTRUE(!"!" %in% var.i)) {

        var.names[[i]] <- paste0(var.i1.root,
                                 as.numeric(paste(var.i1.split[(max(which(var.i1.log == FALSE)) + 1L):length(var.i1.split)], collapse = "")):
                                 as.numeric(paste(var.i2.split[(max(which(var.i2.log == FALSE)) + 1L):length(var.i2.split)], collapse = "")))

      } else {

        var.exclude <- c(var.exclude,
                         paste0(var.i1.root,
                                as.numeric(paste(var.i1.split[(max(which(var.i1.log == FALSE)) + 1L):length(var.i1.split)], collapse = "")):
                                as.numeric(paste(var.i2.split[(max(which(var.i2.log == FALSE)) + 1L):length(var.i2.split)], collapse = ""))))

        var.names[[i]] <- ""

      }

    }

  }

  #...................
  ### List elements with ! ####

  var.exclude <- c(var.exclude, unlist(sapply(var.names, function(y) if (isTRUE(any(y == "!"))) { misty::chr.omit(y, omit = "!", check = FALSE) } )))

  var.names <- var.names[which(sapply(var.names, function(y) !"!" %in% y))]

  #...................
  ### Remove "list" and "" elements ####

  var.names <- misty::chr.omit(var.names, omit = c("list", ""), check = FALSE)

  #...................
  ### Unique element ####

  var.names <- unique(var.names)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Select all Variables ####

  if (isTRUE(any(var.names == "."))) { var.names <- colnames(data) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split, Cluster and Other Variables ####

  #...................
  ### Exclude grouping variable ####
  if (isTRUE(!is.null(check.chr) && !is.null(group))) {

    if (isTRUE(!is.character(group) || length(group) != 1L)) { stop("Please specify a character string for the argument 'group'.", call. = FALSE) }
    if (isTRUE(!group %in% colnames(data))) { stop("Grouping variable specifed in 'group' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, group)

  }

  #...................
  ### Exclude split variable ####
  if (isTRUE(!is.null(check.chr) && !is.null(split))) {

    if (isTRUE(!is.character(split) || length(split) != 1L)) { stop("Please specify a character string for the argument 'split'.", call. = FALSE) }
    if (isTRUE(!split %in% colnames(data))) { stop("Split variable specifed in 'split' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, split)

  }

  #...................
  ### Exclude cluster variable ####
  if (isTRUE(!is.null(check.chr) && !is.null(cluster))) {

    if (isTRUE(!is.character(cluster) || !length(cluster) %in% c(1L, 2L))) { stop("Please specify a character vector for the argument 'cluster'.", call. = FALSE) }

    ##### One cluster variable
    if (isTRUE(length(cluster) == 1L)) {

      if (isTRUE(!cluster %in% colnames(data))) { stop("Cluster variable specifed in 'cluster' was not found in 'data'.", call. = FALSE) }

    ##### Two cluster variables
    } else {

      # Cluster variable in 'data'
      check.cluster <- !cluster %in% colnames(data)
      if (isTRUE(any(check.cluster))) {

        if (isTRUE(sum(check.cluster) == 1L)) {

          stop(paste0("Cluster variable specifed in 'cluster' was not found in 'data': ", cluster[which(check.cluster)]),  call. = FALSE)

        } else {

          stop("Cluster variables specifed in 'cluster' were not found in 'data'.",  call. = FALSE)

        }

      }

      # Order of cluster variables
      check.cluster <- suppressWarnings(tapply(data[, cluster[2L]], data[, cluster[1L]], var, na.rm = TRUE))
      if (isTRUE(all(check.cluster == 0) || all(is.na(check.cluster)))) { stop("Please specify the Level 3 cluster variable first, e.g., cluster = c(\"level3\", \"level2\").", call. = FALSE) }

    }

    var.names <- setdiff(var.names, cluster)

  }

  #...................
  ### Exclude id variable ####
  if (isTRUE(!is.null(check.chr) && !is.null(id))) {

    if (isTRUE(!is.character(id) || length(id) != 1L)) { stop("Please specify a character string for the argument 'id'.", call. = FALSE) }
    if (isTRUE(!id %in% colnames(data))) { stop("Split variable specifed in 'id' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, id)

  }

  #...................
  ### Exclude obs variable ####
  if (isTRUE(!is.null(check.chr) && !is.null(obs))) {

    if (isTRUE(!is.character(obs) || length(obs) != 1L)) { stop("Please specify a character string for the argument 'obs'.", call. = FALSE) }
    if (isTRUE(!id %in% colnames(data))) { stop("Split variable specifed in 'obs' was not found in 'data'.", call. = FALSE) }

    check.obs.dupli <- sapply(split(obs, id), function(x) length(x) != length(unique(x)))
    if (isTRUE(any(check.obs.dupli))) { stop("There are duplicated observations specified in 'obs' within subjects specified in 'id'.", call. = FALSE) }

    var.names <- setdiff(var.names, obs )

  }

  #...................
  ### Exclude day variable ####
  if (isTRUE(!is.null(check.chr) && !is.null(day))) {

    if (isTRUE(!is.character(day) || length(day) != 1L)) { stop("Please specify a character string for the argument 'day'.", call. = FALSE) }
    if (isTRUE(!id %in% colnames(data))) { stop("Split variable specifed in 'day' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, day)

  }

  #...................
  ### Exclude time variable ####
  if (isTRUE(!is.null(check.chr) && !is.null(time))) {

    if (isTRUE(!is.character(time) || length(time) != 1L)) { stop("Please specify a character string for the argument 'time'.", call. = FALSE) }
    if (isTRUE(!id %in% colnames(data))) { stop("Split variable specifed in 'time' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, time)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Variables ####

  if (isTRUE(!is.null(var.exclude))) { var.names <- setdiff(var.names, var.exclude) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check if Variables in '...' are Available in 'data' ####

  if (isTRUE(!is.null(data) && !is.null(check.chr))) {

    var.diff <- setdiff(var.names, colnames(data))
    if (isTRUE(length(var.diff) != 0L)) {

      if (isTRUE(any(c("$", "[", "subset(", "df.subset(") %in% var.names))) {

        stop(paste0("Please do not specify the argument 'data' when specifying ", check.chr, " for the argument '...'."), call. = FALSE)

      } else {

        stop(paste0(ifelse(length(var.diff) == 1L, "Variable specified in '...' was not found in 'data': ", "Variables specified in '...' were not found in 'data': "), paste(var.diff, collapse = ", ")), call. = FALSE)

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return Object ####

  return(var.names)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Extract Grouping, Split, or Cluster Variable ---------------------------------

.var.group <- function(data, group = NULL, split = NULL, cluster = NULL, id = NULL,
                       obs = NULL, day = NULL, time = NULL, drop = TRUE) {

  # Grouping, split, or cluster variable specified with the variable name
  group.chr <- split.chr <- cluster.chr <- id.chr <- obs.chr <- day.chr <- time.chr <- FALSE

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping Variable ####

  if (isTRUE(!is.null(group))) {

    #...................
    ### Grouping Variable Specified with the Variable Name ####

    group.chr <- is.character(group)
    if (isTRUE(group.chr && (length(group) < nrow(data)))) {

      ##### Check if one grouping variable ####
      if (isTRUE(length(group) > 1L)) { stop("Please specify one grouping variable for the argument 'group'.", call. = FALSE) }

      ##### Check if grouping variable in 'data' ####
      if (isTRUE(any(!group %in% colnames(data)))) { stop("Grouping variable specifed in 'group' was not found in '...'.", call. = FALSE) }

      ##### Extract 'data' and 'group' ####

      # Index of grouping variable in 'data'
      group.col <- which(colnames(data) == group)

      # Replace variable name with grouping variable
      group <- data[, group.col]

      # Remove grouping variable from 'data'
      data <- data[, -group.col, drop = drop]

    #...................
    ### Grouping Variable Not Specified with the Variable Name ####

    } else {

      ##### Check if lenght of grouping variable matching with the number of rows in 'data'
      if (isTRUE(nrow(as.data.frame(group)) != nrow(as.data.frame(data)))) { stop("Grouping variable specified in 'group' does not match with the number of rows in '...'.", call. = FALSE) }

    }

    ##### Check if grouping variable is completely missing
    if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

    ##### Check if only one group represented in the grouping variable
    if (isTRUE(length(unique(na.omit(group))) == 1L)) { stop("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Split Variable ####

  if (isTRUE(!is.null(split))) {

    #...................
    ### Split Variable Specified with the Variable Name ####

    split.chr <- is.character(split)
    if (isTRUE(split.chr && (length(split) < nrow(data)))) {

      ##### Check if one split variable ####
      if (isTRUE(length(split) > 1L)) { stop("Please specify one split variable for the argument 'split'.", call. = FALSE) }

      ##### Check if split variable in 'data' ####
      if (isTRUE(any(!split %in% colnames(data)))) { stop("Split variable specifed in 'split' was not found in '...'.", call. = FALSE) }

      ##### Extract 'data' and 'split' ####

      # Index of split variable in 'data'
      split.col <- which(colnames(data) == split)

      # Replace variable name with split variable
      split <- data[, split.col]

      # Remove split variable from 'data'
      data <- data[, -split.col, drop = drop]

    #...................
    ### Split Variable Not Specified with the Variable Name ####

    } else {

      ##### Check if lenght of split variable matching with the number of rows in 'data'
      if (isTRUE(nrow(as.data.frame(split)) != nrow(as.data.frame(data)))) { stop("Split variable specified in 'split' does not match with the number of rows in '...'.", call. = FALSE) }

    }

    ##### Check if split variable is completely missing
    if (isTRUE(all(is.na(split)))) { stop("The split variable specified in 'split' is completely missing.", call. = FALSE) }

    ##### Check if only one group represented in the split variable
    if (isTRUE(length(unique(na.omit(split))) == 1L)) { stop("There is only one split represented in the grouping variable specified in 'split'.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Cluster Variable ####

  if (isTRUE(!is.null(cluster))) {

    #...................
    ### Cluster variable specified with the variable name ####

    cluster.chr <- is.character(cluster)
    if (isTRUE(cluster.chr && (length(cluster) < nrow(data)))) {

      ##### Check if one or two cluster variables ####
      if (isTRUE(length(cluster) > 2L)) { stop("Please specify one or two cluster variables for the argument 'cluster'.", call. = FALSE) }

      ##### Check if cluster variable in 'data' ####
      if (isTRUE(any(!cluster %in% colnames(data)))) {

        # One cluster variable
        if (isTRUE(length(cluster) == 1L)) {

          stop("Cluster variable specifed in 'cluster' was not found in '...'.", call. = FALSE)

        # Two cluster variables
        } else {

          # Cluster variable in 'data'
          cluster.diff <- setdiff(cluster, colnames(data))
          if (isTRUE(length(cluster.diff) == 1L)) {

            stop(paste0("Cluster variable \"", cluster.diff, "\" specifed in 'cluster' was not found in '...'."), call. = FALSE)

          } else {

            stop("Cluster variables specifed in 'cluster' were not found in '...'.", call. = FALSE)

          }

          # Order of cluster variables
          if (isTRUE(length(unique(data[, cluster[, 2L]])) < length(unique(data[, cluster[, 1L]])))) { stop("Please specify the Level 3 cluster variable first, e.g., cluster = c(\"level3\", \"level2\").", call. = FALSE) }

        }

      }

      ##### Extract 'data' and 'cluster' ####

      # One cluster variable
      if (isTRUE(length(cluster) == 1L)) {

        # Index of cluster variable in 'data'
        cluster.col <- which(colnames(data) %in% cluster)

        # Replace variable name with cluster variable
        cluster <- data[, cluster.col]

        # Remove cluster variable from 'data'
        data <- data[, -cluster.col, drop = drop]

      } else {

        # Index of Level-3 cluster variable in 'data'
        cluster3.col <- which(colnames(data) == cluster[1L])

        # Index of Level-2 cluster variable in 'data'
        cluster2.col <- which(colnames(data) == cluster[2L])

        # Replace variable name with cluster variable
        cluster <- data[, c(cluster3.col, cluster2.col)]

        # Remove cluster variable from 'data'
        data <- data[, -c(cluster3.col, cluster2.col), drop = drop]

      }

    #...................
    ### Cluster Variable Not Specified with the Variable Name ####

    } else {

      ##### Check if lenght of cluster variable matching with the number of rows in 'data'
      if (isTRUE(nrow(as.data.frame(cluster)) != nrow(as.data.frame(data)))) {

        stop("Cluster variables specified in 'cluster' do not match with the number of rows in '...'.", call. = FALSE)

      }

    }

    ##### Check if cluster variable is completely missing

    # One cluster variable
    if (isTRUE(ncol(as.data.frame(cluster)) == 1L)) {

      if (isTRUE(all(is.na(cluster)))) { stop("The cluster variable specified in 'cluster' is completely missing.", call. = FALSE) }

    # Two cluster variables
    } else {

      cluster.na <- sapply(cluster, function(y) all(is.na(y)))
      if (isTRUE(any(cluster.na))) {

        if (isTRUE(sum(cluster.na)) == 1L) {

          stop(paste0("A cluster variable specified in 'cluster' is completely missing.: ", names(which(cluster.na))), call. = FALSE)

        } else {

          stop("Cluster variables specified in 'cluster' are completely missing.", call. = FALSE)

        }

      }

    }

    ##### Check if only one group represented in the cluster variable

    # One cluster variable
    if (isTRUE(ncol(as.data.frame(cluster)) == 1L)) {

      if (isTRUE(length(unique(na.omit(unlist(cluster)))) == 1L)) { stop("There is only one group represented in the cluster variable 'cluster'.", call. = FALSE) }

    # Two cluster variables
    } else {

      cluster.unique <- sapply(cluster, function(y) length(unique(na.omit(y))) == 1L)
      if (isTRUE(any(cluster.unique))) {

        if (isTRUE(sum(cluster.unique)) == 1L) {

          stop(paste0("There is only one group represented in a cluster variable specified in 'cluster': ", names(which(cluster.unique))), call. = FALSE)

        } else {

          stop("There is only one group represented in both cluster variables specified in 'cluster'.", call. = FALSE)

        }

      }

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Subject ID Variable ####

  if (isTRUE(!is.null(id))) {

    #...................
    ### Subject ID Variable Specified with the Variable Name ####

    id.chr <- is.character(id)
    if (isTRUE(id.chr && (length(id) < nrow(data)))) {

      ##### Check if one ID variable ####
      if (isTRUE(length(id) > 1L)) { stop("Please specify one id variable for the argument 'id'.", call. = FALSE) }

      ##### Check if ID variable in 'data' ####
      if (isTRUE(any(!id %in% colnames(data)))) { stop("ID variable specifed in 'id' was not found in '...'.", call. = FALSE) }

      ##### Extract 'data' and 'id' ####

      # Index of ID variable in 'data'
      id.col <- which(colnames(data) == id)

      # Replace variable name with id variable
      id <- data[, id.col]

      # Remove id variable from 'data'
      data <- data[, -id.col, drop = drop]

    #...................
    ### ID Variable Not Specified with the Variable Name ####

    } else {

      ##### Check if lenght of id variable matching with the number of rows in 'data'
      if (isTRUE(nrow(as.data.frame(id)) != nrow(as.data.frame(data)))) { stop("ID variable specified in 'id' does not match with the number of rows in '...'.", call. = FALSE) }

    }

    ##### Check if ID variable is completely missing
    if (isTRUE(all(is.na(id)))) { stop("The ID variable specified in 'id' is completely missing.", call. = FALSE) }

    ##### Check if only one ID represented in the ID variable
    if (isTRUE(length(unique(na.omit(id))) == 1L)) { stop("There is only one ID represented in the ID variable specified in 'id'.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Observation Number Variable ####

  if (isTRUE(!is.null(obs))) {

    #...................
    ### Observation Number Variable Specified with the Variable Name ####

    obs.chr <- is.character(obs)
    if (isTRUE(obs.chr && (length(obs) < nrow(data)))) {

      ##### Check if one observation number variable ####
      if (isTRUE(length(obs) > 1L)) { stop("Please specify one observation number variable for the argument 'obs'.", call. = FALSE) }

      ##### Check if observation number variable in 'data' ####
      if (isTRUE(any(!obs %in% colnames(data)))) { stop("Observation number variable specifed in 'obs' was not found in '...'.", call. = FALSE) }

      ##### Extract 'data' and 'obs' ####

      # Index of observation number variable in 'data'
      obs.col <- which(colnames(data) == obs)

      # Replace variable name with observation number variable
      obs <- data[, obs.col]

      # Remove observation number variable from 'data'
      data <- data[, -obs.col, drop = drop]

    #...................
    ### Observation number variable not specified with the variable name ####

    } else {

      ##### Check if lenght of observation number variable matching with the number of rows in 'data'
      if (isTRUE(nrow(as.data.frame(obs)) != nrow(as.data.frame(data)))) { stop("ObservationnNumber variable specified in 'obs' does not match with the number of rows in '...'.", call. = FALSE) }

    }

    ##### Check if observation number variable is completely missing
    if (isTRUE(all(is.na(obs)))) { stop("The observation number variable specified in 'obs' is completely missing.", call. = FALSE) }

    ##### Check if duplicated obs within id
    check.obs.dupli <- sapply(split(obs, id), function(x) length(x) != length(unique(x)))
    if (isTRUE(any(check.obs.dupli))) { stop("There are duplicated observations specified in 'obs' within subjects specified in 'id'.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Day Number Variable ####

  if (isTRUE(!is.null(day))) {

    #...................
    ### Day Number Variable Specified with the Variable Name ####

    day.chr <- is.character(day)
    if (isTRUE(day.chr && (length(day) < nrow(data)))) {

      ##### Check if one day variable ####
      if (isTRUE(length(day) > 1L)) { stop("Please specify one day number variable for the argument 'day'.", call. = FALSE) }

      ##### Check if day number variable in 'data' ####
      if (isTRUE(any(!day %in% colnames(data)))) { stop("Day number variable specifed in 'day' was not found in '...'.", call. = FALSE) }

      ##### Extract 'data' and 'day' ####

      # Index of day number variable in 'data'
      day.col <- which(colnames(data) == day)

      # Replace variable name with day number variable
      day <- data[, day.col]

      # Remove day number variable from 'data'
      data <- data[, -day.col, drop = drop]

    #...................
    ### Day Number Variable Not Specified with the Variable Name ####

    } else {

      ##### Check if lenght of day number variable matching with the number of rows in 'data'
      if (isTRUE(nrow(as.data.frame(day)) != nrow(as.data.frame(data)))) { stop("Day number variable specified in 'day' does not match with the number of rows in '...'.", call. = FALSE) }

    }

    ##### Check if day number variable is completely missing
    if (isTRUE(all(is.na(day)))) { stop("The day variable specified in 'day' is completely missing.", call. = FALSE) }

    ##### Check if only one group represented in the day number variable
    if (isTRUE(length(unique(na.omit(day))) == 1L)) { stop("There is only one day number represented in the grouping variable specified in 'day'.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Actual Date and Time Variable ####

  if (isTRUE(!is.null(time))) {

    #...................
    ### Actual Date and Time Variable Specified with the Variable Name ####

    time.chr <- is.character(time)
    if (isTRUE(time.chr && (length(time) < nrow(data)))) {

      ##### Check if one actual date and time variable ####
      if (isTRUE(length(time) > 1L)) { stop("Please specify one date and time variable for the argument 'time'.", call. = FALSE) }

      ##### Check if actual date and time variable in 'data' ####
      if (isTRUE(any(!time %in% colnames(data)))) { stop("Date and time variable specifed in 'time' was not found in '...'.", call. = FALSE) }

      ##### Extract 'data' and 'time' ####

      # Index of actual date and time variable in 'data'
      time.col <- which(colnames(data) == time)

      # Replace variable name with actual date and time variable
      time <- data[, time.col]

      # Remove actual date and time variable from 'data'
      data <- data[, -time.col, drop = drop]

    #...................
    ### Actual Date and Time Variable Not Specified with the Variable Name ####

    } else {

      ##### Check if lenght of actual date andtTime variable matching with the number of rows in 'data'
      if (isTRUE(nrow(as.data.frame(time)) != nrow(as.data.frame(data)))) { stop("Date and time variable specified in 'time' does not match with the number of rows in '...'.", call. = FALSE) }

    }

    ##### Check if actual date and time variable is completely missing
    if (isTRUE(all(is.na(time)))) { stop("The date and time variable specified in 'time' is completely missing.", call. = FALSE) }

    ##### Check if only one group represented in the actual date and time variable
    if (isTRUE(length(unique(na.omit(time))) == 1L)) { stop("There is only one date and time represented in the grouping variable specified in 'time'.", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return Object ####

  # Grouping, split, or cluster variable specified with the variable name
  if (isTRUE(any(c(group.chr, split.chr, cluster.chr, id.chr, obs.chr, day.chr, time.chr)))) {

    return(list(data = data, group = group, split = split, cluster = cluster,
                id = id, obs = obs, day = day, time = time))

  # Grouping, split, or cluster variable not specified with the variable name
  } else {

    return(list(data = NULL, group = NULL, split = NULL, cluster = NULL,
                id = NULL, obs = NULL, day = NULL, time = NULL))

  }

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Convert user-missing values into NA ------------------------------------------

.as.na <- function(x, na) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Replace user-specified values with NAs ####
  x <- misty::as.na(x, na = na, check = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Vector or factor ####

  if (isTRUE(is.null(dim(x)))) {

    # Check for missing values only
    if (isTRUE(all(is.na(x)))) { stop("After converting user-missing values into NA, 'x' is completely missing.", call. = FALSE) }

    # Check for zero variance
    if (isTRUE(length(na.omit(unique(x))) == 1L)) { stop("After converting user-missing values into NA, 'x' has zero variance.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Matrix or data frame ####

  } else {

    # Check for variables with missing values only
    x.na.all <- vapply(as.data.frame(x), function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.na.all))) {

      if (isTRUE(sum(x.na.all) == 1L)) {

        stop(paste0("After converting user-missing values into NA, following variable has zero variance: ", names(which(x.na.all))), call. = FALSE)

      } else {

        stop(paste0("After converting user-missing values into NA, following variables have zero variance: ", paste(names(which(x.na.all)), collapse = ", ")), call. = FALSE)

      }

    }

    # Check for variables with zero variance
    x.zero.var <- vapply(as.data.frame(x), function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1L))
    if (isTRUE(any(x.zero.var))) {

      if (isTRUE(sum(x.zero.var) == 1L)) {

        stop(paste0("After converting user-missing values into NA, following variable has zero variance: ", names(which(x.zero.var))), call. = FALSE)

      } else {

        stop(paste0("After converting user-missing values into NA, following variables have zero variance: ", paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

      }

    }

  }

  return(x)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the coding() function ---------------------------------
#
# - .contr.sum
# - .contr.wec
# - .contr.repeat
# - .forward.helmert
# - .reverse.helmert
#
# wec: wec: Weighted Effect Coding
# https://cran.r-project.org/web/packages/wec/index.html
#
# MASS: Support Functions and Datasets for Venables and Ripley's MASS
# https://cran.r-project.org/web/packages/MASS/index.html
#
# codingMatrices: Alternative Factor Coding Matrices for Linear Model Formulae
# https://cran.r-project.org/web/packages/codingMatrices/index.html
#
# faux: Simulation for Factorial Designs
# https://cran.r-project.org/web/packages/faux/index.html

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified contr.sum Function from the stats Package ####

.contr.sum <- function(n, omitted) {
  cont <- structure(diag(1L, length(n), length(n)), dimnames = list(n, n))
  cont <- cont[, -omitted, drop = FALSE]
  cont[omitted, ] <- -1L
  return(cont)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified contr.wec Function from the wec Package ####

.contr.wec <- function (x, omitted) {
  frequ <- table(x)
  omitted <- which(names(frequ) == omitted)
  cont <- contr.treatment(length(frequ), base = omitted)
  cont[omitted, ] <- -1L * frequ[-omitted]/frequ[omitted]
  colnames(cont) <- names(frequ[-omitted])
  return(cont)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified contr.sdif Function from the MASS Package ####

.contr.repeat <- function(n) {
  n.length <- length(n)
  cont <- col(matrix(nrow = n.length, ncol = n.length - 1L))
  upper.tri <- !lower.tri(cont)
  cont[upper.tri] <- cont[upper.tri] - n.length
  cont <- structure(cont/n.length, dimnames = list(n, n[-1L]))
  return(cont)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified code_helmert_forward Function from the codingMatrices Package ####

.forward.helmert <- function(n) {
  n.length <- length(n)
  cont <- rbind(diag(n.length:2L - 1L), 0)
  cont[lower.tri(cont)] <- -1L
  cont <- cont/rep(n.length:2L, each = n.length)
  dimnames(cont) <- list(n, n[1L:ncol(cont)])
  return(cont)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified contr_code_helmert Function from the faux Package ####

.reverse.helmert <- function(n) {
  n.length <- length(n)
  cont <- contr.helmert(n.length)
  for (i in 1L:(n.length - 1L)) {
    cont[, i] <- cont[, i]/(i + 1L)
  }
  comparison <- lapply(1L:(n.length - 1L), function(y) {
    paste(n[1L:y], collapse = ".")
  })
  dimnames(cont) <- list(n, n[-1L])
  return(cont)
}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the cor.matrix() function -----------------------------
#
# - .internal.tau.c
# - .internal.cor.test.pearson
# - .internal.cor.test.spearman
# - .internal.cor.test.kendall.b
# - .internal.polychoric

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .internal.tau.c Function ####

.internal.tau.c <- function(xx, yy) {

  # Contingency table
  x.table <- table(xx, yy)

  # Number of rows
  x.nrow <- nrow(x.table)

  # Number of columns
  x.ncol <- ncol(x.table)

  # Sample size
  x.n <- sum(x.table)

  # Minimum of number of rows/columns
  x.m <- min(dim(x.table))

  if (isTRUE(x.n > 1L && x.nrow > 1L && x.ncol > 1L)) {

    pi.c <- pi.d <- matrix(0L, nrow = x.nrow, ncol = x.ncol)

    x.col <- col(x.table)
    x.row <- row(x.table)

    for (i in 1L:x.nrow) {

      for (j in 1L:x.ncol) {

        pi.c[i, j] <- sum(x.table[x.row < i & x.col < j]) + sum(x.table[x.row > i & x.col > j])
        pi.d[i, j] <- sum(x.table[x.row < i & x.col > j]) + sum(x.table[x.row > i & x.col < j])

      }

    }

    # Concordant
    x.con <- sum(pi.c * x.table)/2L

    # Discordant
    x.dis <- sum(pi.d * x.table)/2L

    # Kendall-Stuart Tau-c
    tau.c <- (x.m*2L * (x.con - x.dis)) / ((x.n^2L) * (x.m - 1L))

  } else {

    tau.c <- NA

  }

  #-----------------------------------------#
  # If n > 2
  if (isTRUE(x.n > 2L & x.nrow > 1L & x.ncol > 1L)) {

    # Asymptotic standard error
    sigma <- sqrt(4L * x.m^2L / ((x.m - 1L)^2L * x.n^4L) * (sum(x.table * (pi.c - pi.d)^2L) - 4L * (x.con - x.dis)^2L / x.n))

    # Test statistic
    z <- tau.c / sigma

    # Two-tailed p-value
    pval <- pnorm(abs(z), lower.tail = FALSE)*2L

  } else {

    sigma <- NA
    z <- NA
    pval <- NA

  }

  object <- list(result = list(tau.c = tau.c, n = x.n, sigma = sigma, stat = z, df = NA, pval = pval))

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .internal.cor.test.pearson Function ####

.internal.cor.test.pearson <- function(x, y) {

  # At least three cases
  if (isTRUE(nrow(na.omit(data.frame(x = x, y = y))) >= 3L)) {

    result.cor.test <- suppressWarnings(cor.test(x, y, method = "pearson"))

    object <- list(stat = result.cor.test$statistic, df = result.cor.test$parameter, pval = result.cor.test$p.value)

  # Less than three cases
  } else {

    object <- list(stat = NA, df = NA, pval = NA)

  }

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .internal.cor.test.spearman Function ####

.internal.cor.test.spearman <- function(x, y, continuity) {

  # Complete data
  xy.dat <- na.omit(data.frame(x = x, y = y))

  # Number of cases
  n <- nrow(xy.dat)

  # At least three cases
  if (isTRUE(n >= 3L)) {

    # Correlation coefficient
    r <- cor(xy.dat[, c("x", "y")], method = "spearman")[1L, 2L]

    # Continuity correction
    if (isTRUE(continuity)) { r <- 1L - ((n^3L - n) * (1L - r) / 6L) / (((n * (n^2L - 1)) / 6L) + 1L) }

    # Test statistic
    stat <- r * sqrt((n - 2L) / (1L - r^2L))

    # Degrees of freedom
    df <- n - 2L

    # p-value
    pval <- min(pt(stat, df = df), pt(stat, df = df, lower.tail = FALSE))*2L

    # Return object
    object <- list(stat = stat, df = df, pval = pval)

  # Less than three cases
  } else {

    # Return object
    object <- list(stat = NA, df = NA, pval = NA)

  }

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .internal.cor.test.kendall.b Function ####

.internal.cor.test.kendall.b <- function(x, y, continuity) {

  # Complete data
  xy.dat <- na.omit(data.frame(x = x, y = y))

  # Number of cases
  n <- nrow(xy.dat)

  # At least three cases
  if (isTRUE(n >= 3L)) {

    # Correlation coefficient
    r <- cor(xy.dat[, c("x","y")], method = "kendall")[1L, 2L]

    xties <- table(x[duplicated(x)]) + 1L
    yties <- table(y[duplicated(y)]) + 1L
    T0 <- n * (n - 1L)/2L
    T1 <- sum(xties * (xties - 1L))/2L
    T2 <- sum(yties * (yties - 1L))/2L
    S <- r * sqrt((T0 - T1) * (T0 - T2))
    v0 <- n * (n - 1L) * (2L * n + 5L)
    vt <- sum(xties * (xties - 1L) * (2L * xties + 5L))
    vu <- sum(yties * (yties - 1L) * (2L * yties + 5L))
    v1 <- sum(xties * (xties - 1L)) * sum(yties * (yties - 1L))
    v2 <- sum(xties * (xties - 1L) * (xties - 2)) * sum(yties * (yties - 1L) * (yties - 2L))

    var_S <- (v0 - vt - vu) / 18L + v1 / (2L * n * (n - 1L)) + v2 / (9L * n * (n - 1L) * (n - 2L))

    # Continuity correction
    if (isTRUE(continuity)) { S <- sign(S) * (abs(S) - 1L) }

    # Test statistic
    stat <- S / sqrt(var_S)

    # p-value
    pval <-  min(pnorm(stat), pnorm(stat, lower.tail = FALSE))*2L

    object <- list(stat = stat, df = NA, pval = pval)

  # Less than three cases
  } else {

    object <- list(stat = NA, df = NA, pval = NA)
  }

  return(object)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .internal.polychoric Function ####

.internal.polychoric <- function(x, smooth = TRUE, global = TRUE, weight = NULL, correct = 0,
                                 progress = FALSE, na.rm = TRUE, delete = TRUE) {

  #...................
  ### cor.smooth ####

  cor.smooth <- function (x, eig.tol = 10^-12) {

    eigens <- try(eigen(x), TRUE)

    if (isTRUE(class(eigens) == as.character("try-error"))) {

      warning("There is something wrong with the correlation matrix, i.e., cor.smooth() failed to smooth it because some of the eigenvalues are NA.", call. = FALSE)

    } else {

      if (isTRUE(min(eigens$values) < .Machine$double.eps)) {

        warning("Matrix was not positive definite, smoothing was done.", call. = FALSE)

        eigens$values[eigens$values < eig.tol] <- 100L * eig.tol
        nvar <- dim(x)[1L]
        tot <- sum(eigens$values)
        eigens$values <- eigens$values * nvar/tot
        cnames <- colnames(x)
        rnames <- rownames(x)
        x <- eigens$vectors %*% diag(eigens$values) %*% t(eigens$vectors)
        x <- cov2cor(x)
        colnames(x) <- cnames
        rownames(x) <- rnames

      }

    }

    return(x)

  }

  #...................
  ### mcmapply ####

  mcmapply <- function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE,
                        mc.preschedule = TRUE, mc.set.seed = TRUE, mc.silent = FALSE,
                        mc.cores = 1L, mc.cleanup = TRUE, affinity.list = NULL) {

    cores <- as.integer(mc.cores)

    if (isTRUE(cores < 1L)) {

      stop("'mc.cores' must be >= 1", call. = FALSE)

    }

    if (isTRUE(cores > 1L)) {

      stop("'mc.cores' > 1 is not supported on Windows", call. = FALSE)

    }

    mapply(FUN = FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,
           USE.NAMES = USE.NAMES)

  }

  #...................
  ### tableF ####

  tableF <- function(x,y) {

    minx <- min(x,na.rm = TRUE)
    maxx <- max(x,na.rm = TRUE)
    miny <- min(y,na.rm = TRUE)
    maxy <- max(y,na.rm = TRUE)
    maxxy <- (maxx+(minx == 0L))*(maxy + (miny == 0L))
    dims <- c(maxx + 1L - min(1L, minx), maxy + 1L - min(1L, minx))
    bin <- x - minx + (y - miny)*(dims[1L]) + max(1L, minx)
    ans <- matrix(tabulate(bin, maxxy), dims)

    return(ans)
  }

  #...................
  ### tableFast ####

  tableFast <- function(x, y, minx, maxx, miny, maxy) {

    maxxy <- (maxx + (minx == 0L))*(maxy + (minx == 0L))
    bin <- x-minx + (y - minx) *maxx + 1
    dims <- c(maxx + 1L - min(1L, minx), maxy + 1L - min(1L, miny))
    ans <- matrix(tabulate(bin, maxxy), dims)

    return(ans)

  }

  #...................
  ### polyBinBvn ####

  polyBinBvn <- function(rho, rc, cc) {

    row.cuts <- c(-Inf, rc,Inf)
    col.cuts <- c(-Inf, cc, Inf)
    nr <- length(row.cuts) - 1L
    nc <- length(col.cuts) - 1L

    P <- matrix(0L, nr, nc)
    R <- matrix(c(1L, rho, rho,1), 2L, 2L)

    for (i in seq_len((nr - 1L))) {

      for (j in seq_len((nc - 1L))) {

        P[i, j] <- mnormt::sadmvn(lower = c(row.cuts[i], col.cuts[j]),
                                  upper = c(row.cuts[i + 1L], col.cuts[j + 1L]), mean = rep(0L, 2L),
                                  varcov = R)
      }

    }

    P[1L, nc] <- pnorm(rc[1L]) - sum(P[1L, seq_len((nc - 1L))])
    P[nr, 1L] <- pnorm(cc[1L]) - sum(P[seq_len((nr - 1L)), 1L])
    if (isTRUE(nr >2L)) { for (i in (2L:(nr - 1L))) {P[i, nc] <- pnorm(rc[i]) -pnorm(rc[i - 1L])- sum(P[i, seq_len((nc - 1L))]) }}
    if (isTRUE(nc >2L)) { for (j in (2L:(nc - 1L))) {P[nr, j] <- pnorm(cc[j]) - pnorm(cc[j - 1L])-sum(P[seq_len((nr - 1L)), j]) }}
    if (isTRUE(nc > 1L))  P[nr, nc] <- 1L - pnorm(rc[nr - 1L]) - sum(P[nr, seq_len((nc - 1L))])
    P

  }

  #...................
  ### polyF ####

  polyF <- function(rho, rc, cc, tab) {

    P <- polyBinBvn(rho, rc, cc)
    P[P <= 0L] <- NA
    lP <- log(P)
    lP[lP == -Inf] <- NA
    lP[lP == Inf] <- NA
    -sum(tab * lP, na.rm = TRUE)  }

  #...................
  ### wtd.table ####

  wtd.table <- function(x, y, weight) {

    tab <- tapply(weight, list(x, y), sum, na.rm = TRUE, simplify = TRUE)
    tab[is.na(tab)] <- 0L

    return(tab)

  }

  #...................
  ### polyc ####

  polyc <- function(x, y = NULL, taux, tauy, global = TRUE, weight = NULL, correct = correct,
                    gminx, gmaxx, gminy, gmaxy) {

    if (is.null(weight)) {

      tab <- tableFast(x, y, gminx, gmaxx, gminy, gmaxy)

    }  else {

      tab <- wtd.table(x,y,weight)

    }

    fixed <- 0L
    tot <- sum(tab)
    if (isTRUE(tot == 0L)) {

      result <- list(rho = NA, objective = NA, fixed = 1L)

      return(result)

    }

    tab <- tab/tot

    if (isTRUE(correct > 0L)) {

      if (isTRUE(any(tab[] == 0L))) {

        fixed <- 1L
        tab[tab == 0L] <- correct/tot

      }

    }

    if (isTRUE(global)) {

      rho <- optimize(polyF, interval = c(-1L, 1L), rc = taux, cc = tauy, tab)

    } else {

      if (isTRUE(!is.na(sum(tab))))  {

        zerorows <- apply(tab, 1L, function(x) all(x == 0L))
        zerocols <- apply(tab, 2L, function(x) all(x == 0L))
        zr <- sum(zerorows)
        zc <- sum(zerocols)
        tab <- tab[!zerorows, , drop = FALSE]
        tab <- tab[, !zerocols, drop = FALSE]
        csum <- colSums(tab)
        rsum <- rowSums(tab)

        if (isTRUE(min(dim(tab)) < 2L)) {

          rho <- list(objective = NA)

        } else {

          cc <-  qnorm(cumsum(csum)[-length(csum)])
          rc <-  qnorm(cumsum(rsum)[-length(rsum)])
          rho <- optimize(polyF, interval = c(-1L, 1L), rc = rc, cc = cc, tab)

        }

      } else {

        rho <- list(objective = NA, rho= NA)

      }

    }

    if (isTRUE(is.na(rho$objective))) {

      result <- list(rho = NA, objective = NA, fixed = fixed)

    } else {

      result <- list(rho=rho$minimum, objective=rho$objective, fixed = fixed)

    }

    return(result)

  }

  #...................
  ### polydi ####

  polydi <- function(p, d, taup, taud, global = TRUE, ML = FALSE, std.err = FALSE, weight = NULL,
                     progress = TRUE, na.rm = TRUE, delete = TRUE, correct = 0.5) {

    myfun <- function(x, i, j, correct, taup, taud, gminx, gmaxx, gminy, gmaxy, np) {

      polyc(x[, i], x[, j], taup[, i], taud[1L, (j - np)], global = global, weight = weight,
            correct = correct, gminx = gminx, gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy)

    }

    matpLower <- function(x, np, nd, taup, taud, gminx, gmaxx, gminy, gmaxy) {

      k <- 1
      il <- vector()
      jl <- vector()
      for(i in seq_len(np)) {

        for (j in seq_len(nd)) {

          il[k] <- i
          jl [k] <- j
          k <- k + 1

        }

      }

      poly <- mcmapply(function(i, j) myfun(x, i, j, correct = correct,
                                            taup = taup, taud = taud, gminx = gminx, gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy, np = np), il, jl + np)

      mat <- matrix(np,nd)
      mat <- as.numeric(poly[1L, ])

      return(mat)

    }

    if (isTRUE(!is.null(weight))) {

      if (isTRUE(length(weight) != nrow(x))) {

        stop("Length of the weight vector must match the number of cases.", call. = FALSE)

      }
    }

    cl <- match.call()
    np <- dim(p)[2L]
    nd <- dim(d)[2L]

    if (isTRUE(is.null(np))) np <- 1L
    if (isTRUE(is.null(nd))) nd <- 1L

    nsub <- dim(p)[1L]
    p <- as.matrix(p)
    d <- as.matrix(d)
    nvalues <- max(p, na.rm = TRUE) - min(p, na.rm = TRUE) + 1L
    dmin <- apply(d, 2L, function(x) min(x, na.rm = TRUE))
    dmax <- apply(d, 2L, function(x) max(x, na.rm = TRUE))
    dvalues <- max(dmax - dmin)

    if (isTRUE(dvalues != 1L)) stop("You did not supply a dichotomous variable.", call. = FALSE)

    if (isTRUE(nvalues > 8L)) stop("You have more than 8 categories for your items, polychoric is probably not needed.", call. = FALSE)

    item.var <- apply(p, 2L, sd, na.rm = na.rm)
    bad <- which((item.var <= 0L) | is.na(item.var))

    if (isTRUE(length(bad) > 0L && delete)) {

      for (baddy in seq_len(length(bad))) {

        message("Item = ", colnames(p)[bad][baddy], " had no variance and was deleted")

      }

      p <- p[, -bad]
      np <- np - length(bad)

    }

    pmin <- apply(p, 2, function(x) min(x, na.rm = TRUE))
    minx <- min(pmin)
    p <- t(t(p) - pmin + 1L)

    miny <- min(dmin)
    d <-  t(t(d) - dmin + 1L)
    gminx <- gminy <- 1L

    pmax <- apply(p,2,function(x)  max(x,na.rm = TRUE))
    gmaxx <- max(pmax)

    if (isTRUE(min(pmax) != max(pmax))) { global <- FALSE
    warning("The items do not have an equal number of response alternatives, setting global to FALSE.", call. = FALSE)}

    gmaxy <- max(apply(d, 2L, function(x) max(x, na.rm = TRUE)))
    pfreq <- apply(p, 2L, tabulate, nbins = nvalues)
    n.obs <- colSums(pfreq)
    pfreq <- t(t(pfreq)/n.obs)

    taup <- as.matrix(qnorm(apply(pfreq, 2L, cumsum))[seq_len(nvalues - 1L), ], ncol = ncol(pfreq))

    rownames(taup) <- paste(seq_len(nvalues - 1L))
    colnames(taup) <- colnames(p)

    dfreq <- apply(d, 2L, tabulate, nbins = 2L)
    if (isTRUE(nd < 2L)) {

      n.obsd <- sum(dfreq)

    } else {

      n.obsd <- colSums(dfreq)

    }

    dfreq <- t(t(dfreq)/n.obsd)
    taud <-  qnorm(apply(dfreq, 2L, cumsum))

    mat <- matrix(0L, np, nd)
    rownames(mat) <- colnames(p)
    colnames(mat) <- colnames(d)

    x <- cbind(p,d)

    mat <- matpLower(x, np, nd, taup, taud, gminx, gmaxx, gminy, gmaxy)

    mat <- matrix(mat, np, nd, byrow = TRUE)
    rownames(mat) <- colnames(p)
    colnames(mat)  <- colnames(d)

    taud <- t(taud)
    result <- list(rho = mat,tau = taud, n.obs = nsub)

    class(result) <- c("psych","polydi")

    return(result)

  }

  #...................
  ### polytab ####

  polytab <- function(tab, correct = TRUE) {

    tot <- sum(tab)
    tab <- tab/tot
    if (isTRUE(correct > 0L)) tab[tab == 0L] <- correct/tot

    csum <- colSums(tab)
    rsum <- rowSums(tab)
    cc <-  qnorm(cumsum(csum[-length(csum)]))
    rc <-  qnorm(cumsum(rsum[-length(rsum)]))
    rho <- optimize(polyF, interval = c(-1L, 1L), rc = rc, cc = cc, tab)

    result <- list(rho = rho$minimum, objective = rho$objective, tau.row = rc, tau.col = cc)

    return(result)

  }

  #...................
  ### myfun ####

  myfun <- function(x, i, j, gminx, gmaxx, gminy, gmaxy) {

    polyc(x[, i], x[, j], tau[, i], tau[, j], global = global, weight = weight, correct = correct,
          gminx = gminx, gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy)

  }

  #...................
  ### matpLower ####

  matpLower <- function(x, nvar, gminx, gmaxx, gminy, gmaxy) {

    k <- 1L
    il <- vector()
    jl <- vector()
    for(i in 2L:nvar) {

      for (j in seq_len(i - 1L)) {

        il[k] <- i
        jl[k] <- j
        k <- k + 1L

      }

    }

    poly <- mcmapply(function(i, j) myfun(x, i, j, gminx = gminx, gmaxx = gmaxx, gminy = gminy, gmaxy = gmaxy), il, jl)

    mat <- diag(nvar)
    if (isTRUE(length(dim(poly)) == 2L)) {

      mat[upper.tri(mat)] <- as.numeric(poly[1L, ])
      mat <- t(mat) + mat
      fixed <- as.numeric(poly[3L, ])
      diag(mat) <- 1L
      fixed <- sum(fixed)

      if (isTRUE(fixed > 0L && correct > 0L)) {

        warning(fixed ," cell(s) adjusted for 0 values using the correction for continuity.", call. = FALSE)

      }

      return(mat)

    } else {

      warning("Something is wrong in polycor.", call. = FALSE)

      return(poly)

      stop("Something was seriously wrong. Please look at the results.", call. = FALSE)

    }

  }

  if (isTRUE(!is.null(weight))) {

    if (isTRUE(length(weight) !=nrow(x))) {

      stop("Length of the weight vector must match the number of cases", call. = FALSE)

    }

  }

  nvar <- dim(x)[2L]
  nsub <- dim(x)[1L]
  if (isTRUE((prod(dim(x)) == 4L) || is.table(x))) {

    result <- polytab(x, correct = correct)

  } else {

    x <- as.matrix(x)
    if (isTRUE(!is.numeric(x))) {

      x <- matrix(as.numeric(x), ncol = nvar)
      message("Non-numeric input converted to numeric.")

    }

    xt <- table(x)
    nvalues <- length(xt)
    maxx <- max(x, na.rm = TRUE)

    if (isTRUE(maxx > nvalues)) {

      xtvalues <- as.numeric(names(xt))
      for(i in seq_len(nvalues)) {

        x[x == xtvalues[i]] <- i

      }

    }

    nvalues <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 1L

    item.var <- apply(x, 2L, sd, na.rm = na.rm)
    bad <- which((item.var <= 0)|is.na(item.var))

    if (isTRUE(length(bad) > 0L && delete)) {

      for (baddy in seq_len(length(bad))) {

        message("Item = ", colnames(x)[bad][baddy], " had no variance and was deleted.")

      }

      x <- x[, -bad]
      nvar <- nvar - length(bad)

    }

    xmin <- apply(x, 2L, function(x) min(x, na.rm = TRUE))

    xmin <- min(xmin)
    x <- t(t(x) - xmin + 1L)

    gminx <- gminy <- 1L
    xmax <- apply(x, 2L, function(x) max(x, na.rm = TRUE))
    xmax <- max(xmax)
    gmaxx <- gmaxy <- xmax

    if (isTRUE(min(xmax) != max(xmax))) {

      global <- FALSE
      warning("Items do not have an equal number of response categories, global set to FALSE.", call. = FALSE)

    }

    xfreq <- apply(x, 2L, tabulate, nbins = nvalues)
    n.obs <- colSums(xfreq)
    xfreq <- t(t(xfreq) / n.obs)
    tau <- qnorm(apply(xfreq, 2L, cumsum))[seq_len(nvalues - 1L), ]

    if (isTRUE(!is.matrix(tau))) tau <- matrix(tau, ncol = nvar)

    rownames(tau) <- seq_len(nvalues - 1L)
    colnames(tau) <- colnames(x)

    mat <- matrix(0L, nvar, nvar)
    colnames(mat) <- rownames(mat) <- colnames(x)

    mat <- matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy)

    if (isTRUE(any(is.na(mat)))) {

      message("Some correlations are missing, smoothing turned off.")
      smooth <- FALSE

    }

    if (isTRUE(smooth)) {

      mat <- cor.smooth(mat)

    }

    colnames(mat) <- rownames(mat) <- colnames(x)

  }

  return(mat)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the eff.categ() function ------------------------------
#
# - .phi
# - .cramer
# - .tschuprow
# - .cont
# - .cohen.w
# - .fei

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .phi Function ####

.phi <- function(x, adjust, p = NULL, conf.level, alternative) {

  # Cross tabulation
  tab <- table(x)

  # Vector of probabilities
  if (isTRUE(is.null(p))) { p <- rep(1 / length(tab), times = length(tab)) }

  # Chi-squared Test
  model <- suppressWarnings(chisq.test(tab, correct = FALSE, p = p))

  # Test statistic
  chisq <- model$statistic

  # Degrees of freedom
  df <- model$parameter

  # Sample size
  n <- sum(tab)

  # Noncentral parameter
  chisqs <- vapply(chisq, .get_ncp_chi, FUN.VALUE = numeric(2L), df = df, conf.level = conf.level, alternative = alternative)

  # Convert into phi coefficient
  low <- sqrt(chisqs[1L, ] / n)
  upp <- sqrt(chisqs[2L, ] / n)

  # Result table
  result <- data.frame(phi = sqrt(chisq / n), low = low, upp = upp, row.names = NULL)

  # Adjusted phi coefficient
  if (isTRUE(adjust)) {

    phi.max <- min(c(sqrt((sum(tab[1L, ])*sum(tab[, 2L])) / (sum(tab[, 1L])*sum(tab[2L, ]))),
                     sqrt((sum(tab[, 1L])*sum(tab[2L, ])) / (sum(tab[1L, ])*sum(tab[, 2L])))))

    result <- result / phi.max

  }

  # One-sided confidence interval
  switch(alternative, less = { result$low <- 0L }, greater = { result$upp <- 1L })

  # Add sample size
  result <- data.frame(n = n, result)

  return(result)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .cramer Function ####

.cramer <- function(x, adjust, conf.level = conf.level, alternative = alternative) {

  # Cross tabulation
  tab <- table(x)

  # Number of rows and columns
  nrow <- nrow(tab)
  ncol <- ncol(tab)

  # Phi coefficient
  result <- .phi(x, adjust = FALSE, conf.level = conf.level, alternative = alternative)[, -1L]

  #...................
  ### Finite sample bias-correction ####
  if (isTRUE(adjust)) {

    # Sample size
    n <- sum(tab)

    # Correction
    result <- lapply(result, function(y) { sqrt(pmax(0, y^2L - ((nrow - 1L)*(ncol - 1L)) / (n - 1L))) })

    # Cramer's V
    result <- data.frame(lapply(result, function(y) y / sqrt(pmin((nrow - ((nrow - 1L)^2L) / (n - 1L)) - 1L, (ncol - ((ncol - 1L)^2L) / (n - 1L)) - 1L))))

  #...................
  ### No finite sample bias-correction ####
  } else {

    # Cramer's V
    result <- data.frame(lapply(result, function(y) y / sqrt(pmin(nrow - 1L, ncol - 1L))))

  }

  # One-sided confidence interval
  switch(alternative, less = { result$low <- 0L }, greater = { result$upp <- 1L })

  # Add sample size and column names
  result <- data.frame(n = sum(tab), setNames(result, nm = c("v", "low", "upp")))

  return(result)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .tschuprow Function ####

.tschuprow <- function(x, adjust, conf.level = conf.level, alternative = alternative) {

  # Cross tabulation
  tab <- table(x)

  # Number of rows and columns
  nrow <- nrow(tab)
  ncol <- ncol(tab)

  # Sample size
  n <- sum(tab)

  # Phi coefficient
  result <- .phi(x, adjust = FALSE, conf.level = conf.level, alternative = alternative)[, -1L]

  #...................
  ### Finite sample bias-correction ####
  if (isTRUE(adjust)) {

    # Correction
    result <- lapply(result, function(y) { sqrt(pmax(0, y^2L - ((nrow - 1L)*(ncol - 1L)) / (n - 1L))) })

    # Tschuprow's T
    result <- data.frame(lapply(result, function(y) y / sqrt(sqrt(((nrow - ((nrow - 1L)^2L) / (n - 1L)) - 1L) * ((ncol - ((ncol - 1L)^2L) / (n - 1L)) - 1L)))))

    #...................
    ### No finite sample bias-correction ####
  } else {

    # Tschuprow's T
    result <- data.frame(lapply(result, function(y) y / sqrt(sqrt((nrow - 1L) * (ncol - 1L)))))


  }

  # One-sided confidence interval
  switch(alternative, less = { result$low <- 0L }, greater = { result$upp <- 1L })

  # Add sample size and column names
  result <- data.frame(n = n, setNames(result, nm = c("t", "low", "upp")))

  return(result)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .cont Function ####

.cont <- function(x, adjust, p = NULL, conf.level = conf.level, alternative = alternative) {

  # Cross tabulation
  tab <- table(x)

  # Phi coefficient
  result <- .phi(x, adjust = FALSE, p = p, conf.level = conf.level, alternative = alternative)[, -1L]

  # Contingency coefficient
  result <- data.frame(lapply(result, function(y) y / sqrt(y^2L + 1L)))

  # Sakoda's adjustment
  if (isTRUE(adjust)) {

    k <- min(c(nrow(tab), ncol(tab)))

    result <- result / sqrt((k - 1L) / k)

  }

  # One-sided confidence interval
  switch(alternative, less = { result$low <- 0L }, greater = { result$upp <- 1L })

  # Add sample size and column names
  result <- data.frame(n = sum(tab), setNames(result, nm = c("c", "low", "upp")))

  return(result)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .cohen.w Function ####

.cohen.w <- function(x, adjust, p = NULL, conf.level = conf.level, alternative = alternative) {

  # Cross tabulation
  tab <- table(x)

  # Number of rows and columns
  nrow <- nrow(tab)
  ncol <- ifelse(isTRUE(is.na(ncol(tab))), 1L, ncol(tab))

  # Phi coefficient
  result <- .phi(x, adjust = FALSE, p = p, conf.level = conf.level, alternative = alternative)[, -1L]

  # Cohen's w
  if (isTRUE(ncol == 1L || nrow == 1L)) {

    if (isTRUE(is.null(p))) {

      max.poss <- Inf

    } else {

      max.poss <- sqrt((1L / min(p / sum(p))) - 1L)

    }

  } else {

    max.poss <- sqrt((pmin(ncol, nrow) - 1L))

  }

  # One-sided confidence interval
  switch(alternative, less = { result$upp <- pmin(result$upp, max.poss) }, greater = { result$upp <- max.poss })

  # Add sample size and column names
  result <- data.frame(n = sum(tab), setNames(result, nm = c("w", "low", "upp")))

  return(result)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .fei Function ####

.fei <- function(x, adjust, p = NULL, conf.level = conf.level, alternative = alternative) {

  # Cross tabulation
  tab <- table(x)

  # Vector of probabilities
  if (isTRUE(is.null(p))) { p <- rep(1 / length(tab), times = length(tab)) }

  # Fei
  result <- .phi(x, adjust = FALSE, p = p, conf.level = conf.level, alternative = alternative)[, -1L]

  result <- data.frame(lapply(result, function(y) y / sqrt(1 / min(p) - 1L)))

  # One-sided confidence interval
  switch(alternative, less = { result$upp <- pmin(result$upp, 1) }, greater = { result$upp <- 1})

  # Add sample size and column names
  result <- data.frame(n = sum(tab), setNames(result, nm = c("fei", "low", "upp")))

  return(result)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .get_ncp_chi Function ####

.get_ncp_chi <- function(chi, df, conf.level, alternative) {

  alpha <- 1L - ifelse(alternative != "two.sided", 2L * conf.level - 1L, conf.level)
  probs <- c(alpha / 2L, 1L - alpha / 2L)

  ncp <- suppressWarnings(stats::optim(par = 1.1 * rep(chi, 2L), fn = function(y) {
    p <- pchisq(q = chi, df, ncp = y)
    abs(max(p) - probs[2L]) + abs(min(p) - probs[1L])
  }, control = list(abstol = 1e-09)))

  chi_ncp <- sort(ncp$par)

  if (chi <= stats::qchisq(probs[1L], df)) { chi_ncp[2L] <- 0L }
  if (chi <= stats::qchisq(probs[2L], df)) { chi_ncp[1L] <- 0L }

  return(chi_ncp)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the item.omega() function -----------------------------
#
# - omega.function
# - .catOmega
# - .getThreshold
# - .polycorLavaan
# - .refit
# - .p2
#
# MBESS: The MBESS R Package
# https://cran.r-project.org/web/packages/MBESS/index.html

omega.function <- function(y, y.rescov = NULL, y.type = type, y.std = std, check = TRUE) {

  std <- type <- NULL

  # Variable names
  varnames <- colnames(y)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Omega for continuous items ####

  if (isTRUE(y.type != "categ")) {

    #...................
    ### Mode specification ####

    # Factor model
    mod.factor <- paste("f =~", paste(varnames, collapse = " + "))

    # Residual covariance

    if (isTRUE(!is.null(y.rescov))) {

      mod.rescov <- vapply(y.rescov, function(y) paste(y, collapse = " ~~ "), FUN.VALUE = character(1L))

      # Paste residual covariances
      mod.factor <- paste(mod.factor, "\n", paste(mod.rescov, collapse = " \n "))

    }

    #...................
    ### Model estimation ####

    mod.fit <- suppressWarnings(lavaan::cfa(mod.factor, data = y, ordered = FALSE, se = "none", std.lv = TRUE, estimator = "ML", missing = "fiml"))

    #...................
    ### Check for convergence and negative degrees of freedom ####

    if (isTRUE(check)) {

      # Model convergence
      if (!isTRUE(lavaan::lavInspect(mod.fit, "converged"))) {

        warning("CFA model did not converge, results are most likely unreliable.", call. = FALSE)

      }

      # Degrees of freedom
      if (isTRUE(lavaan::lavInspect(mod.fit, "fit")["df"] < 0L)) {

        warning("CFA model has negative degrees of freedom, results are most likely unreliable.", call. = FALSE)

      }

    }

    #...................
    ### Parameter estimates ####

    if (!isTRUE(y.std)) {

      # Unstandardized parameter estimates
      param <- lavaan::parameterestimates(mod.fit)

    } else {

      # Standardized parameter estimates
      param <- lavaan::standardizedSolution(mod.fit)

      names(param)[grep("est.std", names(param))] <- "est"

    }

    #...................
    ### Factor loadings ####

    param.load <- param[which(param$op == "=~"), ]

    #...................
    ### Residual covariance ####

    param.rcov <- param[param$op == "~~" & param$lhs != param$rhs, ]

    #...................
    ### Residuals ####

    param.resid <- param[param$op == "~~" & param$lhs == param$rhs & param$lhs != "f" & param$rhs != "f", ]

    #...................
    ### Omega ####

    # Numerator
    load.sum2 <- sum(param.load$est)^2L

    # Total omega
    if (isTRUE(y.type != "hierarch"))  {

      resid.sum <- sum(param.resid$est)

      # Residual covariances
      if (isTRUE(!is.null(y.rescov))) { resid.sum <- resid.sum + 2L*sum(param.rcov$est) }

      omega <- load.sum2 / (load.sum2 + resid.sum)

      #...................
      ### Hierarchical omega ####
    } else {

      mod.cov <- paste(apply(combn(seq_len(length(varnames)), m = 2L), 2L, function(z) paste(varnames[z[1L]], "~~", varnames[z[2L]])), collapse = " \n ")

      mod.cov.fit <- suppressWarnings(lavaan::cfa(mod.cov, data = y, ordered = FALSE, se = "none", std.lv = TRUE, estimator = "ML", missing = "fiml"))

      if (!isTRUE(std)) {

        var.total <- sum(lavaan::inspect(mod.cov.fit, "cov.ov")[varnames, varnames])

      } else {

        var.total <- sum(lavaan::inspect(mod.cov.fit, "cor.ov")[varnames, varnames])

      }

      omega <- load.sum2 / var.total

    }

    # Return object
    object <- list(mod.fit = mod.fit, omega = omega)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Omega for ordered-categorical items ####
  } else {

    object <- .catOmega(y, y.rescov = y.rescov, check = TRUE)

  }

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .catOmega Function ####

.catOmega <- function(dat, y.rescov = NULL, check = TRUE) {

  # Variable names
  varnames <- colnames(dat)

  # Sequence from 1 to the number of columns
  q <- seq_len(ncol(dat))

  # Convert in ordered factor
  dat <- data.frame(lapply(dat, ordered))

  # Factor loadings
  loadingLine <- paste(paste0(paste0("L", q), "*", varnames), collapse = " + ")

  # Factor variance
  factorLine <- "f1 ~~ 1*f1\n"

  # Paste model syntax
  model <- paste0("f1 =~ NA*", varnames[1L], " + ", loadingLine, "\n", factorLine)

  # Residual covariance
  if (isTRUE(!is.null(y.rescov))) {

    rescovLine <- vapply(y.rescov, function(y) paste(y, collapse = " ~~ "), FUN.VALUE = character(1L))

    # Paste residual covariances
    model <- paste0(model, paste(rescovLine, collapse = "\n "))

  }

  # Estimate model
  mod.fit <- suppressWarnings(lavaan::cfa(model, data = dat, estimator = "DWLS", se = "none", ordered = TRUE))

  # Model convergence
  if (isTRUE(check)) { if (!isTRUE(lavaan::lavInspect(mod.fit, "converged"))) { warning("CFA model did not converge, results are most likely unreliable.", call. = FALSE) } }

  param <- lavaan::inspect(mod.fit, "coef")

  ly <- param[["lambda"]]
  ps <- param[["psi"]]

  threshold <- .getThreshold(mod.fit)[[1L]]

  denom <- .polycorLavaan(mod.fit, dat)[varnames, varnames]

  invstdvar <- 1L / sqrt(diag(lavaan::lavInspect(mod.fit, "implied")$cov))

  polyr <- diag(invstdvar) %*% ly%*%ps%*%t(ly) %*% diag(invstdvar)

  sumnum <- 0L
  addden <- 0L

  for (j in q) {

    for (jp in q) {

      sumprobn2 <- 0L
      addprobn2 <- 0L

      t1 <- threshold[[j]]
      t2 <- threshold[[jp]]

      for(c in seq_along(t1)) {

        for(cp in seq_along(t2)) {

          sumprobn2 <- sumprobn2 + .p2(t1[c], t2[cp], polyr[j, jp])
          addprobn2 <- addprobn2 + .p2(t1[c], t2[cp], denom[j, jp])

        }

      }

      sumprobn1 <- sum(pnorm(t1))
      sumprobn1p <- sum(pnorm(t2))

      sumnum <- sumnum + (sumprobn2 - sumprobn1 * sumprobn1p)
      addden <- addden + (addprobn2 - sumprobn1 * sumprobn1p)

    }

  }

  object <- list(mod.fit = mod.fit, omega = sumnum / addden)

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .getThreshold Function ####

.getThreshold <- function(object) {

  ngroups <- lavaan::inspect(object, "ngroups")

  coef <- lavaan::inspect(object, "coef")

  result <- NULL
  if (isTRUE(ngroups == 1L)) {

    targettaunames <- rownames(coef$tau)

    barpos <- sapply(strsplit(targettaunames, ""), function(x) which(x == "|"))

    varthres <- apply(data.frame(targettaunames, barpos - 1L, stringsAsFactors = FALSE), 1L, function(x) substr(x[1], 1L, x[2L]))

    result <- list(split(coef$tau, varthres))

  } else {

    result <- list()

    for (g in 1L:ngroups) {

      targettaunames <- rownames(coef[[g]]$tau)

      barpos <- sapply(strsplit(targettaunames, ""), function(x) which(x == "|"))

      varthres <- apply(data.frame(targettaunames, barpos - 1L, stringsAsFactors = FALSE), 1L, function(x) substr(x[1L], 1L, x[2L]))

      result[[g]] <- split(coef[[g]]$tau, varthres)

    }

  }

  return(result)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .polycorLavaan Function ####

.polycorLavaan <- function(object, data) {

  ngroups <- lavaan::inspect(object, "ngroups")

  coef <- lavaan::inspect(object, "coef")

  targettaunames <- NULL

  if (isTRUE(ngroups == 1L)) {

    targettaunames <- rownames(coef$tau)

  } else {

    targettaunames <- rownames(coef[[1L]]$tau)

  }

  barpos <- sapply(strsplit(targettaunames, ""), function(x) which(x == "|"))

  varnames <- unique(apply(data.frame(targettaunames, barpos - 1L, stringsAsFactors = FALSE), 1L, function(x) substr(x[1L], 1L, x[2L])))

  script <- ""

  for(i in 2L:length(varnames)) {

    temp <- paste0(varnames[1L:(i - 1L)], collapse = " + ")

    temp <- paste0(varnames[i], "~~", temp, "\n")

    script <- paste(script, temp)

  }

  suppressWarnings(newobject <- .refit(script, data, varnames, object))

  if (isTRUE(ngroups == 1L)) {

    return(lavaan::inspect(newobject, "coef")$theta)

  } else {

    return(lapply(lavaan::inspect(newobject, "coef"), "[[", "theta"))

  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .refit Function ####

.refit <- function(pt, data, vnames, object) {

  args <- lavaan::lavInspect(object, "call")

  args$model <- pt
  args$data <- data
  args$ordered <- vnames
  tempfit <- do.call(eval(parse(text = paste0("lavaan::", "lavaan"))), args[-1])

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .p2 Function ####

.p2 <- function(t1, t2, r) {

  mnormt::pmnorm(c(t1, t2), c(0L, 0L), matrix(c(1L, r, r, 1L), 2L, 2L))

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the mplus.run() function ------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Functions to identify the operating system ####

is.windows <- function() {
  if (isTRUE(exists("Sys.info"))) {
    os <- Sys.info()[["sysname"]]
  } else {
    os <- .Platform$OS.type
  }
  os <- tolower(os)
  if (isTRUE(grepl("windows", os))) {
    TRUE
  } else {
    FALSE
  }
}


is.macos <- function() {
  if (isTRUE(exists("Sys.info"))) {
    os <- Sys.info()[["sysname"]]
  } else {
    os <- R.version$os
  }
  os <- tolower(os)
  if (isTRUE(grepl("darwin", os))) {
    TRUE
  } else {
    FALSE
  }
}


is.linux <- function() {
  if (isTRUE(exists("Sys.info"))) {
    os <- Sys.info()[["sysname"]]
  } else {
    os <- R.version$os
  }
  os <- tolower(os)
  if (isTRUE(grepl("linux", os))) {
    TRUE
  } else {
    FALSE
  }
}

os <- function() {
  windows <- is.windows()
  macos <- is.macos()
  linux <- is.linux()

  count <- windows + macos + linux

  if (isTRUE(count > 1) || isTRUE(count == 0)) {
    "unknown"
  } else if (windows) {
    "windows"
  } else if (macos) {
    "macos"
  } else if (linux) {
    "linux"
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Detect the location/name of the Mplus command ####

detectMplus <- function() {

  ostype <- os()

  if (identical(ostype, "windows")) {

    suppressWarnings(mplus <- system("where Mplus", intern = TRUE, ignore.stderr = TRUE))

    if (isTRUE(length(mplus) > 0) && isTRUE(file.exists(mplus))) {
      mplus <- "Mplus"
    } else {

      if (isTRUE(file.exists("C:\\Program Files\\Mplus\\Mplus.exe"))) {
        mplus <- "C:\\Program Files\\Mplus\\Mplus.exe"
      } else {

        suppressWarnings(mplus <- system("where Mpdemo8", intern = TRUE, ignore.stderr = TRUE))

        if (isTRUE(length(mplus) > 0 && file.exists(mplus))) {
          mplus <- "Mpdemo8"
        } else {
          if (isTRUE(file.exists("C:\\Program Files\\Mplus Demo\\Mpdemo8.exe"))) {
            mplus <- "C:\\Program Files\\Mplus Demo\\Mpdemo8.exe"
          } else {
            note <- paste0(
              "Mplus and Mpdemo8 are either not installed or could not be found\n",
              "Try installing Mplus or Mplus Demo. If Mplus or Mplus Demo are already installed, \n",
              "make sure one can be found on your PATH. The following may help\n\n",
              "Windows 10:\n",
              " (1) In Search, search for and then select: System (Control Panel)\n",
              " (2) Click the Advanced system settings link.\n",
              " (3) Click Environment Variables ...\n",
              " (4) In the Edit System Variable (or New System Variable ) window,\n",
              " (5) specify the value of the PATH environment variable...\n",
              " (6) Close and reopen R and run:\n\n",
              "mplusAvailable(silent=FALSE)",
              "\n")
            stop(note)
          }
        }
      }
    }
  }

  if (isTRUE(identical(ostype, "macos"))) {

    suppressWarnings(mplus <- system("which mplus", intern = TRUE, ignore.stderr = TRUE))

    if (isTRUE(length(mplus) > 0 && file.exists(mplus))) {
      mplus <- "mplus"
    } else {
      if (isTRUE(file.exists("/Applications/Mplus/mplus"))) {
        mplus <- "/Applications/Mplus/mplus"
      } else {

        suppressWarnings(mplus <- system("which mpdemo", intern = TRUE, ignore.stderr = TRUE))

        if (isTRUE(length(mplus) > 0 && file.exists(mplus))) {
          mplus <- "mpdemo"
        } else {
          if (isTRUE(file.exists("/Applications/MplusDemo/mpdemo"))) {
            mplus <- "/Applications/MplusDemo/mpdemo"
          } else {
            stop("mplus and mpdemo not found on the system path or in the 'usual' /Applications/Mplus location. Ensure Mplus or the Mplus Demo are installed and that the location of the command is on your system path.")
          }
        }
      }
    }
  }

  if (isTRUE(identical(ostype, "linux"))) {
    failure_note <- paste0(
      "Mplus is either not installed or could not be found\n",
      "Try installing Mplus or if it already is installed,\n",
      "making sure it can be found by adding it to your PATH or adding a symlink\n\n",
      "To see directories on your PATH, From a terminal, run:\n\n",
      "  echo $PATH",
      "\n\nthen try something along these lines:\n\n",
      "  sudo ln -s /path/to/mplus/on/your/system /directory/on/your/PATH",
      "\n"
    )

    mplus_found <- FALSE

    suppressWarnings(mplus <- system("which mplus", intern = TRUE, ignore.stderr = TRUE))

    if (isTRUE(length(mplus) > 0L && file.exists(mplus))) {
      mplus <- "mplus"
      mplus_found <- TRUE
    } else {
      if (isTRUE(dir.exists("/opt/mplus"))) {
        test <- file.path(
          list.dirs("/opt/mplus", recursive = FALSE)[1],
          "mplus"
        )
        if (isTRUE(file.exists(test))) {
          mplus <- test
          mplus_found <- TRUE
        }
      } else {

        suppressWarnings(mplus <- system("which mpdemo", intern = TRUE, ignore.stderr = TRUE))

        if (isTRUE(length(mplus) > 0L && file.exists(mplus))) {
          mplus <- "mpdemo"
          mplus_found <- TRUE
        }
      }
    }

    if (isTRUE(!mplus_found)) {
      stop(failure_note)
    }
  }

  if (isTRUE(identical(ostype, "unknown"))) {
    stop("OS Type not known. Cannot auto detect Mplus command name. You must specify it.")
  }

  return(mplus)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## convert_to_filelist() ####

convert_to_filelist <- function(target, filefilter = NULL, recursive = FALSE) {

  filelist <- c()
  for (tt in target) {

    fi <- file.info(tt)

    if (isTRUE(is.na(fi$size))) { stop("Cannot find target: ", tt) }

    if (isTRUE(fi$isdir)) {
      directory <- sub("(\\\\|/)?$", "", tt, perl = TRUE)

      if (is.windows() && isTRUE(grepl("^[a-zA-Z]:$", directory))) {directory <- paste0(directory, "/") }

      if (isTRUE(!file.exists(directory))) stop("Cannot find directory: ", directory)

      this_set <- list.files(path=directory, recursive=recursive, pattern=".*\\.inp?$", full.names = TRUE)
      filelist <- c(filelist, this_set)

    } else {
      if (isTRUE(!grepl(".*\\.inp?$", tt, perl = TRUE))) {
        warning("Target: ", tt, "does not appear to be an .inp file. Ignoring it.")
        next
      } else {
        if (isTRUE(!file.exists(tt))) { stop("Cannot find input file: ", tt) }

        filelist <- c(filelist, tt)
      }
    }
  }

  if (isTRUE(!is.null(filefilter))) filelist <- grep(filefilter, filelist, perl = TRUE, value = TRUE)

  filelist <- normalizePath(filelist)

  return(filelist)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## splitFilePath() ####

splitFilePath <- function(filepath, normalize = FALSE) {

  if (isTRUE(!is.character(filepath))) stop("Path not a character string")
  if (isTRUE(nchar(filepath) < 1L || is.na(filepath))) stop("Path is missing or of zero length")

  filepath <- sub("(\\\\|/)?$", "", filepath, perl = TRUE)

  components <- strsplit(filepath, split="[\\/]")[[1L]]
  lcom <- length(components)

  stopifnot(lcom > 0L)

  relFilename <- components[lcom]
  absolute <- FALSE

  if (isTRUE(lcom == 1L)) {
    dirpart <- NA_character_
  } else if (isTRUE(lcom > 1L)) {
    components <- components[-lcom]
    dirpart <- do.call("file.path", as.list(components))

    if (grepl("^([A-Z]{1}:|~/|/|//|\\\\)+.*$", dirpart, perl=TRUE)) absolute <- TRUE

    if (normalize) { #convert to absolute path
      dirpart <- normalizePath(dirpart)
      absolute <- TRUE
    }
  }

  return(list(directory = dirpart, filename = relFilename, absolute = absolute))

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the mplus() and mplus.update() function ---------------
#
# - .extract.section

# Function for Extracting Input Command Sections ####
.extract.section <- function(section, x, section.pos) {

  # Split input
  split.x <- strsplit(x, "")[[1L]]

  # Start and end position of the section
  start <- as.numeric(gregexec(section, toupper(x))[[1L]])

  if (isTRUE(length(start) > 1L)) { stop(paste0("There are more than one ", dQuote(section), " sections in the input text."), call. = FALSE) }

  end <- if (isTRUE(any(section.pos > start))) { section.pos[which(section.pos > start)][1L] - 1L } else { length(split.x) }

  # Extract section, remove "", and paste "\n"
  object <- sapply(misty::chr.omit(misty::chr.trim(strsplit(paste(split.x[start:end], collapse = ""), "\n")[1L], side = "right", check = FALSE), check = FALSE),
                   function(y) if (grepl(";", y)) { paste0(y, "\n") } else { y }, USE.NAMES = FALSE)

  # Remove last "\n"
  object[length(object)] <- gsub("\n", "", object[length(object)])

  # Collapse with "\n" and return object
  object <- paste(object, collapse = "\n")

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the mplus.update() function ----------------------------
#
# - .variable.section

.variable.section <- function(section, input, update) {

  # Colon or semicolon position
  input.semicol <- misty::chr.grep(c(":", ";"), unlist(strsplit(input, "")))
  update.semicol <- misty::chr.grep(c(":", ";"), unlist(strsplit(update, "")))

  # Section in update input
  if (isTRUE(grepl(section, toupper(update)))) {

    # Start and end position of the updated section
    start.update <- rev(update.semicol[which(update.semicol < rev(unlist(gregexec(section, toupper(update))))[1L])])[1L] + 1L
    end.update <- update.semicol[which(update.semicol > unlist(gregexec(section, toupper(update)))[1L])][1L]

    ## Subsection is in Input Object
    if (isTRUE(grepl(section, toupper(input)))) {

      ### Start/End Position of the Removal Section
      start.inp <- rev(input.semicol[which(input.semicol < rev(unlist(gregexec(section, toupper(input))))[1L])])[1L] + 1L
      end.inp <- input.semicol[which(input.semicol > unlist(gregexec(section, toupper(input)))[1L])][1L]

      ### Insert Updated Sub-Section
      input <- sub("...", "",
                   paste(c(unlist(strsplit(input, ""))[seq_len(start.inp - 1L)], "\n",
                           unlist(strsplit(update, ""))[start.update:end.update],
                           if (isTRUE(end.inp != nchar(input))) { unlist(strsplit(input, ""))[((end.inp + 1L):nchar(input))] }),
                         collapse = ""), fixed = TRUE)

    ## Subsection is not in Input Object
    } else {

      input <- sub("...", "", paste(c(input, "\n\n", unlist(strsplit(update, ""))[start.update:end.update]), collapse = ""), fixed = TRUE)

    }

  }

  return(invisible(input))

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the robust.coef() function ----------------------------
#
# - .sandw
# - .coeftest
# - .waldtest

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Making Sandwiches with Bread and Meat ####

.sandw <- function(x, type = c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")) {

  # Hat values
  diaghat <- try(hatvalues(x), silent = TRUE)

  # Specify omega function
  switch(type,
         const = { omega <- function(residuals, diaghat, df) rep(1, length(residuals)) * sum(residuals^2L) / df },
         HC0   = { omega <- function(residuals, diaghat, df) residuals^2L },
         HC1   = { omega <- function(residuals, diaghat, df) residuals^2L * length(residuals)/df },
         HC2   = { omega <- function(residuals, diaghat, df) residuals^2L / (1L - diaghat) },
         HC3   = { omega <- function(residuals, diaghat, df) residuals^2L / (1L - diaghat)^2L },
         HC4   = { omega <- function(residuals, diaghat, df) {
           n <- length(residuals)
           p <- as.integer(round(sum(diaghat),  digits = 0L))
           delta <- pmin(4L, n * diaghat/p)
           residuals^2L / (1L - diaghat)^delta
         }},
         HC4m  = { omega <- function(residuals, diaghat, df) {
           gamma <- c(1.0, 1.5) ## as recommended by Cribari-Neto & Da Silva
           n <- length(residuals)
           p <- as.integer(round(sum(diaghat)))
           delta <- pmin(gamma[1L], n * diaghat/p) + pmin(gamma[2L], n * diaghat/p)
           residuals^2L / (1L - diaghat)^delta
         }},
         HC5   = { omega <- function(residuals, diaghat, df) {
           k <- 0.7 ## as recommended by Cribari-Neto et al.
           n <- length(residuals)
           p <- as.integer(round(sum(diaghat)))
           delta <- pmin(n * diaghat / p, pmax(4L, n * k * max(diaghat) / p))
           residuals^2L / sqrt((1L - diaghat)^delta)
         }})

  if (isTRUE(type %in% c("HC2", "HC3", "HC4", "HC4m", "HC5"))) {

    if (isTRUE(inherits(diaghat, "try-error"))) stop(sprintf("hatvalues() could not be extracted successfully but are needed for %s", type), call. = FALSE)

    id <- which(diaghat > 1L - sqrt(.Machine$double.eps))

    if(length(id) > 0L) {

      id <- if (isTRUE(is.null(rownames(X)))) { as.character(id) } else { rownames(X)[id] }

      if(length(id) > 10L) id <- c(id[1L:10L], "...")

      warning(sprintf("%s covariances become numerically unstable if hat values are close to 1 as for observations %s", type, paste(id, collapse = ", ")), call. = FALSE)

    }

  }

  # Ensure that NAs are omitted
  if(is.list(x) && !is.null(x$na.action)) class(x$na.action) <- "omit"

  # Extract design matrix
  X <- model.matrix(x)
  if (isTRUE(any(alias <- is.na(coef(x))))) X <- X[, !alias, drop = FALSE]

  # Number of observations
  n <- NROW(X)

  # Generalized Linear Model
  if (isTRUE(inherits(x, "glm"))) {

    wres <- as.vector(residuals(x, "working")) * weights(x, "working")
    dispersion <- if (isTRUE(substr(x$family$family, 1L, 17L) %in% c("poisson", "binomial", "Negative Binomial"))) { 1L } else { sum(wres^2L, na.rm = TRUE) / sum(weights(x, "working"), na.rm = TRUE) }

    ef <- wres * X / dispersion

    # Linear Model
  } else {

    # Weights
    wts <- if (isTRUE(is.null(weights(x)))) { 1L } else { weights(x) }
    ef <- as.vector(residuals(x)) * wts * X

  }

  # Meat
  meat <- crossprod(sqrt(omega(rowMeans(ef / X, na.rm = TRUE), diaghat, n - NCOL(X))) * X) / n

  # Bread
  sx <- summary.lm(x)
  bread <- sx$cov.unscaled * as.vector(sum(sx$df[1L:2L]))

  # Sandwich
  sandw <- 1L / n * (bread %*% meat %*% bread)

  return(invisible(sandw))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Inference for Estimated Coefficients ####

.coeftest <- function(x, vcov = NULL) {

  # Extract coefficients and standard errors
  est <- coef(x)
  se <- sqrt(diag(vcov))

  ## match using names and compute t/z statistics
  if (isTRUE(!is.null(names(est)) && !is.null(names(se)))) {

    if (length(unique(names(est))) == length(names(est)) && length(unique(names(se))) == length(names(se))) {

      anames <- names(est)[names(est) %in% names(se)]
      est <- est[anames]
      se <- se[anames]

    }

  }

  # Test statistic
  stat <- as.vector(est) / se

  df <- try(df.residual(x), silent = TRUE)

  # Generalized Linear Model
  if (isTRUE(inherits(x, "glm"))) {

    pval <- 2L * pnorm(abs(stat), lower.tail = FALSE)
    cnames <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    mthd <- "z"

    # Linear Model
  } else {

    pval <- 2L * pt(abs(stat), df = df, lower.tail = FALSE)
    cnames <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    mthd <- "t"

  }

  object <- cbind(est, se, stat, pval)
  colnames(object) <- cnames

  return(invisible(object))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Wald Test of Nested Models ####

.waldtest <- function(object, ..., vcov = NULL, name = NULL) {

  coef0 <- function(x, ...) { na.omit(coef(x, ...)) }

  nobs0 <- function(x, ...) {

    nobs1 <- nobs
    nobs2 <- function(x, ...) { NROW(residuals(x, ...)) }

    object <- try(nobs1(x, ...), silent = TRUE)

    if (isTRUE(inherits(object, "try-error") | is.null(object))) object <- nobs2(x, ...)

    return(object)

  }

  df.residual0 <- function(x) {

    df <- try(df.residual(x), silent = TRUE)

    if (isTRUE(inherits(df, "try-error") | is.null(df))) { df <- try(nobs0(x) - attr(logLik(x), "df"), silent = TRUE) }
    if (isTRUE(inherits(df, "try-error") | is.null(df))) { df <- try(nobs0(x) - length(as.vector(coef0(x))), silent = TRUE) }
    if (isTRUE(inherits(df, "try-error"))) df <- NULL

    return(df)

  }

  cls <- class(object)[1L]

  # 1. Extracts term labels
  tlab <- function(x) {

    tt <- try(terms(x), silent = TRUE)
    if (isTRUE(inherits(tt, "try-error"))) "" else attr(tt, "term.labels")

  }

  # 2. Extracts model name
  if (isTRUE(is.null(name))) name <- function(x) {

    object <- try(formula(x), silent = TRUE)

    if (isTRUE(inherits(object, "try-error") | is.null(object))) { object <- try(x$call, silent = TRUE) }
    if (isTRUE(inherits(object, "try-error") | is.null(object))) { return(NULL) } else { return(paste(deparse(object), collapse="\n")) }

  }

  # 3. Compute an updated model object
  modelUpdate <- function(fm, update) {

    if (isTRUE(is.numeric(update))) {

      if (isTRUE(any(update < 1L))) {

        warning("For numeric model specifications all values have to be >= 1", call. = FALSE)
        update <- abs(update)[abs(update) > 0L]

      }

      if (isTRUE(any(update > length(tlab(fm))))) {

        warning(paste("More terms specified than existent in the model:", paste(as.character(update[update > length(tlab(fm))]), collapse = ", ")), call. = FALSE)
        update <- update[update <= length(tlab(fm))]

      }

      update <- tlab(fm)[update]

    }

    if (isTRUE(is.character(update))) {

      if (isTRUE(!all(update %in% tlab(fm)))) {

        warning(paste("Terms specified that are not in the model:", paste(dQuote(update[!(update %in% tlab(fm))]), collapse = ", ")), call. = FALSE)
        update <- update[update %in% tlab(fm)]

      }

      if (isTRUE(length(update) < 1L)) { stop("Empty model specification", call. = FALSE)  }
      update <- as.formula(paste(". ~ . -", paste(update, collapse = " - ")))

    }

    if (isTRUE(inherits(update, "formula"))) {

      update <- update(fm, update, evaluate = FALSE)
      update <- eval(update, parent.frame(3))

    }

    if (isTRUE(!inherits(update, cls))) { stop(paste("Original model was of class \"", cls, "\", updated model is of class \"", class(update)[1], "\"", sep = ""), call. = FALSE) }

    return(update)

  }

  # 4. Compare two fitted model objects
  modelCompare <- function(fm, fm.up, vfun = NULL) {

    q <- length(coef0(fm)) - length(coef0(fm.up))

    if (isTRUE(q > 0L)) {

      fm0 <- fm.up
      fm1 <- fm

    } else {

      fm0 <- fm
      fm1 <- fm.up

    }

    k <- length(coef0(fm1))
    n <- nobs0(fm1)

    # Determine omitted variables
    if (isTRUE(!all(tlab(fm0) %in% tlab(fm1)))) { stop("Nesting of models cannot be determined", call. = FALSE) }

    ovar <- which(!(names(coef0(fm1)) %in% names(coef0(fm0))))

    if (isTRUE(abs(q) != length(ovar))) { stop("Nesting of models cannot be determined", call. = FALSE) }

    # Get covariance matrix estimate
    vc <- if (isTRUE(is.null(vfun))) { vcov(fm1) } else if (isTRUE(is.function(vfun))) { vfun(fm1) } else { vfun }

    ## Compute Chisq statistic
    stat <- t(coef0(fm1)[ovar]) %*% solve(vc[ovar,ovar]) %*% coef0(fm1)[ovar]

    return(c(-q, stat))

  }

  # Recursively fit all objects
  objects <- list(object, ...)
  nmodels <- length(objects)

  if (isTRUE(nmodels < 2L)) {

    objects <- c(objects, . ~ 1)
    nmodels <- 2L

  }

  # Remember which models are already fitted
  no.update <- sapply(objects, function(obj) inherits(obj, cls))

  # Updating
  for(i in 2L:nmodels) objects[[i]] <- modelUpdate(objects[[i - 1L]], objects[[i]])

  # Check responses
  getresponse <- function(x) {

    tt <- try(terms(x), silent = TRUE)
    if (isTRUE(inherits(tt, "try-error"))) { "" } else { deparse(tt[[2L]]) }

  }

  responses <- as.character(lapply(objects, getresponse))
  sameresp <- responses == responses[1L]

  if (isTRUE(!all(sameresp))) {

    objects <- objects[sameresp]
    warning("Models with response ", deparse(responses[!sameresp]), " removed because response differs from ", "model 1", call. = FALSE)

  }

  # Check sample sizes
  ns <- sapply(objects, nobs0)
  if (isTRUE(any(ns != ns[1L]))) {

    for(i in 2L:nmodels) {

      if (isTRUE(ns[1L] != ns[i])) {

        if (isTRUE(no.update[i])) { stop("Models were not all fitted to the same size of dataset")

        } else {

          commonobs <- row.names(model.frame(objects[[i]])) %in% row.names(model.frame(objects[[i - 1L]]))
          objects[[i]] <- eval(substitute(update(objects[[i]], subset = commonobs), list(commonobs = commonobs)))

          if (isTRUE(nobs0(objects[[i]]) != ns[1L])) { stop("Models could not be fitted to the same size of dataset", call. = FALSE) }

        }

      }

    }

  }

  # ANOVA matrix
  object <- matrix(rep(NA, 4L * nmodels), ncol = 4L)

  colnames(object) <- c("Res.Df", "Df", "F", "pval")
  rownames(object) <- 1L:nmodels

  object[, 1L] <- as.numeric(sapply(objects, df.residual0))
  for(i in 2L:nmodels) object[i, 2L:3L] <- modelCompare(objects[[i - 1L]], objects[[i]], vfun = vcov)

  df <- object[, 1L]
  for(i in 2L:nmodels) if (isTRUE(object[i, 2L] < 0L)) { df[i] <- object[i - 1L, 1L] }
  object[, 3L] <- object[, 3L] / abs(object[, 2L])
  object[, 4L] <- pf(object[, 3L], abs(object[, 2L]), df, lower.tail = FALSE)

  variables <- lapply(objects, name)
  if (isTRUE(any(sapply(variables, is.null)))) { variables <- lapply(match.call()[-1L], deparse)[1L:nmodels] }

  return(invisible(object))

}
