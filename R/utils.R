#_______________________________________________________________________________
#
# Internal Functions
#
# Collection of internal function used within functions of the misty package

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Check Argument Specification -------------------------------------------------

.check.input <- function(logical = NULL, numeric = NULL, character = NULL, m.character = NULL, s.character = NULL, args = NULL, package = NULL, envir = environment(), input.check = check) {

  # Check input 'input.check'
  if (isTRUE(!is.null(dim(input.check)) || length(input.check) != 1L || !is.logical(input.check) || is.na(input.check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  # Check inputs
  if (isTRUE(input.check)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check TRUE/FALSE Input ####

    if (isTRUE(!is.null(logical))) { invisible(sapply(logical, function(y) { eval(parse(text = y), envir = envir) |> (\(z) if (isTRUE(!is.null(dim(z)) || length(z) != 1L || !is.logical(z) || is.na(z))) { stop(paste0("Please specify TRUE or FALSE for the argument '", y,  "'."), call. = FALSE) })() })) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check Numeric Input ####

    if (isTRUE(!is.null(numeric))) {

      invisible(sapply(names(numeric), function(y) {

        eval(parse(text = y), envir = envir) |> (\(z) if (isTRUE(!all(is.na(z)) && !is.null(z) && (!is.numeric(z) || length(z) != numeric[[y]]))) {

          if (isTRUE(numeric[[y]] == 1L)) {

            stop(paste0("Please specify a numeric value for the argument '", y,  "'."), call. = FALSE)

          } else {

            stop(paste0("Please specify a numeric vector with ", switch(as.character(numeric[[y]]), "1" = "one", "2" = "two", "3" = "three",  "4" = "four"),  " elements for the argument '", y,  "'."), call. = FALSE)

          }

          })()

        }))

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check Character Input ####

    if (isTRUE(!is.null(character))) {

      invisible(sapply(names(character), function(y) {

        eval(parse(text = y), envir = envir) |> (\(z) if (isTRUE(!is.null(z) && (!is.character(z) || length(z) != character[[y]]))) {

          if (isTRUE(character[[y]] == 1L)) {

            stop(paste0("Please specify a character string for the argument '", y,  "'."), call. = FALSE)

          } else {

            stop(paste0("Please specify a character vector with ", switch(as.character(character[[y]]), "1" = "one", "2" = "two", "3" = "three", "4" = "four", "5" = "five", "6" = "six"), " elements for the argument '", y,  "'."), call. = FALSE)

          }

        })()

      }))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check Multiple Character Input ####

    if (isTRUE(!is.null(m.character))) {

      invisible(sapply(names(m.character), function(y) { if (isTRUE(any(!eval(parse(text = y), envir = envir) %in% m.character[[y]]))) {

        if (isTRUE(length(eval(parse(text = y), envir = envir)) == 1L)) {

          stop(paste0("Character string specified in the argument '", y , "' does not all match with ",  paste0(paste(unlist(m.character[y]) |> (\(z) paste(sapply(z[-length(z)], dQuote, q = FALSE)))(), collapse = ", "), ", or ", dQuote(rev(unlist(m.character[y]))[1L], q = FALSE)), "."), call. = FALSE)

        } else {

          stop(paste0("Character strings specified in the argument '", y , "' do not all match with ",  paste0(paste(unlist(m.character[y]) |> (\(z) paste(sapply(z[-length(z)], dQuote, q = FALSE)))(), collapse = ", "), ", or ", dQuote(rev(unlist(m.character[y]))[1L], q = FALSE)), "."), call. = FALSE)

        }

        }}))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check Single Character Input ####

    if (isTRUE(!is.null(s.character))) {

      invisible(sapply(names(s.character), function(y) eval(parse(text = y), envir = envir) |> (\(z) if (isTRUE(!is.character(z) || any(!z %in% s.character[[y]]) || (!all(z %in% s.character[[y]]) && length(z) != 1L))) {

            stop(paste0("Please specify ", paste0(paste(unlist(s.character[y]) |> (\(z) paste(sapply(z[-length(z)], dQuote, q = FALSE)))(), collapse = ", "), ", or ", dQuote(rev(unlist(s.character[y]))[1L], q = FALSE)), " for the argument ", sQuote(y, q = FALSE), "."), call. = FALSE)

          })()))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check Additional Arguments ####

    if (isTRUE(!is.null(args))) {

      # Check input 'digits'
      if (isTRUE("digits" %in% args)) { eval(parse(text = "digits"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.numeric(y) || length(y) != 1L || y %% 1L != 0L || y < 0L))) { stop("Please specify a positive integer number for the argument 'digits'.", call. = FALSE) })() }

      # Check input 'p.digits'
      if (isTRUE("p.digits" %in% args)) { eval(parse(text = "p.digits"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.numeric(y) || length(y) != 1L || y %% 1L != 0L || y < 0L))) { stop("Please specify a positive integer number for the argument 'p.digits'.", call. = FALSE) })() }

      # Check input 'icc.digits'
      if (isTRUE("icc.digits" %in% args)) { eval(parse(text = "icc.digits"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.numeric(y) || length(y) != 1L || y %% 1L != 0L || y < 0L))) { stop("Please specify a positive integer number for the argument 'icc.digits'.", call. = FALSE) })() }

      # Check input 'r.digits'
      if (isTRUE("r.digits" %in% args)) { eval(parse(text = "r.digits"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.numeric(y) || length(y) != 1L || y %% 1L != 0L || y < 0L))) { stop("Please specify a positive integer number for the argument 'r.digits'.", call. = FALSE) })() }

      # Check input 'ess.digits'
      if (isTRUE("ess.digits" %in% args)) { eval(parse(text = "ess.digits"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.numeric(y) || length(y) != 1L || y %% 1L != 0L || y < 0L))) { stop("Please specify a positive integer number for the argument 'ess.digits'.", call. = FALSE) })() }

      # Check input 'mcse.digits'
      if (isTRUE("mcse.digits" %in% args)) { eval(parse(text = "mcse.digits"), envir = envir) |> (\(y) if (isTRUE(!!is.null(y) && (is.numeric(y) || length(y) != 1L || y %% 1L != 0L || y < 0L))) { stop("Please specify a positive integer number for the argument 'mcse.digits'.", call. = FALSE) })() }

      # Check input 'conf.level'
      if (isTRUE("conf.level" %in% args)) { eval(parse(text = "conf.level"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.numeric(y) || length(y) != 1L || y >= 1L || y <= 0L))) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) })() }

      # Check input 'hist.alpha'
      if (isTRUE("hist.alpha" %in% args)) { eval(parse(text = "hist.alpha"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.numeric(y) || length(y) != 1L || y >= 1L || y <= 0L))) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'hist.alpha '.", call. = FALSE) })() }

      # Check input 'R'
      if (isTRUE("R" %in% args)) { eval(parse(text = "R"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.numeric(y) || length(y) != 1L || y %% 1L != 0L || y < 0L))) { stop("Please specify a positive integer number for the argument 'R'.", call. = FALSE) })() }

      # Check input 'alternative'
      if (isTRUE("alternative" %in% args)) { eval(parse(text = "alternative"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && !all(c("two.sided", "less", "greater") %in% y) && (length(y) != 1L || !is.character(y) || any(!y %in% c("two.sided", "less", "greater"))))) { stop("Character string specified in the argument 'alternative' does not match with \"two.sided\", \"less\", or \"greater\".", call. = FALSE) })() }

      # Check input 'p.adj'
      if (isTRUE("p.adj" %in% args)) { eval(parse(text = "p.adj"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && !all(c("none", "holm", "bonferroni", "hochberg", "hommel", "BH", "BY", "fdr") %in% y) && (length(y) != 1L || !is.character(y) || any(!y %in% c("none", "holm", "bonferroni", "hochberg", "hommel", "BH", "BY", "fdr"))))) { stop("Character string specified in the argument 'p.adj' does not match with \"none\", \"bonferroni\", \"holm\", \"hochberg\", \"hommel\", \"BH\", \"BY\", or \"fdr\".", call. = FALSE) })() }

      # Check input 'linetype'
      if (isTRUE("linetype" %in% args)) { eval(parse(text = "linetype"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.character(y) || any(!y %in% c("twodash", "solid", "longdash", "dotted", "dotdash", "dashed"))))) { stop("Character string specified in the argument 'linetype' does not match with \"twodash\", \"solid\", \"longdash\", \"dotted\", \"ldotdash\", or \"dashed\".", call. = FALSE) })() }

      # Check input 'units'
      if (isTRUE("units" %in% args)) { eval(parse(text = "units"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && !all(c("in", "cm", "mm", "px") %in% y) && (length(y) != 1L || !is.character(y) || any(!y %in% c("in", "cm", "mm", "px"))))) { stop("Character string specified in the argument 'units' does not match with \"in\", \"cm\", \"mm\", or \"px\".", call. = FALSE) })() }

      # Check input 'facet.scales'
      if (isTRUE("facet.scales" %in% args)) { eval(parse(text = "facet.scales"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && !all(c("fixed", "free_x", "free_y", "free") %in% y) && (length(y) != 1L || !is.character(y) || any(!y %in% c("fixed", "free_x", "free_y", "free"))))) { stop("Character string specified in the argument 'facet.scales' does not match with \"fixed\", \"free_x\", \"free_y\", or \"free\".", call. = FALSE) })() }

      # Check input 'legend.position'
      if (isTRUE("legend.position" %in% args)) { eval(parse(text = "legend.position"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && !all(c("right", "top", "left", "bottom", "none") %in% y) && (length(y) != 1L || !is.character(y) || any(!y %in% c("right", "top", "left", "bottom", "none"))))) { stop("Character string specified in the argument 'legend.position' does not match with \"right\", \"top\", \"left\", \"bottom\", or \"none\".", call. = FALSE) })() }

      # Check input 'write' text file
      if (isTRUE("write1" %in% args)) { eval(parse(text = "write"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.null(y) && (!is.character(y) || !grepl(".txt", y))))) { stop("Please specify a character string with file extenstion '.txt' for the argument 'write'.") })() }

      # Check input 'write' text and Excel file
      if (isTRUE("write2" %in% args)) { eval(parse(text = "write"), envir = envir) |> (\(y) if (isTRUE(!is.null(y) && (!is.null(y) && (!is.character(y) || all(!misty::chr.grepl(c(".txt", ".xlsx"), y)))))) { stop("Please specify a character string with file extenstion '.txt' or '.xlsx' for the argument 'write'.") })() }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check Packages ####

    if (isTRUE(!is.null(package))) { invisible(sapply(package, function(y) { if (isTRUE(!requireNamespace(y, quietly = TRUE))) { stop(paste0("Package \"",  y ,"\" is needed for this function to work, please install it."), call. = FALSE) } })) }

  }

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Extract Variable Names Specified in the ... Argument  ------------------------

.var.names <- function(data, ..., group = NULL, split = NULL, cluster = NULL, id = NULL, obs = NULL, day = NULL, time = NULL) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check if input 'data' is a data frame ####

  # Check if input 'data' is data frame
  if (isTRUE(!is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert tibble into data frame ####

  if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- as.data.frame(data) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Elements in the '...' Argument ####

  var.names <- sapply(substitute(list(...)), as.character)[-1L]

  #...................
  ### Check for ! operators ####

  var.names.excl <- sapply(var.names, function(y) any(y %in% "!"))

  if (isTRUE(any(var.names.excl))) {

    for (i in which(var.names.excl)) {

      var.names.i <- unlist(strsplit(var.names[[i]], ""))

      # Plus (+) Operator
      if (isTRUE("+" %in% var.names.i)) {

         var.names[[i]] <- unlist(strsplit(var.names[[i]], "(?=[/+])", perl = TRUE))

      # Minus (-) Operator
      } else if (isTRUE("-" %in% var.names.i)) {

        var.names[[i]] <- unlist(strsplit(var.names[[i]], "(?=[/-])", perl = TRUE))

      # Tilde (~) Operator
      } else if (isTRUE("~" %in% var.names.i)) {

        var.names[[i]] <- unlist(strsplit(var.names[[i]], "(?=[/~])", perl = TRUE))

      # Colon (:) operator
      } else if (isTRUE(sum(var.names.i %in% ":") == 1L)) {

        var.names[[i]] <- c("!", ":", unlist(strsplit(var.names[[i]], ":"))[2L:3L])

      # Double Colon (::) Operator
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

        var.exclude <- c(var.exclude, misty::chr.grep(misty::chr.omit(var.i, omit = "!", check = FALSE), colnames(data), value = TRUE))

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

      setdiff(misty::chr.omit(var.i, omit = "!", check = FALSE), colnames(data)) |>
        (\(y) if (isTRUE(length(y) != 0L)) {

          stop(paste0(ifelse(length(y) == 1L, "Variable name involved in the : operator was not found in 'data': ", "Variable names involved in the : operator were not found in 'data': "), paste0(y, collapse = ", ")), call. = FALSE)

        })()

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
      var.i1.log <- sapply(var.i1.split, function(y) y %in% as.character(0L:9L))
      # Numeric values ending variable
      var.i2.log <- sapply(var.i2.split, function(y) y %in% as.character(0L:9L))

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
  ### Remove "" elements ####

  var.names <- misty::chr.omit(var.names, omit = "", check = FALSE)

  #...................
  ### Unique element ####

  var.names <- unique(var.names)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Select all Variables ####

  if (isTRUE(length(var.names) == 0L)) { var.names <- colnames(data) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split, Cluster and Other Variables ####

  #...................
  ### Exclude grouping variable ####
  if (isTRUE(!is.null(group))) {

    if (isTRUE(!is.character(group) || length(group) != 1L)) { stop("Please specify a character string for the argument 'group'.", call. = FALSE) }
    if (isTRUE(!group %in% colnames(data))) { stop("Grouping variable specifed in 'group' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, group)

  }

  #...................
  ### Exclude split variable ####
  if (isTRUE(!is.null(split))) {

    if (isTRUE(!is.character(split) || length(split) != 1L)) { stop("Please specify a character string for the argument 'split'.", call. = FALSE) }
    if (isTRUE(!split %in% colnames(data))) { stop("Split variable specifed in 'split' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, split)

  }

  #...................
  ### Exclude cluster variable ####
  if (isTRUE(!is.null(cluster))) {

    if (isTRUE(!is.character(cluster) || !length(cluster) %in% c(1L, 2L))) { stop("Please specify a character vector for the argument 'cluster'.", call. = FALSE) }

    ##### One cluster variable
    if (isTRUE(length(cluster) == 1L)) {

      if (isTRUE(!cluster %in% colnames(data))) { stop("Cluster variable specifed in 'cluster' was not found in 'data'.", call. = FALSE) }

    ##### Two cluster variables
    } else {

      # Cluster variable in 'data'
      (!cluster %in% colnames(data)) |>
        (\(y) if (isTRUE(any(y))) {

          if (isTRUE(sum(y) == 1L)) {

            stop(paste0("Cluster variable specifed in 'cluster' was not found in 'data': ", cluster[which(y)]),  call. = FALSE)

          } else {

            stop("Cluster variables specifed in 'cluster' were not found in 'data'.",  call. = FALSE)

          }

        })()

      # Order of cluster variables
      suppressWarnings(tapply(data[, cluster[2L]], data[, cluster[1L]], var, na.rm = TRUE)) |>
        (\(y) if (isTRUE(all(y == 0) || all(is.na(y)))) { stop("Please specify the Level 3 cluster variable first, e.g., cluster = c(\"level3\", \"level2\").", call. = FALSE) })()

    }

    var.names <- setdiff(var.names, cluster)

  }

  #...................
  ### Exclude id variable ####
  if (isTRUE(!is.null(id))) {

    if (isTRUE(!is.character(id) || length(id) != 1L)) { stop("Please specify a character string for the argument 'id'.", call. = FALSE) }
    if (isTRUE(!id %in% colnames(data))) { stop("Split variable specifed in 'id' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, id)

  }

  #...................
  ### Exclude obs variable ####
  if (isTRUE(!is.null(obs))) {

    if (isTRUE(!is.character(obs) || length(obs) != 1L)) { stop("Please specify a character string for the argument 'obs'.", call. = FALSE) }
    if (isTRUE(!id %in% colnames(data))) { stop("Observation number variable specifed in 'obs' was not found in 'data'.", call. = FALSE) }

    if (isTRUE(any(sapply(split(obs, id), function(x) length(x) != length(unique(x)))))) { stop("There are duplicated observations specified in 'obs' within subjects specified in 'id'.", call. = FALSE) }

    var.names <- setdiff(var.names, obs )

  }

  #...................
  ### Exclude day variable ####
  if (isTRUE(!is.null(day))) {

    if (isTRUE(!is.character(day) || length(day) != 1L)) { stop("Please specify a character string for the argument 'day'.", call. = FALSE) }
    if (isTRUE(!id %in% colnames(data))) { stop("Day variable specifed in 'day' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, day)

  }

  #...................
  ### Exclude time variable ####
  if (isTRUE(!is.null(time))) {

    if (isTRUE(!is.character(time) || length(time) != 1L)) { stop("Please specify a character string for the argument 'time'.", call. = FALSE) }
    if (isTRUE(!id %in% colnames(data))) { stop("Date and time variable specifed in 'time' was not found in 'data'.", call. = FALSE) }

    var.names <- setdiff(var.names, time)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Variables ####

  if (isTRUE(!is.null(var.exclude))) { var.names <- setdiff(var.names, var.exclude) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check if Variables in '...' are available in 'data' ####

  if (isTRUE(!is.null(data))) { setdiff(var.names, colnames(data)) |> (\(y) if (isTRUE(length(y) != 0L)) { stop(paste0(ifelse(length(y) == 1L, "Variable specified in '...' was not found in 'data': ", "Variables specified in '...' were not found in 'data': "), paste(y, collapse = ", ")), call. = FALSE) })() }

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
    if (isTRUE(length(unique(na.omit(unlist(group)))) == 1L)) { stop("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

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
    if (isTRUE(length(unique(na.omit(unlist(split)))) == 1L)) { stop("There is only one split represented in the grouping variable specified in 'split'.", call. = FALSE) }

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
          setdiff(cluster, colnames(data)) |>
            (\(y) if (isTRUE(length(y) == 1L)) {

              stop(paste0("Cluster variable \"", y, "\" specifed in 'cluster' was not found in '...'."), call. = FALSE)

            } else {

              stop("Cluster variables specifed in 'cluster' were not found in '...'.", call. = FALSE)

            })()

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

      sapply(cluster, function(y) all(is.na(y))) |>
        (\(y) if (isTRUE(any(y))) {

          if (isTRUE(sum(y)) == 1L) {

            stop(paste0("A cluster variable specified in 'cluster' is completely missing.: ", names(which(y))), call. = FALSE)

          } else {

            stop("Cluster variables specified in 'cluster' are completely missing.", call. = FALSE)

          }

        })()

    }

    ##### Check if only one group represented in the cluster variable

    # One cluster variable
    if (isTRUE(ncol(as.data.frame(cluster)) == 1L)) {

      if (isTRUE(length(unique(na.omit(unlist(cluster)))) == 1L)) { stop("There is only one group represented in the cluster variable 'cluster'.", call. = FALSE) }

    # Two cluster variables
    } else {

      sapply(cluster, function(y) length(unique(na.omit(unlist(y)))) == 1L) |>
        (\(y) if (isTRUE(any(y))) {

          if (isTRUE(sum(y)) == 1L) {

            stop(paste0("There is only one group represented in a cluster variable specified in 'cluster': ", names(which(y))), call. = FALSE)

          } else {

            stop("There is only one group represented in both cluster variables specified in 'cluster'.", call. = FALSE)

          }

        })()

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

      ##### Check if length of day number variable matching with the number of rows in 'data'
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
# Internal functions for the blimp.run() function ------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Run Blimp ####

.blimp.source <- function(target, Blimp, posterior, folder, format, clear) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## File Name, Blimp Path, and Plot Folder ####

  # File name
  base <- sub("\\.imp", "", basename(target))

  # Path name
  dirnam <- dirname(target)

  # Blimp path
  blimp_path <- Blimp

  # Preprocess path for non-UNIX platform
  if (isTRUE(.Platform$OS.type != "unix")) { blimp_path <- paste0('"', blimp_path, '"') }

  # Make Posterior folder
  if (isTRUE(posterior)) { dir.create(file.path(dirnam, paste0(folder, base)), showWarnings = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Modify Input ####

  if (isTRUE(posterior)) {

    target.read <- suppressWarnings(readLines(target))

    suppressWarnings(writeLines(target.read |>
                       (\(x) c(x, paste("SAVE:\n",
                                        if (isTRUE(any(grepl("BYGROUP:", x, ignore.case = TRUE)))) {

                                          paste0("estimates = ", folder, base, "/estimates*.csv;\n")

                                        } else {

                                          paste0("estimates = ", folder, base, "/estimates.csv;\n")

                                        },
                                        paste0("iterations = ", folder, base, "/iter*.csv;\n"))))(), target))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Make Command ####

  if (isTRUE(posterior)) {

    cmd <- paste(blimp_path, shQuote(target), "--traceplot",  shQuote(file.path(paste0(folder, base))), "--truncate 100", "--output", shQuote(file.path(dirnam, paste0(base, ".blimp-out"))))

  } else {

    cmd <- paste(blimp_path, shQuote(target), "--truncate 100", "--output", shQuote(file.path(dirnam, paste0(base, ".blimp-out"))))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Run Command ####

  out <- tryCatch(system(cmd), error = function(y) { stop("Running Blimp failed.", call. = FALSE) })

  # ERROR message
  if (isTRUE(out != 0)) {

    sink(file.path(dirnam, paste0(base, ".blimp-out")), append = TRUE)

    cat("ERROR:\n\n",
        suppressWarnings(system(cmd, intern = TRUE)) |> (\(y) paste("", sub("ERROR: ", "", y)))())

    sink()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Post-Process Posterior Data ####

  if (isTRUE(posterior && file.exists(paste0(folder, base, "/labels.dat")))) {

    if (isTRUE(all(!grepl("BYGROUP:", target.read, ignore.case = TRUE)))) {

      param <- chain <- iter <- latent1 <- latent2 <- latent3 <- NULL

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Message ####

      cat("Saving posterior distribution for all parameters, this may take a while.\n")

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Labels ####

       labels <- read.table(paste0(folder, base, "/labels.dat"), header = TRUE) |>
         setNames(object = _, nm = c("latent1", "latent2", "latent3", "param")) |>
         (\(z) within(z, param <- paste0("p", param)))()

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Burn-In Data ####

      # Data in long format
      burnin <- lapply(list.files(paste0(folder, base), pattern = "burn", full.names = TRUE), function(y) {

        read.csv(y, header = FALSE) |>
           setNames(object = _, nm = c("iter", labels$param)) |>
           (\(z) reshape(z, varying = list(colnames(z)[-1L]), v.names = "value", idvar = colnames(z)[1L], times = colnames(z)[-1], direction = "long"))()

      })

      # Add chain
      for (i in seq_along(burnin)) { burnin[[i]] <- data.frame(chain = i, burnin[[i]]) }

      # Row bind
      burnin <- do.call(rbind, burnin)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Post-Burn-In Data ####

      # Data in long format
      postburn <- lapply(list.files(paste0(folder, base), pattern = "iter", full.names = TRUE), function(y) {

        read.csv(y, header = FALSE) |>
           setNames(object = _, nm = labels$param) |>
           (\(z) data.frame(iter = max(burnin$iter) + 1L:nrow(z), z))() |>
           (\(w) reshape(w, varying = list(colnames(w)[-1L]), v.names = "value", idvar = colnames(w)[1L], times = colnames(w)[-1], direction = "long"))()

      })

      # Add chain
      for (i in seq_along(postburn)) { postburn[[i]] <- data.frame(chain = i, postburn[[i]]) }

      # Row bind
      postburn <- do.call(rbind, postburn)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Combine Burn-in Data and Post-Burn-In Data ####

      # Row bind and merge with labels
      posterior <- misty::df.rename(rbind(data.frame(postburn = 0L, burnin), data.frame(postburn = 1L, postburn)), from = "time", to = "param") |>
         (\(z) misty::df.move(latent1, latent2, latent3, data = merge(x = z, labels, by = "param"), after = "param"))() |>
         (\(w) misty::df.sort(w, param, chain, iter))() |>
         (\(q) within(q, param <- as.numeric(sub("p", "", param))))()

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Save Data ####

      if (isTRUE("csv" %in% format)) { utils::write.csv(posterior, file.path(paste0(folder, base), "posterior.csv"), row.names = FALSE) }

      if (isTRUE("csv2" %in% format)) { utils::write.csv2(posterior, file.path(paste0(folder, base), "posterior.csv"), row.names = FALSE) }

      if (isTRUE("xlsx" %in% format)) { misty::write.xlsx(posterior, file.path(paste0(folder, base), "posterior.xlsx")) }

      if (isTRUE("rds" %in% format)) { base::saveRDS(posterior, file = file.path(paste0(folder, base), "posterior.rds")) }

      if (isTRUE("RData" %in% format)) { base::save(posterior, file = file.path(paste0(folder, base), "posterior.RData")) }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Save Parameter Table ####

      labels[, c("param", "latent1", "latent2", "latent3")] |>
        (\(z) within(z, param <- as.numeric(gsub("p", "", param))))() |>
        (\(w) format(rbind(c("Param", "L1", "L2", "L3"), w), justify = "left"))() |>
        write.table(x = _, file = paste0(folder, base, "/partable.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Post-Process Estimates ####

      write.csv(read.csv(paste0(folder, base, "/estimates.csv"), check.names = FALSE) |>
         (\(z) data.frame(param = as.numeric(sub("p", "", labels$param)), latent1 = labels$latent1, latent2 = labels$latent2, latent3 = labels$latent3, z, check.names = FALSE))(), paste0(folder, base, "/estimates.csv"), row.names = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Clear Console ####

  if (isTRUE(clear && .Platform$GUI == "RStudio")) { misty::clear() }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Detect Blimp Location ####

.detect.blimp <- function(exec = "blimp") {

  env_r_blimp <- Sys.getenv("R_BLIMP", unset = NA)

  if (isTRUE(!is.na(env_r_blimp))) { if (isTRUE(file.exists(env_r_blimp) & !dir.exists(env_r_blimp))) { return(env_r_blimp) } }

  user_os <- tolower(R.Version()$os)

  # Mac
  if (isTRUE(grepl("darwin", user_os))) {

    return(.detect_blimp_macos(exec))

  # Linux
  } else if (isTRUE(grepl("linux", user_os))) {

    return(.detect_blimp_linux(exec))

  # Windows
  } else if (isTRUE(grepl("windows", user_os) || grepl("mingw32", user_os))) {

    return(.detect_blimp_windows(paste0(exec, ".exe")))

  }

  stop("Unable to detect the operating system.", call. = FALSE)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Find Blimp on MacOS ####

.detect_blimp_macos <- function(exec) {

  output <- suppressWarnings(system(paste("which", exec), intern = TRUE, ignore.stderr = TRUE)[1L])

  if (isTRUE(length(output) != 0L)) { if (isTRUE(file.exists(output))) { return(output) } }

  output <- paste0("/Applications/Blimp/", exec)

  if (isTRUE(file.exists(output))) { return(output) }

  output <- paste0("~", output)

  if (isTRUE(file.exists(output))) { return(output) }

  stop("Unable to find blimp executable, make sure Blimp is installed.", call. = FALSE)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Find Blimp on Linux ####

.detect_blimp_linux <- function(exec) {

  output <- suppressWarnings(system(paste("which", exec), intern = TRUE, ignore.stderr = TRUE)[1L])

  if (isTRUE(length(output) != 0L)) { if (isTRUE(file.exists(output))) { return(output) }  }

  stop("Unable to find blimp executable, ,ake sure Blimp is installed.", call. = FALSE)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Find Blimp on Windows ####

.detect_blimp_windows <- function(exec) {

  output <- suppressWarnings(system(paste("where", exec), intern = TRUE, ignore.stderr = TRUE))

  if (isTRUE(length(output) != 0L)) { if (isTRUE(file.exists(output))) { return(output) } }

  output <- paste0("C:\\Program Files\\Blimp\\", exec)

  if (isTRUE(file.exists(output))) { return(output) }

  output <- paste0("D:\\Program Files\\Blimp\\", exec)

  if (isTRUE(file.exists(output))) { return(output) }

  stop("Unable to find blimp executable, ,ake sure Blimp is installed.", call. = FALSE)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the blimp.bayes() function ----------------------------
#                            mplus.bayes() function ----------------------------
#
# - .map
# - .hdi
# - .rhat
# - .split.chains
# - .zscale
# - .autocovariance
# - .ess
# - .mcse
#
# rstan package
# https://github.com/stan-dev/rstan/blob/develop/rstan/rstan/R/monitor.R
#
# bayestestR package
# https://github.com/easystats/bayestestR/blob/main/R/map_estimate.R
# https://github.com/easystats/bayestestR/blob/main/R/hdi.R

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified map_estimate Function from the bayestestR Package ####
#
# Maximum A Posteriori probability estimate

.map <- function(x) {

  x.density <- density(na.omit(x), n = 2L^10L, bw = "SJ", from = range(x)[1L], to = range(x)[2L])
  x.density$x[which.max(x.density$y)]

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified hdi Function from the bayestestR Package ####
#
# Highest density interval

.hdi <- function(x, conf.level = NULL) {

  x.sorted <- unname(sort.int(x, method = "quick"))

  window.size <- ceiling(conf.level * length(x.sorted))

  if (isTRUE(window.size < 2L)) { return(data.frame(low = NA, upp = NA)) }

  nCIs <- length(x.sorted) - window.size

  if (isTRUE(nCIs < 1L)) { return(data.frame(low = NA, upp = NA)) }

  ci.width <- sapply(seq_len(nCIs), function(y) x.sorted[y + window.size] - x.sorted[y])

  min.i <- which(ci.width == min(ci.width))

  if (isTRUE(length(min.i) > 1L)) {

    if (isTRUE(any(diff(sort(min.i)) != 1L))) {

      min.i <- max(min.i)

    } else {

      min.i <- floor(mean(min.i))

    }

  }

  c(low = x.sorted[min.i], upp = x.sorted[min.i + window.size])

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified rhat_rfun Function from the rstan Package ####
#
# Compute the Rhat convergence diagnostic for a single parameter

.rhat <- function(x, fold, split, rank) {

  # One chain
  if (isTRUE(is.vector(x))) { dim(x) <- c(length(x), 1L) }

  # Fold
  if (isTRUE(fold)) { x <-  abs(x - median(x)) }

  # Split chains
  if (isTRUE(split)) { x <- .split.chains(x) }

  # Rank-normalization
  if (isTRUE(rank)) { x <- .zscale(x) }

  # Number of iterations
  n.iter <- nrow(x)

  # Compute R hat
  rhat <- sqrt((n.iter * var(colMeans(x)) / mean(apply(x, 2L, var)) + n.iter - 1L) / n.iter)

  # Return value
  return(rhat)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified split_chains Function from the rstan Package ####
#
# Split Markov Chains

.split.chains <- function(x) {

  # One chain
  if (isTRUE(is.vector(x))) { dim(x) <- c(length(x), 1L) }

  # Number of iterations
  n.iter <- nrow(x)

  # Combine by columns
  x <- cbind(x[1L:floor(n.iter / 2L), ], x[ceiling(n.iter / 2L + 1L):n.iter, ])

  # Return value
  return(x)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified z_scale Function from the rstan Package ####
#
# Compute rank normalization for a numeric array

.zscale <- function(x) {

  z <- qnorm((rank(x, ties.method = "average") - 1L / 2L) / length(x))

  z[is.na(x)] <- NA

  if (!is.null(dim(x))) { z <- array(z, dim = dim(x), dimnames = dimnames(x)) }

  return(z)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified autocovariance Function from the rstan Package ####
#
# Compute autocorrelation estimates for every lag for the specified input sequence
# using a fast Fourier transform approach

.autocovariance <- function(x) {

  N <- length(x)

  varx <- var(x)

  if (isTRUE(varx == 0L)) { return(rep(0L, N)) }

  ac <- Re(fft(abs(fft(c((x - mean(x)), rep.int(0L, (2L * nextn(N)) - N))))^2L, inverse = TRUE)[1L:N])

  ac <- ac / ac[1L] * varx * (N - 1L) / N

  return(ac)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified ess_rfun Function from the rstan Package ####
#
# Compute the effective sample size estimate for a sample of several chains for
# one parameter

.ess <- function(x, split, rank) {

  # One chain
  if (isTRUE(is.vector(x))) { dim(x) <- c(length(x), 1L) }

  # Split chains
  if (isTRUE(split)) { x <- .split.chains(x) }

  # Rank-normalization
  if (isTRUE(rank)) { x <- .zscale(x) }

  chains <- ncol(x)
  n_samples <- nrow(x)

  if (isTRUE(n_samples < 3L)) { return(NA_real_) }

  acov <- do.call(cbind, lapply(seq_len(chains), function(y) .autocovariance(x[, y])))

  mean_var <- mean(acov[1L, ]) * n_samples / (n_samples - 1L)

  var_plus <- mean_var * (n_samples - 1L) / n_samples

  if (isTRUE(chains > 1L)) { var_plus <- var_plus + var(colMeans(x)) }

  rho_hat_t <- rep.int(0L, n_samples)
  t <- 0L
  rho_hat_even <- 1L
  rho_hat_t[t + 1L] <- rho_hat_even
  rho_hat_odd <- 1L - (mean_var - mean(acov[t + 2L, ])) / var_plus
  rho_hat_t[t + 2L] <- rho_hat_odd

  while (isTRUE(t < nrow(acov) - 5L && !is.nan(rho_hat_even + rho_hat_odd) && (rho_hat_even + rho_hat_odd > 0L))) {

    t <- t + 2L
    rho_hat_even <- 1L - (mean_var - mean(acov[t + 1L, ])) / var_plus
    rho_hat_odd <- 1L - (mean_var - mean(acov[t + 2L, ])) / var_plus

    if (isTRUE((rho_hat_even + rho_hat_odd) >= 0L)) {

      rho_hat_t[t + 1L] <- rho_hat_even
      rho_hat_t[t + 2L] <- rho_hat_odd

    }

  }

  max_t <- t

  if (isTRUE(rho_hat_even > 0L)) { rho_hat_t[max_t + 1L] <- rho_hat_even }

  t <- 0L
  while (isTRUE(t <= max_t - 4L)) {

    t <- t + 2L

    if (isTRUE(rho_hat_t[t + 1L] + rho_hat_t[t + 2L] > rho_hat_t[t - 1L] + rho_hat_t[t])) {

      rho_hat_t[t + 1L] = (rho_hat_t[t - 1L] + rho_hat_t[t]) / 2L
      rho_hat_t[t + 2L] = rho_hat_t[t + 1L]

    }

  }

  ess <- chains * n_samples

  tau_hat <- -1L + 2L * sum(rho_hat_t[1L:max_t]) + rho_hat_t[max_t + 1L]

  tau_hat <- max(tau_hat, 1L/log10(ess))

  ess <- ess / tau_hat

  return(ess)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified conv_quantile and  mcse_mean Function from the rstan Package ####
#
# Compute Monte Carlo standard error for the mean and a quantile

.mcse <- function(x, quant = FALSE, prob = NULL, split = TRUE, rank = TRUE) {

  # MCSE for a quantile
  if (isTRUE(quant)) {

    ess <- .ess(x <= quantile(x, prob), split = split, rank = rank)

    a <- qbeta(c(0.1586553, 0.8413447), ess * prob + 1L, ess * (1L - prob) + 1L)
    x.sort <- sort(x)
    S <- length(x.sort)

    mcse <- (x.sort[min(round(a[2L] * S), S)] - x.sort[max(round(a[1L] * S), 1L)] ) / 2L

  # MCSE for the mean
  } else {

    mcse <- sd(x) / sqrt(.ess(x, split = split, rank = rank))

  }

  return(mcse)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the check.resid() function ----------------------------
#
# - .resid.partial
# - .calc_ranef
#
# remef: Remove Partial Effects
# https://github.com/hohenstein/remef/

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculate partial effects ####
#
# Adapted function partial()

.resid.partial <- function(model, fix = NULL, ran = "all") {

  #---------------------------------------------------------------------------
  # Part 1: Fixed Effects

  is.na(match(fix, colnames(model.matrix(model)) |> (\(y) if (isTRUE(as.logical(attr(terms(model), "intercept")))) { y[-1L] } else { y })())) |> (\(y) if (isTRUE(any(y))) { stop("The following effects are not present in the model: ", paste(fix[y], collapse = ", "), call. = FALSE) })()

  DV <- lme4::getME(model, "y") - as.vector(model.matrix(model)[ , fix, drop = FALSE] %*% lme4::fixef(model)[fix])

  #---------------------------------------------------------------------------
  # Part 2: Random effects

  DV <- DV - .calc_ranef(model, lapply(lapply(lme4::ranef(model), names), seq_along))

  return(unname(DV))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculate random effects based on a subset of variance terms ####
#
# Adapted function calc_ranef()

.calc_ranef <- function(model, ran) {

  if (isTRUE(length(ran) == 0L)) { return(numeric(length = lme4::getME(model, "n"))) }

  if (isTRUE(is.list(ran) && length(ran) > 0L)) {

    stopifnot(is.numeric(unlist(ran)))

    ran <- lapply(ran, unique)
    ran <- ran[vapply(ran, length, 1L) > 0L]

  }

  ran_labs <- lapply(lme4::ranef(model), names)

  if (isTRUE(is.null(names(ran)) || any( !names(ran) %in% names(ran_labs)) || any(duplicated(names(ran))))) { stop("The list 'ran' has invalid names.", call. = FALSE) }

  num_re <- vapply(ran_labs, length, FUN.VALUE = 1L, USE.NAMES = FALSE)
  num_re_previous <- c(0L, cumsum(head(num_re, -1L)))
  rf_idx <- match(names(ran), names(ran_labs))
  ran_model <- lme4::ranef(model)

  ran_model_sub <- lapply(mapply(`[`, ran_model[rf_idx], ran, SIMPLIFY = FALSE), unlist)

  re_list <- lapply(mapply(function(x, y) seq_len(x) + y, num_re, num_re_previous, SIMPLIFY = FALSE), function(x) lme4::getME(model, "Ztlist")[x])

  re_list_sub <- mapply(`[`, re_list[rf_idx], ran, SIMPLIFY = FALSE)
  ran_model_sub <- mapply(`[`, ran_model[rf_idx], ran, SIMPLIFY = FALSE)

  ranef_sums <- rowSums(do.call(cbind, mapply(function(mats, vecs) mapply(function(v, m) as.vector(v %*% m), vecs, mats), re_list_sub, ran_model_sub, SIMPLIFY = FALSE)))

  return(ranef_sums)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the chr.gsub() function -------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fast escape replace ####
#
# Fast escape function for limited case where only one pattern
# provided actually matches anything
#
# Argument string: a character vector where replacements are sought
# Argument pattern: a character string to be matched in the given character vector
# Argument replacement: Character string equal in length to pattern or of length
#                       one which are a replacement for matched pattern.
# Argument ...: arguments to pass to gsub()
.fastReplace <- function(string, pattern, replacement, ...) {

  for (i in seq_along(pattern)) {

    string <- gsub(pattern[i], replacement[i], string, ...)

  }

  return(string)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Filter overlaps from matches ####
#
# Helper function used to identify which results from gregexpr()
# overlap other matches and filter out shorter, overlapped results
#
# Argument x: a matrix of gregexpr() results, 4 columns, index of column matched,
#             start of match, length of match, end of match. Produced exclusively from
#             a .worker function in chr.gsub

.filterOverlap <- function(x) {

  for (i in nrow(x):2L) {

    s <- x[i, 2L]
    ps <- x[1L:(i - 1L), 2L]
    e <- x[i, 4]
    pe <- x[1L:(i - 1L), 4L]

    if (any(ps <= s & pe >= s)){

      x <- x[-i, ]
      next

    }

    if (any(ps <= e & pe >= e)) {

      x <- x[-i,]

      next

    }

  }

  return(matrix(x, ncol = 4L))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Get all matches ####
#
# Helper function to be used in a loop to check each pattern
# provided for matches
#
# Argument string: a character vector where replacements are sought
# Argument pattern: a character string to be matched in the given character vector
# Argument i: an iterator provided by a looping function
# Argument ...: arguments to pass to gregexpr()
.getMatches <- function(string ,pattern, i, ...){

  tmp <- gregexpr(pattern[i], string,...)
  start <- tmp[[1L]]
  length <- attr(tmp[[1L]], "match.length")
  return(matrix(cbind(i, start, length, start + length - 1L), ncol = 4L))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## chr.gsub() .worker ####
#
# Argument string: a character vector where replacements are sought
# Argument pattern: a character string to be matched in the given character vector
# Argument replacement: a character string equal in length to pattern or of length
#                       one which are a replacement for matched pattern.
# Argument ...: arguments to pass to regexpr family
.worker <- function(string, pattern, replacement,...){

  x0 <- do.call(rbind, lapply(seq_along(pattern), .getMatches, string = string, pattern = pattern, ...))
  x0 <- matrix(x0[x0[, 2L] != -1L, ], ncol = 4L)

  uid <- unique(x0[, 1L])
  if (nrow(x0) == 0L) {

    return(string)

  }

  if (length(unique(x0[, 1])) == 1L) {

    return(.fastReplace(string, pattern[uid], replacement[uid], ...))

  }

  if (nrow(x0) > 1L) {

    x <- x0[order(x0[, 3L], decreasing = TRUE), ]
    x <- .filterOverlap(x)
    uid <- unique(x[, 1L])

    if (length(uid) == 1L) {

      return(.fastReplace(string, pattern[uid], replacement[uid], ...))

    }

    x <- x[order(x[, 2L]), ]
  }

  for (i in nrow(x):1L){

    s <- x[i, 2L]
    e <- x[i, 4L]
    p <- pattern[x[i, 1L]]
    r <- replacement[x[i, 1L]]

    pre <- if (s > 1L) { substr(string, 1L, s - 1L) } else { "" }
    r0 <- sub(p,r,substr(string, s, e), ...)
    end <- if (e < nchar(string)) { substr(string, e + 1, nchar(string)) } else { "" }
    string <- paste0(pre, r0, end)

  }

  return(string)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the ci.cor() function ---------------------------------
#
# - .multsolvefun
# - .sqerrVMc
# - .sqerrVMintr
# - .VMparstomoms
# - .Tf2fun
# - .smpmomvecfun
# - .smpmjkfun
# - .zciofrfun
# - .estskkufun
# - .ci.pearson.cor.adjust
# - .ci.spearman.cor.adjust
# - .ci.kendall.b
# - .ci.kendall.c
# - .ci.kendall.c.estimate
# - .norm.inter
# - .boot.func.cor
# - .ci.boot.cor
#
# Bishara et al. (2018) Supporting Information
# https://bpspsychub.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fbmsp.12113&file=bmsp12113-sup-0002-DataS2.txt

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Multiple attempts to find 3rd order polynomial parameters ####

.multsolvefun <- function(xskku, yskku, obsr, seed = NULL, maxtol = 0.0001, nudge = 0.01, tryrndpars = 5L) {

  if (isTRUE(!is.null(seed))) { set.seed(seed) }

  failsolve <- FALSE
  failcountvec <- nudgesmadevec <- rep(0L, 3L)
  xskku.small <- xskku
  yskku.small <- yskku
  obsr.small <- obsr

  Xmod <- try(stats::optim(c(1L, 0L, 0L), .sqerrVMc, sk = xskku[1L], ku = xskku[2L], method = "N"))

  if (isTRUE(Xmod$value > maxtol)) { failsolve <- TRUE }

  while (isTRUE(failsolve)) {

    failcountvec <- failcountvec + c(1L, 0L, 0L)
    randpars <- (stats::runif(3L) - 0.5)*c(3L, 1L, 0.5)
    if (isTRUE((failcountvec[1L] %% tryrndpars == 1L) & (failcountvec[1L] > 1L))) {

      nudgesmadevec <- nudgesmadevec + c(1L, 0L, 0L)
      nudgemult <- 1L - nudge*nudgesmadevec[1L]
      xskku.small <- xskku*nudgemult

    }

    Xmod <- try(optim(randpars, .sqerrVMc, sk = xskku.small[1L], ku = xskku.small[2L], method = "N"))
    if(isTRUE((Xmod$value < maxtol) | (nudgesmadevec[1L] >= 100L))) { failsolve <- FALSE }

  }

  Ymod <- try(optim(c(1L, 0L, 0L), .sqerrVMc, sk = yskku[1L], ku = yskku[2L], method = "N"))
  if (isTRUE(Ymod$value > maxtol)) { failsolve <- TRUE }

  while (isTRUE(failsolve)) {

    failcountvec <- failcountvec + c(0L, 1L, 0L)
    randpars <- (runif(3L) - 0.5)*c(3L, 1L, 0.5)
    if (isTRUE((failcountvec[2] %% tryrndpars == 1) & (failcountvec[2] > 1))) {

      nudgesmadevec <- nudgesmadevec + c(0L, 1L, 0L)
      nudgemult <- 1L - nudge*nudgesmadevec[2L]
      yskku.small <- yskku*nudgemult

    }

    Ymod <- try(optim(randpars, .sqerrVMc, sk = yskku.small[1L], ku = yskku.small[2L], method = "N"))
    if (isTRUE((Ymod$value < maxtol) | (nudgesmadevec[2L] >= 100L))) { failsolve <- FALSE }

  }

  estconstvec <- c(Xmod$par,Ymod$par)
  if (isTRUE(nudgesmadevec[1L] >= 100)) { estconstvec[1L:3L] <- c(1L, 0L, 0L) }
  if (isTRUE(nudgesmadevec[2L] >= 100)) { estconstvec[4L:6L] <- c(1L, 0L, 0L) }

  intrmod <- try(optimize(.sqerrVMintr, cpars = estconstvec, obsr = obsr, interval = c(-1L, 1L)))
  if (isTRUE(intrmod$objective > maxtol)) { failsolve <- TRUE }

  while (isTRUE(failsolve)) {

    failcountvec <- failcountvec + c(0L, 0L, 1L)
    nudgesmadevec <- nudgesmadevec + c(0L, 0L, 1L)
    nudgemult <- 1L - nudge*nudgesmadevec[3L]
    obsr.small <- obsr*nudgemult
    intrmod <- try(optimize(.sqerrVMintr, cpars = estconstvec, obsr = obsr.small, interval = c(-1L, 1L)))
    if (isTRUE((intrmod$objective < maxtol) | (nudgesmadevec[3L] >= 100L))) { failsolve <- FALSE }

  }

  if (isTRUE(nudgesmadevec[3L] < 100L)) {

    intr <- intrmod$minimum

  } else {

    intr <- 0L

  }

  estconstmat <- rbind(estconstvec[1L:3L], estconstvec[4L:6L])
  rownames(estconstmat) <- c("X","Y")
  colnames(estconstmat) <- c("b","c","d")

  return(list(estxyc = estconstmat, intr = intr, totsqerr = Xmod$value + Ymod$value + intrmod$objective, failed = failcountvec, nudgesmade = nudgesmadevec))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Squared error of 3rd order polynomial parameters ####

.sqerrVMc <- function(trypars, sk, ku) {

  devvec <- rep(NA, 3L)
  b  <- trypars[1L]
  c1 <- trypars[2L]
  d  <- trypars[3L]

  devvec[1L] <- b^2L + 6L*b*d + 2L*c1^2L + 15L*d^2L - 1L
  devvec[2L] <- 2L*c1*(b^2L + 24L*b*d + 105L*d^2L + 2L) - sk
  devvec[3L] <- 24L*(b*d + c1^2L*(1 + b^2L + 28L*b*d) + d^2L*(12L + 48L*b*d + 141L*c1^2L + 225L*d^2L)) - ku

  return(sum(devvec^2L))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Squared error for intermediate correlation parameter ####

.sqerrVMintr <- function(tryintr, cpars, obsr) {

  b1 <- cpars[1L]
  c1 <- cpars[2L]
  d1 <- cpars[3L]
  b2 <- cpars[4L]
  c2 <- cpars[5L]
  d2 <- cpars[6L]

  return((((b1*b2 + 3L*b1*d2 + 3L*d1*b2 + 9L*d1*d2)*tryintr) + ((2L*c1*c2)*tryintr^2L) + ((6L*d1*d2)*tryintr^3L) - obsr)^2L)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Third order polynomial parameters to analytically solve ####

.VMparstomoms <- function(consX, consY, intr) {

  Power <- function(g, h) { g^h }

  b1 <- consX[1L]
  c1 <- consX[2L]
  d1 <- consX[3L]
  b2 <- consY[1L]
  c2 <- consY[2L]
  d2 <- consY[3L]
  r <- intr

  VMm22 <- Power(b1, 2L)*Power(b2, 2L) +
    2L*Power(r, 2L)*Power(b1, 2L)*Power(b2, 2L) +
    2L*Power(b2, 2L)*Power(c1, 2L) +
    8L*Power(r, 2L)*Power(b2, 2L)*Power(c1, 2L) +
    16L*r*b1*b2*c1*c2 +
    24L*Power(r, 3L)*b1*b2*c1*c2 +
    2L*Power(b1, 2L)*Power(c2, 2L) +
    8L*Power(r, 2L)*Power(b1, 2L)*Power(c2, 2L) +
    4L*Power(c1, 2L)*Power(c2, 2L) +
    32L*Power(r, 2L)*Power(c1, 2L)*Power(c2, 2L) +
    24L*Power(r, 4L)*Power(c1, 2L)*Power(c2, 2L) +
    6L*b1*Power(b2, 2L)*d1 +
    24L*Power(r, 2L)*b1*Power(b2, 2L)*d1 +
    96L*r*b2*c1*c2*d1 +
    216L*Power(r, 3L)*b2*c1*c2*d1 +
    12L*b1*Power(c2, 2L)*d1 +
    96L*Power(r, 2L)*b1*Power(c2, 2L)*d1 +
    48L*Power(r, 4L)*b1*Power(c2, 2L)*d1 +
    15L*Power(b2, 2L)*Power(d1, 2L) +
    90L*Power(r, 2L)*Power(b2, 2L)*Power(d1, 2L) +
    30L*Power(c2, 2L)*Power(d1, 2L) +
    360L*Power(r, 2L)*Power(c2, 2L)*Power(d1, 2L) +
    360L*Power(r, 4L)*Power(c2, 2L)*Power(d1, 2L) +
    6L*Power(b1, 2L)*b2*d2 +
    24L*Power(r, 2L)*Power(b1, 2L)*b2*d2 +
    12L*b2*Power(c1, 2L)*d2 +
    96L*Power(r, 2L)*b2*Power(c1, 2L)*d2 +
    48L*Power(r, 4L)*b2*Power(c1, 2L)*d2 +
    96L*r*b1*c1*c2*d2 +
    216L*Power(r, 3L)*b1*c1*c2*d2 +
    36L*b1*b2*d1*d2 +
    288L*Power(r, 2L)*b1*b2*d1*d2 +
    96L*Power(r, 4L)*b1*b2*d1*d2 +
    576L*r*c1*c2*d1*d2 +
    1944L*Power(r, 3L)*c1*c2*d1*d2 +
    480L*Power(r, 5L)*c1*c2*d1*d2 +
    90L*b2*Power(d1, 2L)*d2 +
    1080L*Power(r, 2L)*b2*Power(d1, 2L)*d2 +
    720L*Power(r, 4L)*b2*Power(d1, 2L)*d2 +
    15L*Power(b1, 2L)*Power(d2, 2L) +
    90L*Power(r, 2L)*Power(b1, 2L)*Power(d2, 2L) +
    30L*Power(c1, 2L)*Power(d2, 2L) +
    360L*Power(r, 2L)*Power(c1, 2L)*Power(d2, 2L) +
    360L*Power(r, 4L)*Power(c1, 2L)*Power(d2, 2L) +
    90L*b1*d1*Power(d2, 2L) +
    1080L*Power(r, 2L)*b1*d1*Power(d2, 2L) +
    720L*Power(r, 4L)*b1*d1*Power(d2, 2L) +
    225L*Power(d1, 2L)*Power(d2, 2L) +
    4050L*Power(r, 2L)*Power(d1, 2L)*Power(d2, 2L) +
    5400L*Power(r, 4L)*Power(d1, 2L)*Power(d2, 2L) +
    720L*Power(r, 6L)*Power(d1, 2L)*Power(d2, 2L)

  VMm13 <- 3L*r*b1*Power(b2, 3L) +
    30L*Power(r, 2L)*Power(b2, 2L)*c1*c2 +
    30L*r*b1*b2*Power(c2, 2L) +
    60L*Power(r, 2L)*c1*Power(c2, 3L) +
    9L*r*Power(b2, 3L)*d1 +
    6L*Power(r, 3L)*Power(b2, 3L)*d1 +
    90L*r*b2*Power(c2, 2L)*d1 +
    144L*Power(r, 3L)*b2*Power(c2, 2L)*d1 +
    45L*r*b1*Power(b2, 2L)*d2 +
    468L*Power(r, 2L)*b2*c1*c2*d2 +
    234L*r*b1*Power(c2, 2L)*d2 +
    135L*r*Power(b2, 2L)*d1*d2 +
    180L*Power(r, 3L)*Power(b2, 2L)*d1*d2 +
    702L*r*Power(c2, 2L)*d1*d2 +
    1548L*Power(r, 3L)*Power(c2, 2L)*d1*d2 +
    315L*r*b1*b2*Power(d2, 2L) +
    2250L*Power(r, 2L)*c1*c2*Power(d2, 2L) +
    945L*r*b2*d1*Power(d2, 2L) +
    1890L*Power(r, 3L)*b2*d1*Power(d2, 2L) +
    945L*r*b1*Power(d2, 3L) +
    2835L*r*d1*Power(d2, 3L) +
    7560L*Power(r, 3L)*d1*Power(d2, 3L)

  VMm31 <- 3L*r*Power(b1, 3L)*b2 +
    30L*r*b1*b2*Power(c1, 2L) +
    30L*Power(r, 2L)*Power(b1, 2L)*c1*c2 +
    60L*Power(r, 2L)*Power(c1, 3L)*c2 +
    45L*r*Power(b1, 2L)*b2*d1 +
    234L*r*b2*Power(c1, 2L)*d1 +
    468L*Power(r, 2L)*b1*c1*c2*d1 +
    315L*r*b1*b2*Power(d1, 2L) +
    2250L*Power(r, 2L)*c1*c2*Power(d1, 2L) +
    945L*r*b2*Power(d1, 3L) +
    9L*r*Power(b1, 3L)*d2 +
    6L*Power(r, 3L)*Power(b1, 3L)*d2 +
    90L*r*b1*Power(c1, 2L)*d2 +
    144L*Power(r, 3L)*b1*Power(c1, 2L)*d2 +
    135L*r*Power(b1, 2L)*d1*d2 +
    180L*Power(r, 3L)*Power(b1, 2L)*d1*d2 +
    702L*r*Power(c1, 2L)*d1*d2 +
    1548L*Power(r, 3L)*Power(c1, 2L)*d1*d2 +
    945L*r*b1*Power(d1, 2L)*d2 +
    1890L*Power(r, 3L)*b1*Power(d1, 2L)*d2 +
    2835L*r*Power(d1, 3L)*d2 +
    7560L*Power(r, 3L)*Power(d1, 3L)*d2

  VMm40 <- 936L*b1*c1^2L*d1 + 60L*b1^2L*c1^2L + 60L*b1^3L*d1 + 630L*b1^2L*d1^2L + 3780L*b1*d1^3L + 3L*b1^4L + 4500L*c1^2L*d1^2L + 60L*c1^4L + 10395L*d1^4L
  VMm04 <- 936L*b2*c2^2L*d2 + 60L*b2^2L*c2^2L + 60L*b2^3L*d2 + 630L*b2^2L*d2^2L + 3780L*b2*d2^3L + 3L*b2^4L + 4500L*c2^2L*d2^2L + 60L*c2^4L + 10395L*d2^4L

  return(c(VMmu04 = VMm04, VMmu13 = VMm13, VMmu22 = VMm22, VMmu31 = VMm31, VMmu40 = VMm40))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Hawkins (1989) tau_f^2 term ####

.Tf2fun <- function(rho, mujkvec)  {

  m04 <- mujkvec[1L]
  m13 <- mujkvec[2L]
  m22 <- mujkvec[3L]
  m31 <- mujkvec[4L]
  m40 <- mujkvec[5L]

  return(unname((((1L - rho^2L)^(-2L))*0.25)*((m40 + 2L*m22 + m04)*(rho^2L) - 4L*(m31 + m13)*rho + 4L*m22)))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Vector of relevant sample joint moments ####

.smpmomvecfun <- function(x, y) {

  momvec <- rep(NA, 5L)
  xcent <- x - mean(x)
  ycent <- y - mean(y)
  xstand <- scale(x)
  ystand <- scale(y)

  for (i in 0L:4L) {

    momvec[i + 1L] <- .smpmjkfun(xstand, ystand, i, 4L - i)
    names(momvec)[i + 1L] <- paste0("m", i, 4L - i)

  }

  return(momvec)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sample joint moment ####

.smpmjkfun <- function(x, y, j, k) {

  (1L / length(x))*sum((x^j)*(y^k))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CI of correlation using mean and standard error of z ####

.zciofrfun <- function(z, zsd, alternative, conf.level) {

  if (isTRUE(alternative == "two.sided")) {

    alphavec <- c((1L - conf.level) / 2L,  (1L + conf.level) / 2L)

  } else {

    alphavec <- c((1L - conf.level), conf.level)

  }

  object <- tanh(z + c(qnorm(alphavec[1L]), qnorm(alphavec[2L]))*zsd)

  if (isTRUE(alternative != "two.sided")) { switch(alternative, less = { object[1L] <- -1L }, greater = { object[2L] <- 1L }) }

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Skewness and kurtosis in a sample ####

.estskkufun <- function(x, sample = TRUE, center = TRUE) {

  return(c(g1 = misty::skewness(x, sample = sample, check = FALSE), g2 = misty::kurtosis(x, sample = sample, center = center, check = FALSE)))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Main function to estimate confidence intervals for Pearson correlation ####

.ci.pearson.cor.adjust <- function(x, y, adjust = c("none", "joint", "approx"), alternative = c("two.sided", "less", "greater"),
                                   conf.level = 0.95, sample = TRUE, center = TRUE, seed = NULL, maxtol = 0.00001, nudge = 0.001) {

  xy <- na.omit(data.frame(x, y))

  x1 <- xy$x
  x2 <- xy$y

  n <- nrow(xy)
  nNA <- length(x) - n
  pNA <- (nNA / (n + nNA)) * 100

  adjmethnames <- c("none", "joint", "approx")
  boundsmat <- matrix(NA, nrow = 2L, ncol = 3L, dimnames = list(c("low", "upp"), adjmethnames))

  sdzvec <- setNames(rep(NA, length(adjmethnames)), nm = adjmethnames)

  #...................
  ### At least n = 4 and Variance Unequal 0 ####

  if (isTRUE(n >= 4L && var(x1) != 0L && var(x2) != 0L)) {

    r <- cor(x1, x2)
    r.z <- atanh(r)

    #...................
    ### Unadjusted Standard Error ####

    if (isTRUE("none" %in% adjust)) {

      sdzvec[1L] <- 1L / sqrt(n - 3L)

    } else {

      sdzvec[1L] <- NA

    }

    #...................
    ### Adjusted by Sample Joint Moments ####

    if (isTRUE("joint" %in% adjust)) {

      smpmomvec <- .smpmomvecfun(x1, x2)
      jointmomTf2 <- .Tf2fun(rho = r, mujkvec = smpmomvec)
      sdzvec[2L] <- sqrt(jointmomTf2 / (n - 3L))

    } else {

      smpmomvec <- jointmomTf2 <- NA

    }

    #...................
    ### Adjusted by Approximate Distributing using Marginal Skewness and Kurtosis ####

    XYg12mat <- matrix(c(.estskkufun(x1, sample = sample, center = center), .estskkufun(x2, sample = sample, center = center)),
                       nrow = 2L, ncol = 2L, byrow = TRUE, dimnames = list(c("X", "Y"), c("g1", "g2")))

    if (isTRUE("approx" %in% adjust)) {

      VMparlist <- .multsolvefun(xskku = XYg12mat[1L, ], yskku = XYg12mat[2L, ], obsr = r, maxtol = maxtol, nudge = nudge)

      # Optimization succeeded
      if (isTRUE(is.list(VMparlist))) {

        VMmoms <- .VMparstomoms(VMparlist$estxyc[1, ], VMparlist$estxyc[2L, ], VMparlist$intr)
        nudgedrho <- r*(1L - 0.01*VMparlist$nudgesmade[3L])

        ApproxDistTf2 <- .Tf2fun(rho = nudgedrho, mujkvec = VMmoms)

        sdzvec[3L] <- sqrt(ApproxDistTf2 / (n - 3L))

        # Optimization failed
      } else {

        VMmoms <- setNames(rep(NA, 5L), nm = c("VMmu04", "VMmu13", "VMmu22", "VMmu31", "VMmu40"))
        sdzvec[3L] <- ApproxDistTf2 <- NA

      }

    } else {

      VMmoms <- setNames(rep(NA, 5L), nm = c("VMmu04", "VMmu13", "VMmu22", "VMmu31", "VMmu40"))
      sdzvec[3L] <- ApproxDistTf2 <- NA

    }

    #...................
    ### Confidence intervals ####

    for (curadj in seq_len(length(adjmethnames))) {

      boundsmat[, curadj] <- .zciofrfun(r.z, sdzvec[curadj], alternative = alternative, conf.level = conf.level)

    }

    #...................
    ### Return Object ####

    object <- list(alternative = alternative, conf.level = conf.level, n = n,  nNA = nNA, pNA = pNA, skew1 = XYg12mat[1L, "g1"], kurt1 = XYg12mat[1L, "g2"], skew2 = XYg12mat[2L, "g1"],  kurt2 = XYg12mat[2L, "g2"],
                   cor = r, adjust = adjust, se = sdzvec, ci = boundsmat, skew.kurt = XYg12mat, joint.moments = smpmomvec, approx.moments = VMmoms, joint.tau2.f = jointmomTf2, approx.tau2.f = ApproxDistTf2)

    #...................
    ### Number of Cases n < 4 or Variance Equal 0 ####

  } else {

    #...................
    ### Return Object ####

    object <- list(alternative = alternative, conf.level = conf.level, n = n,  nNA = nNA, pNA = pNA,  skew1 = NA, kurt1 = NA,  skew2 = NA, kurt2 = NA,
                   cor = if (isTRUE(nrow(xy) == 3L && var(x1) != 0L && var(x2) != 0L)) { suppressWarnings(cor(x1, x2)) } else { NA }, adjust = adjust, se = sdzvec, ci = boundsmat, skew.kurt = NA, joint.moments = NA, approx.moments = NA, joint.tau2.f = NA, approx.tau2.f = NA)


  }

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## CI for the Spearman Correlation with Fieller et al (1957), Bonett and Wright (2000), and RIN Standard Error ####
#
# Bishara and Hittner (2017) Supplementary Materials A
# https://static-content.springer.com/esm/art%3A10.3758%2Fs13428-016-0702-8/MediaObjects/13428_2016_702_MOESM1_ESM.pdf
#
# RIN transformation
# https://rpubs.com/seriousstats/616206

.ci.spearman.cor.se <- function(x, y, se = c("fisher", "fieller", "bonett", "rin"), sample = TRUE, alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {

  xy <- na.omit(data.frame(x, y))

  x1 <- xy$x
  x2 <- xy$y

  n <- nrow(xy)
  nNA <- length(x) - n
  pNA <- (nNA / (n + nNA)) * 100

  #...................
  ### At least n = 4 and Variance Unequal 0 ####

  if (isTRUE(n >= 4L && var(x1) != 0L && var(x2) != 0L)) {

    rs <- cor(x1, x2, method = "spearman")

    if (isTRUE(se %in% c("fieller", "bonett"))) {

      if (isTRUE(alternative == "two.sided")) {

        alphavec <- c((1L - conf.level) / 2L, (1L + conf.level) / 2L)

      } else {

        alphavec <- c((1L - conf.level), conf.level)

      }

      ci <- tanh(atanh(rs) + c(qnorm(alphavec[1L]), qnorm(alphavec[2L])) * switch(se, "fisher" = { sqrt(1 / (n - 3L)) }, "fieller" = { sqrt(1.06 / (n - 3L)) }, "bonett" = { sqrt((1L + (rs^2L) / 2L) / (n - 3L)) }))

      if (isTRUE(alternative != "two.sided")) { switch(alternative, less = { ci[1L] <- -1L }, greater = { ci[2L] <- 1L }) }

    } else {

      RIN <- function(y) { qnorm((rank(y) - 0.5) / (length(rank(y)))) }

      ci <- cor.test(RIN(x1), RIN(x2), alternative = alternative, conf.level = conf.level)$conf.int

    }

    object <- list(se = se, sample = sample, alternative = alternative, conf.level = conf.level, n = n,  nNA = nNA, pNA = pNA, skew1 = misty::skewness(x1, sample = sample, check = FALSE), kurt1 = misty::kurtosis(x1, sample = sample, check = FALSE), skew2 = misty::skewness(x2, sample = sample, check = FALSE), kurt2 = misty::kurtosis(x2, sample = sample, check = FALSE), cor = rs, ci = ci)

    #...................
    ### Number of Cases n < 4 or Variance Equal 0 ####

  } else {

    object <- list(se = se, sample = sample, alternative = alternative, conf.level = conf.level, n = n,  nNA = nNA, pNA = pNA, skew1 = NA, kurt1 = NA, skew2 = NA, kurt2 = NA, cor = if (isTRUE(nrow(xy) == 3L && var(x1) != 0L && var(x2) != 0L)) { suppressWarnings(cor(x1, x2, method = "spearman")) } else { NA }, ci = c(NA, NA))

  }

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .ci.kendall.b Function ####

.ci.kendall.b <- function(x, y, sample = TRUE, alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {

  xy <- na.omit(data.frame(x, y))

  x1 <- xy$x
  x2 <- xy$y

  n <- nrow(xy)
  nNA <- length(x) - n
  pNA <- (nNA / (n + nNA)) * 100

  #...................
  ### At least n = 4 and Variance Unequal 0 ####

  if (isTRUE(n >= 4L && var(x1) != 0L && var(x2) != 0L)) {

    #...................
    ### Significance Testing ####

    # Test for Association
    test.tau.b <- cor.test(x1, x2, method = "kendall", alternative = alternative, exact = FALSE, continuity = FALSE)

    # Kendall's tau b
    tau <- unname(test.tau.b$estimate)

    # Fieler et al. (1957) standard error
    tau.se <- sqrt(0.437 / (length(x) - 4L))

    #...................
    ### Confidence Interval ####

    if (isTRUE(alternative == "two.sided")) {

      alphavec <- c((1L - conf.level) / 2L, (1L + conf.level) / 2L)

    } else {

      alphavec <- c((1L - conf.level), conf.level)

    }

    if (isTRUE(!is.na(tau.se))) {

      ci <- tanh(atanh(tau) + c(qnorm(alphavec[1L]), qnorm(alphavec[2L]))*tau.se)

      if (isTRUE(alternative != "two.sided")) { switch(alternative, less = { ci[1L] <- -1L }, greater = { ci[2L] <- 1L }) }

    } else {

      ci <- c(NA, NA)

    }

    #...................
    ### Return Object ####

    object <- list(alternative = alternative, conf.level = conf.level, n = n, nNA = nNA, pNA = pNA, skew1 = misty::skewness(x1, sample = sample, check = FALSE), kurt1 = misty::kurtosis(x1, sample = sample, check = FALSE), skew2 = misty::skewness(x2, sample = sample, check = FALSE), kurt2 = misty::kurtosis(x2, sample = sample, check = FALSE),
                   stat = test.tau.b$statistic, tau = tau, se = tau.se, pval = test.tau.b$p.value, ci = ci)

    #...................
    ### Number of Cases n < 4 or Variance Equal 0 ####

  } else {

    #...................
    ### Return Object ####

    object <- list(alternative = alternative, conf.level = conf.level, n = n, nNA = nNA, pNA = pNA, skew1 = NA, kurt1 = NA, skew2 = NA, kurt2 = NA,
                   stat = NA, tau = if (isTRUE(nrow(xy) == 3L && var(x1) != 0L && var(x2) != 0L)) { suppressWarnings(cor(x1, x2, method = "kendall")) } else { NA }, se = NA, pval = NA, ci = c(NA, NA))

  }

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .ci.kendall.c.estimate Function ####

.ci.kendall.c.estimate <- function(x, y) {

  # Contingency table
  x.table <- table(x, y)

  # Number of rows
  x.nrow <- nrow(x.table)

  # Number of columns
  x.ncol <- ncol(x.table)

  # Sample size
  x.n <- sum(x.table)

  # Minimum of number of rows/columns
  x.m <- min(dim(x.table))

  pi.c <- pi.d <- matrix(0L, nrow = x.nrow, ncol = x.ncol)

  x.col <- col(x.table)
  x.row <- row(x.table)

  for (i in seq_len(x.nrow)) {

    for (j in seq_len(x.ncol)) {

      pi.c[i, j] <- sum(x.table[x.row < i & x.col < j]) + sum(x.table[x.row > i & x.col > j])
      pi.d[i, j] <- sum(x.table[x.row < i & x.col > j]) + sum(x.table[x.row > i & x.col < j])

    }

  }

  # Concordant
  x.con <- sum(pi.c * x.table) / 2L

  # Discordant
  x.dis <- sum(pi.d * x.table) / 2L

  #...................
  ### Kendall Tau-c ####

  tau <- (x.m*2L * (x.con - x.dis)) / (x.n^2L * (x.m - 1L))

  #...................
  ### Return Object ####

  return(tau)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .ci.kendall.c Function ####

.ci.kendall.c <- function(x, y, sample = TRUE, alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {

  xy <- na.omit(data.frame(x, y))

  x1 <- xy$x
  x2 <- xy$y

  n <- nrow(xy)
  nNA <- length(x) - n
  pNA <- (nNA / (n + nNA)) * 100

  #...................
  ### At least n = 4 and Variance Unequal 0 ####

  if (isTRUE(n >= 4L && var(x1) != 0L && var(x2) != 0L)) {

    #...................
    ### Kendall Tau-c ####

    tau <- .ci.kendall.c.estimate(x1, x2)

    # Fieler et al. (1957) standard error
    tau.se <- sqrt(0.437 / (length(x) - 4L))

    # Test statistic
    z <- tau / tau.se

    # p-value
    switch(alternative, "two.sided" = {

      pval <- pnorm(abs(z), lower.tail = FALSE)*2L

    }, "less" = {

      pval <- ifelse(z < 0L, pnorm(abs(z), lower.tail = FALSE), 1 - pnorm(abs(z), lower.tail = FALSE))

    }, "greater" = {

      pval <- ifelse(z > 0L, pnorm(abs(z), lower.tail = FALSE), 1 - pnorm(abs(z), lower.tail = FALSE))

    })

    #...................
    ### Confidence Interval ####

    if (isTRUE(alternative == "two.sided")) {

      alphavec <- c((1L - conf.level) / 2L, (1L + conf.level) / 2L)

    } else {

      alphavec <- c((1L - conf.level), conf.level)

    }

    ci <- tanh(atanh(tau) + c(qnorm(alphavec[1L]), qnorm(alphavec[2L]))*tau.se)

    if (isTRUE(alternative != "two.sided")) { switch(alternative, less = { ci[1L] <- -1L }, greater = { ci[2L] <- 1L }) }

    #...................
    ### Return Object ####

    return(list(alternative = alternative, conf.level = conf.level, n = n, nNA = nNA, pNA = pNA, skew1 = misty::skewness(x, sample = sample, check = FALSE), kurt1 = misty::kurtosis(x, sample = sample, check = FALSE), skew2 = misty::skewness(y, sample = sample, check = FALSE), kurt2 = misty::kurtosis(y, sample = sample, check = FALSE),
                tau = tau, se = tau.se, stat = z, pval = pval, ci = ci))

    #...................
    ### Number of Cases n < 4 or Variance Equal 0 ####

  } else {

    #...................
    ### Return Object ####

    object <- list(alternative = alternative, conf.level = conf.level, n = n, nNA = nNA, pNA = pNA, skew1 = NA, kurt1 = NA, skew2 = NA, kurt2 = NA,
                   tau = if (isTRUE(nrow(xy) == 3L && var(x1) != 0L && var(x2) != 0L)) { suppressWarnings(.ci.kendall.c.estimate(x1, x2)) } else { NA }, se = NA, stat = NA, pval = NA, ci = c(NA, NA))

  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Interpolation on the Normal Quantile Scale ####
# Equation 5.8 of Davison and Hinkley (1997)
#
# .norm.inter from the R package 'boot'
# see: https://github.com/cran/boot/blob/master/R/bootfuns.q

.norm.inter <- function(t, alpha) {

  t <- t[is.finite(t)]
  R <- length(t)
  rk <- (R + 1L)*alpha
  k <- trunc(rk)
  inds <- seq_along(k)
  out <- inds
  kvs <- k[k > 0L & k < R]

  tstar <- sort(t, partial = sort(union(c(1L, R), c(kvs, kvs + 1L))))

  ints <- (k == rk)

  if (isTRUE(any(ints))) { out[inds[ints]] <- tstar[k[inds[ints]]] }

  out[k == 0L] <- tstar[1L]
  out[k == R] <- tstar[R]

  not <- function(v) { xor(rep(TRUE,length(v)), v) }
  temp <- inds[not(ints) & k != 0L & k != R]

  temp1 <- qnorm(alpha[temp])
  temp2 <- qnorm(k[temp] / (R + 1L))
  temp3 <- qnorm((k[temp] + 1L)/(R + 1L))

  tk <- tstar[k[temp]]
  tk1 <- tstar[k[temp] + 1L]

  out[temp] <- tk + (temp1 - temp2) / (temp3 - temp2)*(tk1 - tk)

  return(out)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Bootstrap Function Correlation Coefficient ####

.boot.func.cor <- function(data, ind, method) {

  data.boot <- data[ind, ]

  if (isTRUE(method != "kendall-c")) { cor <- cor(data.boot[, 1L], data.boot[, 2L], method = method) } else { cor <- .ci.kendall.c.estimate(data.boot[, 1L], data.boot[, 2L]) }

  return(cor = cor)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Nonparametric Bootstrap Confidence Intervals for the Correlation Coefficient ####

.ci.boot.cor <- function(data, x, y, method, statistic = .boot.func.cor, R = 1000, min.n = 10,
                         boot = c("norm", "basic", "perc", "bc", "bca"),
                         fisher = TRUE, sample = TRUE, alternative = c("two.sided", "less", "greater"),
                         conf.level = 0.95, seed = NULL) {

  #...................
  ### Correlation Coefficient ####

  method <- ifelse(isTRUE(method == "kendall-b"), "kendall", method)

  #...................
  ### Fisher-z transformation ####

  if (isTRUE(fisher)) { h <- function(t) atanh(t); hinv <- function(t) tanh(t) } else { h <- function(t) t; hinv <- function(t) t }

  #...................
  ### Adjust Confidence Level ####

  if (isTRUE(alternative %in% c("less", "greater"))) { conf.level.alter <- conf.level - (1 - conf.level) } else { conf.level.alter <- conf.level }

  #...................
  ### Data ####

  xy <- na.omit(data.frame(x = data[, x], y = data[, y]))

  x1 <- xy$x
  x2 <- xy$y

  n <- nrow(xy)
  nNA <- nrow(data) - n
  pNA <- (nNA / (n + nNA)) * 100L

  #...................
  ### At least n = min.n Cases and Variance Unequal 0 ####

  if (isTRUE(n >= min.n && var(x1) != 0L && var(x2) != 0L)) {

    #...................
    ### Bootstrap Replicates ####

    if (isTRUE(!is.null(seed))) { set.seed(seed) }

    boot.repli <- suppressWarnings(boot::boot(xy, statistic = statistic, method = method, R = R))

    #...................
    ### Bootstrap Confidence Interval ####

    switch(boot, "norm" = {

      result <- suppressWarnings(boot::boot.ci(boot.repli, type = "norm", conf = conf.level.alter, h = h, hinv = hinv)) |>
        (\(y) data.frame(low = y$normal[2L], upp = y$normal[3L]))()

    }, "basic" = {

      result <- suppressWarnings(boot::boot.ci(boot.repli, type = "basic", conf = conf.level.alter, h = h, hinv = hinv)) |>
        (\(y) data.frame(low = y$basic[4L], upp = y$basic[5L]))()

    }, "perc" = {

      result <- suppressWarnings(boot::boot.ci(boot.repli, type = "perc", conf = conf.level.alter)) |>
        (\(y) data.frame(low = y$perc[4L], upp = y$perc[5L]))()

    }, "bc" = {

      result <- qnorm(mean(boot.repli$t < boot.repli$t0, na.rm = TRUE)) |>
        (\(y) suppressWarnings(.norm.inter(boot.repli$t[, 1L], pnorm(y + (y + qnorm((1L + c(-conf.level.alter, conf.level.alter)) / 2L))))))() |>
        (\(z) data.frame(low = z[1L], upp = z[2L]))()

    }, "bca" = {

      result <- suppressWarnings(boot::boot.ci(boot.repli, type = "bca", method = method, conf = conf.level.alter)) |>
        (\(y) data.frame(low = y$bca[4L], upp = y$bca[5L]))()

    })

    #...................
    ### Adjust Lower or Upper Bound ####

    switch(alternative, "less" = { result$low <- -1 }, "greater" = { result$upp <- 1 })

    #...................
    ### Return Object ####

    object <- list(n = n, nNA = nNA, pNA = pNA, skew1 = misty::skewness(x1, sample = sample, check = FALSE), kurt1 = misty::kurtosis(x1, sample = sample, check = FALSE), skew2 = misty::skewness(x2, sample = sample, check = FALSE), kurt2 = misty::kurtosis(x2, sample = sample, check = FALSE),
                   cor = boot.repli$t0, t = as.vector(boot.repli$t), ci = result)

    #...................
    ### Number of Cases n < min.n or Variance Equal 0 ####

  } else {

    #...................
    ### Return Object ####

    object <- list(n = n, nNA = nNA, pNA = pNA, skew1 = NA, kurt1 = NA, skew2 = NA, kurt2 = NA,
                   cor = if (isTRUE(nrow(xy) == 3L && var(x1) != 0L && var(x2) != 0L)) {

                     if (isTRUE(method != "kendall-c")) {

                       suppressWarnings(cor(x1, x2, method = ifelse(isTRUE(method == "kendall-b"), "kendall", method)))

                     } else {

                       suppressWarnings(.ci.kendall.c.estimate(x1, x2))

                     }

                   } else {

                     NA

                   }, t = rep(NA, times = R), ci = c(NA, NA))

  }

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the ci._() functions ----------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Nonparametric Bootstrap Confidence Intervals ####

.ci.boot <- function(data, statistic, R = 1000, min.n = 10, boot = c("norm", "basic", "stud", "perc", "bc", "bca"),
                     sample = TRUE, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, seed = NULL) {

  #...................
  ### Adjust Confidence Level ####

  if (isTRUE(alternative %in% c("less", "greater"))) { conf.level.alter <- conf.level - (1 - conf.level) } else { conf.level.alter <- conf.level }

  #...................
  ### Data ####

  x <- na.omit(data)

  n <- length(x)
  nNA <- length(data) - n
  pNA <- (nNA / (n + nNA)) * 100L

  #...................
  ### At least n = min.n Cases and Variance Unequal 0 ####

  if (isTRUE(n >= min.n && var(x) != 0L)) {

    #...................
    ### Bootstrap Replicates ####

    if (isTRUE(!is.null(seed))) { set.seed(seed) }

    boot.repli <- suppressWarnings(boot::boot(x, statistic = statistic, R = R))

    #...................
    ### Bootstrap Confidence Interval ####

    switch(boot, "norm" = {

      result <- suppressWarnings(boot::boot.ci(boot.repli, type = "norm", conf = conf.level.alter)) |>
        (\(y) data.frame(low = y$normal[2L], upp = y$normal[3L]))()

    }, "basic" = {

      result <- suppressWarnings(boot::boot.ci(boot.repli, type = "basic", conf = conf.level.alter)) |>
        (\(y) data.frame(low = y$basic[4L], upp = y$basic[5L]))()

    }, "stud" = {

      result <- suppressWarnings(boot::boot.ci(boot.repli, type = "stud", conf = conf.level.alter)) |>
        (\(y) data.frame(low = y$student[4L], upp = y$student[5L]))()

    }, "perc" = {

      result <- suppressWarnings(boot::boot.ci(boot.repli, type = "perc", conf = conf.level.alter)) |>
        (\(y) data.frame(low = y$perc[4L], upp = y$perc[5L]))()

    }, "bc" = {

      result <- qnorm(mean(boot.repli$t[, 1L] < boot.repli$t0[1L], na.rm = TRUE)) |>
        (\(y) suppressWarnings(.norm.inter(boot.repli$t[, 1L], pnorm(y + (y + qnorm((1L + c(-conf.level.alter, conf.level.alter)) / 2L))))))() |>
        (\(z) data.frame(low = z[1L], upp = z[2L]))()

    }, "bca" = {

      result <- suppressWarnings(boot::boot.ci(boot.repli, type = "bca", conf = conf.level.alter)) |>
        (\(y) data.frame(low = y$bca[4L], upp = y$bca[5L]))()

    })

    #...................
    ### Adjust Lower or Upper Bound ####

    switch(alternative, "less" = { result$low <- -1 }, "greater" = { result$upp <- 1 })

    #...................
    ### Return Object ####

    object <- list(n = n, nNA = nNA, pNA = pNA, m = mean(x), sd = sd(x), iqr = IQR(x), freq = sum(x == 1), skew = suppressWarnings(misty::skewness(x, sample = sample, check = FALSE)), kurt = suppressWarnings(misty::kurtosis(x, sample = sample, check = FALSE)), t0 = boot.repli$t0[1L], t = as.vector(boot.repli$t[, 1L]), ci = result)

  #...................
  ### Number of Cases n < min.n or Variance Equal 0 ####

  } else {

    #...................
    ### Return Object ####

    object <- list(n = n, nNA = nNA, pNA = pNA, m = if (isTRUE(length(x) >= 2L && var(x) != 0L)) { mean(x) } else { NA }, sd = if (isTRUE(length(x) >= 2L && var(x) != 0L)) { sd(x) } else { NA }, iqrt = if (isTRUE(length(x) >= 2L && var(x) != 0L)) { IQR(x) } else { NA }, freq = if (isTRUE(length(x) >= 1L)) { sum(x == 1, na.rm = TRUE) } else { NA },
                   skew = if (isTRUE(length(x) >= 3L && var(x) != 0L)) { suppressWarnings(misty::skewness(x, sample = sample, check = FALSE)) } else { NA }, kurt = if (isTRUE(length(x) >= 4L && var(x) != 0L)) { suppressWarnings(misty::kurtosis(x, sample = sample, check = FALSE))} else { NA }, t0 = NA, t = rep(NA, times = R), ci = c(NA, NA)) }

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the ci.mean() function --------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Confidence interval for the mean ####

.m.conf <- function(x, sigma, adjust, alternative, conf.level, side) {

  # Data
  x <- na.omit(x)

  # Difference-adjustment factor
  adjust.factor <- ifelse(isTRUE(adjust), sqrt(2L) / 2L, 1L)

  # One observation or SD = 0
  if (isTRUE(length(x) <= 1L || sd(x) == 0L)) {

    ci <- c(NA, NA)

  # More than one observation
  } else {

    x.m <- mean(x)

    #...................
    ### Known population standard deviation ####

    if (isTRUE(!is.null(sigma))) {

      crit <- qnorm(switch(alternative,
                           two.sided = 1L - (1L - conf.level) / 2L,
                           less = conf.level,
                           greater = conf.level))

      se <- sigma / sqrt(length(na.omit(x)))

    #...................
    ### Unknown population standard deviation ####
    } else {

      crit <- qt(switch(alternative,
                        two.sided = 1L - (1L - conf.level) / 2L,
                        less = conf.level,
                        greater = conf.level), df = length(x) - 1L)

      se <- sd(x) / sqrt(length(x))

    }

    #...................
    ### Confidence interval ####
    ci <- switch(alternative,
                 two.sided = c(low = x.m - adjust.factor * crit * se,
                               upp = x.m + adjust.factor * crit * se),
                 less = c(low = -Inf,
                          upp = x.m + adjust.factor * crit * se),
                 greater = c(low = x.m - adjust.factor * crit * se,
                             upp = Inf))

  }

  #...................
  ### Return object ####
  object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Bootstrap Function Arithmetic Mean ####

.boot.func.mean <- function(data, ind) { return(c(mean(data[ind], na.rm = TRUE), var(data[ind], na.rm = TRUE) / length(na.omit(data[ind])))) }

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the ci.median() function ------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Confidence interval for the median ####

.med.conf <- function(x, alternative, conf.level, side) {

  # Data
  x <- na.omit(x)

  n <- length(x)

  # Number of observations less than 6 observations
  if (isTRUE(n < 6L)) {

    ci <- c(NA, NA)

  # At least six observations
  } else {

    #...................
    ### Confidence interval ####

    # Two-sided CI
    switch(alternative, two.sided = {

      k <- qbinom((1L - conf.level)/2L, size = n, prob = 0.5, lower.tail = TRUE)

      ci <- sort(x)[c(k, n - k + 1L)]

    # One-sided CI: less
    }, less = {

      k <- qbinom(1L - 2L * (1L - conf.level), size = n, prob = 0.5, lower.tail = TRUE)

      ci <- c(-Inf, sort(x)[k])

    # One-sided CI: greater
    }, greater = {

      k <- qbinom(1L - 2L * (1L - conf.level), size = n, prob = 0.5, lower.tail = FALSE)

      ci <- c(sort(x)[k], Inf)

    })

  }

  #...................
  ### Return object ####

  # Lower or upper limit
  object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Bootstrap Function Median ####

.boot.func.median <- function(data, ind) { return(c(median(data[ind], na.rm = TRUE), (pi / 2L) * var(data[ind], na.rm = TRUE) / length(na.omit(data[ind])))) }

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the ci.prop() function ------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Confidence interval for the proportion ####

.prop.conf <- function(x, method, alternative, conf.level, side) {

  # Data
  x <- na.omit(x)

  n <- length(x)

  # Number of observations
  if (isTRUE(n <= 1L)) {

    ci <- c(NA, NA)

  } else {

    s <- sum(x)

    p <- s / n
    q <- 1L - p

    z <- switch(alternative,
                two.sided = qnorm(1L - (1 - conf.level)/2L),
                less = qnorm(1L - (1L - conf.level)),
                greater = qnorm(1L - (1L - conf.level)))

    #...................
    ### Wald method ####
    if (isTRUE(method == "wald")) {

      term <- z * sqrt(p * q) / sqrt(n)

      ci <- switch(alternative,
                   two.sided = c(low = max(0L, p - term), upp = min(1L, p + term)),
                   less = c(low = 0L, upp = min(1, p + term)),
                   greater = c(low = max(0L, p - term), upp = 1L))

    #...................
    ### Wilson method ####
    } else if (isTRUE(method == "wilson")) {

      term1 <- (s + z^2 / 2L) / (n + z^2L)
      term2 <- z * sqrt(n) / (n + z^2L) * sqrt(p * q + z^2L / (4L * n))

      ci <- switch(alternative,
                   two.sided = c(low = max(0L, term1 - term2), upp = min(1L, term1 + term2)),
                   less = c(0L, upp = min(1L, term1 + term2)),
                   greater = c(low = max(0L, term1 - term2), upp = 1L))

    }

  }

  # Lower or upper limit
  object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the ci.var() function ---------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Confidence interval for the variance ####

.var.conf <- function(x, method, alternative, conf.level, side) {

  # Data
  x <- na.omit(x)
  x.var <- var(x)

  # Number of observations
  if (isTRUE((length(x) < 2L && method == "chisq") || (length(x) < 4L && method == "bonett"))) {

    ci <- c(NA, NA)

  } else {

    #...................
    ### Chi square method ####
    if (isTRUE(method == "chisq")) {

      df <- length(x) - 1L

      # Two-sided CI
      switch(alternative, two.sided = {

        crit.low <- qchisq((1L - conf.level)/2L, df = df, lower.tail = FALSE)
        crit.upp <- qchisq((1L - conf.level)/2L, df = df, lower.tail = TRUE)

        ci <- c(low = df*x.var / crit.low, upp = df*x.var / crit.upp)

        # One-sided CI: less
      }, less = {

        crit.upp <- qchisq((1L - conf.level), df = df, lower.tail = TRUE)

        ci <- c(low = 0L, upp = df*x.var / crit.upp)

        # One-sided CI: greater
      }, greater = {

        crit.low <- qchisq((1L - conf.level), df = df, lower.tail = FALSE)

        ci <- c(low = df*x.var / crit.low, upp = Inf)

      })

    #...................
    ### Bonett method ####
    } else if (isTRUE(method == "bonett")) {

      n <- length(x)

      z <- switch(alternative,
                  two.sided = qnorm(1L - (1L - conf.level)/2L),
                  less = qnorm(1L - (1L - conf.level)),
                  greater = qnorm(1L - (1L - conf.level)))

      cc <- n/(n - z)

      gam4 <- n * sum((x - mean(x, trim = 1L / (2L * (n - 4L)^0.5)))^4L) / (sum((x - mean(x))^2L))^2L

      se <- cc * sqrt((gam4 - (n - 3L)/n) / (n - 1L))

      ci <- switch(alternative,
                   two.sided = c(low = exp(log(cc * x.var) - z * se), upp = exp(log(cc * x.var) + z * se)),
                   less = c(low = 0L, upp = exp(log(cc * x.var) + z * se)),
                   greater = c(low = exp(log(cc * x.var) - z * se), upp = Inf))

    }

  }

  # Lower or upper limit
  object <- switch(side, both = ci, low = ci[1], upp = ci[2L])

  return(object)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Bootstrap Function Variance ####

.boot.func.var <- function(data, ind) { return(var(data[ind], na.rm = TRUE)) }

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the ci.sd() function ----------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Confidence interval for the standard deviation ####

.sd.conf <- function(x, method, alternative, conf.level, side) {

  # Data
  x <- na.omit(x)
  x.var <- var(x)

  # Number of observations
  if (isTRUE((length(x) < 2L && method == "chisq") || (length(x) < 4L && method == "bonett"))) {

    ci <- c(NA, NA)

  } else {

    #...................
    ### Chi square method ####
    if (isTRUE(method == "chisq")) {

      df <- length(x) - 1L

      # Two-sided CI
      switch(alternative, two.sided = {

        crit.low <- qchisq((1L - conf.level)/2L, df = df, lower.tail = FALSE)
        crit.upp <- qchisq((1L - conf.level)/2L, df = df, lower.tail = TRUE)

        ci <- sqrt(c(low = df*x.var / crit.low, upp = df*x.var / crit.upp))

      # One-sided CI: less
      }, less = {

        crit.upp <- qchisq((1L - conf.level), df = df, lower.tail = TRUE)

        ci <- c(low = 0L, upp = sqrt(df*x.var / crit.upp))

      # One-sided CI: greater
      }, greater = {

        crit.low <- qchisq((1L - conf.level), df = df, lower.tail = FALSE)

        ci <- c(low = sqrt(df*x.var / crit.low), upp = Inf)

      })

    #...................
    ### Bonett ####
    } else if (isTRUE(method == "bonett")) {

      n <- length(x)

      z <- switch(alternative,
                  two.sided = qnorm(1L - (1L - conf.level)/2L),
                  less = qnorm(1L - (1L - conf.level)),
                  greater = qnorm(1L - (1L - conf.level)))

      cc <- n/(n - z)

      gam4 <- n * sum((x - mean(x, trim = 1L / (2L * (n - 4L)^0.5)))^4L) / (sum((x - mean(x))^2))^2L

      se <- cc * sqrt((gam4 - (n - 3L)/n) / (n - 1L))

      ci <- switch(alternative,
                   two.sided = sqrt(c(low = exp(log(cc * x.var) - z * se), upp = exp(log(cc * x.var) + z * se))),
                   less = c(low = 0, upp = sqrt(exp(log(cc * x.var) + z * se))),
                   greater = c(low = sqrt(exp(log(cc * x.var) - z * se)), upp = Inf))

    }

  }

  # Lower or upper limit
  object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

  return(object)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Bootstrap Function Standard Deviation ####

.boot.func.sd <- function(data, ind) { return(sd(data[ind], na.rm = TRUE)) }

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the ci.mean.diff() function ---------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Confidence interval for the difference of arithmetic means ####

.m.diff.conf <- function(x, y, sigma, var.equal, alternative, paired, conf.level, side) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Independent samples ####
  if (!isTRUE(paired)) {

    #...................
    ### Data ####

    x <- na.omit(x)
    y <- na.omit(y)

    x.n <- length(x)
    y.n <- length(y)

    yx.mean <- mean(y) - mean(x)

    x.var <- var(x)
    y.var <- var(y)

    # At least 2 observations for x and y
    if (isTRUE(x.n >= 2L && y.n >= 2L & (x.var != 0L && y.var != 0L))) {

      #### Known Population SD ####
      if (isTRUE(!is.null(sigma))) {

        se <- sqrt((sigma[1L]^2L / x.n) + (sigma[2L]^2L / y.n))

        crit <- qnorm(switch(alternative,
                             two.sided = 1L - (1L - conf.level) / 2L,
                             less = conf.level,
                             greater = conf.level))

        term <- crit*se

      #### Unknown Population SD ####
      } else {

        #### Equal variance ####
        if (isTRUE(var.equal)) {

          se <- sqrt(((x.n - 1L)*x.var + (y.n - 1L)*y.var) / (x.n + y.n - 2L)) * sqrt(1 / x.n + 1L / y.n)

          crit <- qt(switch(alternative,
                            two.sided = 1L - (1L - conf.level) / 2L,
                            less = conf.level,
                            greater = conf.level), df = sum(x.n, y.n) - 2L)

          term <- crit*se

        #### Unequal variance ####
        } else {

          se <- sqrt(x.var / x.n + y.var / y.n)

          df <- (x.var / x.n + y.var / y.n)^2L / (((x.var / x.n)^2L / (x.n - 1L)) + ((y.var / y.n)^2L / (y.n - 1L)))

          crit <- qt(switch(alternative,
                            two.sided = 1L - (1L - conf.level) / 2L,
                            less = conf.level,
                            greater = conf.level), df = df)

          term <- crit*se

        }

      }

      #...................
      ### Confidence interval ####
      ci <- switch(alternative,
                   two.sided = c(low = yx.mean - term, upp = yx.mean + term),
                   less = c(low = -Inf, upp = yx.mean + term),
                   greater = c(low = yx.mean - term, upp = Inf))

      # Less than  2 observations for x and y
    } else {

      ci <- c(NA, NA)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dependent samples ####
  } else {

    xy.dat <- na.omit(data.frame(x = x, y = y, stringsAsFactors = FALSE))

    xy.diff <- xy.dat$y - xy.dat$x

    xy.diff.mean <- mean(xy.diff)

    xy.diff.sd <- sd(xy.diff)

    xy.diff.n <- nrow(xy.dat)

    # At least 2 observations for x
    if (isTRUE(xy.diff.n >= 2L && xy.diff.sd != 0L)) {

      #...................
      ### Known Population SD ####
      if (isTRUE(!is.null(sigma))) {

        se <- sigma / sqrt(xy.diff.n)

        crit <- qnorm(switch(alternative,
                             two.sided = 1L - (1L - conf.level) / 2L,
                             less = conf.level,
                             greater = conf.level))

        term <- crit*se

      #...................
      ### Unknown Population SD ####
      } else {

        se <- xy.diff.sd / sqrt(xy.diff.n)

        crit <- qt(switch(alternative,
                          two.sided = 1L - (1L - conf.level) / 2L,
                          less = conf.level,
                          greater = conf.level), df = xy.diff.n - 1L)

        term <- crit*se

      }

      ci <- switch(alternative,
                   two.sided = c(low = xy.diff.mean - term, upp = xy.diff.mean + term),
                   less = c(low = -Inf, upp = xy.diff.mean + term),
                   greater = c(low = xy.diff.mean - term, upp = Inf))

    # Less than 2 observations for x
    } else {

      ci <- c(NA, NA)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Return Object ####
  object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the ci.prop.diff() function ---------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Confidence interval for the difference of proportions ####

.prop.diff.conf <- function(x, y, method, alternative, paired, conf.level, side) {

  crit <- qnorm(switch(alternative,
                       two.sided = 1L - (1L - conf.level) / 2L,
                       less = conf.level,
                       greater = conf.level))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Independent samples ####

  if (!isTRUE(paired)) {

    #.................
    # Data
    x <- na.omit(x)
    y <- na.omit(y)

    x.n <- length(x)
    y.n <- length(y)

    p1 <- sum(x) / x.n
    p2 <- sum(y) / y.n

    p.diff <- p2 - p1

    #...................
    ### Wald confidence interval ####
    if (isTRUE(method == "wald")) {

      #......
      # At least 2 observations for x or y
      if (isTRUE((x.n >= 2L || y.n >= 2L) && (var(x) != 0L || var(y) != 0L))) {

        term <- crit * sqrt(p1*(1 - p1) / x.n + p2*(1 - p2) / y.n)

        # Confidence interval
        ci <- switch(alternative,
                     two.sided = c(low = max(-1L, p.diff - term), upp = min(1L, p.diff + term)),
                     less = c(low = -1, upp = min(1, p.diff + term)),
                     greater = c(low = max(-1L, p.diff - term), upp = 1L))

        # Less than 2 observations for x or y
      } else {

        ci <- c(NA, NA)

      }

    #...................
    ### Newcombes Hybrid Score interval ####
    } else if (isTRUE(method == "newcombe")) {

      # At least 1 observations for x and y
      if (isTRUE((x.n >= 1L && y.n >= 1L))) {

        if (isTRUE(alternative == "two.sided")) {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", conf.level = conf.level, output = FALSE)$result

        } else if (isTRUE(alternative == "less")) {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", alternative = "greater", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", alternative = "less", conf.level = conf.level, output = FALSE)$result

        } else if (isTRUE(alternative == "greater")) {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", alternative = "less", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", alternative = "greater", conf.level = conf.level, output = FALSE)$result

        }

        # Confidence interval
        ci <- switch(alternative,
                     two.sided = c(p.diff - crit * sqrt((x.ci.wilson$upp*(1 - x.ci.wilson$upp) / x.n) + (y.ci.wilson$low*(1 - y.ci.wilson$low) / y.n)),
                                   p.diff + crit * sqrt((x.ci.wilson$low*(1 - x.ci.wilson$low) / x.n) + (y.ci.wilson$upp*(1 - y.ci.wilson$upp) / y.n))),
                     less = c(-1, p.diff + crit * sqrt((x.ci.wilson$low*(1 - x.ci.wilson$low) / x.n) + (y.ci.wilson$upp*(1 - y.ci.wilson$upp) / y.n))),
                     greater = c(p.diff - crit * sqrt((x.ci.wilson$upp*(1 - x.ci.wilson$upp) / x.n) + (y.ci.wilson$low*(1 - y.ci.wilson$low) / y.n)), 1))

        # Less than 1 observations for x or y
      } else {

        ci <- c(NA, NA)

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dependent samples ####
  } else {

    xy.dat <- na.omit(data.frame(x = x, y = y, stringsAsFactors = FALSE))

    x.p <- mean(xy.dat$x)
    y.p <- mean(xy.dat$y)

    xy.diff.mean <- y.p - x.p

    xy.diff.n <- nrow(xy.dat)

    a <- as.numeric(sum(xy.dat$x == 1 & xy.dat$y == 1))
    b <- as.numeric(sum(xy.dat$x == 1 & xy.dat$y == 0))
    c <- as.numeric(sum(xy.dat$x == 0 & xy.dat$y == 1))
    d <- as.numeric(sum(xy.dat$x == 0 & xy.dat$y == 0))

    #...................
    ### Wald confidence interval ####
    if (isTRUE(method == "wald")) {

      #......
      # At least 2 observations for x or y
      if (isTRUE(xy.diff.n >= 2 && (var(xy.dat$x) != 0 || var(xy.dat$y) != 0))) {

        term <- crit * sqrt((b + c) - (b - c)^2 / xy.diff.n) / xy.diff.n

        #......
        # Confidence interval
        ci <- switch(alternative,
                     two.sided = c(low = max(-1, xy.diff.mean - term), upp = min(1, xy.diff.mean + term)),
                     less = c(low = -1, upp = min(1, xy.diff.mean + term)),
                     greater = c(low = max(-1, xy.diff.mean - term), upp = 1))

      } else {

        ci <- c(NA, NA)

      }

    #...................
    ### Newcombes Hybrid Score interval ####
    } else if (isTRUE(method == "newcombe")) {

      # At least 1 observations for x and y
      if (isTRUE(xy.diff.n >= 1L)) {

        if (isTRUE(alternative == "two.sided")) {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", conf.level = conf.level, output = FALSE)$result

        } else if (isTRUE(alternative == "less")) {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", alternative = "greater", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", alternative = "less", conf.level = conf.level, output = FALSE)$result

        } else if (isTRUE(alternative == "greater")) {

          x.ci.wilson <- misty::ci.prop(x, method = "wilson", alternative = "less", conf.level = conf.level, output = FALSE)$result
          y.ci.wilson <- misty::ci.prop(y, method = "wilson", alternative = "greater", conf.level = conf.level, output = FALSE)$result

        }

        A <- (a + b) * (c + d) * (a + c) * (b + d)

        as.numeric(a)

        if (isTRUE(A == 0L)) {

          phi <- 0L

        } else {

          phi <- (a * d - b * c) / sqrt(A)

        }

        ci <- switch(alternative,
                     two.sided = c(xy.diff.mean - sqrt((y.p - y.ci.wilson$low)^2 - 2L * phi * (y.p - y.ci.wilson$low) * (x.ci.wilson$upp - x.p) + (x.ci.wilson$upp - x.p)^2L),
                                   xy.diff.mean + sqrt((x.p - x.ci.wilson$low)^2 - 2L * phi * (x.p - x.ci.wilson$low) * (y.ci.wilson$upp - y.p) + (y.ci.wilson$upp - y.p)^2L)),
                     less = c(-1L, xy.diff.mean + sqrt((x.p - x.ci.wilson$low)^2 - 2L * phi * (x.p - x.ci.wilson$low) * (y.ci.wilson$upp - y.p) + (y.ci.wilson$upp - y.p)^2L)),
                     greater = c(xy.diff.mean - sqrt((y.p - y.ci.wilson$low)^2 - 2L * phi * (y.p - y.ci.wilson$low) * (x.ci.wilson$upp - x.p) + (x.ci.wilson$upp - x.p)^2), 1L))

      } else {

        ci <- c(NA, NA)

      }

    }

  }

  # Return object
  object <- switch(side, both = ci, low = ci[1L], upp = ci[2L])

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for Plotting Confidence Intervals -------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot Function Confidence Interval ####

.plot.ci <- function(result, stat, group = NULL, split = NULL, point.size, point.shape, errorbar.width, dodge.width,
                     line, intercept, linetype, line.col, xlab, ylab, xlim , ylim, xbreaks, ybreaks,
                     axis.title.size, axis.text.size, strip.text.size, title, subtitle, group.col, plot.margin,
                     legend.title, legend.position, legend.box.margin, facet.ncol, facet.nrow, facet.scales) {

  low <- upp <- x <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #...................
    ### Plot Data ####

    # Correlation coefficient
    if (isTRUE(stat == "cor")) {

      plotdat <- data.frame(x = paste(result$var1, result$var2, sep = "\n"), y = result$cor, low = result$low, upp = result$upp)

    # Mean, Median, Proportion, SD, Variance
    } else {

      plotdat <- data.frame(x = factor(result$variable, levels = unique(result$variable)), y = result[, stat], low = result$low, upp = result$upp)

    }

    #...................
    ### Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(x, y))

    #...................
    ### Horizontal Line ####

    if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = intercept, linetype = linetype, color = line.col) }

    #...................
    ### Error Bars  ####

    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = upp), width = errorbar.width) +
      ggplot2::geom_point(size = point.size, shape = point.shape) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.margin = ggplot2::unit(c(plot.margin[1L], plot.margin[2L], plot.margin[3L], plot.margin[4L]), "pt"),
                     axis.text = ggplot2::element_text(size = axis.text.size),
                     axis.title = ggplot2::element_text(size = axis.title.size))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    #...................
    ### Plot Data ####

    # Correlation coefficient
    if (isTRUE(stat == "cor")) {

      plotdat <- data.frame(group = result$group, x = paste(result$var1, result$var2, sep = "\n"), y = result$cor, low = result$low, upp = result$upp)

    # Mean, Median, Proportion, SD, Variance
    } else {

      plotdat <- data.frame(group = result$group, x = factor(result$variable, levels = unique(result$variable)), y = result[, stat], low = result$low, upp = result$upp)

    }

    #...................
    ### Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(x, y, group = group, color = group))

    #...................
    ### Horizontal Line ####

    if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = intercept, linetype = linetype, color = line.col) }

    #...................
    ### Error Bars  ####

    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = upp), width = errorbar.width,
                             position = ggplot2::position_dodge(dodge.width)) +
      ggplot2::geom_point(size = point.size, shape = point.shape, position = ggplot2::position_dodge(dodge.width)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::labs(title = title, subtitle = subtitle, color = legend.title) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.margin = ggplot2::unit(c(plot.margin[1L], plot.margin[2L], plot.margin[3L], plot.margin[4L]), "pt"),
                     axis.text = ggplot2::element_text(size = axis.text.size),
                     axis.title = ggplot2::element_text(size = axis.title.size),
                     legend.position = legend.position,
                     legend.box.background = ggplot2::element_blank(),
                     legend.box.margin = ggplot2::margin(legend.box.margin[1L], legend.box.margin[2L], legend.box.margin[3L], legend.box.margin[4L]))

    #...................
    ### Manual Colors ####

    if (isTRUE(!is.null(group.col))) { p <- p + ggplot2::scale_color_manual(values = group.col) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    #...................
    ### Plot Data ####

    # Correlation coefficient
    if (isTRUE(stat == "cor")) {

      plotdat <- data.frame(split = as.vector(sapply(result, nrow) |> (\(y) unlist(sapply(seq_len(length(result)), function(z) rep(names(y)[z], times = y[z]))))()),
                            do.call("rbind", result)) |> (\(y) data.frame(split = y$split, x = paste(y$var1, y$var2, sep = "\n"), y = y$cor, low = y$low, upp = y$upp))()

    # Mean, Median, Proportion, SD, Variance
    } else {

      plotdat <- data.frame(split = as.vector(sapply(result, nrow) |> (\(y) unlist(sapply(seq_len(length(result)), function(z) rep(names(y)[z], times = y[z]))))()), do.call("rbind", result)) |>
        (\(z) data.frame(split = z$split, x = factor(z$variable, levels = unique(z$variable)), y = z[, stat], low = z$low, upp = z$upp))()

    }

    #...................
    ### Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(x, y))

    #...................
    ### Horizontal Line ####

    if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = intercept, linetype = linetype, color = line.col) }

    #...................
    ### Error Bars  ####

    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = upp), width = errorbar.width) +
      ggplot2::geom_point(size = point.size, shape = point.shape) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::facet_wrap(~ split, ncol = facet.ncol, nrow = facet.nrow) +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.margin = ggplot2::unit(c(plot.margin[1L], plot.margin[2L], plot.margin[3L], plot.margin[4L]), "pt"),
                     axis.text = ggplot2::element_text(size = axis.text.size),
                     axis.title = ggplot2::element_text(size = axis.title.size))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    #...................
    ### Plot Data ####

    # Correlation coefficient
    if (isTRUE(stat == "cor")) {

      plotdat <- data.frame(split = as.vector(sapply(result, nrow) |> (\(y) unlist(sapply(seq_len(length(result)), function(z) rep(names(y)[z], times = y[z]))))()),
                            do.call("rbind", result)) |> (\(y) data.frame(group = y$group, split = y$split, x = paste(y$var1, y$var2, sep = "\n"), y = y$cor, low = y$low, upp = y$upp))()

    # Mean, Median, Proportion, SD, Variance
    } else {

      plotdat <- data.frame(split = as.vector(sapply(result, nrow) |> (\(y) unlist(sapply(seq_len(length(result)), function(z) rep(names(y)[z], times = y[z]))))()), do.call("rbind", result)) |>
        (\(z) data.frame(group = z$group, split = z$split, x = factor(z$variable, levels = unique(z$variable)), y = z[, stat], low = z$low, upp = z$upp))()

    }

    #...................
    ### Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(x, y, group = group, color = group))

    #...................
    ### Horizontal Line ####

    if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = intercept, linetype = linetype, color = line.col) }

    #...................
    ### Error Bars  ####

    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = low, ymax = upp), width = errorbar.width,
                             position = ggplot2::position_dodge(dodge.width)) +
      ggplot2::geom_point(size = point.size, shape = point.shape, position = ggplot2::position_dodge(dodge.width)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::facet_wrap(~ split, ncol = facet.ncol, nrow = facet.nrow) +
      ggplot2::labs(title = title, subtitle = subtitle, color = legend.title) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.margin = ggplot2::unit(c(plot.margin[1L], plot.margin[2L], plot.margin[3L], plot.margin[4L]), "pt"),
                     axis.text = ggplot2::element_text(size = axis.text.size),
                     axis.title = ggplot2::element_text(size = axis.title.size),
                     legend.position = legend.position,
                     legend.box.margin = ggplot2::margin(legend.box.margin[1L], legend.box.margin[2L], legend.box.margin[3L], legend.box.margin[4L]))

    #...................
    ### Manual Colors ####

    if (isTRUE(!is.null(group.col))) { p <- p + ggplot2::scale_color_manual(values = group.col) }

  }

  return(list(p = p, plotdat = plotdat))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot Function Bootstrap Samples ####

.plot.boot <- function(result, boot.sample, stat, group = NULL, split = NULL, hist, binwidth, bins, alpha, fill,
                       density, density.col, density.linewidth, density.linetype, plot.point, point.col, point.linewidth, point.linetype,
                       plot.ci, ci.col, ci.linewidth, ci.linetype, line, intercept, linetype, line.col,
                       xlab, ylab, xlim, ylim, xbreaks, ybreaks, axis.title.size, axis.text.size, strip.text.size, title, subtitle, group.col,
                       plot.margin, legend.title, legend.position, legend.box.margin, facet.ncol, facet.nrow, facet.scales) {

  point <- low <- upp <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #...................
    ### Plot Data ####

    # Correlation coefficient
    if (isTRUE(stat == "cor")) {

      plotdat <- merge(data.frame(x = paste(boot.sample$var1, boot.sample$var2, sep = " - "), y = boot.sample$cor),
                       data.frame(x = paste(result$var1, result$var2, sep = " - "), point = result$cor, low = result$low, upp = result$upp), by = "x")

    # Mean, Median, Proportion, SD, Variance
    } else {

      plotdat <- merge(data.frame(x = factor(boot.sample$variable, levels = unique(result$variable)), y = boot.sample[, stat]),
                       data.frame(x = factor(result$variable, levels = unique(result$variable)), point = result[, stat], low = result$low, upp = result$upp), by = "x")

    }

    #...................
    ### Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(y)) +
      ggplot2::facet_wrap(~ x, scales = facet.scales) +
      ggplot2::scale_x_continuous(name = xlab, expand = c(0.02, 0), limits = xlim, breaks = xbreaks) +
      ggplot2::scale_y_continuous(name = ylab, expand = ggplot2::expansion(mult = c(0L, 0.05))) +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     strip.text = ggplot2::element_text(size = strip.text.size),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.margin = ggplot2::unit(c(plot.margin[1L], plot.margin[2L], plot.margin[3L], plot.margin[4L]), "pt"),
                     axis.text = ggplot2::element_text(size = axis.text.size),
                     axis.title = ggplot2::element_text(size = axis.title.size))

    #...................
    ### Histogram ####

    if (isTRUE(hist)) { p <- p + ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), binwidth = binwidth, bins = bins, color = "black", alpha = alpha, fill = fill) }

    #...................
    ### Density Curve ####

    if (isTRUE(density)) { p <- p + ggplot2::geom_density(color = density.col, linewidth = density.linewidth, linetype = density.linetype) }

    #...................
    ### Vertical Line ####

    if (isTRUE(line)) { p <- p + ggplot2::geom_vline(xintercept = intercept, linetype = linetype, color = line.col) }

    #...................
    ### Point Estimate ####

    if (isTRUE(plot.point)) { p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = point), color = point.col, linetype = point.linetype, linewidth = point.linewidth) }

    #...................
    ### Confidence Interval ####

    if (isTRUE(plot.ci)) { p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = low), color = ci.col, linetype = ci.linetype, linewidth = ci.linewidth) + ggplot2::geom_vline(ggplot2::aes(xintercept = upp), color = ci.col, linetype = ci.linetype , linewidth = ci.linewidth) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    #...................
    ### Plot Data ####

    # Correlation coefficient
    if (isTRUE(stat == "cor")) {

      plotdat <- merge(data.frame(by = factor(paste(boot.sample$group, boot.sample$var1, boot.sample$var2, sep = " - ")), x = paste(boot.sample$var1, boot.sample$var2, sep = " - "), group = factor(boot.sample$group, levels = unique(result$group)), y = boot.sample$cor),
                       data.frame(by = factor(paste(result$group, result$var1, result$var2, sep = " - ")), point = result$cor, low = result$low, upp = result$upp),  by = "by") |> (\(y) y[, -grep("by", colnames(y))])()

    # Mean, Median, Proportion, SD, Variance
    } else {

      plotdat <- merge(data.frame(by = factor(paste(boot.sample$group, boot.sample$variable, sep = " - ")), x = factor(boot.sample$variable, levels = unique(result$variable)), group = factor(boot.sample$group, levels = unique(result$group)), y = boot.sample[, stat]),
                       data.frame(by = factor(paste(result$group, result$variable, sep = " - ")), point = result[, stat], low = result$low, upp = result$upp), by = "by") |> (\(y) y[, -grep("by", colnames(y))])()

    }

    #...................
    ### Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(y, group = group, color = group)) +
      ggplot2::facet_wrap(~ x, scales = facet.scales) +
      ggplot2::scale_x_continuous(name = xlab, expand = c(0.02, 0), limits = xlim, breaks = xbreaks) +
      ggplot2::scale_y_continuous(name = ylab, expand = ggplot2::expansion(mult = c(0L, 0.05))) +
      ggplot2::labs(title = title, subtitle = subtitle, color = legend.title, fill = legend.title) +
      ggplot2::guides(color = "none") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     strip.text = ggplot2::element_text(size = strip.text.size),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.margin = ggplot2::unit(c(plot.margin[1L], plot.margin[2L], plot.margin[3L], plot.margin[4L]), "pt"),
                     axis.text = ggplot2::element_text(size = axis.text.size),
                     axis.title = ggplot2::element_text(size = axis.title.size),
                     legend.position = legend.position,
                     legend.box.background = ggplot2::element_blank(),
                     legend.box.margin = ggplot2::margin(legend.box.margin[1L], legend.box.margin[2L], legend.box.margin[3L], legend.box.margin[4L]))

    #...................
    ### Histogram ####

    if (isTRUE(hist)) { p <- p + ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density), fill = group), position = "identity", binwidth = binwidth, bins = bins, color = "black", alpha = alpha) }

    #...................
    ### Density Curve ####

    if (isTRUE(density)) { p <- suppressMessages(p + ggplot2::geom_density(alpha = alpha, linewidth = density.linewidth, linetype = density.linetype)) }

    #...................
    ### Vertical Line ####

    if (isTRUE(line)) { p <- p + ggplot2::geom_vline(xintercept = intercept, linetype = linetype, color = line.col) }

    #...................
    ### Point Estimate ####

    if (isTRUE(plot.point)) { p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = point, color = group), linetype = point.linetype, linewidth = point.linewidth) }

    #...................
    ### Confidence Interval ####

    if (isTRUE(plot.ci)) { p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = low, color = group), linetype = ci.linetype, linewidth = ci.linewidth) + ggplot2::geom_vline(ggplot2::aes(xintercept = upp, color = group), linetype = ci.linetype, linewidth = ci.linewidth) }

    #...................
    ### Manual Colors ####

    if (isTRUE(!is.null(group.col))) { p <- p + ggplot2::scale_color_manual(values = group.col) + ggplot2::scale_fill_manual(values = group.col) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    #...................
    ### Plot Data ####

    # Correlation coefficient
    if (isTRUE(stat == "cor")) {

      plotdat <- merge(data.frame(by = factor(paste(boot.sample$split, boot.sample$var1, boot.sample$var2, sep = " - ")), x = paste(boot.sample$var1, boot.sample$var2, sep = " - "), split = factor(boot.sample$split, levels = names(result)), y = boot.sample$cor),
                       data.frame(split = rep(names(result), each = unique(sapply(result, nrow))), do.call("rbind", result)) |> (\(y) data.frame(by = factor(paste(y$split, y$var1, y$var2, sep = " - ")), point = y$cor, low = y$low, upp = y$upp))(),  by = "by") |> (\(z) z[, -grep("by", names(z))])()

    # Mean, Median, Proportion, SD, Variance
    } else {

      plotdat <- merge(data.frame(by = factor(paste(boot.sample$split, boot.sample$variable, sep = " - ")), x = factor(boot.sample$variable, levels = unique(do.call("rbind", result)$variable)), split = factor(boot.sample$split, levels = names(result)), y = boot.sample[, stat]),
                       data.frame(split = rep(names(result), each = unique(sapply(result, nrow))), do.call("rbind", result)) |> (\(y) data.frame(by = factor(paste(y$split, y$variable, sep = " - ")), point = y[, stat], low = y$low, upp = y$upp))(),  by = "by") |> (\(z) z[, -grep("by", names(z))])()

    }

    #...................
    ### Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(y)) +
      ggplot2::facet_wrap(~ x + split, scales = facet.scales) +
      ggplot2::scale_x_continuous(name = xlab, expand = c(0.02, 0), limits = xlim, breaks = xbreaks) +
      ggplot2::scale_y_continuous(name = ylab, expand = ggplot2::expansion(mult = c(0L, 0.05))) +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     strip.text = ggplot2::element_text(size = strip.text.size),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.margin = ggplot2::unit(c(plot.margin[1L], plot.margin[2L], plot.margin[3L], plot.margin[4L]), "pt"),
                     axis.text = ggplot2::element_text(size = axis.text.size),
                     axis.title = ggplot2::element_text(size = axis.title.size))

    #...................
    ### Histogram ####

    if (isTRUE(hist)) { p <- p + ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), binwidth = binwidth, bins = bins, color = "black", alpha = alpha, fill = fill) }

    #...................
    ### Density Curve ####

    if (isTRUE(density)) { p <- p + ggplot2::geom_density(color = density.col, linewidth = density.linewidth, linetype = density.linetype) }

    #...................
    ### Vertical Line ####

    if (isTRUE(line)) { p <- p + ggplot2::geom_vline(xintercept = intercept, linetype = linetype, color = line.col) }

    #...................
    ### Point Estimate ####

    if (isTRUE(plot.point)) { p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = point), color = point.col, linetype = point.linetype, linewidth = point.linewidth) }

    #...................
    ### Confidence Interval ####

    if (isTRUE(plot.ci)) { p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = low), color = ci.col, linetype = ci.linetype, linewidth = ci.linewidth) + ggplot2::geom_vline(ggplot2::aes(xintercept = upp), color = ci.col, linetype = ci.linetype, linewidth = ci.linewidth) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    #...................
    ### Plot Data ####

    # Correlation coefficient
    if (isTRUE(stat == "cor")) {

      plotdat <- merge(data.frame(by = factor(paste(boot.sample$split, boot.sample$group, boot.sample$var1, boot.sample$var2, sep = " - ")), x = paste(boot.sample$var1, boot.sample$var2, sep = " - "), split = factor(boot.sample$split, levels = names(result)), group = factor(boot.sample$group, levels = unique(do.call("rbind", result)$group)), y = boot.sample$cor),
                       data.frame(split = rep(names(result), each = unique(sapply(result, nrow))), do.call("rbind", result)) |> (\(y) data.frame(by = factor(paste(y$split, y$group, y$var1, y$var2, sep = " - ")), point = y$cor, low = y$low, upp = y$upp))(),  by = "by") |> (\(z) z[, -grep("by", names(z))])()

    # Mean, Median, Proportion, SD, Variance
    } else {

      plotdat <- merge(data.frame(by = factor(paste(boot.sample$split, boot.sample$group, boot.sample$variable, sep = " - ")), x = factor(boot.sample$variable, levels = unique(do.call("rbind", result)$variable)), split = factor(boot.sample$split, levels = names(result)), group = factor(boot.sample$group, levels = unique(do.call("rbind", result)$group)), y = boot.sample[, stat]),
                       data.frame(split = rep(names(result), each = unique(sapply(result, nrow))), do.call("rbind", result)) |> (\(y) data.frame(by = factor(paste(y$split, y$group, y$variable, sep = " - ")), point = y[, stat], low = y$low, upp = y$upp))(),  by = "by") |> (\(z) z[, -grep("by", names(z))])()

    }

    #...................
    ### Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(y, group = group, color = group)) +
      ggplot2::facet_wrap(~ x + split, scales = facet.scales) +
      ggplot2::scale_x_continuous(name = xlab, expand = c(0.02, 0), limits = xlim, breaks = xbreaks) +
      ggplot2::scale_y_continuous(name = ylab, expand = ggplot2::expansion(mult = c(0L, 0.05))) +
      ggplot2::labs(title = title, subtitle = subtitle, color = legend.title, fill = legend.title) +
      ggplot2::guides(color = "none") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     strip.text = ggplot2::element_text(size = strip.text.size),
                     plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.margin = ggplot2::unit(c(plot.margin[1L], plot.margin[2L], plot.margin[3L], plot.margin[4L]), "pt"),
                     axis.text = ggplot2::element_text(size = axis.text.size),
                     axis.title = ggplot2::element_text(size = axis.title.size),
                     legend.position = legend.position,
                     legend.box.background = ggplot2::element_blank(),
                     legend.box.margin = ggplot2::margin(legend.box.margin[1L], legend.box.margin[2L], legend.box.margin[3L], legend.box.margin[4L]))

    #...................
    ### Histogram ####

    if (isTRUE(hist)) { p <- p + ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density), fill = group), position = "identity", binwidth = binwidth, bins = bins, color = "black", alpha = alpha) }

    #...................
    ### Density Curve ####

    if (isTRUE(density)) { p <- p + ggplot2::geom_density(alpha = alpha, linewidth = density.linewidth, linetype = density.linetype) }

    #...................
    ### Vertical Line ####

    if (isTRUE(line)) { p <- p + ggplot2::geom_vline(xintercept = intercept, linetype = linetype, color = line.col) }

    #...................
    ### Point Estimate ####

    if (isTRUE(plot.point)) { p <- p + ggplot2::geom_vline(plotdat, ggplot2::aes(xintercept = point, color = group), linetype = point.linetype, linewidth = point.linewidth) }

    #...................
    ### Confidence Interval ####

    if (isTRUE(plot.ci)) { p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = low, color = group), linetype = ci.linetype, linewidth = ci.linewidth) + ggplot2::geom_vline(ggplot2::aes(xintercept = upp, color = group), linetype = ci.linetype, linewidth = ci.linewidth) }

    #...................
    ### Manual Colors ####

    if (isTRUE(!is.null(group.col))) { p <- p + ggplot2::scale_color_manual(values = group.col) }

  }

  return(list(p = p, plotdat = plotdat))

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the df.rbind() function ---------------------------------
#
# - .make_names
# - .quickdf
# - .make_assignment_call
# - .allocate_column
# - .output_template

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .make_names ####

.make_names <- function(x, prefix = "X") {

  nm <- names(x)

  if (isTRUE(is.null(nm))) {

    nm <- rep.int("", length(x))

  }

  n <- sum(nm == "", na.rm = TRUE)

  nm[nm == ""] <- paste0(prefix, seq_len(n))

  return(nm)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .quickdf ####

.quickdf <- function (list) {

  rows <- unique(unlist(lapply(list, NROW)))

  stopifnot(length(rows) == 1L)

  names(list) <- .make_names(list, "X")

  class(list) <- "data.frame"

  attr(list, "row.names") <- c(NA_integer_, -rows)

  return(list)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .make_assignment_call ####

.make_assignment_call <- function (ndims) {

  assignment <- quote(column[rows] <<- what)

  if (isTRUE(ndims >= 2L)) {

    assignment[[2L]] <- as.call(c(as.list(assignment[[2]]), rep(list(quote(expr = )), ndims - 1L)))

  }

  return(assignment)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .allocate_column ####

.allocate_column <- function(example, nrows, dfs, var) {

  a <- attributes(example)
  type <- typeof(example)
  class <- a$class
  isList <- is.recursive(example)

  a$names <- NULL
  a$class <- NULL

  if (isTRUE(is.data.frame(example))) {

    stop("Data frame column '", var, "' not supported by the df.rbind() function.", call. = FALSE)

  }

  if (isTRUE(is.array(example))) {

    if (isTRUE(length(dim(example)) > 1L)) {

      if (isTRUE("dimnames" %in% names(a))) {

        a$dimnames[1L] <- list(NULL)

        if (isTRUE(!is.null(names(a$dimnames))))

          names(a$dimnames)[1L] <- ""

      }

      # Check that all other args have consistent dims
      df_has <- vapply(dfs, function(df) var %in% names(df), FALSE)

      dims <- unique(lapply(dfs[df_has], function(df) dim(df[[var]])[-1]))

      if (isTRUE(length(dims) > 1L))

        stop("Array variable ", var, " has inconsistent dimensions.", call. = FALSE)

      a$dim <- c(nrows, dim(example)[-1L])

      length <- prod(a$dim)

    } else {

      a$dim <- NULL
      a$dimnames <- NULL
      length <- nrows

    }

  } else {

    length <- nrows

  }

  if (isTRUE(is.factor(example))) {

    df_has <- vapply(dfs, function(df) var %in% names(df), FALSE)

    isfactor <- vapply(dfs[df_has], function(df) is.factor(df[[var]]), FALSE)

    if (isTRUE(all(isfactor))) {

      levels <- unique(unlist(lapply(dfs[df_has], function(df) levels(df[[var]]))))

      a$levels <- levels

      handler <- "factor"

    } else {

      type <- "character"
      handler <- "character"
      class <- NULL
      a$levels <- NULL

    }

  } else if (isTRUE(inherits(example, "POSIXt"))) {

    tzone <- attr(example, "tzone")
    class <- c("POSIXct", "POSIXt")
    type <- "double"
    handler <- "time"

  } else {

    handler <- type

  }

  column <- vector(type, length)

  if (isTRUE(!isList)) {

    column[] <- NA

  }

  attributes(column) <- a

  assignment <- .make_assignment_call(length(a$dim))

  setter <- switch(
    handler,
    character = function(rows, what) {
      what <- as.character(what)
      eval(assignment)
    },
    factor = function(rows, what) {
      #duplicate what `[<-.factor` does
      what <- match(what, levels)
      #no need to check since we already computed levels
      eval(assignment)
    },
    time = function(rows, what) {
      what <- as.POSIXct(what, tz = tzone)
      eval(assignment)
    },
    function(rows, what) {
      eval(assignment)
    })

  getter <- function() {
    class(column) <<- class

    column

  }

  list(set = setter, get = getter)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .output_template ####

.output_template <- function(dfs, nrows) {

  vars <- unique(unlist(lapply(dfs, base::names)))
  output <- vector("list", length(vars))
  names(output) <- vars

  seen <- rep(FALSE, length(output))
  names(seen) <- vars

  for (df in dfs) {

    matching <- intersect(names(df), vars[!seen])

    for (var in matching) {

      output[[var]] <- .allocate_column(df[[var]], nrows, dfs, var)

    }

    seen[matching] <- TRUE
    if (isTRUE(all(seen))) break

  }

  list(setters = lapply(output, `[[`, "set"), getters = lapply(output, `[[`, "get"))
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
  cont[omitted, ] <- -1L * frequ[-omitted] / frequ[omitted]
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
  cont <- structure(cont / n.length, dimnames = list(n, n[-1L]))

  return(cont)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified code_helmert_forward Function from the codingMatrices Package ####

.forward.helmert <- function(n) {

  n.length <- length(n)
  cont <- rbind(diag(n.length:2L - 1L), 0)
  cont[lower.tri(cont)] <- -1L
  cont <- cont / rep(n.length:2L, each = n.length)
  dimnames(cont) <- list(n, n[1L:ncol(cont)])

  return(cont)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Modified contr_code_helmert Function from the faux Package ####

.reverse.helmert <- function(n) {

  n.length <- length(n)
  cont <- contr.helmert(n.length)
  for (i in 1L:(n.length - 1L)) {
    cont[, i] <- cont[, i] / (i + 1L)
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
# Internal functions for the cohens.d() function -------------------------------

.internal.d.function <- function(x, y, mu, paired, weighted, cor, ref, correct,
                                 alternative, conf.level) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One-sample ####
  if (isTRUE(is.null(y))) {

    # Unstandardized mean difference
    yx.diff <- mean(x, na.rm = TRUE) - mu

    # Standard deviation
    sd.group <- x.sd <- sd(x, na.rm = TRUE)

    # Sample size
    x.n <- length(na.omit(x))

    #...................
    ### Cohen's d ####

    d <- yx.diff / sd.group

    #...................
    ### Correction factor ####

    # Bias-corrected Cohen's d
    if (isTRUE(correct)) {

      v <- x.n - 1

      # Correction factor based on gamma function
      corr.factor <- gamma(0.5*v) / ((sqrt(v / 2L)) * gamma(0.5 * (v - 1L)))

      # Correction factor based on approximation method
      if (isTRUE(is.na(corr.factor) || is.nan(corr.factor) || is.infinite(corr.factor))) {

        corr.factor <- (1L - (3L / (4L * v - 1L)))

      }

      d <- d*corr.factor

    }

    #...................
    ### Confidence interval ####

    # Standard error
    d.se <- sqrt((x.n / (x.n / 2L)^2L) + 0.5*(d^2L / x.n))

    # Noncentrality parameter
    t <- yx.diff / (x.sd / sqrt(x.n))
    df <- x.n - 1

    conf1 <- ifelse(alternative == "two.sided", (1L + conf.level) / 2L, conf.level)
    conf2 <- ifelse(alternative == "two.sided", (1L - conf.level) / 2L, 1L - conf.level)

    st <- max(0.1, abs(t))

    ###

    end1 <- t
    while(suppressWarnings(pt(q = t, df = df, ncp = end1)) < conf1) { end1 <- end1 - st }

    ncp1 <- uniroot(function(x) conf1 - suppressWarnings(pt(q = t, df = df, ncp = x)), c(end1, 2*t - end1))$root

    ###

    end2 <- t
    while(suppressWarnings(pt(q = t, df = df, ncp = end2)) > conf2) { end2 <- end2 + st }

    ncp2 <- uniroot(function(x) conf2 - suppressWarnings(pt(q = t, df = df, ncp = x)), c(2*t - end2, end2))$root

    # Confidence interval around ncp
    conf.int <- switch(alternative,
                       two.sided = c(low = ncp1 / sqrt(df), upp = ncp2 / sqrt(df)),
                       less = c(low = -Inf, upp = ncp2 / sqrt(df)),
                       greater = c(low = ncp1 / sqrt(df), upp = Inf))

    # With correction factor
    if(isTRUE(correct)) {

      conf.int <- conf.int*corr.factor

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two-sample ####
  } else if (!isTRUE(paired)) {

    #...................
    ### Data ####

    # x and y
    x <- na.omit(x)
    y <- na.omit(y)

    # Sample size for x and y
    x.n <- length(x)
    y.n <- length(y)

    # Total sample size
    xy.n <- sum(c(x.n, y.n))

    # Unstandardized mean difference
    yx.diff <- mean(y) - mean(x)

    # Variance
    x.var <- var(x)
    y.var <- var(y)

    # Standard deviation
    x.sd <- sd(x)
    y.sd <- sd(y)

    # At least 2 observations for x/y and variance in x/y
    if (isTRUE((x.n >= 2L && y.n >= 2L) && (x.var != 0L && y.var != 0L))) {

      #...................
      ### Standard deviation ####

      # Pooled standard deviation
      if (isTRUE(is.null(ref))) {

        # Weighted pooled standard deviation, Cohen's d.s
        if (isTRUE(weighted)) {

          sd.group <- sqrt(((x.n - 1L)*x.var + (y.n - 1L)*y.var) / (xy.n - 2L))

          # Unweighted pooled standard deviation
        } else {

          sd.group <- sqrt(sum(c(x.var, y.var)) / 2L)

        }

      # Standard deviation from reference group x or y, Glass's delta
      } else {

        sd.group <- ifelse(ref == "x", x.sd, y.sd)

      }

      #...................
      ### Cohen's d ####

      d <- yx.diff / sd.group

      #...................
      ### Correction factor ####

      # Bias-corrected Cohen's d, i.e., Hedges' g
      if (isTRUE(correct)) {

        # Degrees of freedom
        v <- xy.n - 2L

        # Correction factor based on gamma function
        corr.factor <- gamma(0.5*v) / ((sqrt(v / 2L)) * gamma(0.5 * (v - 1L)))

        # Correction factor based on approximation method
        if (isTRUE(is.na(corr.factor) || is.nan(corr.factor) || is.infinite(corr.factor))) {

          corr.factor <- (1L - (3L / (4L * v - 1L)))

        }

        # Applying correction factor
        d <- d*corr.factor

      }

      #...................
      ### Confidence interval ####

      # No reference group
      if (isTRUE(is.null(ref))) {

        # Cohen's d.s

        # Pooled standard deviation
        if (isTRUE(weighted)) {

          d.se <- sd.group * sqrt(1 / x.n + 1 / y.n)
          df <- xy.n - 2

          # Unpooled standard deviation
        } else {

          d.se <- sqrt(sqrt(x.sd^2 / x.n)^2 + sqrt(y.sd^2 / y.n)^2)
          df <- d.se^4 / ( sqrt(x.sd^2 / x.n)^4 / (x.n - 1) + sqrt(y.sd^2 / y.n)^4 / (y.n - 1))

        }

        t <- yx.diff / d.se
        hn <- sqrt(1 / x.n + 1 / y.n)

        # Reference group
      } else {

        d.se <- sqrt(sd(c(x, y))^2*(1 / x.n + 1 / y.n))
        df <- x.n + y.n - 2

        t <- yx.diff / d.se
        hn <- sqrt(1 / x.n + 1 / y.n)

      }

      conf1 <- ifelse(alternative == "two.sided", (1 + conf.level) / 2, conf.level)
      conf2 <- ifelse(alternative == "two.sided", (1 - conf.level) / 2, 1 - conf.level)

      # Noncentrality parameter
      st <- max(0.1, abs(t))

      ###

      end1 <- t
      while(suppressWarnings(pt(q = t, df = df, ncp = end1)) < conf1) { end1 <- end1 - st }

      ncp1 <- uniroot(function(x) conf1 - suppressWarnings(pt(q = t, df = df, ncp = x)), c(end1, 2*t - end1))$root

      ###

      end2 <- t
      while(suppressWarnings(pt(q = t, df = df, ncp = end2)) > conf2) { end2 <- end2 + st }

      ncp2 <- uniroot(function(x) conf2 - suppressWarnings(pt(q = t, df = df, ncp = x)), c(2*t - end2, end2))$root

      # Confidence interval around ncp
      conf.int <- switch(alternative,
                         two.sided = c(low = ncp1 * hn, upp = ncp2 * hn),
                         less = c(low = -Inf, upp = ncp2 * hn),
                         greater = c(low = ncp1 * hn, upp = Inf))

      # With correction factor
      if(isTRUE(correct)) {

        conf.int <- conf.int*corr.factor

      }

    # Not at least 2 observations for x/y and variance in x/y
    } else {

      d <- d.se <- sd.group <- NA
      conf.int <- c(NA, NA)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Paired-sample ####
  } else if (isTRUE(paired)) {

    #...................
    ### Data ####

    xy.dat <- na.omit(data.frame(x = x, y = y, stringsAsFactors = FALSE))

    x <- xy.dat$x
    y <- xy.dat$y

    # Standard deviation of x and y
    x.sd <- sd(x)
    y.sd <- sd(y)

    # Unstandardized mean difference
    yx.diff <- mean(y - x)

    # Sample size
    xy.n <- nrow(xy.dat)

    #...................
    ### Standard deviation ####

    # SD of difference score, Cohen's d.z
    if (isTRUE(weighted)) {

      sd.group <- sd(y - x)

    } else {

      # Controlling correlation, Cohen's d.rm
      if (isTRUE(cor)) {

        # Variance of x and y
        x.var <- var(x)
        y.var <- var(y)

        # Sum of the variances
        xy.var.sum <- sum(c(x.var, y.var))

        # Correlation between x and y
        xy.r <- cor(x, y)

        sd.group <- sqrt(xy.var.sum - 2L * xy.r * prod(c(sqrt(x.var), sqrt(y.var))))

        # Ignoring correlation, Cohen's d.av
      } else {

        sd.group <- (x.sd + y.sd) / 2

      }

    }

    #...................
    ### Cohen's d ####

    # Cohen's d.rm
    if (isTRUE(cor && !isTRUE(weighted))) {

      d <- yx.diff / sd.group * sqrt(2L*(1L -xy.r))

      # Cohen's d.z, d.av, and Glass's delta
    } else {

      d <- yx.diff / sd.group

    }

    #...................
    ### Correction factor ####

    # Degrees of freedom
    v <- xy.n - 1

    # Correction factor based on gamma function
    corr.factor <- gamma(0.5*v) / ((sqrt(v / 2L)) * gamma(0.5 * (v - 1L)))

    # Correction factor based on approximation method
    if (isTRUE(is.na(corr.factor) || is.nan(corr.factor) || is.infinite(corr.factor))) {

      corr.factor <- 1L - 3L / (4L * v - 1L)

    }

    # Bias-corrected Cohen's d
    if (isTRUE(correct)) {

      d <- d*corr.factor

    }

    #...................
    ### Confidence interval ####

    # Standard error
    d.se <- sqrt((xy.n / (xy.n / 2)^2) + 0.5*(d^2 / xy.n))

    # Noncentrality parameter
    t <- yx.diff / (sd.group / sqrt(xy.n))
    df <- xy.n - 1

    conf1 <- ifelse(alternative == "two.sided", (1L + conf.level) / 2L, conf.level)
    conf2 <- ifelse(alternative == "two.sided", (1L - conf.level) / 2L, 1L - conf.level)

    st <- max(0.1, abs(t))

    ###

    end1 <- t
    while(suppressWarnings(pt(q = t, df = df, ncp = end1)) < conf1) { end1 <- end1 - st }

    ncp1 <- uniroot(function(x) conf1 - suppressWarnings(pt(q = t, df = df, ncp = x)), c(end1, 2*t - end1))$root

    ###

    end2 <- t
    while(suppressWarnings(pt(q = t, df = df, ncp = end2)) > conf2) { end2 <- end2 + st }

    ncp2 <- uniroot(function(x) conf2 - suppressWarnings(pt(q = t, df = df, ncp = x)), c(2*t - end2, end2))$root

    # Confidence interval around ncp
    conf.int <- switch(alternative,
                       two.sided = c(low = ncp1 / sqrt(df), upp = ncp2 / sqrt(df)),
                       less = c(low = -Inf, upp = ncp2 / sqrt(df)),
                       greater = c(low = ncp1 / sqrt(df), upp = Inf))

    # With correction factor
    if(isTRUE(correct)) {

      conf.int <- conf.int*corr.factor

    }

  }

  # Return object
  object <- data.frame(m.diff = yx.diff, sd = sd.group,
                       d = d, se = d.se,
                       low = conf.int[1L], upp = conf.int[2], row.names = NULL)

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the cor.matrix() function -----------------------------
#
# - .internal.cor.test.pearson
# - .internal.cor.test.spearman
# - .internal.cor.test.kendall.b
# - .internal.tau.c
# - .internal.polychoric

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .internal.cor.test.pearson Function ####

.internal.cor.test.pearson <- function(x, y) {

  # At least three cases
  if (isTRUE(nrow(na.omit(data.frame(x = x, y = y))) >= 3L)) {

    object <- suppressWarnings(cor.test(x, y, method = "pearson")) |> (\(y) list(stat = y$statistic, df = y$parameter, pval = y$p.value))()

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

  # At least three cases
  if (isTRUE(nrow(na.omit(data.frame(x = x, y = y))) >= 3L)) {

    object <- suppressWarnings(cor.test(x, y, method = "kendall", exact = FALSE, continuity = FALSE)) |> (\(y) list(stat = y$statistic, df = NA, pval = y$p.value))()

  # Less than three cases
  } else {

    object <- list(stat = NA, df = NA, pval = NA)

  }

  return(object)

}

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

    pmin <- apply(p, 2L, function(x) min(x, na.rm = TRUE))
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
# Internal functions for the dominance() function ------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dominance analysis supporting formula-based modeling functions ####

.domin <- function(formula_overall, reg, fitstat, sets = NULL, all = NULL,
                  conditional = TRUE, complete = TRUE, consmodel = NULL, reverse = FALSE, ...) {

  # Input check
  if (isTRUE(!inherits(formula_overall, "formula"))) { stop(paste(formula_overall, "is not a 'formula' class object."), call. = FALSE) }

  if (isTRUE(!is.null(attr(stats::terms(formula_overall), "offset")))) { stop("'offset()' terms not allowed in formula object.", call. = FALSE) }

  if (isTRUE(!is.list(fitstat))) { stop("fitstat is not a list.", call. = FALSE) }

  if (isTRUE(length(sets) > 0L & !is.list(sets))) { stop("sets is not a list.", call. = FALSE) }

  if (isTRUE(is.list(all))) { stop("all is a list.  Please submit it as a vector.", call. = FALSE) }

  if (isTRUE(!attr(stats::terms(formula_overall), "response"))) { stop(paste(deparse(formula_overall), "missing a response."), call. = FALSE) }

  if (isTRUE(any(attr(stats::terms(formula_overall), "order") > 1L))) { warning(paste(deparse(formula_overall), "contains second or higher order terms, fsunction may not handle them correctly."), call. = FALSE) }

  if (isTRUE(length(fitstat) < 2L)) { stop("fitstat requires at least two elements.", call. = FALSE) }

  # Process variable lists
  Indep_Vars <- attr(stats::terms(formula_overall), "term.labels")

  intercept <- as.logical(attr(stats::terms(formula_overall), "intercept"))

  if (isTRUE(length(sets) > 0L)) {

    set_aggregated <- sapply(sets, paste0, collapse = " + ")

    Indep_Vars <- append(Indep_Vars, set_aggregated)

  }

  Dep_Var <- attr(stats::terms(formula_overall), "variables")[[2L]]

  Total_Indep_Vars <- length(Indep_Vars)

  # IV-based exit conditions
  if (isTRUE(Total_Indep_Vars < 2L)) { stop(paste("Total of", Total_Indep_Vars, "independent variables or sets. At least 2 needed for useful dominance analysis."), call. = FALSE) }

  # Create independent variable/set combination list
  Combination_Matrix <- expand.grid(lapply(1:Total_Indep_Vars, function(x) c(FALSE, TRUE)), KEEP.OUT.ATTRS = FALSE)[-1L, ]

  Total_Models_to_Estimate <- 2L**Total_Indep_Vars - 1L

  # Define function to call regression models
  doModel_Fit <- function(Indep_Var_Combin_lgl, Indep_Vars, Dep_Var, reg, fitstat, all = NULL, consmodel = NULL, intercept, ...) {

    Indep_Var_Combination <- Indep_Vars[Indep_Var_Combin_lgl]

    formula_to_use <- stats::reformulate(c(Indep_Var_Combination, all, consmodel), response = Dep_Var, intercept = intercept)

    Model_Result <- list(do.call(reg, list(formula_to_use, ...)))

    if (isTRUE(length(fitstat) > 2L)) { Model_Result <- append(Model_Result, fitstat[3L:length(fitstat)]) }

    Fit_Value <- do.call(fitstat[[1L]], Model_Result)

    return( Fit_Value[[ fitstat[[2L]]]])

  }

  # Constant model adjustments
  Cons_Result <- NULL
  FitStat_Adjustment <- 0L
  if (isTRUE(length(consmodel) > 0L)) {

    FitStat_Adjustment <- Cons_Result <- doModel_Fit(NULL, Indep_Vars, Dep_Var, reg, fitstat, consmodel = consmodel, intercept = intercept, ...)

  }

  # All subsets adjustment
  All_Result <- NULL
  if (isTRUE(length(all) > 0L)) {

    FitStat_Adjustment <- All_Result <- doModel_Fit(NULL, Indep_Vars, Dep_Var, reg, fitstat, all = all, consmodel = consmodel, intercept = intercept, ...)

  }

  # Obtain all subsets regression results
  Ensemble_of_Models <- sapply(1L:nrow(Combination_Matrix), function(x) { doModel_Fit(unlist(Combination_Matrix[x, ]), Indep_Vars, Dep_Var, reg, fitstat, all = all, consmodel = consmodel, intercept = intercept, ...) },
                               simplify = TRUE, USE.NAMES = FALSE)

  # Conditional dominance statistics
  Conditional_Dominance <- NULL
  if (isTRUE(conditional)) {

    Conditional_Dominance <- matrix(nrow = Total_Indep_Vars, ncol = Total_Indep_Vars)

    Combination_Matrix_Anti <-!Combination_Matrix

    IVs_per_Model <- rowSums(Combination_Matrix)

    Combins_at_Order <- sapply(IVs_per_Model, function(x) choose(Total_Indep_Vars, x), simplify = TRUE, USE.NAMES = FALSE)

    Combins_at_Order_Prev <- sapply(IVs_per_Model, function(x) choose(Total_Indep_Vars - 1L, x), simplify = TRUE, USE.NAMES = FALSE)

    Weighted_Order_Ensemble <- ((Combination_Matrix*(Combins_at_Order - Combins_at_Order_Prev))**-1L)*Ensemble_of_Models

    Weighted_Order_Ensemble <- replace(Weighted_Order_Ensemble, Weighted_Order_Ensemble == Inf, 0L)

    Weighted_Order_Ensemble_Anti <- ((Combination_Matrix_Anti*Combins_at_Order_Prev)**-1L)*Ensemble_of_Models

    Weighted_Order_Ensemble_Anti <- replace(Weighted_Order_Ensemble_Anti, Weighted_Order_Ensemble_Anti == Inf, 0L)

    for (order in seq_len(Total_Indep_Vars)) {

      Conditional_Dominance[, order] <- t(colSums(Weighted_Order_Ensemble[IVs_per_Model == order, ]) - colSums(Weighted_Order_Ensemble_Anti[IVs_per_Model == (order - 1L), ]))

    }

    Conditional_Dominance[, 1L] <- Conditional_Dominance[, 1L] - FitStat_Adjustment

  }

  # Complete dominance statistics
  Complete_Dominance <- NULL
  if (isTRUE(complete)) {

    Complete_Dominance <- matrix(data = NA, nrow = Total_Indep_Vars, ncol = Total_Indep_Vars)

    Complete_Combinations <- utils::combn(seq_len(Total_Indep_Vars), 2L)

    for (pair in 1L:ncol(Complete_Combinations)) {

      Focal_Cols <- Complete_Combinations[, pair]

      NonFocal_Cols <- setdiff(1:Total_Indep_Vars, Focal_Cols)

      Select_2IVs <- cbind(Combination_Matrix, 1L:nrow(Combination_Matrix))[rowSums(Combination_Matrix[, Focal_Cols]) == 1L, ]

      Sorted_2IVs <- Select_2IVs[do.call("order", as.data.frame(Select_2IVs[,c(NonFocal_Cols, Focal_Cols)])), ]

      Compare_2IVs <- cbind(Ensemble_of_Models[Sorted_2IVs[(1L:nrow(Sorted_2IVs) %% 2L) == 0L, ncol(Sorted_2IVs)]], Ensemble_of_Models[Sorted_2IVs[(1L:nrow(Sorted_2IVs) %% 2) == 1L, ncol(Sorted_2IVs)]])

      Complete_Designation <- ifelse(all(Compare_2IVs[, 1L] > Compare_2IVs[, 2L]), FALSE, ifelse(all(Compare_2IVs[, 1L] < Compare_2IVs[, 2L]), TRUE, NA))

      Complete_Dominance[Focal_Cols[[2L]], Focal_Cols[[1L]]] <- Complete_Designation

      Complete_Dominance[Focal_Cols[[1L]], Focal_Cols[[2L]]] <- !Complete_Designation

    }

  }

  if (isTRUE(reverse)) { Complete_Dominance <- !Complete_Dominance }

  # General dominance statistics
  General_Dominance <- rowMeans(Conditional_Dominance)
  if (isTRUE(!conditional)) {

    Combination_Matrix_Anti <-!Combination_Matrix

    IVs_per_Model <- rowSums(Combination_Matrix)

    Combins_at_Order <- sapply(IVs_per_Model, function(x) choose(Total_Indep_Vars, x), simplify = TRUE, USE.NAMES = FALSE)

    Combins_at_Order_Prev <- sapply(IVs_per_Model, function(x) choose(Total_Indep_Vars - 1L, x), simplify = TRUE, USE.NAMES = FALSE)

    Indicator_Weight <- Combination_Matrix*(Combins_at_Order - Combins_at_Order_Prev)

    Indicator_Weight_Anti <- (Combination_Matrix_Anti*Combins_at_Order_Prev)*-1L

    Weight_Matrix <- ((Indicator_Weight + Indicator_Weight_Anti)*Total_Indep_Vars)^-1L

    General_Dominance <- colSums(Ensemble_of_Models*Weight_Matrix)

    General_Dominance <- General_Dominance - FitStat_Adjustment/Total_Indep_Vars

  }

  # Overall fit statistic and ranks
  FitStat <- sum(General_Dominance) + FitStat_Adjustment

  if (isTRUE(!reverse)) { General_Dominance_Ranks <- rank(-General_Dominance) } else { General_Dominance_Ranks <- rank(General_Dominance) }

  # Return values and attributes
  if (isTRUE(length(sets) == 0L)) IV_Labels <- { attr(stats::terms(formula_overall), "term.labels") } else { IV_Labels <- c( attr(stats::terms(formula_overall), "term.labels"), paste0("set", 1:length(sets))) }

  names(General_Dominance) <- IV_Labels
  names(General_Dominance_Ranks) <- IV_Labels
  if (isTRUE(conditional)) { dimnames(Conditional_Dominance) <- list(IV_Labels, paste0("IVs_", seq_along(Indep_Vars))) }

  if (isTRUE(complete)) { dimnames(Complete_Dominance) <- list(paste0("Dmnates_", IV_Labels),  paste0("Dmnated_", IV_Labels)) }

  if (isTRUE(!reverse)) { Standardized <- General_Dominance / (FitStat - ifelse(length(Cons_Result) > 0L, Cons_Result, 0L)) } else { Standardized <- -General_Dominance / -(FitStat - ifelse(length(Cons_Result) > 0L, Cons_Result, 0L)) }

  # Return object
  return_list <- list(General_Dominance = General_Dominance,
                      Standardized = Standardized,
                      Ranks = General_Dominance_Ranks,
                      Conditional_Dominance = Conditional_Dominance,
                      Complete_Dominance = Complete_Dominance,
                      Fit_Statistic_Overall = FitStat,
                      Fit_Statistic_All_Subsets = All_Result - ifelse(is.null(Cons_Result), 0, Cons_Result),
                      Fit_Statistic_Constant_Model = Cons_Result,
                      Call = match.call(),
                      Subset_Details = list(Full_Model = stats::reformulate(c(Indep_Vars, all, consmodel), response = Dep_Var, intercept = intercept),
                                            Formula = attr(stats::terms(formula_overall), "term.labels"),
                                            All = all, Sets = sets, Constant = consmodel))

  class(return_list) <- c("domin", "list")

  return(return_list)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the dominance.manual() function -----------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Enumerate the Combinations or Permutation of the ELements of a Vector ####

# combinations() from the gtools package
.combinations <- function(n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE) {

  if (isTRUE(mode(n) != "numeric" || length(n) != 1L || n < 1L || (n %% 1) != 0L)) { stop("bad value of n") }
  if (isTRUE(mode(r) != "numeric" || length(r) != 1L || r < 1L || (r %% 1) != 0L)) { stop("bad value of r") }

  if (isTRUE(!is.atomic(v) || length(v) < n)) { stop("v is either non-atomic or too short") }

  if (isTRUE((r > n) & !repeats.allowed)) { stop("r > n and repeats.allowed = FALSE", call. = FALSE) }

  if (isTRUE(set)) {

    v <- unique(sort(v))
    if (length(v) < n) stop("Too few different elements", call. = FALSE)

  }

  v0 <- vector(mode(v), 0L)

  ## Inner workhorse
  if (repeats.allowed) {

    sub <- function(n, r, v) {

      if (isTRUE(r == 0L)) { v0 } else if (isTRUE(r == 1L)) { matrix(v, n, 1) } else if (isTRUE(n == 1L)) { matrix(v, 1L, r) } else { rbind(cbind(v[1L], Recall(n, r - 1L, v)), Recall(n - 1L, r, v[-1L])) }

    }

  } else {

    sub <- function(n, r, v) {

      if (isTRUE(r == 0L)) { v0 } else if (isTRUE(r == 1L)) { matrix(v, n, 1) } else if (isTRUE(r == n)) { matrix(v, 1L, n) } else { rbind(cbind(v[1], Recall(n - 1L, r - 1L, v[-1L])), Recall(n - 1L, r, v[-1L])) }

    }

    return(sub(n, r, v[1L:n]))

  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dominance analysis functions ####

.DA <- function(cormat, index = NULL) {

  # Correlation matrix of the predictors
  Px <- cormat[-1L, -1L]

  # Correlation vector
  rx <- cormat[-1L, 1L]

  if (isTRUE(is.null(index))) { index = as.list(1:length(rx)) }

  # Number of predictors or groups of predictors
  J <- length(index)

  # R2 for model with subset xi
  R2 <- function(xi) {

    xi <- unlist(index[xi])

    R2 <- t(rx[xi])%*%solve(Px[xi, xi])%*%rx[xi]

  }

  # Average R2 change for a subset model with k size

  # Possible subset models before adding a predictor
  submodel <- function(k) {

    temp0 <- lapply(1L:J, function(i) .combinations(J - 1L, k, (1L:J)[-i]))

    # Possible subset models after adding a predictor
    temp1 <- lapply(1:J, function(i) t(apply(temp0[[i]], 1L, function(x) c(x, i))))

    # R2 before adding a predictor
    R0 <- lapply(temp0, function(y) apply(y, 1L, R2))

    # R2 after adding a predictor
    R1 <- lapply(temp1, function(y) apply(y, 1L, R2))

    # R2 change
    deltaR2 <- mapply(function(x, y) x - y, R1, R0)

    # Average R2 change
    adeltaR2 <- apply(matrix(deltaR2, ncol = J), 2L, mean)

    return(adeltaR2)

  }

  # Different model size k
  R2matrix <- t(sapply(1L:(J - 1L), submodel))

  R2matrix <- rbind(sapply(1L:J, R2), R2matrix)

  # Overall average
  DA <- apply(R2matrix, 2L, mean)

  return(DA)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the effsize() function --------------------------------
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
  model <- tryCatch(suppressWarnings(chisq.test(tab, correct = FALSE, p = p)), error = function(y) { list(statistic = NA, parameter = NA) })

  # Table with at least two rows and two columns and chi-square statistic is not NA or NaN
  if (isTRUE(ncol(tab) >= 2L && nrow(tab) >= 2L && !is.na(model$parameter) && !is.nan(model$statistic))) {

    # Test statistic
    chisq <- model$statistic

    # Degrees of freedom
    df <- model$parameter

    # Sample size
    n <- sum(tab)

    # Noncentral parameter
    chisqs <- vapply(chisq, .get_ncp_chi, FUN.VALUE = numeric(2L), df = df, conf.level = conf.level, alternative = alternative)

    # Result table
    result <- data.frame(phi = sqrt(chisq / n), low = sqrt(chisqs[1L, ] / n), upp = sqrt(chisqs[2L, ] / n), row.names = NULL)

    # Adjusted phi coefficient
    if (isTRUE(adjust)) { result <- result / min(c(sqrt((sum(tab[1L, ])*sum(tab[, 2L])) / (sum(tab[, 1L])*sum(tab[2L, ]))), sqrt((sum(tab[, 1L])*sum(tab[2L, ])) / (sum(tab[1L, ])*sum(tab[, 2L]))))) }

    # One-sided confidence interval
    switch(alternative, less = { result$low <- 0L }, greater = { result$upp <- 1L })

    # Add sample size
    result <- data.frame(n = n, result)

  } else {

    result <- data.frame(n = sum(tab), phi = NA, low = NA, upp = NA)

  }

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

  # Phi is not NA
  if (isTRUE(!is.na(result$phi))) {

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

  } else {

    result <- data.frame(n = sum(tab), v = NA, low = NA, upp = NA)

  }

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

  # Phi is not NA
  if (isTRUE(!is.na(result$phi))) {

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

  } else {

    result <- data.frame(n = sum(tab), t = NA, low = NA, upp = NA)

  }

  return(result)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .cont Function ####

.cont <- function(x, adjust, p = NULL, conf.level = conf.level, alternative = alternative) {

  # Cross tabulation
  tab <- table(x)

  # Phi coefficient
  result <- .phi(x, adjust = FALSE, p = p, conf.level = conf.level, alternative = alternative)[, -1L]

  # Phi is not NA
  if (isTRUE(!is.na(result$phi))) {

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

  } else {

    result <- data.frame(n = sum(tab), c = NA, low = NA, upp = NA)

  }

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

  # Phi is not NA
  if (isTRUE(!is.na(result$phi))) {

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

  } else {

    result <- data.frame(n = sum(tab), w = NA, low = NA, upp = NA)

  }

  return(result)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .fei Function ####

.fei <- function(x, adjust, p = NULL, conf.level = conf.level, alternative = alternative) {

  # Cross tabulation
  tab <- table(x)

  # Vector of probabilities
  if (isTRUE(is.null(p))) { p <- rep(1L / length(tab), times = length(tab)) }

  # Fei
  result <- .phi(x, adjust = FALSE, p = p, conf.level = conf.level, alternative = alternative)[, -1L]

  # Phi is not NA
  if (isTRUE(!is.na(result$phi))) {

    result <- data.frame(lapply(result, function(y) y / sqrt(1L / min(p) - 1L)))

    # One-sided confidence interval
    switch(alternative, less = { result$upp <- pmin(result$upp, 1L) }, greater = { result$upp <- 1L})

    # Add sample size and column names
    result <- data.frame(n = sum(tab), setNames(result, nm = c("fei", "low", "upp")))

  } else {

    result <- data.frame(n = sum(tab), fei = NA, low = NA, upp = NA)

  }

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
# Internal functions for the item.alpha() and item.omega() function ------------
#
# - .alpha.omega
# - .categ.alpha.omega
# - .getThreshold
# - .polycorLavaan
# - .refit
# - .p2
#
# MBESS: The MBESS R Package
# https://cran.r-project.org/web/packages/MBESS/index.html

.alpha.omega <- function(y, alpha, y.rescov = NULL, y.type = type, y.std = std, estimator = estimator, missing = missing, check = TRUE) {

  std <- type <- NULL

  # Variable names
  vnames <- colnames(y)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alpha or Omega for Continuous Items ####

  if (isTRUE(y.type != "categ")) {

    #...................
    ### Mode specification ####

    # Factor model: Coefficient Alpha
    if (isTRUE(alpha)) {

      mod.factor <- paste("f =~", paste(paste0("L*", vnames), collapse = " + "))

    # Factor model: Coefficient Omega
    } else {

      mod.factor <- paste("f =~", paste(vnames, collapse = " + "))

    }

    # Residual covariance
    if (isTRUE(!is.null(y.rescov))) { mod.factor <- vapply(y.rescov, function(y) paste(y, collapse = " ~~ "), FUN.VALUE = character(1L)) |> (\(y) paste(mod.factor, "\n", paste(y, collapse = " \n ")))() }

    #...................
    ### Model estimation ####

    mod.fit <- suppressWarnings(lavaan::cfa(mod.factor, data = y, ordered = FALSE, se = "none", std.lv = TRUE, estimator = estimator, missing = missing))

    #...................
    ### Check for convergence and negative degrees of freedom ####

    if (isTRUE(check)) {

      # Model convergence
      if (!isTRUE(lavaan::lavInspect(mod.fit, "converged"))) { warning("CFA model did not converge, results are most likely unreliable.", call. = FALSE) }

      # Degrees of freedom
      if (isTRUE(lavaan::lavInspect(mod.fit, "fit")["df"] < 0L)) { warning("CFA model has negative degrees of freedom, results are most likely unreliable.", call. = FALSE) }

    }

    #...................
    ### Parameter estimates ####

    if (isTRUE(!y.std)) {

      # Unstandardized parameter estimates
      param <- lavaan::parameterestimates(mod.fit)

    } else {

      # Standardized parameter estimates
      param <- misty::df.rename(lavaan::standardizedSolution(mod.fit), from = "est.std", to = "est")

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
    ### Alpha or Omega ####

    # Numerator
    load.sum2 <- sum(param.load$est)^2L

    # Total alpha
    if (isTRUE(y.type != "hierarch"))  {

      resid.sum <- sum(param.resid$est)

      # Residual covariances
      if (isTRUE(!is.null(y.rescov))) { resid.sum <- resid.sum + 2L*sum(param.rcov$est) }

      coef.alpha.omega <- load.sum2 / (load.sum2 + resid.sum)

    #...................
    ### Hierarchical Alpha or Omega ####
    } else {

      mod.cov.fit <- paste(apply(combn(seq_len(length(vnames)), m = 2L), 2L, function(z) paste(vnames[z[1L]], "~~", vnames[z[2L]])), collapse = " \n ") |>
        (\(z) suppressWarnings(lavaan::cfa(z, data = y, ordered = FALSE, se = "none", estimator = estimator, missing = missing)))()

      if (isTRUE(!y.std)) {

        var.total <- lavaan::parameterEstimates(mod.cov.fit) |> (\(y) sum(y[y$lhs == y$rhs, "est"], 2*y[y$lhs != y$rhs & y$op == "~~", "est"]))()

      } else {

        var.total <- lavaan::standardizedSolution(mod.cov.fit) |> (\(y) sum(y[y$lhs == y$rhs, "est"], 2*y[y$lhs != y$rhs & y$op == "~~", "est"]))()

      }

      coef.alpha.omega <- load.sum2 / var.total

    }

    # Return object
    object <- list(mod.fit = mod.fit, coef.alpha.omega = coef.alpha.omega)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alpha or Omega for Ordered-Categorical Items ####
  } else {

    object <- .categ.alpha.omega(dat = y, alpha = alpha, y.rescov = y.rescov, estimator = estimator, missing = missing, check = TRUE)

  }

  return(object)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .categ.alpha.omega Function ####

.categ.alpha.omega <- function(dat, alpha, y.rescov = NULL, estimator = estimator, missing = missing, check = TRUE) {

  # Variable names
  vnames <- colnames(dat)

  # Sequence from 1 to the number of columns
  q <- seq_len(ncol(dat))

  # Convert in ordered factor
  dat <- data.frame(lapply(dat, ordered))

  # Factor model: Coefficient Alpha
  if (isTRUE(alpha)) {

    mod.factor <- paste("f =~", paste(paste0("L*", vnames), collapse = " + "))

  # Factor model: Coefficient Omega
  } else {

    mod.factor <- paste("f =~", paste(vnames, collapse = " + "))

  }

  # Residual covariances
  if (isTRUE(!is.null(y.rescov))) { mod.factor <- vapply(y.rescov, function(y) paste(y, collapse = " ~~ "), FUN.VALUE = character(1L)) |> (\(y) paste(mod.factor, "\n", paste(y, collapse = " \n ")))() }

  # Estimate model
  mod.fit <- suppressWarnings(lavaan::cfa(mod.factor, data = dat, estimator = estimator, missing = missing, std.lv = TRUE, se = "none", ordered = TRUE))

  # Model convergence
  if (isTRUE(check)) { if (!isTRUE(lavaan::lavInspect(mod.fit, "converged"))) { warning("CFA model did not converge, results are most likely unreliable.", call. = FALSE) } }

  param <- lavaan::inspect(mod.fit, "coef")

  ly <- param[["lambda"]]
  ps <- param[["psi"]]

  threshold <- .getThreshold(mod.fit)[[1L]]

  denom <- .polycorLavaan(mod.fit, dat, estimator = estimator, missing = missing)[vnames, vnames]

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

  return(list(mod.fit = mod.fit, coef.alpha.omega = sumnum / addden))

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

.polycorLavaan <- function(object, data, estimator = estimator, missing = missing) {

  ngroups <- lavaan::inspect(object, "ngroups")

  coef <- lavaan::inspect(object, "coef")

  targettaunames <- NULL

  if (isTRUE(ngroups == 1L)) {

    targettaunames <- rownames(coef$tau)

  } else {

    targettaunames <- rownames(coef[[1L]]$tau)

  }

  barpos <- sapply(strsplit(targettaunames, ""), function(x) which(x == "|"))

  vnames <- unique(apply(data.frame(targettaunames, barpos - 1L, stringsAsFactors = FALSE), 1L, function(x) substr(x[1L], 1L, x[2L])))

  script <- ""

  for(i in 2L:length(vnames)) {

    temp <- paste0(vnames[1L:(i - 1L)], collapse = " + ")

    temp <- paste0(vnames[i], " ~~ ", temp, "\n")

    script <- paste(script, temp)

  }

  suppressWarnings(newobject <- .refit(pt = script, data = data, vnames = vnames, object = object, estimator = estimator, missing = missing))

  if (isTRUE(ngroups == 1L)) {

    return(lavaan::inspect(newobject, "coef")$theta)

  } else {

    return(lapply(lavaan::inspect(newobject, "coef"), "[[", "theta"))

  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .refit Function ####

.refit <- function(pt, data, vnames, object, estimator = estimator, missing = missing) {

  args <- lavaan::lavInspect(object, "call")

  args$model <- pt
  args$data <- data
  args$ordered <- vnames
  tempfit <- do.call(eval(parse(text = paste0("lavaan::", "lavaan"))), args[-1])

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## .p2 Function ####

.p2 <- function(t1, t2, r) { mnormt::pmnorm(c(t1, t2), c(0L, 0L), matrix(c(1L, r, r, 1L), 2L, 2L)) }

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the mplus.print() function ----------------------------
#
# - .section.ind.from.to

.section.ind.from.to <- function(x, cur.section, run, input = FALSE) {

  # Input
  if (isTRUE(input)) {

    parse.text <- parse(text = paste0("min(unlist(sapply(x, function(z) if (isTRUE(any(grep(z, run)))) {
                            sapply(grep(z, run), function(q) if (isTRUE(q > max(", paste0(sapply(cur.section, function(w) paste0("grep(", paste0("\"", w, "\""), ", run")), collapse = " | "), ")))) { q } else { length(run) + 1L })
                            } )))"))

    # Output
  } else {

    parse.text <- parse(text = paste0("min(unlist(sapply(x, function(z) if (isTRUE(any(run == z))) {
                            sapply(which(run == z), function(q) if (isTRUE(q > max(which(", paste0(sapply(cur.section, function(w) paste0("run == ", paste0("\"", w, "\""))), collapse = " | "), ")))) { q } else { length(run) + 1L })
                            })))"))

  }

  return(eval(parse.text) - 1L)

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

    return(TRUE)

  } else {

    return(FALSE)

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

    return(TRUE)

  } else {

    return(FALSE)

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

    return(TRUE)

  } else {

    return(FALSE)

  }

}

os <- function() {

  windows <- is.windows()
  macos <- is.macos()
  linux <- is.linux()

  count <- windows + macos + linux

  if (isTRUE(count > 1L) || isTRUE(count == 0L)) {

    return("unknown")

  } else if (windows) {

    return("windows")

  } else if (macos) {

    return("macos")

  } else if (linux) {

    return("linux")

  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Detect the location/name of the Mplus command ####

.detect.mplus <- function() {

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
      "\n")

    mplus_found <- FALSE

    suppressWarnings(mplus <- system("which mplus", intern = TRUE, ignore.stderr = TRUE))

    if (isTRUE(length(mplus) > 0L && file.exists(mplus))) {

      mplus <- "mplus"
      mplus_found <- TRUE

    } else {

      if (isTRUE(dir.exists("/opt/mplus"))) {

        test <- file.path(list.dirs("/opt/mplus", recursive = FALSE)[1], "mplus")
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

    if (isTRUE(!mplus_found)) { stop(failure_note) }

  }

  if (isTRUE(identical(ostype, "unknown"))) { stop("OS Type not known. Cannot auto detect Mplus command name. You must specify it.") }

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

      this_set <- list.files(path = directory, recursive = recursive, pattern = ".*\\.inp?$", full.names = TRUE)
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
#                        the blimp() and blimp.update() function ---------------
#
# - .extract.section

# Function for Extracting Input Command Sections ####
.extract.section <- function(section, x, section.pos) {

  # Sort section position
  section.pos <- sort(section.pos)

  # Split input
  split.x <- strsplit(x, "")[[1L]]

  # Start position of the section
  start <- setdiff(as.numeric(gregexec(section, toupper(x))[[1L]]),
                   c(as.numeric(gregexec(paste0("\\.", section), toupper(x))[[1L]]) + 1L, as.numeric(gregexec(paste0("\\_", section), toupper(x))[[1L]]) + 1L, as.numeric(gregexec("SAVEDATA:", toupper(x))[[1L]]) + 4L))

  # Multiple sections
  if (isTRUE(length(start) > 1L && section != "TEST:")) {

    stop(paste0("There are more than one ", dQuote(section), " sections in the input text."), call. = FALSE)

  # One section
  } else if (isTRUE(length(start) == 1L)) {

    # End position of the section
    end <- if (isTRUE(any(section.pos > start))) { section.pos[which(section.pos > start)][1L] - 1L } else { length(split.x) }

    # Extract section
    object <- misty::chr.trim(strsplit(paste(split.x[start:end], collapse = ""), "\n")[1L], side = "right", check = FALSE)

    # Remove last "\n"
    object[length(object)] <- gsub("\n", "", object[length(object)])

    # Collapse with "\n" and return object
    object <- paste(object, collapse = "\n")

  # Multiple TEST: sections in Blimp
  } else if (isTRUE(length(start) > 1L)) {

    # End position of the section
    end <- sapply(start, function(y) {

      if (isTRUE(any(section.pos > y))) { section.pos[which(section.pos > y)][1L] - 1L } else { length(split.x) }

    })

    # Extract section
    object <- sapply(seq_along(start), function(y) {

      misty::chr.trim(strsplit(paste(split.x[start[y]:end[y]], collapse = ""), "\n")[1L], side = "right", check = FALSE)

    })

    # Paste sections
    object <- paste(object, collapse = "\n")

  }

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the multilevel.r2() function ---------------------------
#
# - .variable.section
# - .r2mlm
# - .r2mlm_lmer
# - .r2mlm_nlme
# - .r2mlm_manual
# - .prepare_data
# - .add_interaction_vars_to_data
# - .get_random_slope_vars
# - .get_cwc
# - .get_interaction_vars
# - .sort_variables

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Internal functions from the r2mlm package ####

### .r2mlm() Function ####
.r2mlm <- function(model) {

  temp_formula <- formula(model)
  grepl_array <- grepl("I(", temp_formula, fixed = TRUE)

  for (bool in grepl_array) {

    if (isTRUE(bool)) {

      stop("Error: r2mlm does not allow for models fit using the I() function; user must thus manually include any desired transformed predictor variables such as x^2 or x^3 as separate columns in dataset.")

    }

  }

  # call appropriate .r2mlm helper function
  if (isTRUE(typeof(model) == "list")) {

    .r2mlm_nlme(model)

  } else if (isTRUE(typeof(model) == "S4")) {

    .r2mlm_lmer(model)

  } else {

    stop("You must input a model generated using either the lme4 or nlme package.")

  }

}

### .r2mlm_lmer() Function ####
.r2mlm_lmer <- function(model) {

  # Visible global function definition
  is <- fixef <- getME <- NULL

  # Step 1) check if model has_intercept
  if (isTRUE(attr(terms(model), which = "intercept") == 1L)) {

    has_intercept <- TRUE

  } else {

    has_intercept <- FALSE

  }

  # Step 2) Pull all variable names from the formula
  all_variables <- all.vars(formula(model))
  cluster_variable <- all_variables[length(all_variables)]

  # Step 3a) Pull and prepare data
  data <- .prepare_data(model, "lme4", cluster_variable)

  # Step 3b) Determine whether data is appropriate format
  # a) Pull all variables except for cluster
  outcome_and_predictors <- all_variables[1L:(length(all_variables) - 1L)]

  # b) If any of those variables is non-numeric, then throw an error
  for (variable in outcome_and_predictors) {

    if (isTRUE(!is(data[[variable]], "integer") && !is(data[[variable]], "numeric"))) {

      stop("Your data must be numeric. Only the cluster variable can be a factor.")

    }

  }

  # Step 4a) Define variables you'll be sorting
  if (isTRUE(length(outcome_and_predictors) == 1L)) {

    predictors <- .get_interaction_vars(model)

  } else {

    predictors <- append(outcome_and_predictors[2L:length(outcome_and_predictors)], .get_interaction_vars(model))

  }

  # Step 4b) Create and fill vectors
  l1_vars <- .sort_variables(data, predictors, cluster_variable)$l1_vars
  l2_vars <- .sort_variables(data, predictors, cluster_variable)$l2_vars

  # Step 5) Pull variable names for L1 predictors with random slopes into a variable called random_slope_vars
  random_slope_vars <- .get_random_slope_vars(model, has_intercept, "lme4")

  # Step 6) Determine value of centered within cluster
  if (is.null(l1_vars)) {

    centeredwithincluster <- TRUE

  } else {

    centeredwithincluster <- .get_cwc(l1_vars, cluster_variable, data)

  }

  # 7a) within_covs (l1 variables)
  within <- .get_covs(l1_vars, data)

  # 7b) pull column numbers for between_covs (l2 variables)
  between <- .get_covs(l2_vars, data)

  # 7c) pull column numbers for random_covs (l1 variables with random slopes)
  random <- .get_covs(random_slope_vars, data)

  # 8a) gamma_w, fixed slopes for L1 variables (from l1_vars list)
  gammaw <- c()
  i <- 1L
  for (variable in l1_vars) {

    gammaw[i] <- fixef(model)[variable]

    i <- i + 1L

  }

  # 8b) gamma_b, intercept value if hasintercept = TRUE, and fixed slopes for L2 variables (from between list)
  gammab <- c()
  if (isTRUE(has_intercept)) {

    gammab[1L] <- fixef(model)[1L]

    i <- 2L

  } else {

    i <- 1L

  }

  for (var in l2_vars) {

    gammab[i] <- fixef(model)[var]

    i = i + 1L

  }

  # Step 9) Tau matrix, results from VarCorr(model)
  vcov <- VarCorr(model)
  tau <- as.matrix(Matrix::bdiag(vcov))

  # Step 10) Sigma^2 value, Rij
  sigma2 <- getME(model, "sigma")^2L

  # Step 11) Input everything into r2mlm_manual
  .r2mlm_manual(as.data.frame(data), within_covs = within, between_covs = between, random_covs = random,
                gamma_w = gammaw, gamma_b = gammab, Tau = tau, sigma2 = sigma2, has_intercept = has_intercept, clustermeancentered = centeredwithincluster)

}

### .r2mlm_nlme() Function ####
.r2mlm_nlme <- function(model) {

  # Visible global function definition
  is <- getME <- NULL

  # Step 1) check if model has_intercept
  if (isTRUE(attr(terms(model), which = "intercept") == 1L)) {

    has_intercept = TRUE

  } else {

    has_intercept = FALSE

  }

  # Step 2) Pull all variable names from the formula
  all_variables <- all.vars(formula(model))
  cluster_variable <- attr(nlme::getGroups(model), which = "label")

  # Add the grouping var to list of all variables, and calculate formula length (for later use, to iterate)
  all_variables[length(all_variables) + 1L] <- cluster_variable
  formula_length <- length(all_variables) # this returns the number of elements in the all_vars list TODO remove this

  # Step 3a) Pull and prepare data
  data <- .prepare_data(model, "nlme", cluster_variable)

  # Step 3b) Determine whether data is appropriate format. Only the cluster variable can be a factor, for now

  outcome_and_predictors <-  all_variables[1L:length(all_variables) - 1L]

  for (variable in outcome_and_predictors) {

    if (isTRUE(!is(data[[variable]], "integer") && !is(data[[variable]], "numeric"))) {

      stop("Your data must be numeric. Only the cluster variable can be a factor.")

    }

  }

  # Step 4a) Define variables you'll be sorting
  if (isTRUE(length(outcome_and_predictors) == 1L)) {

    predictors <- .get_interaction_vars(model)

  } else {

    predictors <- append(outcome_and_predictors[2:length(outcome_and_predictors)], .get_interaction_vars(model))

  }

  # Step 4b) Create and fill vectors
  l1_vars <- .sort_variables(data, predictors, cluster_variable)$l1_vars
  l2_vars <- .sort_variables(data, predictors, cluster_variable)$l2_vars

  # Step 5) pull variable names for L1 predictors with random slopes into a variable called random_slope_vars
  random_slope_vars <- .get_random_slope_vars(model, has_intercept, "nlme")

  # Step 6) determine value of centeredwithincluster
  if (isTRUE(is.null(l1_vars))) {

    centeredwithincluster <- TRUE

  } else {

    centeredwithincluster <- .get_cwc(l1_vars, cluster_variable, data)

  }

  # 7a) within_covs (l1 variables)
  within <- .get_covs(l1_vars, data)

  # 7b) Pull column numbers for between_covs (l2 variables)
  between <- .get_covs(l2_vars, data)

  # 7c) Pull column numbers for random_covs (l1 variables with random slopes)
  random <- .get_covs(random_slope_vars, data)

  # 8a) gamma_w, fixed slopes for L1 variables (from l1_vars list)
  gammaw <- c()
  i = 1L
  for (variable in l1_vars) {

    gammaw[i] <- nlme::fixef(model)[variable]

    i = i + 1L

  }

  # 8b) gamma_b, intercept value if hasintercept = TRUE, and fixed slopes for L2 variables (from between list)
  gammab <- c()
  if (isTRUE(has_intercept)) {

    gammab[1L] <- nlme::fixef(model)[1L]

    i = 2L

  } else {

    i = 1L

  }

  for (variable in l2_vars) {

    gammab[i] <- nlme::fixef(model)[variable]

    i = i + 1L
  }

  # Step 9) Tau matrix
  tau <- nlme::getVarCov(model)

  # Step 10) sigma^2 value, Rij
  sigma2 <- model$sigma^2

  # Step 11) Input everything into r2mlm_manual
  .r2mlm_manual(as.data.frame(data), within_covs = within, between_covs = between, random_covs = random,
                gamma_w = gammaw, gamma_b = gammab, Tau = tau, sigma2 = sigma2, has_intercept = has_intercept, clustermeancentered = centeredwithincluster)

}

### .r2mlm_manual() Function ####
.r2mlm_manual <- function(data, within_covs, between_covs, random_covs,
                          gamma_w, gamma_b, Tau, sigma2, has_intercept = TRUE, clustermeancentered = TRUE) {

  if (isTRUE(has_intercept)) {

    if (isTRUE(length(gamma_b) > 1L)) gamma <- c(1L, gamma_w, gamma_b[2:length(gamma_b)])
    if (isTRUE(length(gamma_b) == 1L)) gamma <- c(1L, gamma_w)
    if (isTRUE(is.null(within_covs))) gamma_w <- 0L

  }

  if (isTRUE(!has_intercept)) {

    gamma <- c(gamma_w, gamma_b)
    if (isTRUE(is.null(within_covs))) gamma_w <- 0L
    if (isTRUE(is.null(between_covs))) gamma_b <- 0L

  }

  if (isTRUE(is.null(gamma))) gamma <- 0L

  # Compute phi
  phi <- var(cbind(1L, data[, c(within_covs)], data[, c(between_covs)]), na.rm = TRUE)
  if (isTRUE(!has_intercept)) phi <- var(cbind(data[, c(within_covs)], data[, c(between_covs)]), na.rm = TRUE)
  if (isTRUE(is.null(within_covs) && is.null(within_covs) && !has_intercept)) phi <- 0L
  phi_w <- var(data[, within_covs], na.rm = TRUE)
  if (isTRUE(is.null(within_covs))) phi_w <- 0L
  phi_b <- var(cbind(1L, data[, between_covs]), na.rm = TRUE)
  if (isTRUE(is.null(between_covs))) phi_b <- 0L

  # Compute psi and kappa
  var_randomcovs <- var(cbind(1, data[, c(random_covs)]), na.rm = TRUE)
  if (isTRUE(length(Tau) > 1L)) psi <- matrix(c(diag(Tau)), ncol = 1L)
  if (isTRUE(length(Tau) == 1L)) psi <- Tau
  if (isTRUE(length(Tau) > 1L)) kappa <- matrix(c(Tau[lower.tri(Tau) == TRUE]), ncol = 1)
  if (isTRUE(length(Tau) == 1L)) kappa <- 0L

  v <- matrix(c(diag(var_randomcovs)), ncol = 1L)
  r <- matrix(c(var_randomcovs[lower.tri(var_randomcovs) == TRUE]), ncol = 1L)

  if (isTRUE(is.null(random_covs))) {

    v <- 0L
    r <- 0L
    m <- matrix(1L, ncol = 1L)

  }

  if (isTRUE(length(random_covs) > 0L)) m <- matrix(c(colMeans(cbind(1, data[, c(random_covs)]), na.rm = TRUE)), ncol = 1L)

  # Total variance
  totalvar_notdecomp <- t(v)%*%psi + 2L*(t(r)%*%kappa) + t(gamma)%*%phi%*%gamma + t(m)%*%Tau%*%m + sigma2
  totalwithinvar <- (t(gamma_w)%*%phi_w%*%gamma_w) + (t(v)%*%psi + 2L*(t(r)%*%kappa)) + sigma2
  totalbetweenvar <- (t(gamma_b)%*%phi_b%*%gamma_b) + Tau[1L]
  totalvar <- totalwithinvar + totalbetweenvar

  # Total decomp
  decomp_fixed_notdecomp <- (t(gamma)%*%phi%*%gamma) / totalvar_notdecomp
  decomp_varslopes_notdecomp <- (t(v)%*%psi + 2L*(t(r)%*%kappa)) / totalvar_notdecomp
  decomp_varmeans_notdecomp <- (t(m)%*%Tau%*%m) / totalvar_notdecomp
  decomp_sigma_notdecomp <- sigma2/totalvar_notdecomp
  decomp_fixed_within <- (t(gamma_w)%*%phi_w%*%gamma_w) / totalvar
  decomp_fixed_between <- (t(gamma_b)%*%phi_b%*%gamma_b) / totalvar
  decomp_fixed <- decomp_fixed_within + decomp_fixed_between
  decomp_varslopes <- (t(v)%*%psi + 2L*(t(r)%*%kappa)) / totalvar
  decomp_varmeans <- (t(m)%*%Tau%*%m) / totalvar
  decomp_sigma <- sigma2 / totalvar

  # Within decomp
  decomp_fixed_within_w <- (t(gamma_w)%*%phi_w%*%gamma_w) / totalwithinvar
  decomp_varslopes_w <- (t(v)%*%psi + 2L*(t(r)%*%kappa)) / totalwithinvar
  decomp_sigma_w <- sigma2 / totalwithinvar

  # Between decomp
  decomp_fixed_between_b <- (t(gamma_b)%*%phi_b%*%gamma_b) / totalbetweenvar
  decomp_varmeans_b <- Tau[1L] / totalbetweenvar

  # New measures
  if (isTRUE(clustermeancentered)) {

    R2_f <- decomp_fixed
    R2_f1 <- decomp_fixed_within
    R2_f2 <- decomp_fixed_between
    R2_fv <- decomp_fixed + decomp_varslopes
    R2_fvm <- decomp_fixed + decomp_varslopes + decomp_varmeans
    R2_v <- decomp_varslopes
    R2_m <- decomp_varmeans
    R2_f_w <- decomp_fixed_within_w
    R2_f_b <- decomp_fixed_between_b
    R2_fv_w <- decomp_fixed_within_w + decomp_varslopes_w
    R2_v_w <- decomp_varslopes_w
    R2_m_b <- decomp_varmeans_b

  }

  if (isTRUE(!clustermeancentered)) {

    R2_f <- decomp_fixed_notdecomp
    R2_fv <- decomp_fixed_notdecomp + decomp_varslopes_notdecomp
    R2_fvm <- decomp_fixed_notdecomp + decomp_varslopes_notdecomp + decomp_varmeans_notdecomp
    R2_v <- decomp_varslopes_notdecomp
    R2_m <- decomp_varmeans_notdecomp

  }

  if (isTRUE(clustermeancentered)) {

    decomp_table <- matrix(c(decomp_fixed_within, decomp_fixed_between, decomp_varslopes, decomp_varmeans, decomp_sigma,
                             decomp_fixed_within_w, "NA", decomp_varslopes_w, "NA", decomp_sigma_w,
                             "NA", decomp_fixed_between_b, "NA", decomp_varmeans_b, "NA"), ncol = 3L)

    decomp_table <- suppressWarnings(apply(decomp_table, 2, as.numeric))
    rownames(decomp_table) <- c("fixed, within", "fixed, between", "slope variation", "mean variation", "sigma2")
    colnames(decomp_table) <- c("total", "within", "between")
    R2_table <- matrix(c(R2_f1, R2_f2, R2_v, R2_m, R2_f, R2_fv, R2_fvm,
                         R2_f_w, "NA", R2_v_w, "NA", "NA", R2_fv_w, "NA",
                         "NA", R2_f_b, "NA", R2_m_b, "NA", "NA", "NA"), ncol = 3L)
    R2_table <- suppressWarnings(apply(R2_table, 2, as.numeric)) # make values numeric, not character
    rownames(R2_table) <- c("f1", "f2", "v", "m", "f", "fv", "fvm")
    colnames(R2_table) <- c("total", "within", "between")

  }

  if (isTRUE(!clustermeancentered)) {
    decomp_table <- matrix(c(decomp_fixed_notdecomp, decomp_varslopes_notdecomp, decomp_varmeans_notdecomp, decomp_sigma_notdecomp), ncol = 1L)
    decomp_table <- suppressWarnings(apply(decomp_table, 2L, as.numeric))
    rownames(decomp_table) <- c("fixed", "slope variation", "mean variation", "sigma2")
    colnames(decomp_table) <- c("total")
    R2_table <- matrix(c(R2_f, R2_v, R2_m, R2_fv, R2_fvm), ncol = 1L)
    R2_table <- suppressWarnings(apply(R2_table, 2L, as.numeric))
    rownames(R2_table) <- c("f", "v", "m", "fv", "fvm")
    colnames(R2_table) <- c("total")

  }

  Output <- list(noquote(decomp_table), noquote(R2_table))
  names(Output) <- c("Decompositions", "R2s")
  return(Output)

}

### .prepare_data() Function ####
.prepare_data <- function(model, calling_function, cluster_variable, second_model = NULL) {

  # Step 1a: Pull dataframe associated with model
  if (isTRUE(calling_function == "lme4")) {

    data <- model@frame

  } else {

    data <- model[["data"]]

  }

  # Step 2a) pull interaction terms into list
  interaction_vars <- .get_interaction_vars(model)

  if (isTRUE(!is.null(second_model))) {

    interaction_vars_2 <- .get_interaction_vars(second_model)
    interaction_vars <-  unique(append(interaction_vars, interaction_vars_2))

  }

  # Step 2b) split interaction terms into halves, multiply halves to create new columns in dataframe
  data <- .add_interaction_vars_to_data(data, interaction_vars)

  return(data)

}

### .add_interaction_vars_to_data() Function ####
.add_interaction_vars_to_data <- function(data, interaction_vars) {

  for (whole in interaction_vars) {

    half1 <- unlist(strsplit(whole, ":"))[1L]
    half2 <- unlist(strsplit(whole, ":"))[2L]

    newcol <- data[[half1]] * data[[half2]]

    data <- within(data, assign(whole, newcol))

  }

  return(data)

}

### .get_covs() Function ####
.get_covs <- function(variable_list, data) {

  cov_list <- c()

  i <- 1L
  for (variable in variable_list) {
    tmp <- match(variable, names(data))
    cov_list[i] <- tmp
    i <- i + 1L
  }

  return(cov_list)

}

### .get_random_slope_vars() Function ####
.get_random_slope_vars <- function(model, has_intercept, calling_function) {

  # Visible global function defnition
  ranef <- NULL

  if (isTRUE(calling_function == "lme4")) {

    temp_cov_list <- ranef(model)[[1L]]

  } else if (calling_function == "nlme") {

    temp_cov_list <- nlme::ranef(model)

  }

  if (isTRUE(has_intercept == 1L)) {

    running_count <- 2L

  } else {

    running_count <- 1L

  }

  random_slope_vars <- c()
  x <- 1L

  while (running_count <= length(temp_cov_list)) {

    random_slope_vars[x] <- names(temp_cov_list[running_count])

    x <- x + 1L

    running_count <- running_count + 1L

  }

  return(random_slope_vars)

}

### .get_cwc() Function ####
.get_cwc <- function(l1_vars, cluster_variable, data) {

  for (variable in l1_vars) {

    t <- tapply(data[, variable], data[, cluster_variable], sum, na.rm = TRUE)

    temp_tracker <- 0L

    # sum all of the sums
    for (i in t) { temp_tracker <- temp_tracker + i }

    if (abs(temp_tracker) < 0.0000001) {

      centeredwithincluster <- TRUE

    } else {

      centeredwithincluster <- FALSE

      break

    }

  }

  return(centeredwithincluster)

}

### .get_interaction_vars() Function ####
.get_interaction_vars <- function(model) {

  interaction_vars <- c()

  x <- 1L
  for (term in attr(terms(model), "term.labels")) {

    if (isTRUE(grepl(":", term))) {

      interaction_vars[x] <- term

      x <- x + 1L

    }

  }

  return(interaction_vars)

}

### .sort_variables() Function ####
.sort_variables <- function(data, predictors, cluster_variable) {

  l1_vars <- c()
  l2_vars <- c()

  l1_counter <- 1L
  l2_counter <- 1L

  for (variable in predictors) {

    t <- tapply(data[, variable], data[, cluster_variable], var, na.rm = TRUE)

    counter <- 1L

    while (counter <= length(t)) {

      if (is.na(t[[counter]])) { t[[counter]] <- 0L }

      counter <- counter + 1L

    }

    variance_tracker <- 0L

    for (i in t) { variance_tracker <- variance_tracker + i }

    if (isTRUE(variance_tracker == 0L)) {

      l2_vars[l2_counter] <- variable
      l2_counter <- l2_counter + 1L

    } else {

      l1_vars[l1_counter] <- variable
      l1_counter <- l1_counter + 1L

    }

  }

  return(list("l1_vars" = l1_vars, "l2_vars" = l2_vars))

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the mplus.update() function ---------------------------
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
# Internal functions for the na.auxiliary() function --------------------------------

.cohens.d.na.auxiliary <- function(formula, data, weighted = TRUE, correct = FALSE) {

  #-----------------------------------------------------------------------------------
  # Formula

  # Variables
  var.formula <- all.vars(as.formula(formula))

  # Outcome(s)
  y.var <- var.formula[-length(var.formula)]

  # Grouping variable
  group.var <- var.formula[length(var.formula)]

  # Data
  data <- as.data.frame(data[, var.formula], stringsAsFactors = FALSE)

  #...................
  # Data and Arguments

  # Outcome
  x.dat <- data[, y.var]

  # Grouping
  group.dat <- data[, group.var]

  #...................
  # Descriptives

  # Mean difference
  x.diff <- diff(tapply(x.dat, group.dat, mean, na.rm = TRUE))

  # Sample size by group
  n.group <- tapply(x.dat, group.dat, function(y) length(na.omit(y)))

  #...................
  # Standard deviation

  # Variance by group
  var.group <- tapply(x.dat, group.dat, var, na.rm = TRUE)

  # Weighted pooled standard deviation
  if (isTRUE(weighted)) {

    sd.group <- sqrt(((n.group[1L] - 1L)*var.group[1] + (n.group[2L] - 1L)*var.group[2L]) / (sum(n.group) - 2L))

    # Unweigted pooled standard deviation
  } else {

    sd.group <- sum(var.group) / 2L

  }

  #........................................
  # Cohen's d estimate

  estimate <- x.diff / sd.group

  #........................................
  # Correction factor

  # Bias-corrected Cohen's d
  if (isTRUE(correct && weighted)) {

    v <- sum(n.group) - 2L

    # Correction factor based on gamma function
    if (isTRUE(sum(n.group) < 200L)) {

      corr.factor <- gamma(0.5*v) / ((sqrt(v / 2)) * gamma(0.5 * (v - 1L)))

      # Correction factor based on approximation
    } else {

      corr.factor <- (1L - (3L / (4L * v - 1L)))

    }

    estimate <- estimate*corr.factor

  }

  return(estimate)

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the multilevel.omega() function -----------------------
#
# - .internal.mvrnorm

.internal.mvrnorm <- function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE, EISPACK = FALSE) {

  p <- length(mu)

  if (!all(dim(Sigma) == c(p, p)))  { stop("incompatible arguments") }
  if (EISPACK)  { stop("'EISPACK' is no longer supported by R", domain = NA) }

  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L])))  { stop("'Sigma' is not positive definite") }

  X <- matrix(rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }

  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
  nm <- names(mu)

  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) { nm <- dn[[1L]] }

  dimnames(X) <- list(nm, NULL)
  if (n == 1L) { drop(X) } else { t(X) }

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the multilevel.r2() function --------------------------
#
# - .var.random

# Variance of random effects
.var.random <- function(model) {

  .sigma_sum <- function(Sigma) {

    X <- lme4::getME(model, "X")
    rn <- rownames(Sigma)

    if (isTRUE(!any(colnames(X) == "(Intercept)"))) { X <- cbind("(Intercept)" = 1L, X) }

    if (isTRUE(!is.null(rn))) {

      valid <- rownames(Sigma) %in% colnames(X)

      if (isTRUE(!all(valid))) {

        rn <- rn[valid]
        Sigma <- Sigma[valid, valid, drop = FALSE]

      }

    }

    X[, rn, drop = FALSE] |> (\(y) sum(diag(crossprod(y %*% Sigma, y))) / nobs(model))()

  }

  sum(sapply(lme4::VarCorr(model)[vapply(lme4::ranef(model), nrow, numeric(1L)) |> (\(y) names(y[y != nobs(model)]))()], .sigma_sum))

}

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Internal functions for the na.test() function --------------------------------
#
# - .LittleMCAR
# - .TestMCARNormality
# - .OrderMissing
# - .DelLessData
# - .Mls
# - .Sexpect
# - .Impute
# - .Mimpute
# - .MimputeS
# - .Hawkins
# - .TestUNey
# - .SimNey
# - .AndersonDarling
#
# BaylorEdPsych:
# https://rdrr.io/cran/BaylorEdPsych/src/R/LittleMCAR.R
#
# MissMech
# https://github.com/cran/MissMech/tree/master/R

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Little's missing completely at random (MCAR) test ####

.LittleMCAR <- function(x) {

  n.var <- ncol(x)
  r <- 1L * is.na(x)

  x.mp <- data.frame(cbind(x, ((r %*% (2L^((seq_len(n.var) - 1L)))) + 1L)))
  colnames(x.mp) <- c(colnames(x), "MisPat")

  n.mis.pat <- length(unique(x.mp$MisPat))

  gmean <- suppressWarnings(mvnmle::mlest(x)$muhat)
  gcov <- suppressWarnings(mvnmle::mlest(x)$sigmahat)
  colnames(gcov) <- rownames(gcov) <- colnames(x)

  x.mp$MisPat2 <- rep(NA, nrow(x))
  for (i in seq_len(n.mis.pat)) { x.mp$MisPat2[x.mp$MisPat == sort(unique(x.mp$MisPat), partial = (i))[i]] <- i }

  x.mp$MisPat <- x.mp$MisPat2
  x.mp <- x.mp[, -which(names(x.mp) %in% "MisPat2")]

  datasets <- list()
  for (i in seq_len(n.mis.pat)) { datasets[[paste0("DataSet", i)]] <- x.mp[which(x.mp$MisPat == i), seq_len(n.var)] }

  kj <- 0L
  for (i in seq_len(n.mis.pat)) {

    no.na <- as.matrix(1L * !is.na(colSums(datasets[[i]])))

    kj <- kj + colSums(no.na)

  }

  df <- kj - n.var
  d2 <- 0L
  for (i in seq_len(n.mis.pat)) {

    mean <- (colMeans(datasets[[i]]) - gmean)
    mean <- mean[!is.na(mean)]
    keep <- 1L * !is.na(colSums(datasets[[i]]))
    keep <- keep[which(keep[seq_len(n.var)] != 0L)]
    cov <- gcov
    cov <- cov[which(rownames(cov) %in% names(keep)), which(colnames(cov) %in% names(keep))]
    d2 <- as.numeric(d2 + (sum(x.mp$MisPat == i)*(t(mean) %*% solve(cov) %*% mean)))

  }

  return(list(chi.square = d2, df = df, p.value = 1L - pchisq(d2, df), missing.patterns = n.mis.pat))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Testing Homoscedasticity, Multivariate Normality, and MCAR ####

.TestMCARNormality <- function(data, delete = 6, m = 20, method = "npar", nrep = 10000,
                               n.min = 30, seed = NULL, pool = "med", impdat = NULL) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Imputation ####

  if (isTRUE(is.null(impdat))) {

    # Order missing data pattern
    newdata <- .OrderMissing(data, delete)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Imputed Data Provided ####

  } else {

    # Number of imputations
    m <- impdat$m

    # Order missing data pattern
    newdata <- .OrderMissing(impdat$data[, colnames(data)], delete)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Check ####

  if(isTRUE(length(newdata$data) == 0L)) { stop("There are no cases left after deleting insufficient cases.", call. = FALSE) }

  if (isTRUE(newdata$g == 1L)) { stop("There is only one missing data pattern present.", call. = FALSE) }

  if (isTRUE(sum(newdata$patcnt == 1L) > 0L)) { stop("At least 2 cases needed in each missing data patterns.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Data ####

  y <- newdata$data
  patused <- newdata$patused
  patcnt <- newdata$patcnt
  spatcnt <- newdata$spatcnt
  caseorder <- newdata$caseorder
  removedcases <- newdata$removedcases

  colnam <- colnames(patused)
  n <- nrow(y)
  p <- ncol(y)
  g <- newdata$g

  spatcntz <- c(0L, spatcnt)
  pvalsn <- matrix(0L, m, g)
  adistar <- matrix(0L, m, g)
  pnormality <- c()
  x <- vector("list", g)
  n4sim <- vector("list", g)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Imputation ####

  if (isTRUE(is.null(impdat))) {

    # Set seed
    if (isTRUE(!is.null(seed))) { set.seed(seed) }

    yimp <- y

    #...................
    ### Non-Parametric ####

    if (isTRUE(method == "npar")) {

      iscomp <- rowSums(patused, na.rm = TRUE) == p

      cind <- which(iscomp)
      ncomp <- patcnt[cind]

      if (isTRUE(length(ncomp) == 0L)) { ncomp <- 0L }

      use.normal <- FALSE

      if (isTRUE(ncomp >= 10L && ncomp >= 2L*p)) {

        compy <- y[seq(spatcntz[cind] + 1L, spatcntz[cind + 1L]), ]
        ybar <- matrix(colMeans(compy))
        sbar <- stats::cov(compy)
        resid <- (ncomp / (ncomp - 1L))^0.5 * (compy - matrix(ybar, ncomp, p, byrow = TRUE))

      } else {

        warning("There are not sufficient number of complete cases for non-parametric imputation, imputation method \"normal\" will be used instead.", call. = FALSE)

        use.normal <- TRUE

        mu <- matrix(0L, p, 1L)
        sig <- diag(1L, p)

        emest <- .Mls(newdata, mu, sig, 1e-6)

      }

    #...................
    ### Parametric Normal ####

    } else {

      mu <- matrix(0L, p, 1L)
      sig <- diag(1L, p)

      emest <- .Mls(newdata, mu, sig, 1e-6)

    }

    #...................
    ### Impute and Analyze ####

    for (k in seq_len(m)) {

      #...................
      ### Parametric Normal ####

      if (isTRUE(method == "normal" || use.normal)) {

        yimp <- .Impute(data = y, emest$mu, emest$sig, method = "normal")
        yimp <- yimp$yimpOrdered

      #...................
      ### Non-Parametric ####

      } else {

        yimp <- .Impute(data = y, ybar, sbar, method = "npar", resid)$yimpOrdered

      }

      if (isTRUE(k == 1L)) { yimptemp <- yimp }

      #...................
      ### Hawkins Test ####

      templist <- .Hawkins(yimp, spatcnt)
      fij <- templist$fij
      tail <- templist$a
      ni <- templist$ni

      for (i in seq_len(g)) {

        if (isTRUE(ni[i] < n.min && k == 1L)) { n4sim[[i]] <- .SimNey(ni[i], nrep) }

        templist <- .TestUNey(tail[[i]], nrep, sim = n4sim[[i]], n.min)
        pn <- templist$pn
        n4 <- templist$n4
        pn <- pn + (pn == 0L) / nrep
        pvalsn[k, i] <- pn

      }

      #...................
      ### Anderson-Darling Non-Parametric Test ####

      if (isTRUE(length(ni) < 2L)) { stop("Not enough groups for Anderson Darling test.", call. = FALSE) }

      templist <- .AndersonDarling(fij, ni)
      adistar[k, ] <- templist$adk.all
      pnormality <- c(pnormality, templist$pn)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Imputed Data Provided ####

  } else {

    for (k in seq_len(m)) {

      #...................
      ### Hawkins Test ####

      templist <- .Hawkins(as.matrix(mice::complete(impdat, action = k)[newdata$caseorder, colnam]), spatcnt)

      fij <- templist$fij
      tail <- templist$a
      ni <- templist$ni

      for (i in seq_len(g)) {

        if (isTRUE(ni[i] < n.min && k == 1L)) { n4sim[[i]] <- .SimNey(ni[i], nrep) }

        templist <- .TestUNey(tail[[i]], nrep, sim = n4sim[[i]], n.min)
        pn <- templist$pn
        n4 <- templist$n4
        pn <- pn + (pn == 0L) / nrep
        pvalsn[k, i] <- pn

      }

      #...................
      ### Anderson-Darling Non-Parametric Test ####

      if (isTRUE(length(ni) < 2L)) { stop("Not enough groups for Anderson Darling test.", call. = FALSE) }

      templist <- .AndersonDarling(fij, ni)
      adistar[k, ] <- templist$adk.all
      pnormality <- c(pnormality, templist$pn)

    }

  }

  #...................
  ### Result Table ####

  #### Test Statistics and p-Values ####
  ts.hawkins <- -2L * rowSums(log(pvalsn))
  p.hawkins <- stats::pchisq(-2L * rowSums(log(pvalsn)), 2L*g, lower.tail = FALSE)
  ts.anderson <- rowSums(adistar)
  p.anderson <- pnormality

  switch(pool,
         "m" = {

           tsa.hawkins <- mean(ts.hawkins)
           pa.hawkins <- mean(p.hawkins)
           tsa.anderson <- mean(ts.anderson)
           pa.anderson <- mean(p.anderson)

         }, "med" = {

           tsa.hawkins <- median(ts.hawkins)
           pa.hawkins <- median(p.hawkins)
           tsa.anderson <- median(ts.anderson)
           pa.anderson <- median(p.anderson)

         }, "min" = {

           tsa.hawkins <- max(ts.hawkins)
           pa.hawkins <- min(p.hawkins)
           tsa.anderson <- max(ts.anderson)
           pa.anderson <- min(p.anderson)

         }, "max" = {

           tsa.hawkins <- min(ts.hawkins)
           pa.hawkins <- max(p.hawkins)
           tsa.anderson <- min(ts.anderson)
           pa.anderson <- max(p.anderson)

         }, "random" = {

           ind.hawkins <- sample(seq_along(ts.hawkins), size = 1L)
           tsa.hawkins <- ts.hawkins[ind.hawkins]
           pa.hawkins <- p.hawkins[ind.hawkins]

           ind.anderson <- sample(seq_along(ts.anderson), size = 1L)
           tsa.anderson <- ts.anderson[ind.anderson]
           pa.anderson <- p.anderson[ind.anderson]

         })

  #### Analysis Data ####
  if (isTRUE(length(removedcases) == 0L)) {

    dataused <- data

  } else {

    dataused <- data[-1L * removedcases, ]

  }

  #### Return object ####
  restab <- list(dat.analysis = dataused, dat.ordered =  y, case.order = caseorder, g = g, pattern = patused, n.pattern = patcnt,
                 t.hawkins = pvalsn, ts.hawkins = ts.hawkins, tsa.hawkins = tsa.hawkins, p.hawkins = p.hawkins, pa.hawkins = pa.hawkins,
                 t.anderson = adistar, ts.anderson = ts.anderson, tsa.anderson = tsa.anderson, p.anderson = p.anderson, pa.anderson = pa.anderson)

  return(restab)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Order Missing Data Pattern ####

.OrderMissing <- function(y, del.lesscases = 0L) {

  if (isTRUE(is.data.frame(y))) { y <- as.matrix(y) }

  if (isTRUE(!is.matrix(y))) { stop("Data is not a matrix or data frame", call. = FALSE) }

  if (isTRUE(length(y) == 0L)) { stop("Data is empty", call. = FALSE) }

  names <- colnames(y)
  pp <- ncol(y)
  yfinal <- c()
  patused <- c()
  patcnt <- c()
  caseorder <- c()
  removedcases <- c()
  ordertemp <- seq_len(nrow(y))
  ntemp <- nrow(y)
  ptemp <- ncol(y)
  done <- FALSE
  yatone <- FALSE

  while (isTRUE(!done)) {

    pattemp <- is.na(y[1L, ])
    indin <- c()
    indout <- c()
    done <- TRUE

    for (i in seq_len(ntemp)) {

      if (isTRUE(all(is.na(y[i, ]) == pattemp))) {

        indout <- c(indout, i)

      } else {

        indin <- c(indin, i)
        done <- FALSE

      }

    }

    if (isTRUE(length(indin) == 1L)) { yatone <- TRUE }

    yfinal <- rbind(yfinal, y[indout, ])
    y <- y[indin, ]
    caseorder <- c(caseorder, ordertemp[indout])
    ordertemp <- ordertemp[indin]
    patcnt <- c(patcnt, length(indout))
    patused <- rbind(patused, pattemp)

    if (isTRUE(yatone)) {

      pattemp <- is.na(y)
      yfinal <- rbind(yfinal, matrix(y, ncol = pp))
      y <- c()
      indin <- c()
      indout <- c(1L)
      caseorder <- c(caseorder, ordertemp[indout])
      ordertemp <- ordertemp[indin]
      patcnt <- c(patcnt, length(indout))
      patused <- rbind(patused, pattemp)
      done <- TRUE

    }

    if (isTRUE(!done)) { ntemp <- nrow(y) }

  }

  caseorder <- c(caseorder, ordertemp)
  patused <- ifelse(patused, NA, 1L)
  rownames(patused) <- NULL
  colnames(patused) <- names
  dataorder <- list(data = yfinal, patused = patused, patcnt = patcnt,
                    spatcnt = cumsum(patcnt), g = length(patcnt),
                    caseorder = caseorder, removedcases = removedcases)

  dataorder$call <- match.call()
  class(dataorder) <- "orderpattern"

  if (isTRUE(del.lesscases > 0L)) { dataorder <- .DelLessData(dataorder, del.lesscases) }

  dataorder$patused <- matrix(dataorder$patused, ncol = pp)
  colnames(dataorder$patused) <- names

  return(dataorder)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Removes groups with identical missing data patterns ####

.DelLessData <- function(data, ncases = 0) {

  if (isTRUE(length(data) == 0L)) { stop("Data is empty", call. = FALSE) }

  if (isTRUE(is.matrix(data))) { data <- .OrderMissing(data) }

  ind <- which(data$patcnt <= ncases)
  spatcntz <- c(0L, data$spatcnt)

  rm <- c()
  removedcases <- c()

  if (isTRUE(length(ind) != 0L)) {

    for (i in seq_len(length(ind))) {

      rm <- c(rm, seq(spatcntz[ind[i]] + 1L, spatcntz[ind[i] + 1L]));

    }

    y <- data$data[-1L * rm, ]
    removedcases <- data$caseorder[rm]
    patused <- data$patused[-1L * ind, ]
    patcnt <- data$patcnt[-1L * ind]
    caseorder <- data$caseorder[-1L * rm]
    spatcnt <- cumsum(patcnt)

  } else {

    patused <- data$patused
    patcnt <- data$patcnt
    spatcnt <- data$spatcnt
    caseorder <- data$caseorder
    y <- data$data

  }

  newdata <- list(data = y, patused = patused, patcnt = patcnt,
                  spatcnt = spatcnt, g = length(patcnt), caseorder = caseorder,
                  removedcases = removedcases)

  class(newdata) <- "orderpattern"
  return(newdata)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ML Estimates of Mean and Covariance Based on Incomplete Data ####

.Mls <- function(data, mu = NA, sig = NA, tol = 1e-6, Hessian = FALSE) {

  if (isTRUE(!is.matrix(data) && !inherits(data, "orderpattern"))) { stop("Data must have the classes of matrix or orderpattern.", call. = FALSE) }

  if (isTRUE(is.matrix(data))) {

    allempty <- which(rowSums(!is.na(data)) == 0L)
    if (isTRUE(length(allempty) != 0L)) { data <- data[rowSums(!is.na(data)) != 0L, ] }

    data <- .OrderMissing(data)

  }

  if (isTRUE(inherits(data, "orderpattern"))) {

    allempty <- which(rowSums(!is.na(data$data)) == 0L)

    if (isTRUE(length(allempty) != 0L)) {

      data <- data$data
      data <- data[rowSums(!is.na(data)) != 0L, ]

      data <- .OrderMissing(data)

    }

  }

  if (isTRUE(length(data$data) == 0L)) { stop("Data is empty", call. = FALSE) }

  if (isTRUE(ncol(data$data) < 2L)) { stop("More than one variable is required.", call. = FALSE) }

  y <- data$data
  patused <- data$patused
  spatcnt <- data$spatcnt

  if (isTRUE(is.na(mu[1L]))) {

    mu <- matrix(0L, ncol(y), 1L)
    sig <- diag(1L, ncol(y))

  }

  itcnt <- 0L
  em <- 0L

  repeat {

    emtemp <- .Sexpect(y, mu, sig, patused, spatcnt)
    ysbar <- emtemp$ysbar
    sstar <- emtemp$sstar
    em <- max(abs(sstar - mu %*% t(mu) - sig), abs(mu - ysbar))
    mu <- ysbar
    sig <- sstar - mu %*% t(mu)
    itcnt <- itcnt + 1L

    if (isTRUE(!(em > tol || itcnt < 2L))) break

  }

  rownames(mu) <- colnames(y)
  colnames(sig) <- colnames(y)

  if (isTRUE(Hessian)) {

    templist <- .Ddf(y, mu, sig)
    hessian <- templist$dd
    stderror <- templist$se

    return (list(mu = mu, sig = sig, hessian = hessian, stderror = stderror, iteration = itcnt))

  }

  return(list(mu = mu, sig = sig, iteration = itcnt))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.Sexpect <- function(y, mu, sig, patused, spatcnt) {

  n <-  nrow(y)
  pp <- ncol(y)
  sstar <- matrix(0L, pp, pp)
  ysbar <- matrix(0L, nrow(mu), ncol(mu))
  first <- 1L

  for (i in seq_len(length(spatcnt))) {

    ni <- spatcnt[i] - first + 1L
    stemp <- matrix(0L, pp, pp)
    indm <- which(is.na(patused[i, ]))
    indo <- which(!is.na(patused[i, ]))
    yo <- matrix(y[first:spatcnt[i], indo], ni, length(indo))
    first <- spatcnt[i] + 1L
    muo <- mu[indo]
    mum <- mu[indm]
    sigoo <- sig[indo, indo]
    sigooi <- solve(sigoo)
    soo <- t(yo) %*% yo
    stemp[indo, indo] <- soo
    ystemp <- matrix(0L, ni, pp)
    ystemp[, indo] <- yo

    if (isTRUE(length(indm)!= 0L)) {

      sigmo <- matrix(sig[indm, indo], length(indm), length(indo))
      sigmm <- sig[indm, indm]
      temp1 <- matrix(mum, ni, length(indm), byrow = TRUE)
      temp2 <- yo - matrix(muo, ni, length(indo), byrow = TRUE)
      ym <- temp1 + temp2 %*% sigooi %*% t(sigmo)
      som <- t(yo) %*% ym
      smm <- ni * (sigmm - sigmo %*% sigooi %*% t(sigmo))+ t(ym)%*%ym
      stemp[indo, indm] <- som
      stemp[indm, indo] <- t(som)
      stemp[indm, indm] <- smm
      ystemp[, indm] <- ym

    }

    sstar <- sstar + stemp
    if (isTRUE(ni == 1L)) {

      ysbar <- t(ystemp) + ysbar

    } else {

      ysbar <- colSums(ystemp) + ysbar

    }

  }

  ysbar <- (1L / n) * ysbar
  sstar <- (1L / n) * sstar
  sstar <- (sstar + t(sstar)) / 2L

  return(list(ysbar = ysbar, sstar = sstar))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hessian of the Observed Data ####

.Ddf <- function(data, mu, sig) {

  y <- data
  n <- nrow(y)
  p <- ncol(y)
  ns <- p * (p + 1L) / 2L
  nparam <- ns + p
  ddss <- matrix(0L, ns, ns)
  ddmm <- matrix(0L, p, p)
  ddsm <- matrix(0L, ns, p)

  for (i in seq_len(n)) {

    obs <- which(!is.na(y[i, ]))
    lo <- length(obs)
    tmp <- cbind(rep(obs, 1L, each = lo), rep(obs, lo))
    tmp <- matrix(tmp[tmp[, 1L] >= tmp[, 2L], ], ncol = 2L)
    lolo = lo*(lo + 1L) / 2L
    subsig <- sig[obs, obs]
    submu <- mu[obs, ]
    temp <- matrix(y[i, obs] - submu, nrow = 1L)
    a <- solve(subsig)
    b <- a %*% (2L * t(temp) %*% temp - subsig) * a
    d <- temp %*% a
    ddimm <- 2L * a

    ddmm[obs, obs] <- ddmm[obs, obs] + ddimm

    rcnt <- 0L
    ddism <- matrix(0L, lolo, lo)
    for (k in seq_len(lo)) {

      for (l in seq_len(k)) {

        rcnt <- rcnt + 1L
        ccnt <- 0L
        for (kk in seq_len(lo)) {

          ccnt <- ccnt + 1L
          ddism[rcnt, ccnt] <- 2L * (1L - 0.5 * (k == l)) * (a[kk, l] %*% d[k] + a[kk, k] %*% d[l])

        }

      }

    }

    for (k in seq_len(lolo)) {

      par1 <- tmp[k, 1L] * (tmp[k, 1L] -1L) / 2L + tmp[k, 2L]
      for (j in seq_len(lo)) {

        ddsm[par1, obs[j]] <- ddsm[par1, obs[j]] + ddism[k, j]

      }

    }

    ssi <- matrix(0L, lolo, lolo)
    for (m in seq_len(lolo)) {

      u <- which(obs == tmp[m, 1L])
      v <- which(obs == tmp[m, 2L])

      for (q in seq_len(m)) {

        k <- which(obs == tmp[q, 1L])
        l <- which(obs == tmp[q, 2L])

        ssi[m, q] <- (b[v, k] * a[l, u] + b[v, l] * a[k, u] + b[u, k] * a [l, v] + b[u, l] * a[k, v]) * (1L - 0.5 * (u == v)) * (1L - 0.5 * (k == l))

      }

    }

    for (k in seq_len(lolo)) {

      par1 <- tmp[k, 1L] * (tmp[k, 1L] - 1L) / 2L + tmp[k, 2L]
      for (l in seq_len(k)) {

        par2 <- tmp[l, 1L] * (tmp[l, 1L] - 1L) / 2L + tmp[l, 2L]
        ddss[par1, par2] <- ddss[par1, par2] + ssi[k, l]
        ddss[par2, par1] <- ddss[par1, par2]

      }

    }

  }

  dd <- -1L * rbind(cbind(ddmm, t(ddsm)), cbind(ddsm, ddss)) / 2L
  se <- -solve(dd)

  return(list(dd = dd, se = se))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Parametric and Non-Parameric Imputation ####

.Impute <- function(data, mu = NA, sig = NA, method = "normal", resid = NA) {

  if (isTRUE(!is.matrix(data) && !inherits(data, "orderpattern"))) { stop("Data must have the classes of matrix or orderpattern.", call. = FALSE) }

  if (isTRUE(is.matrix(data))) {

    allempty <- which(rowSums(!is.na(data)) == 0L)

    if (isTRUE(length(allempty) != 0L)) {

      data <- data[rowSums(!is.na(data)) != 0L, ]

      warning(length(allempty), " Cases with all variables missing have been removed from the data.", call. = FALSE)

    }

    data <- .OrderMissing(data)

  }

  if (isTRUE(inherits(data, "orderpattern"))) {

    allempty <- which(rowSums(!is.na(data$data)) == 0L)

    if (isTRUE(length(allempty) != 0L)) {

      data <- data$data
      data <- data[rowSums(!is.na(data)) != 0L, ]

      warning(length(allempty), " Cases with all variables missing have been removed from the data.", call. = FALSE)

      data <- .OrderMissing(data)

    }

  }

  if(isTRUE(length(data$data) == 0L)) { stop("Data is empty", call. = FALSE) }

  if (isTRUE(ncol(data$data) < 2L)) { stop("More than one variable is required.", call. = FALSE) }

  y <- data$data
  patused <- data$patused
  spatcnt <- data$spatcnt
  patcnt <- data$patcnt
  g <- data$g
  caseorder <- data$caseorder
  spatcntz <- c(0L, spatcnt)
  p <- ncol(y)
  n <- nrow(y)
  yimp <- y
  use.normal <- TRUE

  #-----------------------------------------------------------------------------
  # Imputation Method: Distribution Free

  if (isTRUE(method == "npar")) {

    if (isTRUE(is.na(mu[1L]))) {

      ybar <- matrix(0L, p, 1L)
      sbar <- diag(1L, p)

      cind <- which(rowSums(patused, na.rm = TRUE) == p)
      ncomp <- patcnt[cind]
      use.normal <- FALSE
      if (isTRUE(ncomp >= 10L && ncomp >= 2L*p)) {

        compy <- y[seq(spatcntz[cind] + 1L, spatcntz[cind + 1L]), ]
        ybar <- matrix(colMeans(compy))
        sbar <- stats::cov(compy)

        if (isTRUE(is.na(resid[1L]))) {

          resid <- (ncomp / (ncomp - 1)) ^ 0.5 * (compy - matrix(ybar, ncomp, p, byrow = TRUE))

        }

      } else {

        stop("There is not sufficient number of complete cases.\n  Dist.free imputation requires a least 10 complete cases\n  or 2*number of variables, whichever is bigger.\n", call. = FALSE)

      }

    }

    if (isTRUE(!is.na(mu[1L]))) {

      ybar <- mu
      sbar <- sig
      cind <- which(rowSums(patused, na.rm = TRUE) == p)
      ncomp <- patcnt[cind]
      use.normal <- FALSE
      compy <- y[seq(spatcntz[cind] + 1L, spatcntz[cind + 1L]), ]

      if (isTRUE(is.na(resid[1L]))) {

        resid <- (ncomp / (ncomp - 1L)) ^ 0.5 * (compy - matrix(ybar, ncomp, p, byrow = TRUE))

      }

    }

    resstar <- resid[sample(ncomp, n - ncomp, replace = TRUE), ]
    indres1 <- 1L

    for (i in seq_len(g)) {

      if (isTRUE(sum(patused[i, ], na.rm = TRUE) != p)) {

        test <- y[(spatcntz[i] + 1L) : spatcntz[i + 1L], ]
        indres2 <- indres1 + patcnt[i] - 1L
        test <- .MimputeS(matrix(test, ncol = p), patused[i, ], ybar, sbar, matrix(resstar[indres1:indres2, ], ncol = p))
        indres1 <- indres2 + 1L

        yimp[(spatcntz[i] + 1L) : spatcntz[i + 1L], ] <- test

      }

    }

  }

  #-----------------------------------------------------------------------------
  # Imputation Method: Normal

  if (isTRUE(method == "normal" | use.normal)) {

    if (isTRUE(is.na(mu[1L]))) {

      emest <- .Mls(data, tol = 1e-6)
      mu <- emest$mu
      sig <- emest$sig

    }

    for (i in seq_len(g)) {

      if (sum(patused[i, ], na.rm = TRUE) != p) {

        test <- y[(spatcntz[i] + 1L) : spatcntz[i + 1L], ]
        test <- .Mimpute(matrix(test, ncol = p), patused[i, ], mu, sig)
        yimp[(spatcntz[i] + 1L) : spatcntz[i + 1L], ] <- test

      }

    }

  }

  imputed <- list(yimp = yimp[order(caseorder), ], yimpOrdered = yimp, caseorder = caseorder, patused = patused, patcnt = patcnt)

  return(imputed)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.Mimpute <- function(data, patused, mu, sig) {

  ni <- nrow(data)
  indm <- which(is.na(patused))
  indo <- which(!is.na(patused))
  pm <- length(indm)
  sigmo <- matrix(sig[indm, indo], pm, length(indo))
  ss1 <- sigmo %*% solve(sig[indo, indo])
  varymiss <- matrix(sig[indm, indm], pm, pm) - ss1 %*% t(sigmo)

  if (isTRUE(pm == 1L)) {

    a <- sqrt(varymiss)

  } else {

    svdvar <- svd(varymiss)
    a <- diag(sqrt(svdvar$d)) %*% t(svdvar$u)

  }

  data[, indm] <- matrix(rnorm(ni * pm), ni, pm) %*% a + (matrix(mu[indm], ni, pm, byrow = TRUE) + (data[, indo] - matrix(mu[indo], ni, length(indo), byrow = TRUE)) %*% t(ss1))

  return(data)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.MimputeS <- function(data, patused, y1, s1, e) {

  ni <- nrow(data)
  indm <- which(is.na(patused))
  indo <- which(!is.na(patused))
  pm <- length(indm)
  po <- length(indo)
  a <- matrix(s1[indm, indo], pm, po) %*% solve(s1[indo, indo])
  zij <- (matrix(y1[indm], ni, pm, byrow = TRUE) + (data[, indo] - matrix(y1[indo], ni, po, byrow = TRUE)) %*% t(a)) + matrix(e[, indm], ni, pm) - matrix(e[, indo], ni, po) %*% t(a)
  data[, indm] <- zij

  return(data)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Test Statistic for the Hawkins Homoscedasticity Test ####

.Hawkins <- function(y, spatcnt) {

  n <- nrow(y)
  p <- ncol(y)
  g <- length(spatcnt)

  spool <- matrix(0L, p, p)
  gind <- c(0L, spatcnt)
  ygc <- matrix(0L, n, p)
  ni <- matrix(0L, g, 1L)

  for (i in seq_len(g)) {

    yg <- y[seq(gind[i] + 1L, gind[i + 1L]), ]
    ni[i] <- nrow(yg)
    spool <- spool + (ni[i] - 1L) * stats::cov(yg)
    ygmean <- colMeans(yg)
    ygc[seq(gind[i] + 1L, gind[i + 1L]), ] <- yg - matrix(ygmean, ni[i], p, byrow = TRUE)

  }

  spool <- solve(spool / (n - g))
  f <- matrix(0L, n, 1L)
  nu <- n - g - 1L
  a <- vector("list", g)

  for (i in seq_len(g)) {

    vij <- ygc [seq(gind[i] + 1L, gind[i + 1L]), ]
    vij <- rowSums(vij %*% spool * vij)
    vij <-  vij*ni[i]
    f[seq(gind[i] + 1L, gind[i + 1L])] <- ((n - g - p) * vij)/ (p * ((ni[i] - 1L ) * (n - g) - vij))
    a[[i]] <- 1L - stats::pf(f[seq(gind[i] + 1L, gind[i + 1L])], p, (nu - p + 1L))

  }

  return(list(fij = f, a = a, ni = ni))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Test of Goodness of Fit (Uniformity) ####

.TestUNey <- function(x, nrep = 10000, sim = NA, n.min = 30) {

  n <- length(x)
  pi <- .LegNorm(x)

  n4 <- (colSums(pi$p1)^2 + colSums(pi$p2)^2L + colSums(pi$p3)^2L + colSums(pi$p4)^2L) / n

  if (isTRUE(n < n.min)) {

    if (isTRUE(is.na(sim[1L]))) {

      sim <- .SimNey(n, nrep)

    }

    pn <- length(which(sim > n4)) / nrep

  } else {

    pn <- stats::pchisq(n4, 4L, lower.tail = FALSE)

  }

  return(list(pn = pn, n4 = n4))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.SimNey <- function(n, nrep) {

  pi <- .LegNorm(matrix(stats::runif(nrep * n), ncol = nrep))
  n4sim <- sort((colSums(pi$p1)^2L + colSums(pi$p2)^2L + colSums(pi$p3)^2L + colSums(pi$p4)^2L) / n )

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Evaluating Legendre's Polynomials of Degree 1, 2, 3, or 4 ####

.LegNorm <- function(x) {

  if (isTRUE(!is.matrix(x))) { x <- as.matrix(x) }

  x <- 2L * x - 1L
  p0 <- matrix(1,nrow(x), ncol(x))
  p1 <- x
  p2 <- (3L * x * p1 - p0) / 2L
  p3 <- (5L * x * p2 - 2L * p1) / 3L
  p4 <- (7L * x * p3 - 3L * p2) / 4L

  p1 <- sqrt(3L) * p1
  p2 <- sqrt(5L) * p2
  p3 <- sqrt(7L) * p3
  p4 <- 3L * p4

  return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## K-Sample Anderson Darling Test ####

.AndersonDarling <- function(x, ni) {

  if (isTRUE(length(ni) < 2L)) { stop("Not enough groups for the Anderson-Darling k-sample test.") }

  k <- length(ni)
  ni.z <- c(0L, cumsum(ni))
  n <- length(x)
  x.sort <- sort(x)[seq_len(n - 1L)]
  ind <- which(duplicated(x.sort) == 0L)
  hj <- (c(ind, length(x.sort) + 1L) - c(0L, ind))[2L:(length(ind) + 1L)]
  hn <- cumsum(hj)
  zj <- x.sort[ind]
  adk <- 0L
  adk.all <- matrix(0L, k, 1L)

  for (i in seq_len(k)) {

    ind <- (ni.z[i] + 1L):ni.z[i + 1L]
    templist <- expand.grid(zj, x[ind])
    b <- templist[, 1L] == templist[, 2L]
    fij <- rowSums(matrix(b, length(zj)))
    mij <- cumsum(fij)
    num <- (n * mij - ni[i] * hn)^ 2L
    dem <- hn*(n - hn)
    adk.all[i] <- (1L / ni[i] * sum(hj * (num / dem)))
    adk <- adk + adk.all[i]

  }

  adk <- (1L / n) * adk
  adk.all <- adk.all / n

  j <- sum(1L / ni)
  i <- seq_len(n - 1)
  h <- sum(1L / i)
  g <- 0L

  for (i in seq_len(n - 2L)) { g <- g + (1L / (n - i)) * sum(1L / seq((i + 1L), (n - 1L))) }

  a <- (4L * g - 6L) * (k - 1L) + (10L - 6L * g) * j
  b <- (2L * g - 4L) * k^2 + 8L * h * k + (2L * g - 14L * h - 4L) * j - 8L * h + 4L * g - 6L
  c <- (6L * h + 2L * g - 2L) * k ^ 2L + (4L * h - 4L * g + 6L) * k + (2L * h - 6L) * j + 4L * h
  d <- (2L * h + 6L) * k ^ 2L - 4L * h * k

  var.adk <- ((a * n^3L) + (b * n^2L) + (c * n) + d) / ((n - 1L) * (n - 2L) * (n - 3L))

  if (isTRUE(var.adk < 0L)) { var.adk <- 0L }

  adk.s <- (adk - (k - 1L)) / sqrt(var.adk)

  a0 <- c(0.25, 0.10, 0.05, 0.025, 0.01)
  b0 <- c(0.675, 1.281, 1.645, 1.96, 2.326)
  b1 <- c(-0.245, 0.25, 0.678, 1.149, 1.822)
  b2 <- c(-0.105, -0.305, -0.362, -0.391, -0.396)
  c0 <- log((1L - a0) / a0)

  qnt <- b0 + b1 / sqrt(k - 1L) + b2 / (k - 1L)

  if (isTRUE(adk.s <= qnt[3L])) {

    ind <- 1L:4L

  } else {

    ind <- 2L:5L

  }

  return(list(pn = 1L / (1L + exp(stats::spline(qnt[ind], c0[ind], xout = adk.s)$y)), adk.all = adk.all, adk = adk, var.adk = var.adk))

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

#_______________________________________________________________________________
#_______________________________________________________________________________
#
# Write Results ----------------------------------------------------------------

.write.result <- function(object, write, append) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Text file ####

  if (isTRUE(grepl("\\.txt", write))) {

    # Send R output to text file
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Excel file ####

  } else {

    misty::write.result(object, file = write)

  }

}

#_______________________________________________________________________________
