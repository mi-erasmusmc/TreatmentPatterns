#' depth
#'
#' Function to find depth of a list element.
#'
#' @param x input list (element)
#' @param thisdepth current list depth
#'
#' @return the depth of the list element
depth <- function(x, thisdepth = 0) {
  # Assertions
  checkmate::assertTRUE(!is.null(x))
  checkmate::assertNumeric(x = thisdepth, len = 1, lower = 0, null.ok = FALSE)
  
  if (!is.list(x)) {
    return(thisdepth)
  } else {
    return(max(unlist(
      lapply(x, depth, thisdepth = thisdepth + 1)
    )))
  }
}

#' stripname
#'
#' Recursive function to remove name from all levels of list.
#'
#' @param x input list
#' @param name the name of the list item from which the names will be removed
#'
#' @return list with removed names
stripname <- function(x, name) {
  # Assertions
  checkmate::assertTRUE(!is.null(x))
  checkmate::assertCharacter(x = name, len = 1, null.ok = FALSE)
  
  thisdepth <- depth(x)
  if (thisdepth == 0) {
    return(x)
  } else if (length(nameIndex <- which(names(x) == name)) > 0) {
    element <- names(x)[nameIndex]
    x[[element]] <- unname(x[[element]])
  }
  return(lapply(x, stripname, name))
}

#' addChild
#'
#' @param j iterator 
#' @param children children to add
#' @param parts 
#' @param root 
#'
#' @return
#' @export
#'
#' @examples
addChild <- function(j, children, parts, root) {
  switch(
    j,
    root[["children"]] <- children,
    root[["children"]][[parts[1]]][["children"]] <- children,
    root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]] <- children,
    root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]][[parts[3]]][["children"]] <- children,
    root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]][[parts[3]]][["children"]][[parts[4]]][["children"]] <- children
  )
  return(root)
}

#' buildHierarchy
#'
#' @param csv matrix
#'
#' @return JSON
buildHierarchy <- function(csv) {
  root <- list(
    name = "root",
    children = list())
  
  # Create nested structure of lists
  for (i in seq_len(nrow(csv))) {
    sequence <- csv[i, 1]
    size <- csv[i, 2]
    
    parts <- unlist(stringr::str_split(sequence, pattern = "-"))
    
    currentNode <- root
    
    for (j in seq_len(length(parts))) {
      children <- currentNode[["children"]]
      nodeName <- parts[j]
      
      if (j < length(parts)) {
        # Not yet at the end of the sequence; move down the tree
        foundChild <- FALSE
        
        if (length(children) != 0) {
          for (k in seq_len(length(children))) {
            if (children[[k]]$name == nodeName) {
              childNode <- children[[k]]
              foundChild <- TRUE
              break
            }
          }
        }
        
        # If we dont already have a child node for this branch, create it
        if (!foundChild) {
          childNode <- list("name" = nodeName, "children" = list())
          children[[nodeName]] <- childNode
          
          # Add to main root
          root <- addChild(j, children, parts, root)
        }
        currentNode <- childNode
      } else {
        # Reached the end of the sequence; create a leaf node
        childNode <- list("name" = nodeName, "size" = size)
        children[[nodeName]] <- childNode
        
        # Add to main root
        root <- addChild(j, children, parts, root)
      }
    }
  }
  
  # Remove list names
  root <- suppressWarnings(stripname(root, "children"))
  
  # Convert nested list structure to json
  json <- rjson::toJSON(root)
  return(json)
}


#' transformCSVtoJSON
#'
#' Help function to transform data in csv format to required JSON format for
#' HTML.
#'
#' @param data input data.frame
#' @param outcomes character vector containing all event cohorts
#' @param folder output folder
#' @param fileName output file name
#'
#' @return the transformed csv as a json string
transformCSVtoJSON <- function(data, outcomes, folder, fileName) {
  # Assertions
  checkmate::assertDataFrame(x = data)
  checkmate::assertCharacter(x = outcomes, null.ok = FALSE)
  #checkmate::assertDirectoryExists(x = folder)
  checkmate::assertCharacter(x = fileName, len = 1, null.ok = FALSE)
  # Add bitwise numbers to define combination treatments
  bitwiseNumbers <- sapply(
    X = seq_along(outcomes),
    FUN = function(o) {
      2 ^ (o - 1)
    })
  
  linking <- data.frame(outcomes, bitwiseNumbers)
  
  # Generate lookup file
  series <- sapply(
    X = seq_len(nrow(linking)),
    FUN = function(row) {
      paste0(
        '{ "key": "', linking$bitwiseNumbers[row],
        '", "value": "', linking$outcomes[row], '"}')
    })
  
  series <- c(series, '{ "key": "End", "value": "End"}')
  lookup <- paste0("[", paste(series, collapse = ","), "]")
  
  # Order names from longest to shortest to adjust in the right order
  linking <- linking[
    order(-sapply(linking$outcomes, function(x) stringr::str_length(x))), ]
  
  # Apply linking
  # Change all outcomes to bitwise number
  updated_path <- sapply(data$path, function(p) {
    stringi::stri_replace_all_fixed(
      p,
      replacement = as.character(linking$bitwiseNumbers),
      pattern = as.character(linking$outcomes),
      vectorize = FALSE)
  })
  
  # Sum the bitwise numbers of combinations (indicated by +)
  digitsPlusRegex <- "[[:digit:]]+[+][[:digit:]]+"
  updated_path <- sapply(
    X = updated_path,
    FUN = function(p) {
      while (!is.na(stringr::str_extract(p, digitsPlusRegex))) {
        pattern <- stringr::str_extract(p, digitsPlusRegex)
        p <- sub(digitsPlusRegex, eval(parse(text = pattern)), p)
      }
      return(p)
    })
  
  transformed_json <- buildHierarchy2(
    cbind(
      oath = updated_path,
      freq = data$freq)
  )
  
  print(transformed_json)
  
  result <- paste0(
    "{ \"data\" : ", transformed_json, ", \"lookup\" : ", lookup, "}")
  
  file <- paste0(folder, fileName)
  writeLines(text = result, con = file)
  # close(file)
  return(result)
}

#' createSunburstPlot
#'
#' Export a sunburst plot from a data.frame object.
#'
#' @param data
#'     A data frame containing two columns: 1) column "path" should specify the
#'     event cohorts separated by dashes - (combinations can be indicated using
#'     &) and 2) column "freq" should specify how often that (unique) path
#'     occurs.
#' @param folder
#'     Root folder to store the results.
#' @param fileName
#'     File name for the results.
#'
#' @export
#'
#' @returns NULL
#'
#' @examples
#' \dontrun{
#' createSunburstPlot(
#'   data = data.frame(
#'     path = c("1", "2"),
#'     freq = c("0.5", "0.5")))
#' }
createSunburstPlot <- function(data, folder, fileName) {
  # Assertions
  checkmate::assertDataFrame(x = data)
  checkmate::checkSubset(x = names(data), choices = c("freq", "path"))
  checkmate::assertCharacter(x = folder, null.ok = TRUE)
  checkmate::assertCharacter(x = fileName, null.ok = TRUE)
  
  outcomes <- unique(unlist(strsplit(
    data$path,
    split = "-", fixed = TRUE
  )))
  
  # Load CSV file and convert to JSON
  json <- transformCSVtoJSON(
    data = data,
    outcomes = outcomes,
    folder = folder,
    fileName = unlist(stringr::str_split(string = fileName, "\\."))[2])
  
  # Load template HTML file
  html <- paste(
    readLines(
      system.file(
        package = "TreatmentPatterns",
        "htmlTemplates", "sunburst_standalone.html")),
    collapse = "\n")
  
  # Replace @insert_data
  html <- sub("@insert_data", json, html)
  
  # Save HTML file
  writeLines(
    text = html,
    con = normalizePath(paste0(folder, fileName), mustWork = FALSE))
}


dat <- data.frame(
  path = c("a", "a-a", "a-b", "a-c", "a-b-d", "a-c-d",
           "b", "b-a", "b-b", "b-c"),
  freq = c("1", "0.25", "0.25", "0.25", "0.1", "0.1",
           "1", "0.25", "0.25", "0.25"))

createSunburstPlot(data = dat, folder = "./", fileName = "sankey_NEW.html")

