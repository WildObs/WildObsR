#' Extract classification information from taxize
#'
#' This function facilitates extracting taxonomic classification infromation from taxize objects.
#' This function helps create the WildObs verified taxonomy database by wrangling the taxize objects to match our standard.
#'
#' @param uids This is a taxize object created by running the function taxize::get_ids() to obtain verified species identifications
#' @param classif This is a taxize object by running the function taxize::classification() to obtain verified species classifications
#' @param request The specific classification information requested by the function. Defaults to species, genus, family, order, class.
#'
#' @return Creates a dataframe that matches the format of the WildObs verified taxonomy database.
#' @author Nathan Travell (I think?)
#' @export
extract_classif = function(uids = NULL, classif = NULL, request = c("species","genus","family","order","class")){

  #warnings
  if(is.null(uids)){ print("Missing argument 1: IDs. Use taxize::get_ids() to obtain the ids"); return(NULL) }
  if(is.null(classif)){ print("Missing argument 2: Classification. Use taxize::classification() to obtain the classification"); return(NULL) }

  #make a table
  sp = data.frame(user_provided_name = attr(uids[[1]],"names")) %>%
    cbind(data.frame(uids[[1]])) %>%
    select(-"class")

  t = data.frame(matrix(NA,nrow = nrow(sp),ncol = length(request)))
  colnames(t) = request
  sp = cbind(sp,t)
  rm(t)

  #remove NAs
  sp = sp[!is.na(sp$ids),]
  classif[[1]] =  classif[[1]][!is.na( classif[[1]])]

  #extract
  for(i in sp$ids){ #id loop
    t = classif[[1]][names(classif[[1]]) == i][[1]]

    for(a in request){ #request loop

      if(length(t$name[t$rank == a]) >0) { #to avoid error. It is not possible to extract species from a family lvl record
        sp[sp$ids == i,colnames(sp) == a] = t$name[t$rank == a]
      }
    }#request loop
  } #id loop
  return(sp)
}
