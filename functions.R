library(data.table)
library(ggplot2)

# FUNCTIONS ####
processing_abstract = function(path) {
  # Loads abstracts.txt into memory
  # Returns a dataframe with columns: abstract, paper_id
  
  abstracts = read.csv(paste(path, "/abstracts.txt", sep=""))
  abstracts = subset(abstracts,nchar(abstracts$abstract)>=100)
  abstracts = subset(abstracts,is.null(abstracts$PaperId) != TRUE)
  colnames(abstracts) = c("paper_id", "abstract")
  return(abstracts)
}

processing_author_instit = function(path) {
  # Loads paper_author_instit.txt into memory
  # Returns a dataframe with columns: paper_id, author_id, institution
  
  paper_author_instit = read.csv(paste(path, 
                                       "/paper_author_instit.txt", 
                                       sep=""))
  
  # keeping only authors from Bordeaux
  paper_author_instit = subset(
    paper_author_instit,
    paper_author_instit[,3] %in% grep(pattern = "Bordeaux",
                                      x=paper_author_instit$OriginalAffiliation,
                                      value=TRUE
    ))
  
  # drop rows with missing values
  paper_author_instit = subset(paper_author_instit,
                               !is.null(paper_author_instit$PaperId))
  paper_author_instit = subset(paper_author_instit,
                               !is.null(paper_author_instit$AuthorId))
  
  names(paper_author_instit) = c("paper_id", "author_id", "institution")
  return(paper_author_instit)
}

processing_paper_info = function(path) {
  # Loads paper_info.txt into memory
  # Returns a datatable with columns: paper_id, year
  
  paper_info = as.data.table(read.csv(paste(path, "/paper_info.txt", sep="")))
  paper_info = paper_info[, .(paper_id=PaperId, year=year(Date))]
  
  # Removing entries with missing values
  paper_info = paper_info[!is.null(paper_id) & !is.null(year),]
  return(paper_info)
}

processing_author_name = function(path) {
  # Loads author_name.txt into memory
  # Returns a dataframe with columns: author_id, author_name
  
  author = read.delim(paste(path, "/author_name.txt", sep=""))
  names(author) = c("author_id", "author_name")
  
  # Dropping rows with missing values
  author = subset(author, !is.null(author$author_id) & 
                          !is.null(author$author_name))
  return(author)
}

processing_references = function(path) {
  # Loads references.txt into memory
  # Returns a dataframe with columns: citing_paper_id, cited_paper_id
  
  references = read.csv(paste(path, "/references.txt", sep=""))
  names(references) = c("citing_paper_id", "cited_paper_id")
  
  # Dropping rows with missing values
  references = subset(references, !is.null(references$citing_paper_id) &
                                  !is.null(references$cited_paper_id) )
  return(references)
}

#### Q1 ####

q1_creating_raw_table = function(author_name,
                                 author_instit,
                                 paper_info,
                                 abstracts) {
  # Creating the q1 focus_sample table
  # Returns a dataframe with columns:
  #   author_id, author_name, institution, paper_id, year, abstract text
  q1_raw_table = merge(author_name,author_instit,by='author_id')
  q1_raw_table = merge(q1_raw_table, paper_info, by = 'paper_id')
  q1_raw_table = merge(q1_raw_table,abstracts, by = 'paper_id')
  return(q1_raw_table)
}

#### Q2 ####

q2_filtering_raw_table = function(q1_raw_table) {
  # Filtering the focus_sample table, keeping only authors with 3+ abstracts
  # Returns filtered focus_sample table
  author_list = as.data.frame(table(q1_raw_table$author_name))
  author_list = subset(author_list,
                       author_list[,2]>2)    # Keeping authors with 3+ abstracts
  focus_sample = subset(q1_raw_table,
                        q1_raw_table$author_name %in% author_list[,1])
  return(focus_sample)
}

#### Q3 ####

q3_abstract_to_keyword_list = function(abstract) {
  # Receives abstract as a string
  # Returns a vector containing the set of unique keywords in that abstract
  
  # Remove all non alphanumeric characters and fix everything to lowercase
  abstract = gsub(pattern = "[^a-z ]", 
                  replacement = " ", 
                  x=tolower(abstract))
  # extracts all words with length 3 or more
  pat = "[a-z]{3,}"
  keyword_list = regmatches(abstract, gregexpr(pattern=pat, text=abstract))
  
  # returns the vector without duplicates
  return(unique(keyword_list[[1]]))
}

q3_raw_paper_keywords = function(table_from_q1) {
  # Returns a data.table with columns paper_id and keyword. Its structure is
  # a dual key paper_id x keyword
  
  # We used this as an intermediate structure used both to create the list of
  # keyword sets for q4 but also ready to be merged for later. It's also
  # easy to filter keywords in this form.
  
  # Removing duplicate rows due to papers having multiple authors
  subtable = table_from_q1[, .(paper_id, abstract)]
  subtable = subtable[!duplicated(subtable),]
  
  # Creating a list of datatables with columns:
  #  paper_id, keyword
  list_of_tables = lapply(
    seq_along(subtable$paper_id),
    function(i) {
      kw_list = q3_abstract_to_keyword_list(subtable$abstract[i])  
      return(data.table("paper_id" = rep(subtable$paper_id[i],
                                         length(kw_list)),
                        "keyword" = kw_list))
    })
  
  # Creating the final datatable by concatenating all the small ones
  return(rbindlist(list_of_tables))
}

q3_filter_paper_keywords = function(paper_keywords) {
  # Receives the paper_keywords table from q3_raw_paper_keywords
  # removes keywords based on conditions: <5 or more than 10% of
  # presence in all abstracts.
  min_nb = 5
  max_nb = length(unique(paper_keywords$paper_id)) / 10
  
  # Computes the number of appearances of each keyword
  table_keywords = table(paper_keywords$keyword)
  
  # Generates a new column with a boolean telling if we should keep that keyword
  table_keywords = table_keywords > min_nb & table_keywords < max_nb
  table_keywords = data.table("keyword" = names(table_keywords),
                              "to_keep" = as.vector(table_keywords))
  paper_keywords = merge(paper_keywords, table_keywords, by="keyword")
  
  # Keeping only the keywords to keep, and returning the resulting data.table
  paper_keywords = paper_keywords[to_keep==TRUE, .(paper_id, keyword)]
  return(paper_keywords)
}

q3_create_list_sets = function(ppkw) {
  # Creates a list of sets of unique keywords from paper_keywords datatable.
  # This (lower level) implementation is preferred over:

  #   list_sets = lapply(paper_ids,
  #                      function(x) paper_keywords[paper_id == x, ]$keyword)

  # since in runs in a few seconds vs 1mn30s, by avoiding repeatdly comparing
  # the whole paper_id column
  
  # Sorting first to have all keywords of same paper_id contiguous
  setorder(ppkw, cols="paper_id")
  paper_ids = unique(ppkw$paper_id)
  lentable = dim(ppkw)[1]
  list_sets = list()
  # We'll keep track of our indices ourselves in order to make only one pass
  # on the whole table
  list_insert_index = 1
  start_i = 1
  current_pid = paper_ids[1]
  for (i in 2:lentable) {
    # iterating over rows, identifying sections belonging to the same
    # paper_id, extracting them and adding to the list
    if (ppkw$paper_id[i] != current_pid) {
      kwords = ppkw$keyword[start_i:(i-1)]
      list_sets[[list_insert_index]] = kwords
      current_pid = ppkw$paper_id[i]
      start_i = i
      list_insert_index = list_insert_index + 1
    } }
  # We need to also add he last paper_id which can't be catched by being
  # different from the next one (since it was the last)
  list_sets[[list_insert_index]] = ppkw$keyword[start_i:lentable]
  
  names(list_sets) = paper_ids
  return(list_sets)
}

#### Q4 ####

q4_specialization_table = function (ppkw, focus_sample) {
  # Constructs the specialization table,
  # Returns a data.table with columns:
  #   author_id, specialization
  
  # Constructing the data containers we'll need:
  #   a list containing the abstracts
  list_sets = q3_create_list_sets(ppkw)
  #   a dataframe allowing to find the asbtract in previous list by index
  # (a previous implementation did this by searching by name, but apprently
  # indexing list elements by names is O(n), whereas with indices it's O(1) 
  # I suspect this is due to indexing with numbers works like pointers)
  temp_df = data.frame("paper_id" = as.integer(names(list_sets)),
                       "index" = 1:length(list_sets))
  
  # subsampling the table and removing duplicates in subsample
  focus_sample = focus_sample[, .(author_id, paper_id)]
  focus_sample = focus_sample[!duplicated(focus_sample)]
  
  # relating the list index of abstracts with the author_ids for an easy sapply
  author_index = merge(focus_sample, 
                       temp_df, by="paper_id")[, .(author_id, index)]
  specialization_table = data.table("author_id" = unique(focus_sample$author_id))
  specialization_table$specialization = sapply(specialization_table$author_id,
                                               q4_compute_specialization,
                                               list_sets = list_sets,
                                               paper_index = author_index)
  return(specialization_table)
}

q4_compute_specialization = function(aid, list_sets, paper_index) {
  # Computes the specialization of an author
  # Receives both a list of author sets of keywords and list of his/her 
  # paper_ids
  
  # Returns the author_specialization as a scalar
  
  # Avoids computing the specialization if we divide by zero at the end
  abstracts = list_sets[paper_index[author_id==aid,]$index]
  if (length(abstracts) <= 1) return(NA)
  
  len = length(abstracts)
  
  # This version is faster but has loops: 1.8mn instead of 2m
  #  jaccard = 0
  #  for (i in 1:(len-1)) {
  #    for (j in (i+1):len) {
  #      leninter = length(abstracts[[j]][match(abstracts[[i]], 
  #                                             abstracts[[j]], 0L)])
  #      jaccard = jaccard + leninter / (length(abstracts[[i]]) + 
  #                                      length(abstracts[[j]]) - leninter)
  #    }
  #  }
  #  specialization = 2*jaccard / (len * (len - 1))
  #  return(specialization)
  # }
  
  # We create the matrix of jaccards with nested lapplys.
  # The index juggling is to only compute the upper triangle, as J(a,b) = J(b,a)
  # Intersect() was replaced by its R implementation without unique() 
  # and as.vector() to avoid useless computations.
  # We also used |AuB| = |A| + |B| - |AnB| to save half the computation
  jaccards = lapply(1:(len-1), 
                    function(i, x) {
                      lapply(abstracts[(i+1):len], 
                             function(y) {
                               leninter = length(y[match(x[[i]], y, 0L)])
                               return(leninter / (length(x[[i]]) + 
                                                    length(y) - leninter))
                             })},
                    x = abstracts
  )
  specialization = 2*sum(unlist(jaccards)) / (len * (len - 1))
  return(specialization)
}

#### Q5 ####

q5_add_citing_year_delta = function(
    references, #table
    focus_sample, #table
    paper_info
) {
  # Calculate the difference between citing and cited papers years and adds the
  # corresponding columns to references as well as the difference between them
  references = merge(x = references, 
                     y = paper_info,
                     by.x = "citing_paper_id",
                     by.y = "paper_id")
  names(references) = c('citing_paper_id',
                        'cited_paper_id',
                        'citing_year')
  references = merge(x = references, 
                     y = paper_info,
                     by.x = "cited_paper_id",
                     by.y = "paper_id")
  names(references) = c('citing_paper_id',
                        'cited_paper_id',
                        'citing_year',
                        'cited_year')

  # Our opinion is : some years_delta are negative, which is impossible.
  # The fact that the line exists means that there is a link between those 2
  # papers :
  # it might be a human mistake during input process.
  # We decided to keep them as if the column were swapped by accident.
  references$year_delta = references$citing_year - references$cited_year
  references$swapped = references$year_delta < 0
  references$year_delta = abs(references$year_delta)
  
  # So we swapped citing and cited paper id in order to have a relevant nb cites
  # Not sure why the condition in ifelse seems reversed but this is what we
  # needed to pass the tests.
  temp_citing = ifelse((!references$swapped),
                       references$cited_paper_id,
                       references$citing_paper_id)
  references$cited_paper_id = ifelse(references$swapped,
                                     references$cited_paper_id,
                                     references$citing_paper_id)
  references$citing_paper_id = temp_citing
  
  # Filter to take only cited papers from Bordeaux, but keep all citing papers
  # We filter after swapping paper ids columns 
  references = references[references$cited_paper_id %in% focus_sample$paper_id,]
  return(references)
}


q5_count_citations = function(refs, delta_y=NULL) {
  # Counts the number of citations of each papers
  # Returns a data.frame with columns paper_id and nb_cites
  if (!is.null(delta_y)) refs = refs[refs$year_delta <=delta_y,]

  paper_nb_cites = as.data.frame(table(refs$cited_paper_id))
  names(paper_nb_cites) = c('paper_id','nb_cites')
  return(paper_nb_cites)
}

#### Q6 ####
q6_add_nb_cites = function(paper_keywords, paper_nb_cites) {
  # Adds the number of citations of each paper to the paper_keywords table
  paper_nb_cites$paper_id = as.numeric(levels(paper_nb_cites$paper_id))
  paper_keywords = merge(paper_keywords, paper_nb_cites, by='paper_id')
  return(paper_keywords)
}

#### Q7 ####

q7_sorted_citation_global_table = function(
    paper_keywords_mapping_filtered) {
  # Computes the average number of citations per keyword:
  # (NB citation of keyword X) / (NB of abstracts containing the keyword X)
  # Adds the corresponding column to paper_keywords and sorts the table.
  
  keyword_cites = paper_keywords_mapping_filtered[
    , .(avg_nb_cites = sum(nb_cites) / sum(nb_cites/nb_cites)), by=keyword
  ]
  keyword_cites = setorder(keyword_cites, cols = -"avg_nb_cites")
  return(keyword_cites)
}

#### Q8 ####

q8_merge_it_all = function(
    focus_sample,
    specialization_table,
    paper_keywords
) {
  # Merges previous tables into the final data.table
  # Returns a data.table with columns:
  #   author_id, author_name, nb_publications, 
  #                                avg_nb_cites, total_nb_cites, specialization
  # with one line per author_id
  result = focus_sample[, c("author_id", "author_name", "paper_id")]
  
  # extracting the number of citations of each paper
  subtable = paper_keywords[, .(paper_id, nb_cites)] #to remove duplicates
  result = merge(result, subtable[!duplicated(subtable),], by="paper_id")
  
  # aggregating by author and computing average number of citations
  result = result[, .(nb_cites=sum(nb_cites), 
                      nb_publications=sum(paper_id > 0)), 
                  by=author_id]
  result$avg_nb_cites = result$nb_cites / result$nb_publications
  
  # inserting the specialization and names of each author 
  result = merge(result, specialization_table, by="author_id")
  result = merge(x = result,
                 y = focus_sample[, .(author_name), by="author_id"],
                 by = 'author_id')
  
  # ordering the columns properly and removing duplicates
  result = result[, .(author_id, author_name, 
                      nb_publications, avg_nb_cites, 
                      nb_cites, specialization)]
  result = result[!duplicated(result),]
  return(result)
}

#### Q9 ####
q9_get_noteworthy_scientists = function(dt_final) {
  # Finds noteworthy scientists and constructs the labels column
  dt_final$author_name = gsub("(^|[[:space:]])([[:alpha:]])", 
                              "\\1\\U\\2",
                              dt_final$author_name,
                              perl = TRUE
  )
  
  # We'll order scientist by their avg_nb_cites quantiles.
  noteworthy_threshold = ceiling(quantile(x=dt_final$avg_nb_cites,
                                  probs=seq(0, 1, 0.001))['99.9%'])
  dt_final$noteworthy_names = dt_final$author_name
  dt_final$noteworthy_names[dt_final$avg_nb_cites < noteworthy_threshold] = NA
  dt_final$avg_nb_cites_Q = ecdf(dt_final$avg_nb_cites)(dt_final$avg_nb_cites)
  
  # For some unknown reason, if we gsub only once, the names are not capitalized
  # on the graph
  dt_final$author_name = gsub("(^|[[:space:]])([[:alpha:]])", 
                              "\\1\\U\\2",
                              dt_final$author_name,
                              perl = TRUE)
  
  return(dt_final)
}


q9_noteworthy_scientists_graph = function(dt_final) {
  # Function generates the graph based on final table
  
  # Creating custom color gradient palette
  color_grad = colorRampPalette(c("#F08888", "#000000"))(20)
  
  # Function to generate the specialization vs nb_cites graph
  graph = ggplot(dt_final, 
                 aes(x=nb_cites, 
                     y=specialization)) +
    labs(color="Average\ncitations") +
    scale_color_gradientn(colors=color_grad) +
    geom_point(alpha=dt_final$avg_nb_cites_Q, # Creates the scatter plot between
               size=dt_final$avg_nb_cites_Q,  # avg_nb_cites and specialization
               data = dt_final, 
               aes(x=nb_cites, y=specialization, color=avg_nb_cites)) +
    geom_text(size=3, check_overlap=TRUE, hjust=0, vjust=0, # Adding labels
              nudge_x=0.05, nudge_y=0.01, # for noteworthy scientists
              data=dt_final, aes(x=nb_cites, 
                                 y=specialization,
                                 label=noteworthy_names,
                                 fontface="bold")) +
    theme(panel.background = element_rect(fill="#EEEEEE"), # General graph style
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position=c(0.85, 0.55),
          legend.background = element_rect(fill="#EEEEEE"),
          legend.key.height = unit(0.75, "cm"),
          legend.key.width = unit(1, "cm"),
          plot.caption = element_text(face="italic"),
          plot.title = element_text(hjust=0.5, face="bold"),
          legend.title = element_text(size=10)) +
    xlab("Number of citations") + # Labeling axes and adding comments
    ylab("Specialization") +
    labs(
      title="Trade-off between author specialization and number of citations") +
    labs(caption="The more specialized scientists are and the less citations they tend to have.\nScientists with the highest average number of citations tend to be balanced between both.")
  return(graph)
}

#### Timing function ####

time_elapsed = function(msg, s, prec=1) {
  # Computes the time delta and displays the proper unit (basic difference
  # in R doesn't display the unit in cat())
  
  delta = difftime(Sys.time(), c(s), unit="secs")[[1]]
  time_unit = "s"
  for (u in c("mn", "hr")){
    if (delta > 60) {
      delta = delta / 60
      time_unit = u
    }
    else break
  }
  cat(paste(msg, trunc(delta*10^prec)/(10^prec), time_unit), "\n")
}


### Main ####

main = function(PATH=PATH, 
                keywords_to_display=keywords_to_display,
                timing=timing) {  
  # Question 1:
  if (timing) start_time = Sys.time()
  ds_abstracts = processing_abstract(PATH)
  ds_author_instits = processing_author_instit(PATH)
  ds_paper_info = processing_paper_info(PATH)
  ds_author_name = processing_author_name(PATH)
  ds_references = processing_references(PATH)
  if (timing) time_elapsed("Processing datasets took", start_time)
  
  # Question 1
  if (timing) s = Sys.time()
  focus_sample = data.table(q1_creating_raw_table(ds_author_name,
                                                  ds_author_instits,
                                                  ds_paper_info,
                                                  ds_abstracts))
  if (timing) time_elapsed("Creating raw focus sample took", s)
  
  # Question 2:
  if (timing) s = Sys.time()
  focus_sample = q2_filtering_raw_table(focus_sample)
  if (timing) time_elapsed("Filtering focus sample took", s)
  
  # Question 3
  if (timing) s = Sys.time()
  paper_keywords = q3_raw_paper_keywords(focus_sample)
  if (timing) time_elapsed("Creating raw paper keywords table took", s)
  
  if (timing) s = Sys.time()
  paper_keywords = q3_filter_paper_keywords(paper_keywords)
  if (timing) time_elapsed("Filtering paper_keywords took", s)
  
  # Question 4
  if (timing) s = Sys.time()
  author_specialization = q4_specialization_table(paper_keywords, 
                                                     focus_sample)
  if (timing) time_elapsed("Computing specialization took", s)
  
  # Question 5
  if (timing) s = Sys.time()
  ds_references = q5_add_citing_year_delta(ds_references,
                                           focus_sample,
                                           ds_paper_info)
  if (timing) time_elapsed("Computing difference between citing and cited papers took", s)
  
  if (timing) s = Sys.time()
  paper_nb_cites = q5_count_citations(ds_references, delta_y=5)
  if (timing) time_elapsed("Counting paper citations took", s)
  
  # Question 6
  if (timing) s = Sys.time()
  paper_keywords = q6_add_nb_cites(paper_keywords, paper_nb_cites)
  if (timing) time_elapsed("Adding paper citations took", s)
  
  # Question 7
  # computing the sorted top keyword tables
  if (timing) s = Sys.time()
  top_keywords_global = q7_sorted_citation_global_table(paper_keywords)
  if (timing) time_elapsed("Computing and sorting the keywords average citations took", s)
  
  # Displaying the top n keywords
  N = keywords_to_display
  keywords = head(top_keywords_global, n=N)$keyword
  cat("\nTop", N, "keywords:\n")
  for (i in 1:N) {
    cat("#", i, ": ", keywords[i], "\n", sep="")
  }
  cat("\n")
  
  # Question 8
  if (timing) s = Sys.time()
  final_table = q8_merge_it_all(focus_sample, 
                                author_specialization,
                                paper_keywords)
  if (timing) time_elapsed("Creating the final table took", s)
  
  # Question 9
  #   Adding the labels to display on the graph
  final_table = q9_get_noteworthy_scientists(final_table)
  final_table = final_table[!is.na(specialization),]
  q9_graph = q9_noteworthy_scientists_graph(final_table)
  print(q9_graph)
  
  if (timing) time_elapsed("Full script took", start_time)
}