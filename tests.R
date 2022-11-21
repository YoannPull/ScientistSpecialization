source("functions.R")
library("testthat")

PATH = "R2022--DATA"

# Switch for running all tests (except the time_elapsed)
run_all = TRUE

allow_tests_q2 = FALSE
allow_tests_q3 = FALSE
allow_tests_q4 = FALSE
allow_tests_q5 = FALSE
allow_tests_q7 = FALSE
allow_tests_q8 = FALSE

#### Testing Q3 ####
if (allow_tests_q3 | run_all) {
  test_that("q3_abstract_to_keyword_list properly extracts words",
            {abkt = "i am a string jane a"
            expect_identical(q3_abstract_to_keyword_list(abkt),
                             c("string", "jane"))
            })
  
  test_that("q3_abstract_to_keyword_list fixes case and removes bad characters",
            {abkt = "i a*m ++a string jaNE a$"
            expect_identical(q3_abstract_to_keyword_list(abkt),
                             c("string", "jane"))
            })
  
  test_that("q3_abstract_to_keyword_list removes extra spaces",
            {abkt = "i a*m ++a string  8=====D   jaNE a$"
            expect_identical(q3_abstract_to_keyword_list(abkt),
                             c("string", "jane"))
            })
  
  test_that("q3_abstract_to_keyword_list properly eliminates duplicates",
            {abkt = "i am a stR+=INg string jane a$ jAne"
            result = q3_abstract_to_keyword_list(abkt)
            expect_identical(result,
                             c("str", "ing", "string", "jane"))
            })
  
  test_that("q3_raw_paper_keywords basic example",
            {
              input_table = data.table("abstract" = c("I am a string",
                                                      "I am also a string"),
                                       "paper_id" = 1:2)
              correct_result = data.table("paper_id" = c(1, 2, 2),
                                          "keyword" = c("string", 
                                                        "also", 
                                                        "string")
              )
              result = q3_raw_paper_keywords(input_table)
              expect_equal(result, correct_result)
            })
  
  test_that("q3_raw_paper_keywords with duplicate keywords",
            {
              input_table = data.table("abstract" = c("I am a boxer",
                                                      "I am also a string",
                                                      "string string string"),
                                       "paper_id" = 1:3)
              correct_result = data.table("paper_id" = c(1, 2, 2, 3),
                                          "keyword" = c("boxer", 
                                                        "also", 
                                                        "string",
                                                        "string")
              )
              result = q3_raw_paper_keywords(input_table)
              expect_equal(result, correct_result)
            })
  
  test_that("q3_raw_paper_keywords with duplicate rows",
            {
              input_table = data.table("abstract" = c(rep("I am a boxer", 3),
                                                      "I am also a string",
                                                      "string string string"),
                                       "paper_id" = c(rep(1, 3), 2:3))
              correct_result = data.table("paper_id" = c(1, 2, 2, 3),
                                         "keyword" = c("boxer", 
                                                       "also", 
                                                       "string",
                                                       "string")
                                         )
              result = q3_raw_paper_keywords(input_table)
              expect_equal(result, correct_result)
            })
  
  test_that("q3_filter_paper_keywords remove everyone",
            {
              input_table = data.table("abstract" = c("I am a string",
                                                      "I am also a string"),
                                       "paper_id" = 1:2)
              correct_result = data.table("paper_id" = integer(),
                                          "keyword" = character()
              )
              setkey(correct_result, keyword)
              result = q3_filter_paper_keywords(
                q3_raw_paper_keywords(input_table))
              
              expect_equal(result, correct_result)
            })
  
  test_that("q3_filter_paper_keywords remove > 10%, remove <5, keep others",
            {
              input_table = data.table(
                "abstract" = c(rep("I am a string", 100),
                               rep("I am also a string", 10),
                               "I am jane string"),
                "paper_id" = 1:111)
              correct_result = data.table("paper_id" = 101:110,
                                          "keyword" = "also"
              )
              setkey(correct_result, keyword)
              result = q3_filter_paper_keywords(
                q3_raw_paper_keywords(input_table))
              
              expect_equal(result, correct_result)
            })
}

#### Tests Q4 ####
if (allow_tests_q4 | run_all) {
  test_that("q4 the lol way, basic case", {
    ppkw = data.table("paper_id" = c(rep(1, 3),
                                     rep(3, 4),
                                     rep(4, 3),
                                     rep(6, 2)),
                      "keyword" = c("a", "z", "e",
                                    "a", "z", "r", "t",
                                    "z", "e", "q", 
                                    "z", "v"))
    focus_sample = data.table("paper_id" = c(1, 3, 4, 6),
                              "author_id" = c(1, 1, 3, 3))
    
    result = q4_specialization_table(ppkw, focus_sample)
    correct_result = data.table("author_id" = c(1, 3),
                                "specialization" = c(2/5, 1/4))
    
    expect_equal(result, correct_result)
  })
  
  test_that("q4 alternating authors", {
    ppkw = data.table("paper_id" = c(rep(1, 3),
                                     rep(33, 4),
                                     rep(4, 3),
                                     rep(6, 2)),
                      "keyword" = c("a", "z", "e",
                                    "a", "z", "r", "t",
                                    "z", "e", "q", 
                                    "z", "v"))
    focus_sample = data.table("paper_id" = c(1, 33, 6, 4),
                              "author_id" = c(1, 3, 1, 3))
    
    result = q4_specialization_table(ppkw, focus_sample)
    correct_result = data.table("author_id" = c(1, 3),
                                "specialization" = c(1/4, 1/6))
    expect_equal(result, correct_result)
  })
  
  test_that("q4 alternating authors", {
    ppkw = data.table("paper_id" = c(rep(1, 3),
                                     rep(33, 4),
                                     rep(4, 3),
                                     rep(6, 2)),
                      "keyword" = c("a", "z", "e",
                                    "a", "z", "r", "t",
                                    "z", "e", "q", 
                                    "z", "v"))
    focus_sample = data.table("paper_id" = c(1, 33, 6, 4, 4),
                              "author_id" = c(1, 3, 1, 3, 1))
    
    result = q4_specialization_table(ppkw, focus_sample)
    correct_result = data.table("author_id" = c(1, 3),
                                "specialization" = c((1/2 + 1/4 + 1/4)/3, 1/6))
    expect_equal(result, correct_result)
  })
}

#### Test Q5 ####
if (allow_tests_q5 | run_all) {
  test_that("q5 add years without swap", {
    references = data.table("citing_paper_id" = 1:2,
                            "cited_paper_id" = 3:4)
    focus_sample = data.table("paper_id"= 1:4)
    paper_info = data.table("paper_id"= 1:4,
                            "year"= 2004:2001)
    
    correct_result = data.table("citing_paper_id" = 1:2,
                                "cited_paper_id" = 3:4,
                                "citing_year" = 2004:2003,
                                "cited_year" = 2002:2001,
                                "year_delta" = c(2, 2),
                                "swapped" = c(FALSE, FALSE))
    
    result = q5_add_citing_year_delta(references, focus_sample, paper_info)
    expect_equal(result, correct_result)
  })
  
  test_that("q5 add years with partial swap no swap", {
    references = data.table("citing_paper_id" = c(3, 4),
                            "cited_paper_id" = c(1, 2))
    focus_sample = data.table("paper_id"= 1:4)
    paper_info = data.table("paper_id"= 1:4,
                            "year"= 2004:2001)
    
    correct_result = data.table("citing_paper_id" = 1:2,
                                "cited_paper_id" = 3:4,
                                "citing_year" = c(2002, 2001),
                                "cited_year" = c(2004, 2003),
                                "year_delta" = c(2, 2),
                                "swapped" = c(TRUE, TRUE))
    
    result = q5_add_citing_year_delta(references, focus_sample, paper_info)
    expect_equal(result, correct_result)
  })
  
  test_that("q5 add years with partial swap no swap", {
    references = data.table("citing_paper_id" = c(3, 2),
                            "cited_paper_id" = c(1, 4))
    focus_sample = data.table("paper_id"= 1:4)
    paper_info = data.table("paper_id"= 1:4,
                            "year"= 2004:2001)
    
    correct_result = data.table("citing_paper_id" = 1:2,
                                "cited_paper_id" = 3:4,
                                "citing_year" = c(2002, 2003),
                                "cited_year" = c(2004, 2001),
                                "year_delta" = c(2, 2),
                                "swapped" = c(TRUE, FALSE))
    
    result = q5_add_citing_year_delta(references, focus_sample, paper_info)
    expect_equal(result, correct_result)
  })
}

#### Tests Q7 ####
if (allow_tests_q7 | run_all) {
  test_that("q7_sorted_citations per papers, with each paper being cited once",
            {
              input_table = data.table(
                "abstract" = c(rep("I am a string", 300),
                               rep("I am also a string", 6),
                               rep("I am also a jane, boat", 12)
                ),
                "paper_id" = 1:318)
              paper_keywords = q3_filter_paper_keywords(
                q3_raw_paper_keywords(input_table)
              )
              paper_keywords$nb_cites = 1
              result = q7_sorted_citation_global_table(
                paper_keywords
              )
              
              correct_result = data.table(
                "keyword" = c("also", "jane", "boat"),
                "avg_nb_cites" = c(18/18, 12/12, 12/12)
              )
              setkey(correct_result, cols=keyword)
              
              expect_equal(result, correct_result)
            })
  
  test_that("q7_sorted_citations, another, per papers",
            {
              input_table = data.table(
                "abstract" = c(rep("I am a string", 300),
                               rep("I am also a jane, table", 10),
                               rep("I am also a string, jane", 6),
                               rep("I am also a jane, boat", 12)
                               ),
                "paper_id" = 1:328)
              
              paper_keywords = q3_filter_paper_keywords(
                q3_raw_paper_keywords(input_table)
              )
              paper_keywords$nb_cites = 1
              result = q7_sorted_citation_global_table(
                paper_keywords
              )
              
              correct_result = data.table(
                "keyword" = c("also", "jane", "boat", "table"),
                "avg_nb_cites" = rep(1, 4)
              )
              setkey(correct_result, cols=keyword)
              
              expect_equal(result, correct_result)
            })
  
  test_that("q7_sorted_citations per papers, with each paper being cited twice",
            {
              input_table = data.table(
                "abstract" = c(rep("I am a string", 300),
                               rep("I am also a string", 6),
                               rep("I am also a jane, boat", 12)
                ),
                "paper_id" = 1:318)
              paper_keywords = q3_filter_paper_keywords(
                q3_raw_paper_keywords(input_table)
              )
              paper_keywords$nb_cites = 2
              result = q7_sorted_citation_global_table(
                paper_keywords
              )
              
              correct_result = data.table(
                "keyword" = c("also", "jane", "boat"),
                "avg_nb_cites" = rep(2, 3)
              )
              setkey(correct_result, cols=keyword)
              
              expect_equal(result, correct_result)
            })
  
  
  test_that("q7_sorted_citations per papers, each paper cited different number",
            {
              input_table = data.table(
                "abstract" = c(rep("I am a string", 300),
                               rep("I am also a string", 6),
                               rep("I am also a jane, boat", 12)
                ),
                "paper_id" = 1:318)
              paper_keywords = q3_filter_paper_keywords(
                q3_raw_paper_keywords(input_table)
              )
              paper_keywords$nb_cites = 1:42
              cites_also = sum(paper_keywords[keyword=="also", nb_cites])
              cites_jane = sum(paper_keywords[keyword=="jane", nb_cites])
              cites_boat = sum(paper_keywords[keyword=="boat", nb_cites])
              correct_result = data.table(
                "keyword" = c("jane", "boat", "also"),
                "avg_nb_cites" = c(cites_jane/12, cites_boat/12, cites_also/18)
              )
              
              result = q7_sorted_citation_global_table(
                paper_keywords
              )
              expect_equal(result, correct_result)
            })
}


#### Tests Q8 ####

if (allow_tests_q8) {
  test_that("q8_merge_it_all check correct columns",
            {
              ex_focus_sample = data.table(
                "author_id" = 1,
                "author_name" = c("john", "john"),
                "institution" = c("Bordeaux 1", "Bordeaux 2"),
                "paper_id" = 1:2,
                "year" = c(2016, 2018),
                "abstract" = c("je suis une vache",
                               "je suis une table"))
              ex_spec_table = data.table(
                "author_id" = 1,
                "specialization" = 0.8
              )
              ex_paper_keyword = data.table("paper_id" = 1:2,
                                            "keyword" = c("a", "b"),
                                            "nb_cites" = c(3, 7))
              
              correct_output = c("author_id",# 
                                 "author_name",# 
                                 "nb_publications", # can calculate from focus_sample
                                 "avg_nb_cites", # = nb_cites / nb_publications
                                 "nb_cites", #
                                 "specialization")#
              
              expect_equal(
                colnames(q8_merge_it_all(ex_focus_sample,
                                         ex_spec_table,
                                         ex_paper_keyword)),
                correct_output)
            })
  
  test_that("q8_merge_it_all check no duplicate author",
            {
              ex_focus_sample = data.table(
                "author_id" = 1,
                "author_name" = c("john", "john"),
                "institution" = c("Bordeaux 1", "Bordeaux 2"),
                "paper_id" = 1:2,
                "year" = c(2016, 2018),
                "abstract" = c("je suis une vache",
                               "je suis une table"))
              ex_spec_table = data.table(
                "author_id" = 1,
                "specialization" = 0.8
              )
              ex_paper_keyword = data.table("paper_id" = 1:2,
                                            "keyword" = c("a", "b"),
                                            "nb_cites" = c(3, 7))
              
              correct_output = 1
              
              expect_equal(
                length(q8_merge_it_all(ex_focus_sample,
                                       ex_spec_table,
                                       ex_paper_keyword)$author_id),
                correct_output)
            })
  
  test_that("q8_merge_it_all check correct length of table",
            {
              ex_focus_sample = data.table(
                "author_id" = c(1, 3),
                "author_name" = c("john", "jack"),
                "institution" = c("Bordeaux 1", "Bordeaux 2"),
                "paper_id" = 1:2,
                "year" = c(2016, 2018),
                "abstract" = c("je suis une vache",
                               "je suis une table"))
              ex_spec_table = data.table(
                "author_id" = c(1, 3),
                "specialization" = c(0.8, 1.2)
              )
              ex_paper_keyword = data.table("paper_id" = 1:2,
                                            "keyword" = c("vache", "table"),
                                            "nb_cites" = c(3, 7))
              
              correct_output = 2
              
              result = q8_merge_it_all(ex_focus_sample,
                                       ex_spec_table,
                                       ex_paper_keyword)
              
              expect_equal(
                length(result$author_id),
                correct_output)
            })
  
  test_that("q8_merge_it_all check 3 authors",
            {
              ex_focus_sample = data.table(
                "author_id" = c(1, 2, 2, 3),
                "author_name" = c("john", "jack", "jack", "bernadette"),
                "institution" = c("Bordeaux 1", "Bordeaux 2", "bordeaux1", "bordeaux2"),
                "paper_id" = 1:4,
                "year" = c(2016, 2018, 2019, 2005),
                "abstract" = c("je suis une vache",
                               "je suis une table",
                               "table, goat",
                               "table table"))
              ex_spec_table = data.table(
                "author_id" = 1:3,
                "specialization" = c(0.8, 1.2, 90)
              )
              ex_paper_keyword = data.table("paper_id" = 1:4,
                                            "keyword" = c("vache", "table",
                                                          "goat", "truc"),
                                            "nb_cites" = c(3, 7, 11, 0))
              
              correct_output = 3
              
              result = q8_merge_it_all(ex_focus_sample,
                                       ex_spec_table,
                                       ex_paper_keyword)
              
              expect_equal(dim(result)[1], correct_output)
            })
}
