library(tidyverse)

## NSF award API code (written by Katie Schreyer)

#' Search NSF award api
#' 
#' This function is a wrapper for the public facing award API
#' 
#' NSF public facing award database API is described here: https://resources.research.gov/common/webapi/awardapisearch-v1.htm
#' 
#' Returns a maximum of 10000 results at once, which is the API max. If you need more, you need to chunk your query somehow
#'
#' @param ... Desired search parameters as described in the API spec
#'
#' @returns dataframe of results from API
#'
#' @export
#' @examples {
#' # search for awards about water made to institutions in the state of Texas
#' search_nsf_award_api(keyword = "water",
#'                      awardeeStateCode = "TX")
#' }
search_nsf_award_api <- function(...) {
  
  params <- paste(names(list(...)), list(...), collapse = "&", sep = "=")
  
  offset <- seq(from = 0, to = 7500, by = 2500)
  
  urls <- paste0("https://www.research.gov/awardapi-service/v1/awards.json?",params,"&offset=",offset,"&rpp=2500")
  
  results <- NULL
  
  for (i in urls) {
    this_results <- jsonlite::fromJSON(URLencode(i))
    
    this_results <- as.data.frame(this_results$response$award)
    
    results <- dplyr::bind_rows(results,this_results)
    
    if (nrow(this_results) < 2500) break
  }
  
  if (nrow(results) == 10000) warning("API Max of 10,000 results reached.")
  
  results 
}


# if you already know your award_ids
# does 25 awd_ids at a time
get_nsf_award_api <- function(award_ids) {
  
  results <- NULL
  
  awd_ids <- split(award_ids, ceiling(seq_along(award_ids)/25)) 
  
  awd_ids <- map(awd_ids, ~ paste0(.x, collapse = ","))
  
  urls <- paste0("https://www.research.gov/awardapi-service/v1/awards/",awd_ids,".json")
  
  for (i in urls) {
    
    this_results <- jsonlite::fromJSON(URLencode(i))
    
    this_results <- as.data.frame(this_results$response$award)
    
    results <- dplyr::bind_rows(results,this_results)
    
    Sys.sleep(2)  
  }
  
  results 
}

## NSF-specific stop words
nsf_stop <- c("a", "about", "above", "academia", "academic", "across", "after", 
              "afterwards", "again", "against", "ai", "aim", "aims", "algorithm", 
              "algorithms", "all", "almost", "alone", "along", "already", "also", 
              "although", "always", "am", "among", "amongst", "amoungst", "amount", 
              "an", "analyses", "analysis", "analytics", "and", "another", "any", 
              "anyhow", "anyone", "anything", "anyway", "anywhere", "appendices", 
              "appendix", "applied", "approach", "approaches", "architecture", 
              "architectures", "are", "areas", "around", "artificial", "as", 
              "assessment", "assistant", "at", "award", "awards", "back", "be", 
              "became", "because", "become", "becomes", "becoming", "been", 
              "before", "beforehand", "behind", "being", "below", "beside", 
              "besides", "between", "beyond", "bilateral", "bill", "bio", 
              "biochemistry", "biodiversity", "bioengineering", "biographical", 
              "biological", "biology", "biophysical", "biosciences", "biosketch", 
              "both", "bottom", "broader", "budget", "but", "by", "call", "can", 
              "cannot", "cant", "capabilities", "capability", "capacity", 
              "capacity-building", "career", "catalyst", "catalyze", "ccf", 
              "cell", "cells", "cellular", "center", "centers", "challenge", 
              "cise", "cited", "cns", "co", "co-pi", "collaboration", 
              "collaboration-only", "collaborations", "collaborator", 
              "collaborators", "college", "colleges", "commitment", 
              "communities", "community", "compliance", "computation", 
              "computational", "computer", "computers", "computing", "con", 
              "conference", "conferences", "context", "contributions", 
              "cooperation", "coordination", "copi", "could", "couldnt", 
              "countries", "country", "creative", "creativity", "criteria", 
              "criterion", "cross-border", "cry", "current", "curricular", 
              "curriculum", "cyberinfrastructure", "data", "dataset", "datasets", 
              "dbi", "de", "deb","deemed","deliverable", "deliverables", "department", 
              "departments", "describe", "description", "detail", "development", 
              "director", "directorate", "disciplinary", "disseminating", 
              "dissemination", "distributed", "diversity", "division", "do", 
              "documents", "done", "down", "due", "during", "each", "early", 
              "ecological", "ecology", "ecosystem", "ecosystems", "edu", 
              "education", "educator", "eg", "ehr", "eight", "either", "eleven", 
              "else", "elsewhere", "empty", "engaged", "engineering", "enough", 
              "entrepreneurship", "equipment", "equity", "etc", "evaluate", 
              "evaluation", "even", "ever", "every", "everyone", "everything", 
              "everywhere", "evidence-based", "evolution", "evolutionary", 
              "except", "exchange", "exchanges", "experimental", "facilities", 
              "faculty", "few", "fifteen", "fifty", "fill", "find", "fire", 
              "first", "five", "for", "foreign", "former", "formerly", "forty", 
              "found", "foundation", "four", "framework", "from", "front", 
              "full", "fundamental", "funding", "further", "gene", "genes", 
              "genomic", "genomics", "get", "give", "global", "go", "goals", 
              "grand", "grant", "grantsgov", "guidance", "guidelines", "had", 
              "hardware", "has", "hasnt", "have", "he", "hence", "her", "here", 
              "hereafter", "hereby", "herein", "hereupon", "hers", "herself", 
              "high-reward", "high-risk", "higher", "him", "himself", "his", 
              "host", "hosts", "how", "however", "hundred", "i", "ie", "if", 
              "iis", "impact", "impacts", "in", "inc", "inclusion", "indeed", 
              "industries", "industry", "informatics", "information", 
              "infrastructure", "infrastructures", "initiative", "initiatives", 
              "innovation", "innovative", "institution", "institutional", 
              "institutions", "instruction", "instructional", "integrated", 
              "integration", "integrative", "intellectual", "intelligence", 
              "interest", "international", "into", "investigator", 
              "investigators", "ios", "is", "it", "its", "itself", "iuse", 
              "justification", "keep", "laboratories", "laboratory", "last", 
              "latter", "latterly", "leadership", "learning", "least", "less", 
              "letter", "letters", "life", "lifetime", "ltd", "machine", "made", 
              "management", "many", "may", "mcb", "me", "meanwhile", "mechanism", 
              "mechanisms", "mentoring", "merit", "metabolic", "metabolism", 
              "method", "methods", "microbial", "microbiology", "might", "mill", 
              "mine", "mission", "ml", "model", "models", "molecular", "more", 
              "moreover", "most", "mostly", "move", "much", "multilateral", 
              "multinational", "must", "my", "myself", "name", "namely", "national", 
              "need", "neither", "network", "networks", "neural", "neuroscience", 
              "never", "nevertheless", "next", "nine", "no", "nobody", 
              "noncompliance", "none", "noone", "nor", "not", "nothing", "novel", 
              "now", "nowhere", "nsf", "nsf's","nuclear", "objective", "objectives", 
              "oci", "of", "off", "office", "often", "on", "once", "one", "only", 
              "onto", "optimization", "or", "organ", "organism", "organismal", 
              "organisms", "organization", "organizations", "organs", "other", 
              "others", "otherwise", "our", "ours", "ourselves", "out", "outcome", 
              "outcomes", "outreach", "over", "own", "pappg", "part", "partnership", 
              "partnerships", "path", "pathway", "pathways", "pedagogical", 
              "pedagogy", "pending", "per", "perhaps", "persistence", 
              "personnel", "physiological", "physiology", "pi", "plan", 
              "platform", "platforms", "please", "policy", "population", 
              "populations", "postdoctoral", "practice", "practices", 
              "preliminary", "premier", "principal", "priority", "privacy", 
              "professional", "professor", "professorship", "program", "project", 
              "projects", "proposal", "proposals", "protein", "proteins", "put", 
              "quantum", "rather", "re", "reciprocal", "references", "required", 
              "requirements", "research", "researcher", "researchgov", 
              "resources", "result", "results", "retention", "review", 
              "reviewer", "reviewers", "reviews", "robotics", "same", "science", 
              "security", "see", "seem", "seemed", "seeming", "seems", "senior", 
              "serious", "several", "she", "should", "show", "side", "simulation", 
              "since", "sincere", "six", "sixty", "sketch", "so", "software", 
              "solicitation", "some", "somehow", "someone", "something", 
              "sometime", "sometimes", "somewhere", "species", "stakeholder", 
              "stakeholders","statutory","stem", "still", "student", "students", "studies", 
              "study", "submission", "submit", "submitted", "such", "summary", 
              "supplementary", "support", "synergy", "system", "systems", "take", 
              "teaching", "ten", "tenure", "tenure-track", "than", "that", "the", 
              "their", "them", "themselves", "then", "thence", "there", 
              "thereafter", "thereby", "therefore", "therein", "thereupon", 
              "these", "they", "thick", "thin", "third", "this", "those", 
              "though", "three", "through", "throughout", "thru", "thus", 
              "tissue", "tissues", "to", "together", "too", "top", "toward", 
              "towards", "trailblazer", "transformation", "travel", "twelve", 
              "twenty", "two", "un", "under", "undergraduate", "undergraduates", 
              "universities", "university", "until", "up", "upon", "us", "very", 
              "via", "visiting", "visitor", "was", "we", "well", "were", "what", 
              "whatever", "when", "whence", "whenever", "where", "whereafter", 
              "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", 
              "which", "while", "whither", "who", "whoever", "whole", "whom", 
              "whose", "why", "will", "with", "within", "without", "workshop", 
              "workshops","worthy","would", "yet", "you", "your", "yours", "yourself",
              "yourselves")

load(file = "us_states_hex.RDA")

epscor <- c("AL","AK","AR","DE","GU",'HI','ID','IO','KS','KY','LA','ME','MS',
            'MT','NE','NV','NH','NM','ND','OK','PR','RI','SC','SD','VI','VT',
            'WV','WY')
