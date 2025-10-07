
# Search term for Web of Science

# https://www.webofscience.com/wos/woscc/summary/684b3348-bf92-4f22-9c70-5817df20de37-714c509e/relevance/1
# (ALL=(postgrowth)) NOT WC=(Physics Applied) 
# NOT WC=(Materials Science, Multidisciplinary) NOT WC=(Physics, Condensed Matter) NOT WC=(Optics) 
# NOT WC=(Engineering Electrical Electronic) NOT WC=(Physics Multidisciplinary) NOT WC=(Electrochemistry) 
# NOT WC=(Chemistry Multidisciplinary) NOT WC=(Crystallography)

# Repeat for degrowth, de-growth, post-growth

library(tidyverse)
library(readxl)

# output is limited to 1000 per file and R reads the csv slightly differently depending on the contents, so fix that to combine
columns_that_are_read_in_different_formats  <- c("Volume", "DOI Link", "Web of Science Record", "Issue", "Start Page", "Part Number", "End Page", "Article Number")

degrowth_results <- read_excel("data/degrowth_results_1_1000.xls") %>% select(!all_of(columns_that_are_read_in_different_formats)) %>% 
  bind_rows(read_excel("data/degrowth_results_1001_end.xls") %>% select(!all_of(columns_that_are_read_in_different_formats))) %>% mutate(Search_term = "Degrowth")
de_growth_all <- read_excel("data/de_growth_all.xls") %>% select(!all_of(columns_that_are_read_in_different_formats)) %>% mutate(Search_term = "De-growth")


postgrowth_all <- read_excel("data/postgrowth_all.xls") %>% select(!all_of(columns_that_are_read_in_different_formats)) %>% mutate(Search_term = "Postgrowth")
post_growth_all <- read_excel("data/post_growth_all.xls") %>% select(!all_of(columns_that_are_read_in_different_formats)) %>% mutate(Search_term = "Post-growth")

# do papers appear in multiple searchers
duplicated_in_multiple <- bind_rows(degrowth_results, de_growth_all, postgrowth_all, post_growth_all, agrowth_all) %>% 
  group_by(DOI) %>% 
  summarize(N = n()) %>% filter(N > 1)

# full data

full_data <- degrowth_results  %>% 
  bind_rows(de_growth_all) %>% 
  bind_rows(postgrowth_all) %>%
  bind_rows(post_growth_all) %>%
  select(Search_term, "Article Title", "Source Title", "Document Type", "Author Keywords", "Abstract", "Publication Year", "DOI", "WoS Categories") %>%
  mutate(Search_term = ifelse(DOI %in% duplicated_in_multiple$DOI, "Multiple_searches", Search_term))


library(readr)
scopus <- read_csv("data_collection_files/scopus.csv")

journals_scopus <- scopus %>% group_by(`Source title`) %>% summarize(N=n()) %>% filter(N>1) %>% arrange(desc(N))


A
library(readr)
scopus_final_search_08032023 <- read_csv("data/scopus_final_search_08032023.csv") %>% select(Authors, Title, `Source title`, DOI, Publisher, Year, Abstract) %>% mutate(Scopus = "Yes")%>% filter(!is.na(DOI))

columns_that_are_read_in_different_formats  <- c("Volume", "DOI Link", "Web of Science Record", "Issue", "Start Page", "Part Number", "End Page", "Article Number")

dg_final_search_wos_08032023_first <- read_excel("data/dg_final_search_wos_08032023_first.xls") %>% select(-columns_that_are_read_in_different_formats)
dg_final_search_wos_08032023_last <- read_excel("data/dg_final_search_wos_08032023_last.xls") %>% select(-columns_that_are_read_in_different_formats)

wos_data <- dg_final_search_wos_08032023_first %>% bind_rows(dg_final_search_wos_08032023_last) %>% select(Authors, Title=`Article Title`, `Source Title`, DOI, Year=`Publication Year`, Abstract, Publisher) %>% 
  mutate(WOS = "Yes") %>% filter(!is.na(DOI))

all_search_results <- wos_data %>% full_join(scopus_final_search_08032023, by=c("DOI"))

# manual fixes
all_search_results$DOI[514] <- NA
all_search_results$DOI[1032] <- "10.19246/DOCUGEO2281-7549/202102_24"

# write the search data 
write.csv2(all_search_results, "data/scopus_and_wos_combined_26062023.csv", row.names = F)


# download full text


# Scopus by API
library(rscopus)

get_rscopus_full_text_only <- function(DOI) {
  everything <- article_retrieval(DOI, identifier=c("doi"), view = "FULL")
  text <- everything$content$`full-text-retrieval-response`$originalText
  text <- ifelse(length(text)==0, NA, text)
  return(text)
}
 
all_search_results <- all_search_results %>%
  mutate(full_text_scopus = map(DOI,get_rscopus_full_text_only))

all_search_results <- all_search_results %>%
  mutate(full_text_scopus = unlist(full_text_scopus))

saveRDS(all_search_results, "scopus_and_wos_combined_with_DOI_corrections_AND_full_textSD_26062023.RDS")

all_search_results <- all_search_results %>%
  mutate(full_text_missing = ifelse(is.na(full_text_scopus), T, F))

library(janitor)
all_search_results %>% tabyl(Publisher.x) %>% filter(n>5) %>% arrange(desc(n))
                                       
# Routledge
# generate URL and download file by url
just_rout <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)rout") | str_detect(Publisher.y, "(?i)rout")) %>%
  mutate(study_id = paste("Rout", row_number(), sep="")) %>% as.data.frame()

# "10.1080/17569370.2019.1664513"
# https://www.tandfonline.com/doi/pdf/10.1080/10371397.2016.1148555?download=true

just_rout <- just_rout %>%
  mutate(url_for_download = paste("https://www.tandfonline.com/doi/pdf/", DOI, "?download=true", sep=""))

for (file in 115:194) {
  browseURL(just_rout$url_for_download[file])
  Sys.sleep(20)
}

routs <- list.files("test_data_rout")

# MDPI
# generate links and download


just_mdpi <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)mdpi") | str_detect(Publisher.y, "(?i)mdpi")) %>%
  mutate(study_id = paste("mdpi", row_number(), sep="")) %>% as.data.frame()

# mdpi_test <- read_html("https://dx.doi.org/10.3390/su141911926")
a <- mdpi_test %>% html_node("a") %>% html_attr("href") 

mdpi_get_link_to_pdf <- function(doi) {
  link <- paste("https://dx.doi.org/", doi, sep="")
  page <- read_html(link)
  links <- page %>% html_nodes("a") %>% html_attr("href")
  pdf_link <- links[str_detect(links, "pdf\\?vers")] 
  pdf_link <- pdf_link[!is.na(pdf_link)]
  link <- ifelse(length(pdf_link==1), paste("https://www.mdpi.com", pdf_link, sep=""), NA)
  return(link)
}

just_mdpi <- just_mdpi %>%
  mutate(pdf_link = map(DOI, mdpi_get_link_to_pdf))

just_mdpi <- just_mdpi %>% mutate(pdf_link2 = unlist(pdf_link))


  
for (file in 1:95) {
  browseURL(just_mdpi$pdf_link2[file])
  Sys.sleep(5)
}

# Wiley
# generate link to PDF

just_wiley <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)wiley") | str_detect(Publisher.y, "(?i)wiley")) %>%
  mutate(study_id = paste("wiley", row_number(), sep="")) %>% as.data.frame()

# https://onlinelibrary.wiley.com/doi/10.1002/2475-8876.12216
# https://onlinelibrary.wiley.com/doi/pdfdirect/10.1002/2475-8876.12216?download=true

just_wiley <- just_wiley %>%
  mutate(url_for_download = paste("https://onlinelibrary.wiley.com/doi/pdfdirect/", DOI, "?download=true", sep=""))

for (file in 1:91) {
  browseURL(just_wiley$url_for_download[file])
  Sys.sleep(15)
}


# Sage
# generate link to PDF

just_sage <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)sage") | str_detect(Publisher.y, "(?i)sage")) %>%
  mutate(study_id = paste("sage", row_number(), sep="")) %>% as.data.frame()

# https://journals.sagepub.com/doi/pdf/10.1177/1463499620982121?download=true

just_sage <- just_sage %>%
  mutate(url_for_download = paste("https://journals.sagepub.com/doi/pdf/", DOI, "?download=true", sep=""))

for (file in 1:105) {
  browseURL(just_sage$url_for_download[file])
  Sys.sleep(15)
}


# Springer
# generate link to PDF


just_springer <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)springer") | str_detect(Publisher.y, "(?i)springer")) %>%
  mutate(study_id = paste("springer", row_number(), sep="")) %>% as.data.frame()

# https://link.springer.com/article/10.1007/s11625-015-0298-4
# https://link.springer.com/content/pdf/10.1007/s11625-015-0298-4.pdf?pdf=button

just_springer <- just_springer %>%
  mutate(url_for_download = paste("https://link.springer.com/content/pdf/", DOI, ".pdf", sep=""))

for (file in 1:105) {
  browseURL(just_springer$url_for_download[file])
  Sys.sleep(15)
}

# Nature
# generate link to PDF



just_nature <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)nature") | str_detect(Publisher.y, "(?i)nature")) %>%
  mutate(study_id = paste("nature", row_number(), sep="")) %>% as.data.frame()

# 10.1038/s41893-022-00933-5
# https://www.nature.com/articles/s41893-022-00933-5.pdf

just_nature <- just_nature %>%
  mutate(end_of_DOI = str_extract(DOI, "\\/(.*)"),
         url_for_download = paste("https://www.nature.com/articles", end_of_DOI, ".pdf", sep=""))

for (file in 1:44) {
  browseURL(just_nature$url_for_download[file])
  Sys.sleep(15)
}

# T&F
# generate link to PDF

just_taylor <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)taylor") | str_detect(Publisher.y, "(?i)taylor")) %>%
  mutate(study_id = paste("taylor", row_number(), sep="")) %>% as.data.frame()

just_taylor <- just_taylor %>% anti_join(just_rout, by=c("DOI"))

just_taylor <- just_taylor %>% filter(!is.na(Title.x))

just_taylor <- just_taylor %>%
  mutate(url_for_download = paste("https://www.tandfonline.com/doi/pdf/", DOI, "?download=true", sep=""))

for (file in 1:21) {
  browseURL(just_taylor$url_for_download[file])
  Sys.sleep(20)
}

# Cambridge
# generate link to PDF

just_camb <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)camb") | str_detect(Publisher.y, "(?i)camb")) %>%
  mutate(study_id = paste("camb", row_number(), sep="")) %>% as.data.frame()

# 10.1017/S1474746421000889
# https://www.cambridge.org/core/services/aop-cambridge-core/content/view/F848F6A319871DB97C900FFC9F2FCFE0/S0814062622000337a.pdf/education-through-smoke-and-ash-thinking-without-method-and-the-argument-for-a-post-growth-education.pdf


cup_test <- read_html("https://dx.doi.org/10.1017/S1474746421000889")
a <- cup_test %>% html_nodes("a") %>% html_attr("href") %>% str_subset("pdf") %>% .[1]

cup_get_link_to_pdf <- function(doi) {
  link <- paste("https://dx.doi.org/", doi, sep="")
  page <- read_html(link)
  link <- page %>% html_nodes("a") %>% html_attr("href") %>% str_subset("pdf") %>% .[1]
  link <- paste("https://www.cambridge.org", link, sep="")
  return(link)
}

just_camb <- just_camb %>%
  mutate(pdf_link = map(DOI, cup_get_link_to_pdf))

just_camb <- just_camb %>% mutate(pdf_link = unlist(pdf_link))


for (file in 1:28) {
  browseURL(just_camb$pdf_link[file])
  Sys.sleep(5)
}

# Emerald
# generate link to PDF

just_emerald <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)emerald") | str_detect(Publisher.y, "(?i)emerald")) %>%
  mutate(study_id = paste("emerald", row_number(), sep="")) %>% as.data.frame()

# 10.1038/s41893-022-00933-5
# https://www.emerald.com/insight/content/doi/10.1108/IJSSP-06-2020-0251/full/pdf
# https://www.emerald.com/insight/content/doi/10.1108/IJSSP-06-2020-0251/full/pdf?title=understanding-organisations-for-a-post-growth-era-contributions-from-an-epistemic-analysis

just_emerald <- just_emerald %>%
  mutate(url_for_download = paste("https://www.emerald.com/insight/content/doi/", DOI, "/full/pdf", sep=""))

for (file in 1:17) {
  browseURL(just_emerald$url_for_download[file])
  Sys.sleep(15)
}
# Frontiers


just_front <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)front") | str_detect(Publisher.y, "(?i)front")) %>%
  mutate(study_id = paste("front", row_number(), sep="")) %>% as.data.frame()

# 10.3389/frsc.2020.00012
# https://www.frontiersin.org/articles/10.3389/frsc.2020.00012/pdf

just_front <- just_front %>%
  mutate(url_for_download = paste("https://www.frontiersin.org/articles/", DOI, "/pdf", sep=""))

for (file in 1:1) {
  browseURL(just_front$url_for_download[file])
  Sys.sleep(15)
}
# horse
# Download manually

# Arizona
# generate link to PDF 


just_arizona <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)arizona") | str_detect(Publisher.y, "(?i)arizona")) %>%
  mutate(study_id = paste("arizona", row_number(), sep="")) %>% as.data.frame()

# 10.2458/v27i1.23502
# https://journals.librarypublishing.arizona.edu/jpe/article/2231/galley/2440/download
ari_test <- read_html("https://dx.doi.org/10.2458/v27i1.23502")
a <- ari_test %>% html_nodes("a") %>% html_attr("href") %>% str_subset("galley") %>% .[1]

ari_get_link_to_pdf <- function(doi) {
  link <- paste("https://dx.doi.org/", doi, sep="")
  page <- read_html(link)
  link <- page %>% html_nodes("a") %>% html_attr("href") %>% str_subset("galley") %>% .[2]
  link <- paste("https://journals.librarypublishing.arizona.edu", link, sep="")
  return(link)
}

just_arizona <- just_arizona %>%
  mutate(pdf_link = map(DOI, ari_get_link_to_pdf))

just_arizona <- just_arizona %>% mutate(pdf_link = unlist(pdf_link))

for (file in 1:18) {
  browseURL(just_arizona$pdf_link[file])
  Sys.sleep(15)
}

# Save those saved automatically, attempt download of others manually

all_search_results %>% anti_join(just_mdpi, by="DOI") %>% 
  anti_join(just_nature, by="DOI") %>%
  anti_join(just_rout, by="DOI") %>%
  anti_join(just_sage, by="DOI") %>%
  anti_join(just_springer, by="DOI") %>%
  anti_join(just_taylor, by="DOI") %>% 
  anti_join(just_wiley, by="DOI") %>% 
  filter(full_text_missing==T) -> puuttuu #%>%
  #                                tabyl(Publisher.x) %>% filter(n>5) %>% arrange(desc(n))





# Journals that are dropped based on using search terms in other meanings:
# Raman spectroscopy
# Surface and interface analysis
# Molecular Microbiology
# AICHE
# Eur. J. Inorg. Chem
# strain
# Limnol. Oceanogr
# Anatomical record
# Aging cell
# am. j. anat
# Int. J. Appl. Ceram. Technol
# experimental dermatology
# J. Am. Ceram. Soc
# DEVELOPMENTAL DYNAMICS
# microscopy
# crystallography
# Clinical Investigations
# Journal of Electroceramics
# Mar Biol
# Russian Journal of Inorganic Chemistry
# World J Microbiol Biotechnol
# Applied Nanoscience
# Appl Microbiol Biotechnol
# Diabetologia
# Contrib Mineral Petrol
# hydrobiologia
# Oecologia

