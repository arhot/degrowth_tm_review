library(PRISMAstatement)

fig1 <- prisma(found = 1000,
       found_other = 1000,
       no_dupes = 1787, 
       screened =1771, 
       screen_exclusions = 597, 
       full_text = 1174,
       full_text_exclusions = 17, 
       qualitative = 979,
       quantitative = NULL,
       width = 800, height = 800, 
       labels = list(found = "Web of Science, n = 1533", found_other = "Scopus, n = 698",
                     screened = "Title and publication type and \n platform screened, n = 1771",
                     screen_exclusions = "Removed books, editorials, book reviews, \n and publications not related to \n economic degrowth based on title, n = 597",
                     full_text_exclusions = "Removed based on abstract and \n full-text review, n = 230", 
                     qualitative = "Included in data set, n = 943",
                     quantitative = "  Corpus of 943 documents and 11,008 terms"))
# 
# svg <- export_svg(fig1)
# rsvg::rsvg_pdf(charToRaw(svg), "supplements_for_submission/figure1_flowchart_for_data_management.pdf")
# 
# prisma_export(fig1, "supplements_for_submission/figure1_flowchart_for_data_management.png")

fig1 %>%
  DiagrammeRsvg::export_svg() %>%
  charToRaw() %>%
  rsvg::rsvg_svg(file = "figure1_as_svg.svg")

rsvg::rsvg_pdf("figure1_as_svg.svg", "supplements_for_submission/figure1_flowchart_for_data_management.pdf",  width = 595 , height = 842 )
