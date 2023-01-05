Sys.setlocale(category = "LC_ALL", locale = "us")

library(devtools)
library(SqlRender)
library(DatabaseConnector)
library(dplyr)
library(sunburstR)

getwd()
folder =
setwd(folder)

user <- 
pw <- 
server <- 
dbms <-

connectionDetails <- createConnectionDetails(dbms=dbms,
                                             server=server,
                                             user=user,
                                             password=pw)

conn <- connect(connectionDetails)
#disconnect(conn)

####################################
#       Parameters Setting         #
####################################
# @drug_list - Bevacizumab / Ranibizumab / Aflibercept / Dexamethasone / Triamcinolone

drug_beva <- c('1397141', '42921539', '42920455') #-- Bevacizumab
drug_rani <- c('19080982', '42923236') #-- Ranibizumab
drug_afli <- c('40244266', '42923302') #-- Aflibercept
drug_dexa <- c('40160931') #-- Dexamethasone
drug_triam <- c('41333739', '42941640') #-- Triamcinolone

drug_all <- c(drug_beva, drug_rani, drug_afli, drug_dexa, drug_triam) 

####################################
#        PATIENT COHORT            #
####################################

##RVO COHORT : 1_ATLAS_cohort_generation_in_json_RVO
inputFile <- "./patient cohort.sql"
parameterizedSql <- readSql(inputFile)

renderedSql <- render(parameterizedSql
                      , cdmSchema = cdmSchema
                      , resultsSchema = resultsSchema
                      , drug_all = drug_all
                      , cohort_id = ''
)

executeSql(conn, renderedSql) ###에러가 발생하면 한 번 더 실행하면 됩니다.

treatment_pathway <- querySql(conn, "SELECT * FROM #patientCohort;")

#------------------------------------------------------------------------------------------#

#treatment_pathway$DRUG_EXPOSURE_START_DATE <- as.Date(treatment_pathway$DRUG_EXPOSURE_START_DATE)
#treatment_pathway$DRUG_EXPOSURE_END_DATE <- as.Date(treatment_pathway$DRUG_EXPOSURE_END_DATE)

treatment_pathway <- treatment_pathway %>% group_by(PERSON_ID,DRUG_CONCEPT_ID) %>% summarise(MIN_DATE=min(DRUG_EXPOSURE_START_DATE))
treatment_pathway <- treatment_pathway[order(treatment_pathway$PERSON_ID,treatment_pathway$MIN_DATE),]

# PERSONAL_DRUG_ASD_NUMBER 행을 만드는 코드
treatment_pathway <- treatment_pathway %>% group_by(PERSON_ID) %>% arrange(MIN_DATE) %>% mutate(PERSONAL_DRUG_ASD_NUMBER = row_number())

##### DRUG_CLASS행 생성
treatment_pathway<-treatment_pathway %>% mutate(DRUG_CLASS = ifelse(DRUG_CONCEPT_ID %in% drug_beva, 'Bevacizumab', ifelse(DRUG_CONCEPT_ID %in% drug_rani, 'Ranibizumab', ifelse(DRUG_CONCEPT_ID %in% drug_afli, 'Aflibercept', ifelse(DRUG_CONCEPT_ID %in% drug_dexa, 'Dexamethasone', ifelse(DRUG_CONCEPT_ID %in% drug_triam, 'Triamcinolone', 'Others'))))))

TxPathwayDf <- data.frame(treatment_pathway %>% group_by(PERSON_ID) %>% mutate(TxPathway = paste(DRUG_CLASS, collapse = '-')))
sunburstDf <- TxPathwayDf %>% select('PERSON_ID', 'TxPathway') %>% distinct()

sunburstTxPathway<-c()
for (j in sunburstDf$TxPathway){
  txPathway<-c()
  treatment<-unlist(strsplit(j, '-'))
  
  for (i in 1:length(treatment)){
    txPathway <- c(txPathway, treatment[i])
  }
  sunburstTxPathway<-c(sunburstTxPathway, paste(txPathway, collapse = '-'))
}
sunburstTxPathway <- data.frame(table(sunburstTxPathway))

sum(sunburstTxPathway$Freq) ##RVO COHORT n수와 일치하는지 확인

# sunburst plot
legend_items <- c('Bevacizumab', 'Ranibizumab', 'Aflibercept', 'Dexamethasone', 'Triamcinolone')
cols <- c('Bevacizumab', 'Ranibizumab', 'Aflibercept', 'Dexamethasone', 'Triamcinolone')
cols[legend_items=='Bevacizumab'] = "#984ea3"
cols[legend_items=='Ranibizumab'] = "#ff7f00"
cols[legend_items=='Aflibercept'] = "#377eb8"
cols[legend_items=='Dexamethasone'] = "#cccccc"
cols[legend_items=='Triamcinolone'] = "#8dd3c7"


maintitle = "RVO Tx pathway" # Plot main title
subtitle = paste0(sum(sunburstTxPathway$Freq),' patients; ',length(sunburstTxPathway$sunburstTxPathway),' unique paths') 
plottitle = "All_RVO" # plot file name

rm(sb)

sb = sunburstTxPathway %>% sunburst(colors=list(range=cols, domain=legend_items),
                                    legendOrder = c('Bevacizumab', 'Ranibizumab', 'Aflibercept', 'Dexamethasone', 'Triamcinolone'),
                                    valueField = "size",
                                    percent = TRUE,
                                    count = TRUE,
                                    legend = list(w=150),
                                    width = 700,
                                    height = 700,
                                    withD3=TRUE) 


sb = htmlwidgets::onRender(sb,
                           "
	  function(el, x) {
	  d3.selectAll('.sunburst-legend text').attr('font-size', '15px');
    d3.select(el).select('.sunburst-togglelegend').property('checked',true);
    d3.select(el).select('.sunburst-togglelegend').on('click')();
    d3.select(el).select('.sunburst-togglelegend').remove();
	  }
	  "
)
sb = htmlwidgets::prependContent(sb, htmltools::h1(maintitle), htmltools::h2(subtitle))
sb = htmlwidgets::saveWidget(widget=sb, paste0(plottitle,".html"), selfcontained = FALSE, libdir = getwd())





#------------------------------------------------------------------------------------------#
# csv file
firstyearDf<-treatment_pathway %>% filter(PERSONAL_DRUG_ASD_NUMBER==1) %>% select('PERSON_ID', 'MIN_DATE')
FUN <- function(x){
  substr(as.character(x), 1, 4)
}
firstyearDf['MIN_DATE'] <- lapply(firstyearDf['MIN_DATE'], FUN)
csvfiledf<-merge(firstyearDf, sunburstDf, by='PERSON_ID')
csvfiledf<-data.frame(csvfiledf %>% group_by(MIN_DATE, TxPathway) %>% summarise(count = n()))
write.csv(csvfiledf, file='./count_RVO.csv')