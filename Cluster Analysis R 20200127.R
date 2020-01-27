library(tidyverse)        #for data management, manipulation, and visualization
library(cluster)          #for PAM and Agnes clustering
library(NbClust)          #for cluster count horserace
library(janitor)          #for easy frequency tables with tabyl()
library(factoextra)
#### Data Preparation
# variables
paper_vars <- c('polity_squared', 
                'PopYouthBulgeBy15_new', 
                'gdppc_ln', 
                #'border_conflict', 
                #Using nAC to have continuous representation of border_conflict
                'nAC', 
                #Using sum_PDIS_EDIS to have continuous representation of discrimination
                'sum_PDIS_EDIS', 
                #'discrimination',
                'ResidLifExpectGdppcLog_re_I')
# read raw data
df <- readxl::read_xlsx('Master Minerva v.19 (inductive subset).xlsx') %>% 
  #converts onset and regional variables from character to factor variables
  mutate_at(vars(region:ongoing_conflict), funs(as.factor(.))) %>% 
  filter(full_onset == 1) %>% 
  mutate(country_year = paste(country, year, sep = ', ')) %>% 
  select(country, year, country_year, full_onset, rc_onset, cw_onset, paper_vars) %>% 
  filter(complete.cases(.)) %>% 
  rowid_to_column(.) %>% 
  mutate_at(vars(polity_squared:ResidLifExpectGdppcLog_re_I), funs(scale = as.numeric(scale(.)))) %>% 
  column_to_rownames('country_year')

#### Cluster Analysis
# parameters
cluster_data<-df
set.seed(1234)
dt_filter = c("full_onset","rc_onset","cw_onset")
dist_metric_input = c("euclidean","manhattan")
cluster_method_input = c("agnes","pam")
cluster_paramter = list()
cluster_paramter[["agnes"]] = list(diss = TRUE, method = 'ward', keep.diss = TRUE)
cluster_paramter[["pam"]] = list(diss = TRUE, k=4)
# results
cluster_comparison <- function(cluster_data,set.seed,dt_filter,dist_metric_input,cluster_paramter){
  test_results = list()
  for (column_name in dt_filter){
    test_results[[column_name]] = list()
    for (dist_metric in dist_metric_input) {
      test_results[[column_name]][[dist_metric]] = list()
      for (cluster_method in cluster_method_input){
        test_results[[column_name]][[dist_metric]][[cluster_method]] <-  
          ### Core Coding Part 
          cluster_data %>% 
          filter_(paste(column_name, "==", 1)) %>%
          select(polity_squared_scale:ResidLifExpectGdppcLog_re_I_scale) %>% 
          nest(data = c(polity_squared_scale, PopYouthBulgeBy15_new_scale, 
                        gdppc_ln_scale, nAC_scale, sum_PDIS_EDIS_scale, ResidLifExpectGdppcLog_re_I_scale)) %>% 
          mutate(dist_matrix = map2(data,
                                    dist_metric,
                                    .f= ~daisy(.x, stand = FALSE))) %>% 
          mutate(cluster_model = map(dist_matrix,
                                     .f= ~do.call(cluster_method, append(list(.x),cluster_paramter[[cluster_method]])))) %>%
          mutate(cluster_result = ifelse( cluster_method=="agnes", 
                                          map(cluster_model,.f=~cutree(as.hclust(.x), k = 4)),
                                          map(cluster_model,"clustering"))) %>%
          mutate(cluster_result = map(cluster_result, .f= ~data.frame(Row_ID=c(1:length(.x)),Group=.x)))
        ### Core Coding Part
      }
    }
  }
  return(test_results)
}

final_result = cluster_comparison(cluster_data,set.seed,dt_filter,dist_metric_input,cluster_paramter)