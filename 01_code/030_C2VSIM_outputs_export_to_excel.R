  rm(list=ls())
  
  #library
  library(readxl)
  library(writexl)
  
  #directory information
  wd=list()
  wd$output='C:/sarfaraz/Project_Drought_Recovery/9_data_prepare_from_C2VSIM_output/output/'
  wd$data = 'C:/sarfaraz/Project_Drought_Recovery/9_data_prepare_from_C2VSIM_output/data/'
  setwd(wd$data)

  file_name='C2VSimFG_GW_Budget_beta2.bud'

  start_date_read= '10/31/1973_24:00'
  end_date_read='09/30/2015_24:00'

  data=readLines(file_name)
  start_lines=grep(start_date_read,data)
  end_lines=grep(end_date_read,data)
 
  
  table_list=list()
  
  for (i in 1:length(start_lines))
  {
    table_list[[i]]=read.table(file_name,skip = (start_lines[i]-1),nrows = (end_lines[i]-start_lines[i]+1))

    # colname for CVground
    if(file_name=='C2VSimFG_GW_Budget_beta2.bud'){
      
      colnames(table_list[[i]])=c('time','percolation','Beg_stor','End_stor','Deep_percolation',
                                  'gain_stream','reacharge',
                                'gain_lake','bound_in','subsidence','sub_irrig','tile_outflow','pump','out_to_rtzone',
                                'net_sub_in','discrepancy','cum_subsidance')
    }
    
  }
  
  
  if(file_name=='C2VSimFG_GW_Budget_beta2.bud'){
    write_xlsx(table_list, paste0(wd$output,"3_CVground_budget_AF_beta.xlsx"))}
