#' TDI calculation
#' 
#' This function generate BMI and rank of each site
#' This fucntion operate without parameter, you just operate function 
#' @param x : Excel file that selected in pop-up window
#' @export
#' @examples 
#' TDI_only()

TDI_only <- function(){
    filepath.1 <- file.choose()
    
    raw.abun <- data.frame(readxl::read_excel(filepath.1, sheet = 2, skip = 1))[,c(1,4,28,29,30)]
    
    if( sum(names(raw.abun) == c("site_code", "cell_density", "species_code",
                                 "scientific_name", "cell_sum")) != 5 ){
        stop("need to check identification data")
    }
    site.list <- unique(raw.abun[,1]) 
    
    
    for(i in 1:length(site.list)){
        temp.DB <- raw.abun[raw.abun$site_code == site.list[i], ]
        
        temp.DB$relative_abundance <- temp.DB[,5]/sum(temp.DB[,5])
        temp.DB$calculated_abundance <- temp.DB[,2]*temp.DB$relative_abundance
        
        if(i == 1){
            temp.DB2 <- temp.DB
        }else{
            temp.DB2 <- rbind(temp.DB2, temp.DB)
        }
    }
    add.total_abundance <- temp.DB2
    
    specied.data <- data.frame(readxl::read_excel(filepath.1, sheet = 6, skip = 1))
    
    specied.data2 <- specied.data[c("species_code","KELLYS", "KELLYV")]
    
    all_data_for_TDI <- merge(add.total_abundance, specied.data2, by="species_code")
    if( sum(is.na(all_data_for_TDI$KELLYS)) > 0 ){
        print( paste("NA in KELLYS:", paste0(all_data_for_TDI$species_code[is.na(all_data_for_TDI$KELLYS)], collapse = ", ")))
    }
    if( sum(is.na(all_data_for_TDI$KELLYV)) > 0 ){
        print( paste("NA in KELLYV:", paste0(all_data_for_TDI$species_code[is.na(all_data_for_TDI$KELLYV)], collapse = ", ")))
    }
    
    TDI_matrix <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    
    names(TDI_matrix) <- c("site_code", "TDI")
    
    for(i in 1:length(site.list)){
        subset.0 <- subset(all_data_for_TDI, site_code == site.list[i])
        subset.0$KELLYS[is.na(subset.0$KELLYS)] <- 0 
        subset.0$KELLYV[is.na(subset.0$KELLYV)] <- 0 
        TDI <- round(((sum(subset.0$calculated_abundance * subset.0$KELLYS * subset.0$KELLYV)/
                           sum(subset.0$calculated_abundance * subset.0$KELLYV))*25)-25,digits = 1)
        
        TDI_matrix[i,1] <- site.list[i]
        TDI_matrix[i,2] <- TDI
    }
    TDI_matrix
    
    env.raw <- data.frame(readxl::read_excel(filepath.1, sheet = 3, skip = 1))
    
    
    if( sum(names(env.raw) == c("No.","site_code","site","watershed","water_system","subbasin",
                                "stream","main_tributary_etc","survey_order","lat_degree","lat_minute","lat_second",
                                "long_degree","long_minute","long_second","date","weather","organization","investigator",
                                "stream_type","h_sand_silt_mud_dirt","h_gravel","h_bedrock","h_smallwood","h_bigwood",
                                "h_root","h_sum","flow_ripple","flow_run","flow_pool","flow_sum","canopy","cover",
                                "investigate_tools","sampling_method","s_sand_silt_mud_dirt","s_gravel","s_bedrock",
                                "s_smallwood","s_bigwood","s_root","s_etc","s_etc_detail","s_sum","water_color","odor",
                                "v_herb","v_shrub","v_sum","lu_usedarea","lu_forest","lu_agricultureland","lu_industrialarea",
                                "lu_dredge","lu_livestock","lu_sum","substrate_moldedness","barrage_location",
                                "barrage_distance","barrage_effect","water_temperature","DO","pH","Conductivity",
                                "Turbidity","invetigate_no","IN_special_note","investigate_special_note")) != 68 ){
        stop("need to check environmental data")
    }
    survey_or_not_data <- data.frame(env.raw$site_code,env.raw$invetigate_no)
    
    colnames(survey_or_not_data) <- c("site_code","invetigate_no")
    
    TDI_matrix.converion <- TDI_matrix
    
    TDI_matrix.converion$TDI <- 100 - TDI_matrix$TDI 
    
    colnames(TDI_matrix.converion)[1] <- "site_code" 
    
    TDI_matrix.converion$TDI_special_issue <- TDI_matrix.converion$TDI
    
    TDI_matrix.converion$TDI_special_issue[TDI_matrix.converion$site_code %in% survey_or_not_data$site_code[which(survey_or_not_data$invetigate_no != "-")]] <- "-"
    
    merge(TDI_matrix.converion, survey_or_not_data, by = "site_code")
    
    
    for(i in 1:dim(TDI_matrix.converion)[1]){
        if(TDI_matrix.converion$TDI_special_issue[i] == "-" |is.na(TDI_matrix.converion$TDI_special_issue[i])){
            TDI_matrix.converion$rank[i] <- "-"
        }else{
            TDI_matrix.converion$rank[i] <- c("E","D","C","B","A")[cut(as.numeric(TDI_matrix.converion$TDI_special_issue[i]), breaks = c(0,30,50,70,90,101),right = F)]
        }
    }
    write.csv(TDI_matrix.converion, file = "TDI_matrix.csv", fileEncoding = "EUC-KR")
}