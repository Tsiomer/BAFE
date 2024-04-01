#' BMI calculation
#'
#' This function generate BMI and rank of each site
#' This fucntion operate without parameter, you just operate function
#' @param x : Excel file that selected in pop-up window
#' @export
#' @examples
#' BMI_only()

BMI_only <- function(){
    filepath.1 <- file.choose()

    raw.abun <- data.frame(readxl::read_excel(filepath.1, sheet = 2, skip = 1))[,c(4,6:10)]
    if( sum(names(raw.abun) == c("site_code", "species_name", "inds","sampling_area", "inds_density", "Species_Code")) != 6 ){
        stop("need to check species investigate data")
    }
    if((T %in% is.na(raw.abun$inds))==TRUE){
        raw.abun.1 <- raw.abun[-which(is.na(raw.abun$inds)),]
    }else{
        raw.abun.1 <- raw.abun
    }


    species_list <- data.frame(readxl::read_excel(filepath.1, sheet = 3))[,-c(4:7)]
    if( sum(names(species_list) == c("species_name", "Species_Code", "scientific_name","saprobic_value", "indicator_weight_value", "endangered_species_1","endangered_species_2","Korea_endemic")) != 8 ){
        stop("need to check species list")
    }

    site.list = unique(raw.abun.1$site_code)


    C.code.list <- c("M00811", "M00812", "M00813", "M00814", "M00815", "M00816")
    BMI_matrix <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    colnames(BMI_matrix) <- c("site_code","BMI")

    for(i in 1: length(site.list)){
        subset.0 <- subset(raw.abun.1, site_code == site.list[i])
        subset.0.1 <- aggregate(subset.0$inds, by = list(subset.0$Species_Code), FUN = sum)

        if(T %in% c(subset.0.1$Group.1 %in% C.code.list)){
            location.T <- which(c(subset.0.1$Group.1 %in% C.code.list) == T)
            for(j in 1:length(location.T)){
                if(subset.0.1$Group.1[location.T[j]] == "M00811"){
                    subset.0.1$Group.1[location.T[j]] <- "M00699"
                }else{
                    subset.0.1$Group.1[location.T[j]] <- "M00698"
                }
            }

        }
        subset.0.1.1 <- aggregate(subset.0.1$x, by = list(subset.0.1$Group.1), FUN = sum)

        subset.0.2 <- cbind(rep(site.list[i],dim(subset.0.1.1)[1]), subset.0.1.1)
        colnames(subset.0.2) <- c("site_code","Species_Code", "inds")

        subset.1 <- merge(subset.0.2, species_list, by = "Species_Code")

        subset.1.5 <- subset.1[order(subset.1$inds, decreasing = T),]
        subset.1.5$Rank <- seq(from = 1, to = dim(subset.1.5)[1])



        subset.1.5$total_species <- rep(dim(subset.1.5)[1], dim(subset.1.5)[1])
        subset.1.5$appearence <- subset.1.5$Rank/subset.1.5$total_species
        subset.1.5$appearence_score <- c(5,4,3,2,1)[cut(subset.1.5$appearence, breaks = c(-1,0.2,0.4,0.6,0.8,1))]
        if(T %in% is.na(subset.1.5$saprobic_value)){
            subset.2 <- subset.1.5
            subset.2$saprobic_value[is.na(subset.2$saprobic_value)] <- 0
        }else{
            subset.2 <- subset.1.5
        }
        if(T %in% is.na(subset.2$indicator_weight_value)){
            subset.2$indicator_weight_value[is.na(subset.2$indicator_weight_value)] <- 0
        }


        BMI_matrix[i,1] <- site.list[i]
        BMI_matrix[i,2] <- (4-(sum(subset.2$appearence_score*subset.2$saprobic_value*subset.2$indicator_weight_value)/sum(subset.2$appearence_score*subset.2$indicator_weight_value)))*25
    }
    BMI_matrix

    env.data <- data.frame(readxl::read_excel(filepath.1, sheet = 1, skip = 1))

    if( sum(names(env.data) == c("No.", "site", "watershed", "water_system", "subbasin", "stream", "main_tributary_etc", "survey_order", "site_code","date",
                                 "weather","lat_degree","lat_minute","lat_second","long_degree",
                                 "long_minute","long_second","organization","investigator",	"writer","investigate_special_note","invetigate_no",
                                 "IN_special_note","investigate_tools","tool_width","tool_length","tool_number",	"temperature",	"water_temperature",
                                 "basin_forest","basin_pasture","basin_town","basin_restraunt","basin_agriculture","basin_industrialarea",
                                 "basin_hometown","basin_etc","basin_etc_detail","IP_domesticsewage","IP_agriculturaleffuluent","IP_livestockwastewater",
                                 "IP_industrialeffluent","IP_etc","IP_etc_detail","vegetation_tree","vegetation_shrub",	"vegetation_grass",
                                 "vegetation_sum","canopy",	"FP_natural","FP_agriculture","FP_road","FP_parkinglot","FP_trail",	"FP_etc",
                                 "bank_l_natural","bank_l_stonewall","bank_l_gabion","bank_l_concrete","bank_l_vertical","bank_r_natural",
                                 "bank_r_stonewall","bank_r_gabion","bank_r_concrete","bank_r_vertical","stream_type","river_width","water_width",
                                 "mean_depth","mean_velocity","diff_depth_1_min","diff_depth_1_max","diff_depth_2_min","diff_depth_2_max","diff_depth_3_min",
                                 "diff_depth_3_max","habitat_ripple","habitat_run","habitat_pool","habitat_sum","SC_silt","SC_finesand",
                                 "SC_coarsesand","SC_gravel","SC_pebble","SC_cobble","SC_sum","transparency","odor")) != 89 ){
        stop(" need to check environmental data")
    }
    raw.env <- env.data[,c(9,22)]

    colnames(raw.env) <- c("site_code","invetigate_no")
    raw.env$invetigate_no[is.na(raw.env$invetigate_no)] <- "-"
    BMI_matrix$BMI_special_note <-BMI_matrix$BMI
    BMI_matrix$BMI_special_note[BMI_matrix$site_code %in% raw.env$site_code[!(raw.env$invetigate_no == "-")]] <- "-"

    BMI_matrix.2 <- merge(BMI_matrix, raw.env, by = "site_code")

    for(i in 1:dim(BMI_matrix.2)[1]){
        if(BMI_matrix.2$BMI_special_note[i] == "-" |is.na(BMI_matrix.2$BMI_special_note[i])){
            BMI_matrix.2$rank[i] <- "-"
        }else{
            BMI_matrix.2$rank[i] <- c("E","D","C","B","A")[cut(as.numeric(BMI_matrix.2$BMI_special_note[i]), breaks = c(0,35,50,65,80,101),right = F)]
        }
    }
    write.csv(BMI_matrix.2, file = "BMI_matrix.csv", fileEncoding = "EUC-KR")
}
