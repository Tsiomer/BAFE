#' transfrom the data form for RVI calculation
#' 
#' This function generate variables for RVI calculation
#' This fucntion operate without parameter, you just operate function
#' @param x : Excel file that selected in pop-up window
#' @export
#' @examples 
#' RVI_data.transform()



RVI_data.transform <- function(){
    filepath.1 <- file.choose()
    
    species.appear <- data.frame(readxl::read_excel(filepath.1, sheet = 3, skip = 1))
    
    if( sum(names(species.appear) == c("site_code", "structure", "site_number", "species_name", "dominance", "species_code")) != 6 ){
        stop("need to check vegetative cover data")
    }
    
    plant.area <- data.frame(readxl::read_excel(filepath.1, sheet = 2, skip = 1))
    
    if( sum(names(plant.area) == c("site_code", "species_name", "community_classification", "label", "area", "species_code")) != 6 ){
        stop("need to check vegetative area data")
    }
    
    env.data <- data.frame(readxl::read_excel(filepath.1, sheet = 4, skip = 1))
    
    if( sum(names(env.data) == c("No.", "site_code", "site", "watershed", "water_system", "subbasin",
                                 "stream", "main_tributary_etc", "survey_order", "lat_degree", "lat_minute", 
                                 "lat_second", "long_degree", "long_minute", "long_second", "lat_degree_start", 
                                 "lat_minute_start", "lat_second_start", "long_degree_start", "long_minute_start", 
                                 "long_second_start", "location", "lat_degree_end", "lat_minute_end", "lat_second_end", 
                                 "long_degree_end", "long_minute_end", "long_second_end", "degree_view", "target",
                                 "date", "weather", "organization", "investigator", "river_side_land", 
                                 "river_side_land_disturbance", "inland_left", "inland_right", "invetigate_no", 
                                 "IN_special_note","investigate_special_note")) != 41 ){
        stop("need to check environmental data")
    }
    
    cross_section <- data.frame(readxl::read_excel(filepath.1, sheet = 1, skip = 1))
    
    if( sum(names(cross_section) == c("site_code", "site_number", "site_distance", "slope_degree", "depth",
                                      "substrate", "bare_type", "community", "tree_height", "tree_canopy", 
                                      "subtree_height", "subtree_canopy", "shrub_height", "shrub_canopy", 
                                      "herb_height", "herb_canopy", "species_code")) != 17 ){
        stop("need to check cross section data")
    }
    
    site.list <- unique(env.data$site_code)
    
    plant.area.species <- plant.area[-which(plant.area$label %in% c("w","a","d","m","h","ag1")),]
    all.specieslist <- merge(species.appear,plant.area.species, by = "species_code")[,c(1,2,5)]
    dim(all.specieslist)
    species.richness <- all.specieslist[!duplicated(all.specieslist),]
    species.richness.sort <- species.richness[order(species.richness$site_code),]
    
    
    species.list <- data.frame(readxl::read_excel(filepath.1, sheet = 5, skip = 1))
    
    if( sum(names(species.list) == c("wetland_appear_frequency", "growth_type", "naturalized", "cultivar", "introduced", "Salix_Fraxinus",
                                     "Salix_Fraxinus_Alnus_Ulmus", "tolerant", "ecosystem_disturb", "rare", "endemic", "endangered", 
                                     "family", "family_Korea", "genus", "species_name", "scientific_name", "species_code", "label")) != 19 ){
        stop("need to check species list data")
    }
    
    area.matrix <- data.frame(matrix(nrow = dim(species.list)[1]+1, ncol=length(site.list)))
    rownames(area.matrix) <- c("total_area", species.list$species_code)
    colnames(area.matrix) <- site.list
    
    area.matrix
    
    for(i in 1:dim(area.matrix)[2]){
        if((colnames(area.matrix)[i] %in% unique(plant.area$site_code)) == F){
            next
        }else{
            area.matrix[1,i] <- sum(plant.area$area[plant.area$site_code %in% colnames(area.matrix)[i]])
            
            site.temp <- plant.area[plant.area$site_code %in% colnames(area.matrix)[i],]
            site.not.bio <- site.temp[site.temp$label %in% c("w","a","d","m","h","ag1"),]
            site.not.bio
            w.area <- sum(site.not.bio$area[site.not.bio$label %in% "w"])
            d.area <- sum(site.not.bio$area[site.not.bio$label %in% "d"])
            a.area <- sum(site.not.bio$area[site.not.bio$label %in% "a"])
            m.area <- sum(site.not.bio$area[site.not.bio$label %in% "m"])
            h.area <- sum(site.not.bio$area[site.not.bio$label %in% "h"])
            ag1.area <- sum(site.not.bio$area[site.not.bio$label %in% "ag1"])
            area.matrix[c(2:7),i] <- c(w.area,d.area,a.area,m.area,h.area,ag1.area)
            
            site.temp <- plant.area[plant.area$site_code %in% colnames(area.matrix)[i],]
            site.bio <- site.temp[-which(site.temp$label %in% c("w","a","d","m","h","ag1")),]
            site.bio2 <- aggregate(site.bio$area, by = list(site.bio$species_code), FUN = sum)
            for(j in 1:dim(site.bio2)[1]){
                area.matrix[which(rownames(area.matrix) == site.bio2[j,1]),i] <-  site.bio2[j,2]
            }
        }
    }
    area.matrix
    
    cross_section
    for(i in 1: dim(cross_section)[1]){
        if(i==1){
            range.survey <-  cross_section$site_distance[i]
        }else{
            if(cross_section$site_number[i] == 1){
                range.survey <- c(range.survey, cross_section$site_distance[i])
            }else{
                range.temp <- cross_section$site_distance[i]-cross_section$site_distance[i-1]
                range.survey <- c(range.survey, range.temp) 
            }
            
        }
        
    }
    cross_section$each_site_length <- range.survey
    
    radian <- cross_section$slope_degree*(pi/180)
    survey_height <- sin(radian)*cross_section$each_site_length
    
    cross_section$site_height <- survey_height 
    
    cross_section$relative_height <- NA
    
    for(j in 1:length(site.list)){
        if((site.list[j] %in% cross_section$site_code) == FALSE){
            next
        }else{
            temp.site <- cross_section[which(cross_section$site_code == site.list[j]),]
            zero.index <- which(temp.site$site_height %in% 0)
            zero.index
            packets <- list()
            for(i in 0:length(zero.index)){
                if(i == 0){
                    packets[[i+1]] <- list(temp.site[1:(zero.index[1]-1),])
                }else if(i > 0 & i < length(zero.index)){
                    packets[[i+1]] <- temp.site[c((zero.index[i]+1):(zero.index[i+1]-1)),]
                }else{
                    packets[[i+1]] <- temp.site[c((zero.index[i]+1):dim(temp.site)[1]),]
                }
            }
            packets 
            
            
            if(length(zero.index) == 1){
                height1 <- rev(cumsum(rev(data.frame(packets[[1]])[,19])))
                height.end <- cumsum(data.frame(packets[[length(packets)]])[,19])
                relative.height <- c(height1,0,height.end)
            }else if(length(zero.index) > 1){
                
                height1 <- rev(cumsum(rev(data.frame(packets[[1]])[,19])))
                height.end <- cumsum(data.frame(packets[[length(packets)]])[,19])
                
                for(k in 1:(length(zero.index)-1)){ 
                    packet.b.0 <- data.frame(packets[[k+1]])[,19] 
                    if(length(packet.b.0)%% 2 == 0){
                        right_half <- cumsum(packet.b.0[1:(length(packet.b.0)/2)])
                        left_half <- rev(cumsum(rev(packet.b.0[((length(packet.b.0)/2)+1):length(packet.b.0)])))
                        total.rehe <- c(right_half, left_half)
                    }else{
                        if(length(packet.b.0)==1){
                            total.rehe <- packet.b.0
                        }else{
                            right_half <- cumsum(packet.b.0[1:((length(packet.b.0)+1)/2)])
                            left_half <- rev(cumsum(rev(packet.b.0[(((length(packet.b.0)+1)/2)+1):length(packet.b.0)])))
                            total.rehe <- c(right_half, left_half)
                        }
                        
                    }
                    if(k==1){
                        rehe_between <- c(0,total.rehe)
                    }else{
                        rehe_between <- c(rehe_between,0,total.rehe)
                    }
                    
                }
                relative.height <- c(height1,rehe_between,0,height.end)
            }
            relative.height
            cross_section$relative_height[min(which(cross_section$site_code %in% site.list[j])):max(which(cross_section$site_code %in% site.list[j]))] <-  relative.height
            
        }
        
    }
    
    for(i in 1:length(cross_section$species_code)){
        if(i == 1){
            temp <- species.list$wetland_appear_frequency[species.list$species_code == cross_section$species_code[i]]
            if(length(temp) == 0){
                temp.2 <- "NA"
            }else{
                temp.2 <- temp
            }
        }else{
            temp <- species.list$wetland_appear_frequency[species.list$species_code == cross_section$species_code[i]]
            if(length(temp) == 0){
                temp.2 <- c(temp.2, "NA")
            }else{
                temp.2 <- c(temp.2, temp)
            }
        }
        
    }
    temp.2
    cross_section$wetland_appear_frequency <- temp.2
    
    for(i in 1:dim(cross_section)[1]){
        if(i==1){
            if(cross_section$depth[i] == 0){
                if(cross_section$wetland_appear_frequency[i] == "UPL"){
                    temp <- 1
                }else if(cross_section$wetland_appear_frequency[i] == "FACU"){
                    temp <- 2
                }else if(cross_section$wetland_appear_frequency[i] == "FAC"){
                    temp <- 3
                }else if(cross_section$wetland_appear_frequency[i] == "FACW"){
                    temp <- 4
                }else if(cross_section$wetland_appear_frequency[i] == "OBL"){
                    temp <- 5
                }else if(cross_section$wetland_appear_frequency[i] == "NA"){
                    temp <- NA
                }
            }else{
                temp <- 0
            }
        }else{
            if(cross_section$depth[i] == 0){
                if(cross_section$wetland_appear_frequency[i] == "UPL"){
                    temp <- c(temp ,1)
                }else if(cross_section$wetland_appear_frequency[i] == "FACU"){
                    temp <- c(temp ,2)
                }else if(cross_section$wetland_appear_frequency[i] == "FAC"){
                    temp <- c(temp ,3)
                }else if(cross_section$wetland_appear_frequency[i] == "FACW"){
                    temp <- c(temp ,4)
                }else if(cross_section$wetland_appear_frequency[i] == "OBL"){
                    temp <- c(temp ,5)
                }else if(cross_section$wetland_appear_frequency[i] == "NA"){
                    temp <- c(temp ,NA)
                }
            }else{
                temp <- c(temp ,0)
            }
        }
    }
    cross_section$wetland_appear_frequency_score <- temp
    cross_section 
    
    
    
    for(i in 1:length(cross_section$species_code)){
        if(i == 1){
            temp <- species.list$introduced[species.list$species_code == cross_section$species_code[i]]
            if(length(temp) == 0){
                temp.2 <- NA
            }else{
                temp.2 <- temp
            }
        }else{
            temp <- species.list$introduced[species.list$species_code == cross_section$species_code[i]]
            if(length(temp) == 0){
                temp.2 <- c(temp.2, NA)
            }else{
                temp.2 <- c(temp.2, temp)
            }
        }
        
    }
    temp.2
    cross_section$introduced <- temp.2
    cross_section
    
    for(i in 1:dim(cross_section)[1]){
        area.type <- cross_section$bare_type[i]
        invade.YN <- cross_section$introduced[i]  
        if(area.type == 1){
            temp <- 0
        }else if(area.type == 2){
            temp <- 1
        }else if(area.type == 3){
            if(is.na(invade.YN)){
                temp <- 1
            }else{
                temp <- 0.5
            }}
        if(i == 1){
            temp2 <- temp    
        }else{
            temp2 <- c(temp2, temp)
        }
    }
    temp2
    cross_section$land_use_type_score <- temp2 
}
