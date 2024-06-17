#' RVI calculation with DB form that we used
#'
#' This function generate csv file that DB form
#' This fucntion operate without parameter, you just operate function
#' @param x : Excel file that selected in pop-up window
#' @export
#' @examples
#' RVI_all()

RVI_all <- function(){
    x <- RVI_data.transform()
    cross_section <- x$cross_section
    area.matrix <- x$area.matrix
    species.list <- x$species.list
    env.data <- x$env.data
    species.richness.sort <- x$species.richness.sort

    site.list <- x$site.list
    species.appear <- x$species.appear

    RVI.matrix <- data.frame(matrix(nrow = dim(area.matrix)[2], ncol = 7))

    colnames(RVI.matrix) <- c("site_code","HAA","EA","WTD","SalFraA","ToSC","BTI")

    for(i in 1:dim(area.matrix)[2]){
        if((FALSE %in% is.na(area.matrix[,i])) == FALSE){
            RVI.matrix$site_code[i] <- names(area.matrix)[i]
            next
        }else{
            aa <- area.matrix[-which(is.na(area.matrix[,i])),][i]
            aa <- cbind(rownames(aa), aa)
            colnames(aa) <- c("species_code","area")
            veg.area <- sum(aa$area[8:length(aa$area)])

            bb <- species.list[which(species.list$species_code %in% rownames(aa)),]
            cc <- merge(bb, aa, by = "species_code")

            RVI.matrix$site_code[i] <- names(area.matrix)[i]

            RVI.matrix$HAA[i] <- sum(cc$area[cc$growth_type %in% c("HerbAn","ClimbAn")])/veg.area*100

            RVI.matrix$EA[i] <- sum(cc$area[which(cc$introduced == "O")])/veg.area*100

            RVI.matrix$WTD[i] <- sum((aa[-c(1:7),2]/veg.area)^2)

            RVI.matrix$SalFraA[i] <- sum(cc$area[which(cc$Salix_Fraxinus == "O")])/veg.area*100

            species.richness.1 <- species.richness.sort[which(species.richness.sort$site_code == unique(species.richness.sort$site_code)[i]),]
            species.richness.data <- species.list[which(species.list$species_code %in% species.richness.1$species_code),]
            RVI.matrix$ToSC[i] <- length(which(species.richness.data$tolerant  == "O"))/dim(species.richness.data)[1]*100

            subset.1 <- subset(cross_section, site_code == site.list[i])
            totallength <- max(subset.1$site_distance)
            RVI.matrix$BTI[i] <- sum(subset.1$each_site_length*subset.1$wetland_appear_frequency_score*subset.1$land_use_type_score, na.rm = T)/totallength

            RVI.matrix[i,-1] <- round(RVI.matrix[i,-1],1)
        }

    }
    RVI.matrix

    HAA.score <- c(0,5,3,1,0)[cut(RVI.matrix$HAA, breaks = c(-1,5,15,36,80,99999))]
    EA.score <- c(5,3,1,0)[cut(RVI.matrix$EA, breaks = c(-1,4,16,50,99999))]
    WTD.score <- c(0,5,3,1,0)[cut(RVI.matrix$WTD, breaks = c(-1,0,0.4,0.52,0.65,99999))]
    SalFraA.score <- c(0,1,3,5)[cut(RVI.matrix$SalFraA, breaks = c(-99999,0,10,30,99999))]
    ToSC.score <- c(5,3,1,0)[cut(RVI.matrix$ToSC, breaks = c(0,5,12,20,99999), right = F)]
    BTI.score <- c(0,1,3,5)[cut(RVI.matrix$BTI, breaks = c(-1,0.5,1.5,2.5,99999))]

    score.ingage <- data.frame(RVI.matrix$site_code, HAA.score, EA.score, WTD.score, SalFraA.score, ToSC.score, BTI.score)
    names(score.ingage)[1] <- "site_code"
    score.ingage$RVI <- round(rowSums(score.ingage[,c(2:7)])*(10/3),1)

    score.ingage$RVI.rank <- c("E","D","C","B","A")[cut(score.ingage$RVI, breaks = c(-1,15,30,50,65,100))]


    approach.status <- data.frame(env.data$site_code,env.data$invetigate_no)
    colnames(approach.status) <- c("site_code","invetigate_no")

    approach.status$invetigate_no[is.na(approach.status$invetigate_no)] <- "-"


    score.ingage$RVI_special_issue <- score.ingage$RVI
    score.ingage$RVI_special_issue[score.ingage$site_code %in% approach.status$site_code[!(approach.status$invetigate_no == "-")]] <- "-"

    score.ingage.2 <- score.ingage

    for(i in 1:dim(score.ingage)[1]){
        if(score.ingage$RVI_special_issue[i] == "-"){
            score.ingage.2$RVI.rank[i] <- "-"
        }else{
            score.ingage.2$RVI.rank[i] <- score.ingage.2$RVI.rank[i]
        }
    }
    score.ingage.2[,11] <- score.ingage.2[,9]
    score.ingage.2 <- score.ingage.2[,-9]
    names(score.ingage.2)[10] <- "RVI.rank"



    latitude <- paste(env.data$lat_degree, env.data$lat_minute, env.data$lat_second)
    longitude <- paste(env.data$long_degree, env.data$long_minute, env.data$long_second)

    latitude.start <- paste(env.data$lat_degree_start, env.data$lat_minute_start, env.data$lat_second_start)
    longitude.start <- paste(env.data$long_degree_start, env.data$long_minute_start, env.data$long_second_start)

    latitude.end <- paste(env.data$lat_degree_end,env.data$lat_minute_end, env.data$lat_second_end)
    longitude.end <- paste(env.data$long_degree_end,env.data$long_minute_end,env.data$long_second_end)

    env.data.tf <- env.data
    colnames(env.data.tf)
    env.data.tf[,10] <- latitude
    env.data.tf[,11] <- longitude
    env.data.tf[,12] <- latitude.start
    env.data.tf[,13] <- longitude.start
    env.data.tf[,14] <- latitude.end
    env.data.tf[,15] <- longitude.end
    env.data.tf.lalo <- env.data.tf[,-c(16:21,23:28)]

    colnames(env.data.tf.lalo)[10:15]<- c("latitude","longitude","lat_start","long_start","lat_end","long_end")

    dominant_data <- data.frame(matrix(nrow = dim(area.matrix)[2], ncol = 21))

    colnames(dominant_data) <- c("site_code",
                                 "Salix_ratio",
                                 "Gramineae_ratio","Cyperaceae_ratio",
                                 "OBL_ratio",
                                 "FACW_ratio",
                                 "OBL_FACW_ratio",
                                 "subtree_tree_ratio",
                                 "naturalized_ratio",
                                 "ecosystem_disturb_ratio",
                                 "Salix_Fraxinus_Alnus_Ulmus_ratio",
                                 "Salix_area_ratio",
                                 "Gramineae_area_ratio",
                                 "Cyperaceae_area_ratio",
                                 "OBL_area_ratio",
                                 "FACW_area_ratio",
                                 "OBL_FACW_area_ratio",
                                 "subtree_tree_area_ratio",
                                 "naturalized_area_ratio",
                                 "ecosystem_disturb_area_ratio",
                                 "Salix_Fraxinus_Alnus_Ulmus_area_ratio")


    for(i in 1:dim(area.matrix)[2]){
        aa <- area.matrix[-which(is.na(area.matrix[,i])),][i]
        aa <- cbind(rownames(aa), aa)
        colnames(aa) <- c("species_code","area")
        veg.area <- sum(aa$area[8:length(aa$area)])
        bb <- species.list[which(species.list$species_code %in% rownames(aa)),]
        cc <- merge(bb, aa, by = "species_code")

        dominant_data$site_code[i] <- colnames(area.matrix)[i]

        dominant_data$Salix_ratio[i] <- length(which(cc$genus %in% "Salix"))/(dim(aa)[1]-7)*100
        dominant_data$Salix_area_ratio[i] <- sum(cc$area[which(cc$genus %in% "Salix")])/veg.area*100

        dominant_data$Gramineae_ratio[i] <- length(which(cc$family %in% "Gramineae"))/(dim(aa)[1]-7)*100
        dominant_data$Gramineae_area_ratio[i] <- sum(cc$area[which(cc$family %in% "Gramineae")])/veg.area*100

        dominant_data$Cyperaceae_ratio[i] <- length(which(cc$family %in% "Cyperaceae"))/(dim(aa)[1]-7)*100
        dominant_data$Cyperaceae_area_ratio[i] <- sum(cc$area[which(cc$family %in% "Cyperaceae")])/veg.area*100

        dominant_data$OBL_ratio[i] <- length(which(cc$wetland_appear_frequency %in% "OBL"))/(dim(aa)[1]-7)*100
        dominant_data$OBL_area_ratio[i] <- sum(cc$area[which(cc$wetland_appear_frequency %in% "OBL")])/veg.area*100

        dominant_data$FACW_ratio[i] <- length(which(cc$wetland_appear_frequency %in% "FACW"))/(dim(aa)[1]-7)*100
        dominant_data$FACW_area_ratio[i] <- sum(cc$area[which(cc$wetland_appear_frequency %in% "FACW")])/veg.area*100

        dominant_data$OBL_FACW_ratio[i] <- length(which(cc$wetland_appear_frequency %in% c("OBL","FACW")))/(dim(aa)[1]-7)*100
        dominant_data$OBL_FACW_area_ratio[i] <- sum(cc$area[which(cc$wetland_appear_frequency %in% c("OBL","FACW"))])/veg.area*100

        dominant_data$subtree_tree_ratio[i] <- length(which(cc$growth_type %in% c("Subtree","Tree")))/(dim(aa)[1]-7)*100
        dominant_data$subtree_tree_area_ratio[i] <- sum(cc$area[which(cc$growth_type %in% c("Subtree","Tree"))])/veg.area*100

        dominant_data$naturalized_ratio[i] <- length(which(cc$naturalized == "O"))/(dim(aa)[1]-7)*100
        dominant_data$naturalized_area_ratio[i] <- sum(cc$area[which(cc$naturalized == "O")])/veg.area*100

        dominant_data$ecosystem_disturb_ratio[i] <- length(which(cc$ecosystem_disturb == "O"))/(dim(aa)[1]-7)*100
        dominant_data$ecosystem_disturb_area_ratio[i] <- sum(cc$area[which(cc$ecosystem_disturb == "O")])/veg.area*100

        dominant_data$Salix_Fraxinus_Alnus_Ulmus_ratio[i] <- length(which(cc$Salix_Fraxinus_Alnus_Ulmus == "O"))/(dim(aa)[1]-7)*100
        dominant_data$Salix_Fraxinus_Alnus_Ulmus_area_ratio[i] <- sum(cc$area[which(cc$Salix_Fraxinus_Alnus_Ulmus == "O")])/veg.area*100

        dominant_data[i,-1] <- round(dominant_data[i,-1],1)
    }

    except.plant <- data.frame(colnames(area.matrix),t(area.matrix[2:7,]))
    names(except.plant)[1:7] <- c("site_code","watercourse","barren","artificial_structure","forest_vegetation","residential_commercial","agriculture")

    only.plant <- data.frame(colnames(area.matrix),t(area.matrix[-c(1:7),]))
    names(only.plant)[1] <- "site_code"
    colnames(only.plant)[2:dim(only.plant)[2]]<-species.list$species_name[7:length(species.list$species_name)]

    whole.area.num <- data.frame(matrix(nrow=dim(area.matrix)[2],ncol = 3))
    for(i in 1:dim(area.matrix)[2]){
        if((FALSE %in% is.na(area.matrix[,i])) == FALSE){
            whole.area.num[i,1] <- names(area.matrix)[i]
            whole.area.num[i,2] <- NA
            whole.area.num[i,3] <- NA
        }else{
            aa <- area.matrix[-which(is.na(area.matrix[,i])),][i]
            whole.area.num[i,1] <- names(aa)
            whole.area.num[i,2] <- c(dim(aa)[1]-7)
            whole.area.num[i,3] <- sum(aa[,1][8:dim(aa)[1]])
        }
    }
    names(whole.area.num) <- c("site_code","total_communties","total_area")

    DF.form.vegetation1.nt <- env.data.tf.lalo[,c(1:11,19:22)] |>
        merge(except.plant, by = "site_code") |>
        merge(env.data.tf.lalo[,c(2,27:29)], by = "site_code") |>
        merge(dominant_data, by = "site_code") |>
        merge(RVI.matrix, by = "site_code") |>
        merge(score.ingage.2, by = "site_code") |>
        merge(whole.area.num, by = "site_code") |>
        merge(only.plant, by = "site_code")

    DF.form.vegetation1.nt.sort <- DF.form.vegetation1.nt[order(DF.form.vegetation1.nt$No.),]

    DF.form.vegetation1 <- data.frame(t(DF.form.vegetation1.nt.sort))

    write.csv(DF.form.vegetation1, "DB.form.vegetation_area.csv", fileEncoding = "EUC-KR")

    plant.spri <- data.frame(matrix(nrow = dim(species.list)[1], ncol = length(site.list)))
    rownames(plant.spri) <- species.list$species_code

    for(i in 1:length(site.list)){
        colnames(plant.spri)[i] <- site.list[i]
        temp.species <- species.richness.sort$species_code[species.richness.sort$site_code %in% site.list[i]]
        for(j in 1:length(temp.species)){
            temp.index <- which(rownames(plant.spri) == temp.species[j])
            plant.spri[temp.index,i] <- 1
        }
    }

    richness.data <- data.frame(matrix(nrow = dim(plant.spri)[2], ncol = 12))
    colnames(richness.data) <- c("site_code",
                                 "tolerant_species_ratio",
                                 "Salix_species_ratio",
                                 "Gramineae_species_ratio",
                                 "Cyperaceae_species_ratio",
                                 "OBL_ratio",
                                 "FACW_ratio",
                                 "naturalized_ratio",
                                 "ecosystem_disturb_ratio",
                                 "ecosystem_disturb_species",
                                 "introduced_species",
                                 "total_species"
    )
    richness.data



    for(i in 1:dim(plant.spri)[2]){
        aa <- plant.spri[-which(is.na(plant.spri[,i])),][i]
        aa <- cbind(rownames(aa), aa)
        colnames(aa) <- c("species_code","present")
        bb <- species.list[which(species.list$species_code %in% rownames(aa)),]
        cc <- merge(bb, aa, by = "species_code")

        richness.data$site_code[i] <- colnames(plant.spri)[i]

        richness.data$tolerant_species_ratio[i] <- length(which(cc$tolerant == "O"))/(dim(aa)[1])*100

        richness.data$Salix_species_ratio[i] <- length(which(cc$genus %in% "Salix"))/(dim(aa)[1])*100

        richness.data$Gramineae_species_ratio[i] <- length(which(cc$family %in% "Gramineae"))/(dim(aa)[1])*100

        richness.data$Cyperaceae_species_ratio[i] <- length(which(cc$family %in% "Cyperaceae"))/(dim(aa)[1])*100

        richness.data$OBL_ratio[i] <- length(which(cc$wetland_appear_frequency %in% "OBL"))/(dim(aa)[1])*100

        richness.data$FACW_ratio[i] <- length(which(cc$wetland_appear_frequency %in% "FACW"))/(dim(aa)[1])*100

        richness.data$naturalized_ratio[i] <- length(which(cc$naturalized == "O"))/(dim(aa)[1])*100

        richness.data$ecosystem_disturb_ratio[i] <- length(which(cc$ecosystem_disturb == "O"))/(dim(aa)[1])*100

        richness.data$ecosystem_disturb_species[i] <- length(which(cc$ecosystem_disturb == "O"))

        richness.data$introduced_species[i] <- length(which(cc$introduced == "O"))

        richness.data$total_species[i] <- dim(aa)[1]
        richness.data[i,-1] <- round(richness.data[i,-1],1)

    }
    richness.data


    rownames(plant.spri)[7:dim(plant.spri)[1]] <- species.list$species_name[7:length(species.list$species_name)]
    plant.spri.t <- data.frame(names(plant.spri),t(plant.spri[-c(1:6),]))
    names(plant.spri.t)[1] <- "site_code"


    DF.form.vegetation2.nt <- env.data.tf.lalo[,c(1:11,19:22,27:29)] |>
        merge(richness.data, by = "site_code") |>
        merge(plant.spri.t, by = "site_code")
    DF.form.vegetation2.nt.sort <- DF.form.vegetation2.nt[order(DF.form.vegetation2.nt$No.),]

    DF.form.vegetation2 <- data.frame(t(DF.form.vegetation2.nt.sort))

    write.csv(DF.form.vegetation2, "DB.form.vegetation_PA.csv", fileEncoding = "EUC-KR")

    cross_section.DF <- data.frame(matrix(nrow = 450, ncol = length(site.list)+2))
    cross_section.DF[,1] <- rep(x=1:30, each=15)
    names(cross_section.DF) <- c("site_number", "invetigation_contents", site.list)

    cross_section2 <- data.frame(cross_section[,1:4],cross_section$relative_height, cross_section[,5:16])
    cross_section.DF[,2] <- rep(x=colnames(cross_section2)[3:17], times = 30)

    for(i in 1:length(site.list)){
        if((site.list[i] %in% cross_section2$site_code) == FALSE){
            next
        }else{
            cross_section.temp <- cross_section2[which(cross_section2$site_code == site.list[i]),]
            for(j in 1:dim(cross_section.temp)[1]){
                min.num <- min(which(cross_section.DF$site_number %in% cross_section.temp$site_number[j]))
                max.num <- max(which(cross_section.DF$site_number %in% cross_section.temp$site_number[j]))
                temp.survey <- t(cross_section.temp[j,3:dim(cross_section.temp)[2]])
                cross_section.DF[min.num:max.num,i+2] <- temp.survey
            }
        }
    }
    cross_section.DF
    species.appear.cs <- species.appear[-which(species.appear$site_number == 0),]

    observed.plant.DF <- data.frame(matrix(nrow = 115, ncol = length(site.list)+2))
    observed.plant.DF[,1] <- c(rep(c("T1","T2"), each = 10), rep(c("S"), times = 15), rep(c("H"), times = 80))
    observed.plant.DF[,2] <- 1:115
    colnames(observed.plant.DF) <- c("structure", "number", site.list)

    for(i in 1:length(site.list)){
        if((site.list[i] %in% species.appear.cs$site_code) == FALSE){
            next
        }else{
            observed.plant.site.temp <- species.appear.cs[which(species.appear.cs$site_code == site.list[i]),]
            S.species <- unique(observed.plant.site.temp$species_code[which(observed.plant.site.temp$structure == "S")])
            T1.species <- unique(observed.plant.site.temp$species_code[which(observed.plant.site.temp$structure == "T1")])
            T2.species <- unique(observed.plant.site.temp$species_code[which(observed.plant.site.temp$structure == "T2")])
            H.species <- unique(observed.plant.site.temp$species_code[which(observed.plant.site.temp$structure == "H")])

            if(length(S.species) != 0){
                observed.plant.DF[,i+2][min(which(observed.plant.DF$structure == "S")):c(min(which(observed.plant.DF$structure == "S"))+length(S.species)-1)] <- S.species
            }

            if(length(T1.species) != 0){
                observed.plant.DF[,i+2][min(which(observed.plant.DF$structure == "T1")):c(min(which(observed.plant.DF$structure == "T1"))+length(T1.species)-1)] <- T1.species
            }

            if(length(T2.species) != 0){
                observed.plant.DF[,i+2][min(which(observed.plant.DF$structure == "T2")):c(min(which(observed.plant.DF$structure == "T2"))+length(T2.species)-1)] <- T2.species
            }
            if(length(H.species) != 0){
                observed.plant.DF[,i+2][min(which(observed.plant.DF$structure == "H")):c(min(which(observed.plant.DF$structure == "H"))+length(H.species)-1)] <- H.species
            }
        }

    }

    observed.plant.DF
    observed.plant.DF.CN <- data.frame(c('presented_vegetation',rep(NA, times = 114)),observed.plant.DF)
    names(observed.plant.DF.CN) <- c("site_number",names(observed.plant.DF))
    for(j in 1:length(site.list)){
        for(i in 1:dim(observed.plant.DF)[1]){
            if(is.na(observed.plant.DF[,j+2][i]) == F){
                observed.plant.DF.CN[,j+3][i] <- species.list$species_name[which(species.list$species_code == observed.plant.DF[,j+2][i])]
            }
        }
    }

    survey_site.dominance_rank <- data.frame(matrix(nrow = 115*30, ncol = 3+length(site.list)))
    survey_site.dominance_rank[,1] <- rep(1:30, each = 115)
    survey_site.dominance_rank[,2] <- c(rep(c("T1","T2"), each = 10), rep(c("S"), times = 15), rep(c("H"), times = 80))
    survey_site.dominance_rank[,3] <- rep(1:115, times = 30)
    colnames(survey_site.dominance_rank) <- c("site_number","structure","number",site.list)

    for(i in 1:length(site.list)){
        survey.num <-  unique(species.appear.cs[which(species.appear.cs$site_code == site.list[i]),]$site_number)
        for(j in survey.num){
            survey.temp <- species.appear.cs[which(species.appear.cs$site_code == site.list[i]),][which(species.appear.cs[species.appear.cs$site_code == site.list[i],][,3] == j),]
            for(k in 1:dim(survey.temp)[1]){
                survey.mat1 <- survey_site.dominance_rank[survey_site.dominance_rank$site_number == j,]
                index.temp1 <- rownames(survey.mat1)[survey.mat1$structure == survey.temp$structure[k]]
                index.temp2 <- which(survey.mat1$structure == survey.temp$structure[k])
                index.temp3 <- index.temp1[which(observed.plant.DF[,i+2][index.temp2] == survey.temp$species_code[k])]
                survey_site.dominance_rank[index.temp3,i+3] <- survey.temp$dominance[k]
            }
        }
    }

    survey_site.dominance_rank


    cross_section.DF.2 <- cbind(c(1:450),cross_section.DF)

    env.data.dom <- data.frame(cbind(rep(NA,times = 22),
                                     rep(NA,times = 22),
                                     rownames(t(env.data.tf.lalo[,c(1:9,21,22,12,13,16,14,15,17,18,23:26)])),
                                     t(env.data.tf.lalo[,c(1:9,21,22,12,13,16,14,15,17,18,23:26)])))

    names(cross_section.DF.2) <- names(env.data.dom)
    names(observed.plant.DF.CN) <- names(env.data.dom)
    names(survey_site.dominance_rank) <- names(env.data.dom)

    cross_section.DF.2$V1 <- c("cross_section",rep(NA, times = dim(cross_section.DF.2)[1]-1))

    DB.form.vegetation3 <- data.frame(rbind(env.data.dom, cross_section.DF.2, observed.plant.DF.CN, survey_site.dominance_rank))
    write.csv(DB.form.vegetation3, file = "DB.form.crosssection.csv", fileEncoding = "EUC-KR",row.names = F)

}
