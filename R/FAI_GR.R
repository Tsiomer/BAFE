#' FAI calculation by waterside survey result
#'
#' This is a function that generates site-specific FAI, ranking, and DB format using data surveyed at the Greatriver site where center surveying is conducted.
#' This function operate without parameter, you just operate function
#' @param filepath  Decide whether to set filepath or not
#' @export
#' @examples
#' FAI_GR()

FAI_GR <- function(filepath = NA){
    if(is.na(filepath) == T){
        filepath.1 <- file.choose()
    }else{
        filepath.1 <- filepath
    }
    raw.abun <- data.frame(readxl::read_excel(filepath.1, sheet = 4, skip = 1))
    raw.abun

    if(sum(names(raw.abun) == c("year", "site", "large", "site_code", "waterside_middle", "survey_order",
                                "number", "species_name", "inds", "abnormal_DE", "abnormal_EF", "abnormal_LE",
                                "abnormalTU_", "abnormal_sum", "species_code")) != 15 ){
        stop("need to check fish data")
    }
    extract.abun_raw <- raw.abun[,c(4,5,8,9,14,15)]
    extract.abun <- extract.abun_raw[extract.abun_raw$waterside_middle == "center",][,-2]
    extract.abun <- extract.abun[-which(is.na(extract.abun$site_code)),]
    species.data <- data.frame(readxl::read_excel(filepath.1, sheet = 6, skip = 2))

    if(sum(names(species.data) == c("species_name", "G_tolerance", "G_feeding", "G_habitat", "exotic",
                                    "endemic", "endangered_1", "endangered_2", "natural_monument",
                                    "order_by_line", "class", "order", "family", "scientific_name", "Species_Code")) != 15 ){
        stop("need to check species list")
    }
    env.raw <- data.frame(readxl::read_excel(filepath.1, sheet = 2, skip = 1))

    if(sum(names(env.raw) == c("No.", "site", "watershed", "water_system", "subbasin", "stream",
                               "main_tributary_etc", "survey_order", "stream_order", "large", "site_code",
                               "organization", "investigator", "session", "investigate_tools", "investigate_time",
                               "date", "weather", "center_tool", "center_time", "center_date", "center_weather",
                               "lat_degree", "lat_minute", "lat_second", "long_degree", "long_minute", "long_second",
                               "flow_status", "bedrock", "concrete", "silt", "finesand", "fine_gravel", "gravel",
                               "pebble", "cobble", "substrate_sum", "stream_type", "velocity", "invetigate_no",
                               "IN_special_note", "investigate_special_note", "unconfirmed_species")) != 44 ){
        stop("need to check environmental data")
    }
    stream_order <- env.raw[,c(11,9)]
    extract.abun$site_code |>
        unique() -> site.list # site list for FAI calculation

    colnames(stream_order) <- c(colnames(extract.abun)[1], "stream_order")


    extract.abun.so.0 <- merge(extract.abun, stream_order, by = "site_code")
    names(extract.abun.so.0)[5] <- "Species_Code"
    extract.abun.so.1 <- merge(extract.abun.so.0, species.data[,c(2:5,14,15)], by = "Species_Code")
    extract.abun.so.2 <- extract.abun.so.1[order(extract.abun.so.1$site_code),]



    M1.aggregation <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    M1.aggregation[,1] <- site.list
    colnames(M1.aggregation) <- c("site_code", "M1")
    M2.aggregation <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    M2.aggregation[,1] <- site.list
    colnames(M2.aggregation) <- c("site_code", "M2")
    M3.aggregation <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    M3.aggregation[,1] <- site.list
    colnames(M3.aggregation) <- c("site_code", "M3")
    M4.aggregation <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    M4.aggregation[,1] <- site.list
    colnames(M4.aggregation) <- c("site_code", "M4")
    M5.aggregation <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    M5.aggregation[,1] <- site.list
    colnames(M5.aggregation) <- c("site_code", "M5")
    M6.aggregation <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    M6.aggregation[,1] <- site.list
    colnames(M6.aggregation) <- c("site_code", "M6")
    M7.aggregation <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    M7.aggregation[,1] <- site.list
    colnames(M7.aggregation) <- c("site_code", "M7")
    M8.aggregation <- data.frame(matrix(nrow = length(site.list), ncol = 2))
    M8.aggregation[,1] <- site.list
    colnames(M8.aggregation) <- c("site_code", "M8")

    for(i in 1:length(site.list)){
        subset.0 <- subset(extract.abun.so.2, site_code == site.list[i])
        if("U00001" %in% subset.0$Species_Code | "U00002" %in% subset.0$Species_Code){
            M1<-0;M2<-0;M3<-0;M4<-0;M5<-0;M6<-0;M7<-0;M8<-0
            M1.aggregation[M1.aggregation[,1] ==  site.list[i],][2] <- M1
            M2.aggregation[M2.aggregation[,1] ==  site.list[i],][2] <- M2
            M3.aggregation[M3.aggregation[,1] ==  site.list[i],][2] <- M3
            M4.aggregation[M4.aggregation[,1] ==  site.list[i],][2] <- M4
            M5.aggregation[M5.aggregation[,1] ==  site.list[i],][2] <- M5
            M6.aggregation[M6.aggregation[,1] ==  site.list[i],][2] <- M6
            M7.aggregation[M7.aggregation[,1] ==  site.list[i],][2] <- M7
            M8.aggregation[M8.aggregation[,1] ==  site.list[i],][2] <- M8

        }else{
            st.order <- unique(subset.0$stream_order)
            DS.R <- dim(subset.0[is.na(subset.0$exotic),])[1] #DS.R - domestic species richness; M1
            RB.R <- dim(subset.0[which(subset.0$G_habitat == "RB"),])[1] #RB.R - rapids benthic richness; M2
            SS.R <- dim(subset.0[which(subset.0$G_tolerance == "SS"),])[1] #SS.R - sensitive species richness; M3
            DS.A <- sum(subset.0[is.na(subset.0$exotic),]$inds) #DS.A - domestic species abundance; M7
            TS.AR <- sum(subset.0[which(subset.0$G_tolerance == "TS"),]$inds)/sum(subset.0$inds)*100
            #TS.R - tolerant species abundance rate
            OS.AR <- sum(subset.0[which(subset.0$G_feeding == "O"),]$inds)/sum(subset.0$inds)*100
            #OS.R - omnivorous species abundance rate
            DS <- subset.0[is.na(subset.0$exotic),] #DS - domestic species matrix
            IS.DAR <- sum(DS[which(DS$G_feeding == "I"),]$inds)/sum(subset.0$inds)*100
            #IS.DAR - Insectivorous species among domestic species abundance rate
            AS.AR <- sum(subset.0$abnormal_sum)/sum(subset.0$inds)*100 #Anormalities abundance rate

            if(st.order == 5){

                M1 <- c(0,6.25,12.5)[cut(DS.R, breaks = c(-1,7,13,999999))]
                M2 <- c(0,6.25,12.5)[cut(RB.R, breaks = c(-1,0,3,999999))]
                M3 <- c(0,6.25,12.5)[cut(SS.R, breaks = c(-1,1,4,999999))]
                M7 <- c(0,6.25,12.5)[cut(DS.A, breaks = c(-1,130,330,999999))]
            }else if(st.order == 6){

                M1 <- c(0,6.25,12.5)[cut(DS.R, breaks = c(-1,8,14,999999))]
                M2 <- c(0,6.25,12.5)[cut(RB.R, breaks = c(-1,0,2,999999))]
                M3 <- c(0,6.25,12.5)[cut(SS.R, breaks = c(-1,0,3,999999))]
                M7 <- c(0,6.25,12.5)[cut(DS.A, breaks = c(-1,120,380,999999))]
            }else if(st.order == 7){

                M1 <- c(0,6.25,12.5)[cut(DS.R, breaks = c(-1,5,11,999999))]
                M2 <- c(0,6.25,12.5)[cut(RB.R, breaks = c(-1,0,1,999999))]
                M3 <- c(0,6.25,12.5)[cut(SS.R, breaks = c(-1,0,2,999999))]
                M7 <- c(0,6.25,12.5)[cut(DS.A, breaks = c(-1,40,200,999999))]
            }

            if(TS.AR > 70){
                M4 <- 0
            }else if(TS.AR >= 30 & TS.AR <= 70){
                M4 <- 6.25
            }else{
                M4 <- 12.5
            }

            if(OS.AR > 70){
                M5 <- 0
            }else if(OS.AR >= 30 & OS.AR <= 70){
                M5 <- 6.25
            }else{
                M5 <- 12.5
            }
            if(IS.DAR < 20){
                M6 <- 0
            }else if(IS.DAR >= 20 & IS.DAR <= 45){
                M6 <- 6.25
            }else{
                M6 <- 12.5
            }

            if(AS.AR == 0){
                M8 <- 12.5
            }else if(AS.AR > 0 & AS.AR <= 1){
                M8 <- 6.25
            }else{
                M8 <- 0
            }

            M1.aggregation[M1.aggregation[,1] ==  site.list[i],][2] <- M1
            M2.aggregation[M2.aggregation[,1] ==  site.list[i],][2] <- M2
            M3.aggregation[M3.aggregation[,1] ==  site.list[i],][2] <- M3
            M4.aggregation[M4.aggregation[,1] ==  site.list[i],][2] <- M4
            M5.aggregation[M5.aggregation[,1] ==  site.list[i],][2] <- M5
            M6.aggregation[M6.aggregation[,1] ==  site.list[i],][2] <- M6
            M7.aggregation[M7.aggregation[,1] ==  site.list[i],][2] <- M7
            M8.aggregation[M8.aggregation[,1] ==  site.list[i],][2] <- M8
        }

    }

    merged_matric <- M1.aggregation |>
        merge(M2.aggregation, by = "site_code") |>
        merge(M3.aggregation, by = "site_code") |>
        merge(M4.aggregation, by = "site_code") |>
        merge(M5.aggregation, by = "site_code") |>
        merge(M6.aggregation, by = "site_code") |>
        merge(M7.aggregation, by = "site_code") |>
        merge(M8.aggregation, by = "site_code")

    merged_matric$FAI <- rowSums(merged_matric[,-1])
    colnames(merged_matric)[1] <- "site_code"

    env.survey <- env.raw[,c(11,41)]
    colnames(env.survey) <- c("site_code","invetigate_no")
    env.survey$invetigate_no[is.na(env.survey$invetigate_no)] <- "-"

    merged_matric$FAI_special_issue <- merged_matric$FAI
    merged_matric$FAI_special_issue[merged_matric$site_code %in% env.survey$site_code[!(env.survey$invetigate_no == "-")]] <- "-"

    merged_matric.2 <- merge(merged_matric, env.survey, by = "site_code")

    for(i in 1:dim(merged_matric.2)[1]){
        if(merged_matric.2$FAI_special_issue[i] == "-" |is.na(merged_matric.2$FAI_special_issue[i])){
            merged_matric.2$rank[i] <- "-"
        }else{
            merged_matric.2$rank[i] <- c("E","D","C","B","A")[cut(as.numeric(merged_matric.2$FAI_special_issue[i]), breaks = c(0,20,40,60,80,101),right = F)]
        }
    }
    #species number and abundance
    extract.abun.so.2
    #species number = count of each site code
    site.list

    species.n.a <- data.frame(matrix(nrow = length(site.list), ncol = 3)) #species number and abundance
    species.n.a[,1] <- site.list
    for(i in 1:length(site.list)){
        aa <-which(extract.abun.so.2$site_code == site.list[i])
        if(T %in% c(extract.abun.so.2$Species_Code[aa] %in% c("U00001","U00002"))){
            s.n <- NA
            s.a <- NA
            species.n.a[i,2] <- s.n
            species.n.a[i,3] <- s.a
        }else{
            c(extract.abun.so.2$site_code == site.list[i]) |>
                which() -> bb #index of site[i] in extract.abun.so.2
            s.n <- length(bb)
            s.a <- sum(extract.abun.so.2$inds[bb])
            species.n.a[i,2] <- s.n
            species.n.a[i,3] <- s.a
        }
    }
    colnames(species.n.a) <- c("site_code","richness","abundance")

    latitude <- paste(env.raw$lat_degree,env.raw$lat_minute,env.raw$lat_second)
    longitude <- paste(env.raw$long_degree,env.raw$long_minute,env.raw$long_second)

    env.raw.la_lo <- env.raw

    env.raw.la_lo[,23] <- latitude
    env.raw.la_lo[,24] <- longitude
    env.raw.la_lo.2 <- env.raw.la_lo[,-c(25:28)]
    colnames(env.raw.la_lo.2)

    colnames(env.raw.la_lo.2)[37:39] <- c("invetigate_no","IN_special_note","investigate_special_note")
    env.raw.la_lo.2.1 <-env.raw.la_lo.2
    env.raw.la_lo.2.1[,c(37:39)][is.na(env.raw.la_lo.2.1[,c(37:39)])] <- "-"
    colnames(env.raw.la_lo.2.1)[11] <- "site_code"

    total_data <- env.raw.la_lo.2.1 |>
        merge(species.n.a, by = "site_code") |>
        merge(merged_matric.2[,-12], by = "site_code")

    total_data.t <- t(total_data)

    colnames(total_data.t) <- total_data.t[which(rownames(total_data.t)=="site_code"),]
    total_data.t <- total_data.t[-which(rownames(total_data.t)=="site_code"),]
    total_data.t.2 <- total_data.t[-c(14:17,10,1),]

    total_data.t.3 <- rbind(total_data.t.2[1:7,],
                            total_data.t.2[16:17,],
                            total_data.t.2[14:15,],
                            total_data.t.2[9:10,],
                            total_data.t.2[c(8,13,12),],
                            total_data.t.2[18:32,],
                            total_data.t.2[34:46,])

    allspecies.matrix <- data.frame(matrix(nrow = dim(species.data)[1], ncol = dim(total_data.t.3)[2]))
    rownames(allspecies.matrix) <- species.data$Species_Code
    colnames(allspecies.matrix) <- colnames(total_data.t.3)

    for(i in 1:dim(allspecies.matrix)[2]){
        temp.site.abundance <- extract.abun.so.2[which(extract.abun.so.2$site_code %in% colnames(allspecies.matrix)[i]),][,c(1,4)]

        for(j in 1:dim(temp.site.abundance)[1]){
            allspecies.matrix[,i][which(rownames(allspecies.matrix) == temp.site.abundance$Species_Code[j])] <- temp.site.abundance$inds[j]
        }


    }
    allspecies.matrix.2 <- allspecies.matrix[-c(which(rownames(allspecies.matrix)=="U00001"),
                                                which(rownames(allspecies.matrix)=="U00002")),]
    rownames(allspecies.matrix.2) <- species.data$species_name[-c(which(rownames(allspecies.matrix)=="U00001"),
                                                                  which(rownames(allspecies.matrix)=="U00002"))]

    DB.form <- rbind(total_data.t.3,allspecies.matrix.2)
    write.csv(DB.form, "DB.form.fish_greatriver.csv", fileEncoding = "EUC-KR")
}

