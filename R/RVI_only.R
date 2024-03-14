#' BMI calculation
#' 
#' This function generate RVI and rank of each site
#' This fucntion operate without parameter, you just operate function 
#' @param x : Excel file that selected in pop-up window
#' @export
#' @examples 
#' RVI_only()

RVI_only <- function(){
    RVI_data.transform()
    
    RVI.matrix <- data.frame(matrix(nrow = dim(area.matrix)[2], ncol = 7))
    
    colnames(RVI.matrix) <- c("site_code","HAA","EA","WTD","SalFraA","ToSC","BTI")
    
    for(i in 1:dim(area.matrix)[2]){
        aa <- area.matrix[-which(is.na(area.matrix[,i])),][i]
        aa <- cbind(rownames(aa), aa)
        colnames(aa) <- c("species_code","area")
        bb <- species.list[which(species.list$species_code %in% rownames(aa)),]
        cc <- merge(bb, aa, by = "species_code")
        
        RVI.matrix$site_code[i] <- names(area.matrix)[i]
        
        RVI.matrix$HAA[i] <- sum(cc$area[cc$growth_type %in% c("HerbAn","ClimbAn")])/aa$area[1]*100
        
        RVI.matrix$EA[i] <- sum(cc$area[which(cc$introduced == "O")])/aa$area[1]*100
        
        RVI.matrix$WTD[i] <- sum((aa[-c(1:7),2]/aa[1,2])^2)
        
        RVI.matrix$SalFraA[i] <- sum(cc$area[which(cc$Salix_Fraxinus == "O")])/aa$area[1]*100
        
        species.richness.1 <- species.richness.sort[which(species.richness.sort$site_code.x == unique(species.richness.sort$site_code.x)[1]),]
        species.richness.data <- species.list[which(species.list$species_code %in% species.richness.1$species_code),]
        RVI.matrix$ToSC[i] <- length(which(species.richness.data$introduced == "O"))/dim(species.richness.data)[1]*100
        
        subset.1 <- subset(cross_section, site_code == site.list[i])
        totallength <- max(subset.1$site_distance)
        RVI.matrix$BTI[i] <- sum(subset.1$each_site_length*subset.1$wetland_appear_frequency_score*subset.1$land_use_type_score, na.rm = T)/totallength
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
    score.ingage$RVI <- rowSums(score.ingage[,c(2:7)])*(10/3)
    
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
    
    
    score.ingage.3 <- merge(score.ingage.2, approach.status, by = "site_code")
    write.csv(score.ingage.3, file = "RVI matrix.csv", fileEncoding = "EUC-KR")
}