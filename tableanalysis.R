data.analysis <- function(arguments){
        
}



make.tables <- function(var, segment) {
#create a table where the row is depvar and column is indvar
        tab_1 <- table(var, segment)
        print(tab_1)
        print(round(prop.table(tab_1,2),2))
}
        
        
        
        
table.analysis <- function(table, alpha=.05, detail=FALSE, correction="holm") {        
#run chisq.test against table
        tab_1 <- table
        print(tab_1)
        print(round(prop.table(tab_1, 2), 2))
        tab_1_chisq <- chisq.test(tab_1)
        print(tab_1_chisq)
#if pvalue of chisq is < alpha, then proceed with pairwise prop comparisons
        if (tab_1_chisq$p.value < alpha) {
                
#                perm <- permutations(ncol(tab_1),2)
                comb <- combinations(ncol(tab_1),2)  

                #create a matrix with the combinations based on number of columns
                comp_tab <- matrix(nrow=nrow(tab_1),ncol=ncol(tab_1))
#                 comp_tab <- as.table(matrix(nrow=nrow(tab_1),ncol=ncol(tab_1)))
#                 colnames(comp_tab) <- colnames(tab_1)
#                 rownames(comp_tab) <- rownames(tab_1)
                comp_tab[is.na(comp_tab)] <- "" 
                
                #loop through each row of the table and conduct a prop test for EACH column pairing
                for (i in 1:nrow(tab_1)) {
                        for (k in 1:nrow(comb)) {
                                counts <- c(tab_1[i,comb[k,1]], tab_1[i,comb[k,2]])
                                totals <- c(sum(tab_1[,comb[k,1]]), sum(tab_1[,comb[k,2]]))
                                result <- prop.test(counts, totals, correct=F)
                                result$p.value <- p.adjust(result$p.value, method=correction, n=nrow(comb)*nrow(tab_1))
                                #build comp_table that summarizes the results of the pairwise prop tests
                                if (result$p.value < alpha & result$estimate[[1]] < result$estimate[[2]]) {
                                        comp_tab[i,comb[k,2]] <- paste(comp_tab[i,comb[k,2]], letters[comb[k,1]], sep="")
                                }
                                if (result$p.value < alpha & result$estimate[[1]] > result$estimate[[2]]) {
                                        comp_tab[i,comb[k,1]] <- paste(comp_tab[i,comb[k,1]], letters[comb[k,2]], sep="")
                                }
                                
                        }
                }
                print(comp_tab)
           
                if (detail==TRUE) {
                        print("Pair comparison table:")
                        print(comb)
                }

#loop through each row of the table and conduct a prop test for EACH UNIQUE column pairing
                for (i in 1:nrow(tab_1)) {
                        for (k in 1:nrow(comb)) {
                                counts <- c(tab_1[i,comb[k,1]], tab_1[i,comb[k,2]])
                                totals <- c(sum(tab_1[,comb[k,1]]), sum(tab_1[,comb[k,2]]))
                                result <- prop.test(counts, totals, correct=F)
                                result$p.value <- p.adjust(result$p.value, method=correction, n=nrow(comb)*nrow(tab_1))
                                
                                if (detail==TRUE) {
                                        print(paste("Row:",as.character(i)," ", "Pair comparison detail:", as.character(k),sep=" "))
                                        print(result)
                                }
                        }
                
                }
        }
        else {stop("Variables are independent of each other")
        }
}