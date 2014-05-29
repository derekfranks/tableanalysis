table.analysis <- function(var, segment, alpha=.05, detail=TRUE) {
#create a table where the row is depvar and column is indvar
        tab_1 <- table(var, segment)
        print(tab_1)
        print(round(prop.table(tab_1,2),2))
#run chisq.test against table
        tab_1_chisq <- chisq.test(tab_1)
        print(tab_1_chisq)
#if pvalue of chisq is < alpha, then proceed with pairwise prop comparisons
        if (tab_1_chisq$p.value < alpha) {
#create a matrix with the combinations based on number of columns
                comb <- combn(ncol(tab_1),2)  
                print("Pair comparisons:")
                print(comb)
#loop through each row of the table and conduct a prop test for each unique column pairing
                for (i in 1:nrow(tab_1)) {
                        for (k in 1:ncol(comb)) {
                                counts <- c(tab_1[i,comb[1,k]], tab_1[i,comb[2,k]])
                                totals <- c(sum(tab_1[,comb[1,k]]), sum(tab_1[,comb[2,k]]))
                                result <- prop.test(counts, totals, correct=F)
                                result$p.value <- p.adjust(result$p.value, method="bonferroni", n=ncol(comb))
                                if (detail==TRUE) {
                                        print(paste("Row:",as.character(i)," ", "Pair-comparison:", as.character(k),sep=" "))
                                        print(result)
                                }
                        }
                }
        }
        else {stop("Variables are independent of each other")
        }
}