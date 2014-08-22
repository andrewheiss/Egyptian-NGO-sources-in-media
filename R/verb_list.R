library(pander)
library(dplyr)

verbs <- read.csv("../Output/verb_list.csv", header=TRUE) %>%
  arrange(desc(count))

colnames(verbs) <- c("Verb", "Count")

cat(pandoc.table.return(cbind(verbs[1:7,], verbs[8:14,], verbs[15:21,]), 
                        caption="Most common verbs in sentences that mention NGOs"), 
    file="../Output/table_verbs.md")


#----------------------
# Export list of NGOs
#----------------------
# Add enough NAs to coerce list into a matrix
num.columns <- 3
cells.to.add <- num.columns - (length(ngos) %% num.columns)
ngo.output <- matrix(c(sort(ngos), rep(NA, cells.to.add)), ncol=num.columns, byrow=TRUE)

# Markdown
cat(pandoc.table.return(ngo.output, split.tables=Inf, 
                        justify="left", caption="List of NGOs"),
    file="../Output/table_ngo_list.md")

