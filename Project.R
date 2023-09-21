# Importing libraries
library(dplyr)
library(arules)
library(arulesViz)
library(htmlwidgets)
library(writexl)
options(warn = -1)

# Importing the dataset
dados = read.csv('dataset_bd3.csv')

## Checking if the columns are Character type
summary(dados)
str(dados)

# We can see that the uneven lines have no data, while the even onas do have data.
# One smart way to solve the problem in the dataset 
# is to separate the even rows from the odd rows.
linhas_pares = seq(2, nrow(dados), 2)
linhas_ímpares = seq(1, nrow(dados), 2)

# We will separate the data and then use the dataset with the even rows (valid data rows).
df1 = dados[linhas_pares, ]
View(df1)

df2 = dados[linhas_ímpares, ]
View(df2)

# Check if there are any missing values in the first item of the purchase, 
# as it would not make sense for an MBA analysis.
sum(is.na(df1$Item01))

# Check if there are any missing values in the second item of purchase
sum(is.na(df1$Item02))
View(df1)

# The sum(is.na()) function shows that there are no missing values in the
# rows, but when we use View(), we can see that there are actually empty 
# spaces in the Item02 column. This means that we can have spaces as characters.

# Check if there are any missing values represented by whitespace
which(nchar(trimws(df1$Item01)) == 0)
which(nchar(trimws(df1$Item02)) == 0)

# The trimws() function removes spaces from the beginning and end of a character:
# " abc " becomes "abc"
# Therefore, if you enter ' ', it becomes ''. If it is NA, it returns NA.
# Thus, nchar() counts all the characters. So if nchar() == 0, it means "", that is, a blank space.

# Check if there are any missing values represented by whitespace 
# (using regular expression)
grepl('^\\s*$', df1$Item02)

# Number of distinct items
n_distinct(df1)

# We will work only with the records where the second item is not null
df1_two = df1[!grepl('^\\s*$', df1$Item02), ]

# Number of distinct items
n_distinct(df1_two)

# Prepare the package by converting the variables to factor type 
# (variables that we will use from now on)
View(df1_two)

# We have a data frame with at least two products, which makes sense
# for the project objective: to analyze the probability that the purchase
# of a product leads to buying more products.
pacote = df1_two

# We don't need to use all 20 columns, it's better to work with less
# space, because we'll have better processing.
# Not all 20 columns have data, so the professor chose 6
# but we can choose another number of columns

for (i in colnames(pacote)[1:6]) {
  pacote[[i]] = as.factor(pacote[[i]])
}
summary(pacote)
View(pacote)
str(pacote)

pacote_split = split(pacote$Item01,
                     pacote$Item02,
                     pacote$Item03,
                     pacote$Item04,
                     pacote$Item05,
                     pacote$Item06,
                     drop = F)
View(pacote_split)

# Transactions
# The as() function is used to convert an object to a different class. 
# The as(pacote_split, 'transactions') line of code converts the pacote_split 
# object to the transactions class.
transacoes = as(pacote_split, 'transactions')

## The transactions class is required for the function inspect()

# Rules Inspection
inspect(head(transacoes, 5))

### Note: in the results, we have lhs and rhs. The lhs means antecedent
# The rhs means consequent

# Checking the rules of a product: Dust-Off Compressed Gas 2 pack
regras_produto1 = apriori(transacoes,
                          parameter = list(conf = 0.5, minlen = 3),
                          appearance = list(rhs = 'Dust-Off Compressed Gas 2 pack',
                                            default = 'lhs'))

# Rules Inspection
inspect(head(sort(regras_produto1, by = 'confidence'), 5))

# Checking the rules of a product: HP 61 ink
regras_produto2 = apriori(transacoes,
                          parameter = list(minlen = 3, conf = 0.5),
                          appearance = list(rhs = 'HP 61 ink',
                                            default = 'lhs'))

# Rules Inspection
inspect(head(sort(regras_produto2, by = 'confidence'), 5))

# Checking the rules of a product: VIVO Dual LCD Monitor Desk mount
regras_produto3 = apriori(transacoes,
                          parameter = list(minlen = 3, conf = 0.5),
                          appearance = list(rhs = 'VIVO Dual LCD Monitor Desk mount',
                                            default = 'lhs'))

# Rules Inspection
inspect(head(sort(regras_produto3, by = 'confidence'), 5))

# Checking the rules of a product again: Dust-Off Compressed Gas 2 pack,
# chenging the metrics
regras_produto1 = apriori(transacoes,
                          parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = 'rules'),
                          appearance = list(rhs = 'Dust-Off Compressed Gas 2 pack',
                                            default = 'lhs'))

# Rules Inspection
inspect(head(sort(regras_produto1, by = 'confidence'), 5))

# Filter out redundant rules
regras_produto1_clean = regras_produto1[!is.redundant(regras_produto1)]

# Rules Inspection
inspect(head(sort(regras_produto1_clean, by = 'confidence'), 5))

# Summary
summary(regras_produto1_clean)

# Plot
plot(regras_produto1_clean, 
     measure = 'support', 
     shading = 'confidence',
     method = 'graph',
     engine = 'html')


# Checking the rules of a product again: HP 61 ink,
# chenging the metrics
regras_produto2 = apriori(transacoes,
                          parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = 'rules'),
                          appearance = list(rhs = 'HP 61 ink',
                                            default = 'lhs'))

# Rules Inspection
inspect(head(sort(regras_produto2, by = 'confidence'), 5))

# Filter out redundant rules
regras_produto2_clean = regras_produto2[!is.redundant(regras_produto2)]

# Rules Inspection
inspect(head(sort(regras_produto2_clean, by = 'confidence'), 5))

# Summary
summary(regras_produto2_clean)

# Plot
plot(regras_produto2_clean, 
     measure = 'support', 
     shading = 'confidence',
     method = 'graph',
     engine = 'html')

# Checking the rules of a product again: VIVO Dual LCD Monitor Desk mount,
# chenging the metrics
regras_produto3 = apriori(transacoes,
                          parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = 'rules'),
                          appearance = list(rhs = 'VIVO Dual LCD Monitor Desk mount',
                                            default = 'lhs'))

# Rules Inspection
inspect(head(sort(regras_produto3, by = 'confidence'), 5))

# Filter out redundant rules
regras_produto3_clean = regras_produto3[!is.redundant(regras_produto3)]

# Summary
summary(regras_produto3_clean)

# Plot
plot(regras_produto3_clean, 
     measure = 'support', 
     shading = 'confidence',
     method = 'graph',
     engine = 'html')

# Top 3 rules
inspect(head(sort(regras_produto1_clean, 
                  by = 'support',
                  decreasing = T), 1))
inspect(head(sort(regras_produto2_clean,
                  by = 'confidence',
                  decreasing = T), 1))
inspect(head(sort(regras_produto3_clean,
                  by = 'confidence',
                  decreasing = T), 1))

# Save the set of rules for the 3 products as a dataframe and then
# save it to disk
df_produto1 = as(regras_produto1_clean, 'data.frame')
View(df_produto1)
write_xlsx(df_produto1, 'df_produto1.xlsx')

df_produto2 = as(regras_produto2_clean, 'data.frame')
View(df_produto2)
write_xlsx(df_produto2, 'df_produto2.xlsx')

df_produto3 = as(regras_produto3_clean, 'data.frame')
View(df_produto3)
write_xlsx(df_produto3, 'df_produto3.xlsx')
