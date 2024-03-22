
package_name <- "factoextra"


if (!require(package_name, character.only = TRUE)) {
 
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  } else {
    message(sprintf("Package '%s' is already installed.", package_name))
    library(package_name, character.only = TRUE)
    }



# Adapt this code for the PCA and you can also add a script on the  major task is to 
# get the data in the right shape to feed to this pipeline. Your method or this one still requires us to 
# code the data that we going to use 

household_data <- data.frame(
  # replace this data with the HH data keep the long, 
  # lat, unique ID  ea_number new  etc 
  TV = c(1, 0, 1, 1, 0),
  Radio = c(1, 1, 0, 1, 0),
  Water_Source = c(1, 0, 1, 1, 0),
  Bathroom_Type = c(1, 0, 1, 1, 0),
  Shared_Bathroom = c(0, 1, 0, 0, 1)
)


pca_result <- prcomp(household_data, center = TRUE, scale. = TRUE)
#### 
# End here 
#### 

# below is just an aside to view the PCA output

print(summary(pca_result))


fviz_pca_ind(pca_result,
             geom.ind = "point", 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Cos2"
)


plot(pc_scores[,1], pc_scores[,2], xlab="PC1", ylab="PC2", main="PCA of Household Data",
     pch=19, col=rainbow(nrow(household_data)))
text(pc_scores[,1], pc_scores[,2], labels = row.names(household_data), pos=4)


pc_scores <- pca_result$x 
print(pc_scores)


var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
plot(var_explained, xlab="Principal Component", ylab="Proportion of Variance Explained",
     type='b', pch=19, col="blue", main="Variance Explained by Each Principal Component")






