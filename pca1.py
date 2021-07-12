import pandas as pd 
import numpy as np
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from sklearn.preprocessing import scale
import seaborn as sns
from scipy.cluster.hierarchy import linkage
import scipy.cluster.hierarchy as sch
from sklearn.cluster import KMeans

wine = pd.read_csv("C:/Users/DELL/Downloads/wine.csv")
wine.describe()

wine.isna().sum()
wine.isnull().sum()

#to check duplicates
dups = wine.duplicated()
sum(dups)

wine_drop = wine.drop_duplicates()


wine.describe()
wine_drop = wine.drop(["Type"], axis = 1)

 # Considering only numerical data 
wine.data = wine.iloc[:, 1:]

# Normalizing the numerical data 
wine_normal = scale(wine.data)
wine_normal

pca = PCA(n_components = 6)
pca_values = pca.fit_transform(wine_normal)

# The amount of variance that each PCA explains is 
var = pca.explained_variance_ratio_
var

pca.components_
pca.components_[0]
# Cumulative variance 

var1 = np.cumsum(np.round(var, decimals = 4) * 100)
var1

# Variance plot for PCA components obtained 
plt.plot(var1, color = "red")

# PCA scores
pca_values

pca_data = pd.DataFrame(pca_values)
pca_data.columns = "comp0", "comp1", "comp2", "comp3", "comp4", "comp5"
final = pd.concat([wine.Type, pca_data.iloc[:, 0:3]], axis = 1)

# Scatter diagram
import matplotlib.pylab as plt
plt.scatter(x = final.comp2, y = final.comp1)

#boxplot
sns.boxplot(final.comp0)
sns.boxplot(final.comp1)
sns.boxplot(final.comp2)

#detection of outliers(find the RM based on IQR)
IQR = final["comp2"].quantile(0.75) - final["comp2"].quantile(0.25)
lower_limit_comp2 = final["comp2"].quantile(0.25) - (IQR * 1.5)
upper_limit_comp2 = final["comp2"].quantile(0.75) + (IQR * 1.5)

#replace the outliers by the maximum and minimum limit
final["comp2"] = pd.DataFrame(np.where(final["comp2"] > upper_limit_comp2, upper_limit_comp2,
                                                      np.where(final["comp2"] < lower_limit_comp2,lower_limit_comp2,final["comp2"])))
sns.boxplot(final["comp2"]);plt.title("Boxplot");plt.show()

#for creating dendogram
final_linkage = linkage(wine_normal,method = "complete", metric = "euclidean")

plt.figure(figsize=(15, 8));plt.title('Hierarchical Clustering Dendrogram');plt.xlabel('Index');plt.ylabel('Distance')
sch.dendrogram(final_linkage, 
    leaf_rotation = 0,  # rotates the x axis labels
    leaf_font_size = 10 # font size for the x axis labels
)
plt.show()

#kmeans
###### scree plot or elbow curve ############
TWSS = []
k = list(range(2, 9))

for i in k:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(wine_normal)
    TWSS.append(kmeans.inertia_)
    
TWSS
# Scree plot 
plt.plot(k, TWSS, 'ro-');plt.xlabel("No_of_Clusters");plt.ylabel("total_within_SS")

# Selecting 5 clusters from the above scree plot which is the optimum number of clusters 
model = KMeans(n_clusters = 3)
model.fit(wine_normal)

model.labels_ # getting the labels of clusters assigned to each row 
mb = pd.Series(model.labels_)  # converting numpy array into pandas series object 
final['clust'] = mb # creating a  new column and assigning it to new column 

final = final.iloc[:,[4,0,1,2,3]]
final.head()

final['Type'].replace({1:0 ,2:2 ,3:1})


######################### problem2 ###############################
import pandas as pd 
import numpy as np
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from sklearn.preprocessing import scale
import seaborn as sns
from scipy.cluster.hierarchy import linkage
import scipy.cluster.hierarchy as sch
from sklearn.cluster import KMeans

heart_ds = pd.read_csv("C:/Users/DELL/Downloads/heart disease.csv")
heart_ds.describe()

heart_ds.describe()
heart_ds_drop =heart_ds.drop(["target"], axis = 1)

 # Considering only numerical data 
heart_ds = heart_ds.iloc[:, 0:]

# Normalizing the numerical data 
heart_ds_normal = scale(heart_ds)
heart_ds_normal

pca = PCA(n_components = 6)
pca_values = pca.fit_transform(heart_ds_normal)

# The amount of variance that each PCA explains is 
var = pca.explained_variance_ratio_
var

pca.components_
pca.components_[0]
# Cumulative variance 

var1 = np.cumsum(np.round(var, decimals = 4) * 100)
var1

# Variance plot for PCA components obtained 
plt.plot(var1, color = "red")

# PCA scores
pca_values

pca_data = pd.DataFrame(pca_values)

pca_data.columns = "comp0", "comp1", "comp2", "comp3", "comp4", "comp5"
final = pca_data.iloc[:, 0:3] 

#boxplot
sns.boxplot(final.comp0)
sns.boxplot(final.comp1)
sns.boxplot(final.comp2)

#detection of outliers(find the RM based on IQR)
IQR = final["comp1"].quantile(0.75) - final["comp1"].quantile(0.25)
lower_limit_comp2 = final["comp1"].quantile(0.25) - (IQR * 1.5)
upper_limit_comp2 = final["comp1"].quantile(0.75) + (IQR * 1.5)

#replace the outliers by the maximum and minimum limit
final["comp1"] = pd.DataFrame(np.where(final["comp1"] > upper_limit_comp2, upper_limit_comp2,
                                                      np.where(final["comp1"] < lower_limit_comp2,lower_limit_comp2,final["comp1"])))
sns.boxplot(final["comp1"]);plt.title("Boxplot");plt.show()

#detection of outliers(find the RM based on IQR)
IQR = final["comp2"].quantile(0.75) - final["comp2"].quantile(0.25)
lower_limit_comp2 = final["comp2"].quantile(0.25) - (IQR * 1.5)
upper_limit_comp2 = final["comp2"].quantile(0.75) + (IQR * 1.5)

#replace the outliers by the maximum and minimum limit
final["comp2"] = pd.DataFrame(np.where(final["comp2"] > upper_limit_comp2, upper_limit_comp2,
                                                      np.where(final["comp2"] < lower_limit_comp2,lower_limit_comp2,final["comp2"])))
sns.boxplot(final["comp2"]);plt.title("Boxplot");plt.show()

#for creating dendogram
final_linkage = linkage(heart_ds_normal,method = "complete", metric = "euclidean")

plt.figure(figsize=(15, 8));plt.title('Hierarchical Clustering Dendrogram');plt.xlabel('Index');plt.ylabel('Distance')
sch.dendrogram(final_linkage, 
    leaf_rotation = 0,  # rotates the x axis labels
    leaf_font_size = 10 # font size for the x axis labels
)
plt.show()

#kmeans
###### scree plot or elbow curve ############
TWSS = []
k = list(range(2, 9))

for i in k:
    kmeans = KMeans(n_clusters = i)
    kmeans.fit(heart_ds_normal)
    TWSS.append(kmeans.inertia_)
    
TWSS
# Scree plot 
plt.plot(k, TWSS, 'ro-');plt.xlabel("No_of_Clusters");plt.ylabel("total_within_SS")

# Selecting 3 clusters from the above scree plot which is the optimum number of clusters 
model = KMeans(n_clusters = 3)
model.fit(heart_ds_normal)

model.labels_ # getting the labels of clusters assigned to each row 
mb = pd.Series(model.labels_)  # converting numpy array into pandas series object 
final['clust'] = mb # creating a  new column and assigning it to new column 

final = final.iloc[:,[3,0,1,2]]
final.head()

final = pd.concat([heart_ds.target, final.iloc[:, 0:4]], axis = 1)
final['target'].replace({1:0 , 0:1} , inplace=True)



