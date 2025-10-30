# Cluster Analysis
# Data : Utilities

mydata = read.csv("Utilities.csv") #sales / fuelcost 만 실습목적으로 이용
row.names(mydata) <- mydata[,1] #text labeling
mydata <- mydata[,c(8,10)] #sales/fuelcost만 이용
mydata
plot(mydata,pch=20) #scattered plot으로 2차원에서 보기(sales/fuelcost 2차원)
text(mydata,labels=row.names(mydata),pos=1,offset=0.3,cex=0.5) #textlabel표시


## [1] K-means

km.res.ns <- kmeans(mydata,2,nstart=20) #k=2, 20번 시도 (local minimum 중 제일 낮은 sum of squares 구하기위해)
km.res.ns #비율이 1에 가까울수록 바람직
km.res.ns$cluster
plot(mydata,col=km.res.ns$cluster,pch=20,main="K-Means without scaling")
text(mydata,labels=row.names(mydata),col=km.res.ns$cluster,pos=1,offset=0.3,cex=0.5) 
#그려보니까 sales기준으로만cluster됨 (scaling안되어서 단위큰 sales에만 영향받음>>fuelcost무시됨)

#scaling하기
mydata.sc <- scale(mydata)
mydata.sc

#scaling된 data로 다시 k-means clustering
km.res.2 <- kmeans(mydata.sc,2,nstart=20)
km.res.2
plot(mydata,col=km.res.2$cluster,pch=20,main="K-Means with scaling")
text(mydata,labels=row.names(mydata),col=km.res.2$cluster,pos=1,offset=0.3,cex=0.5)

km.res.3 <- kmeans(mydata.sc,3,nstart=20)
km.res.4 <- kmeans(mydata.sc,4,nstart=20)
km.res.5 <- kmeans(mydata.sc,5,nstart=20)
km.res.6 <- kmeans(mydata.sc,6,nstart=20)

#show multiple plots together : "par" function
par(mfrow=c(2,2)) #multiframe by row (mfcol로 세로로채우기도가능)

plot(mydata,col=km.res.2$cluster,pch=20,main="K-Means with 2 Clusters")
text(mydata,labels=row.names(mydata),col=km.res.2$cluster,pos=1,offset=0.3,cex=0.5)
plot(mydata,col=km.res.3$cluster,pch=20,main="K-Means with 3 Clusters")
text(mydata,labels=row.names(mydata),col=km.res.3$cluster,pos=1,offset=0.3,cex=0.5)
plot(mydata,col=km.res.4$cluster,pch=20,main="K-Means with 4 Clusters")
text(mydata,labels=row.names(mydata),col=km.res.4$cluster,pos=1,offset=0.3,cex=0.5)
plot(mydata,col=km.res.5$cluster,pch=20,main="K-Means with 5 Clusters")
text(mydata,labels=row.names(mydata),col=km.res.5$cluster,pos=1,offset=0.3,cex=0.5)

#k값을 몇으로할것인가? >> elbow chart에서 elbow point 고르기 (cost-benefit analysis)
#objective function : total within cluster sum of squares
#compute total within cluster sum of squares under different k values (tot.withiss)
ss1 <- km.res.2$totss #one cluster일때는 전체거리의 제곱합
ss2 <- km.res.2$tot.withinss
ss3 <- km.res.3$tot.withinss
ss4 <- km.res.4$tot.withinss
ss5 <- km.res.5$tot.withinss
ss6 <- km.res.6$tot.withinss
ss.vec <-c(ss1,ss2,ss3,ss4,ss5,ss6)
par(mfrow=c(1,1))
plot(ss.vec, type="b", xlab="Number of Clusters", ylab="Total within-Cluster SS") #type b는 both 의미 (dots/lines를 둘다 보여주기)

#k=4를 optimal로 정함
plot(mydata,col=km.res.4$cluster,pch=20,main="K-Means with 4 Clusters")
text(mydata,labels=row.names(mydata),col=km.res.4$cluster,pos=1,offset=0.3,cex=0.5)


## [2] Hierarchical Clustering (실무적으로 average/complete linkage 많이씀)

hc.res.average <- hclust(dist(mydata.sc), method="average") #scaled data이용
plot(hc.res.average, cex=0.5, main="Dendrogram (Average Linkage)", xlab="",ylab="",sub="")
abline(h=1.14,col="red",lty=2) #4개 cluster를 만들려면 h=1.14정도 끊어야함

hc.res.centroid <- hclust(dist(mydata.sc), method="centroid") #scaled data이용
plot(hc.res.centroid, cex=0.5, main="Dendrogram (Centroid Linkage)", xlab="",ylab="",sub="")
abline(h=0.805,col="red",lty=2)
#centroid를 돌리면 inversion현상 발생 가능(군집거리의 비단조성) 
#맨마지막 cluster에서 inversion현상 관측됨 >> 실무적으로 centroid잘안씀

hc.res.single <- hclust(dist(mydata.sc), method="single") #scaled data이용
plot(hc.res.single, cex=0.5, main="Dendrogram (Single Linkage)", xlab="",ylab="",sub="")
abline(h=0.71,col="red",lty=2)
#single을 돌리면 trailing cluster(꼬리형 군집)이 생김(군집이 제대로 분리되지 않음>>균형잡힌 나무가 아니라 한쪽으로 기울어진 덩굴모양됨)
#맨마지막 cluster에서 trailing cluster현상 관측됨 >> 실무적으로 single도 잘안씀

hc.res.complete <- hclust(dist(mydata.sc), method="complete") #scaled data이용
plot(hc.res.complete, cex=0.5, main="Dendrogram (Complete Linkage)", xlab="",ylab="",sub="")
abline(h=1.6,col="red",lty=2)

#실제로 k=4로 자르기
cutree(hc.res.average,k=4)

par(mfrow=c(1,2)) #k-means와 Hierarchical Clustering 비교
plot(mydata,col=km.res.4$cluster,pch=20,main="K-Means with 4 Clusters")
text(mydata,labels=row.names(mydata),col=km.res.4$cluster,pos=1,offset=0.3,cex=0.5)
plot(mydata,col=cutree(hc.res.average,k=4), pch=20,main="Hierarchical Clustering (Average Linkage)")
text(mydata,labels=row.names(mydata),col=cutree(hc.res.average,k=4),pos=1,offset=0.3,cex=0.5)

par(mfrow=c(2,2)) #Hierarchical Clustering (실무적으로 average/complete linkage 많이씀)
plot(mydata,col=cutree(hc.res.average,k=4), pch=20,main="Hierarchical Clustering (Average Linkage)")
text(mydata,labels=row.names(mydata),col=cutree(hc.res.average,k=4),pos=1,offset=0.3,cex=0.5)
plot(mydata,col=cutree(hc.res.complete,k=4), pch=20,main="Hierarchical Clustering (Complete Linkage)")
text(mydata,labels=row.names(mydata),col=cutree(hc.res.complete,k=4),pos=1,offset=0.3,cex=0.5)
plot(mydata,col=cutree(hc.res.single,k=4), pch=20,main="Hierarchical Clustering (Single Linkage)")
text(mydata,labels=row.names(mydata),col=cutree(hc.res.single,k=4),pos=1,offset=0.3,cex=0.5)
plot(mydata,col=cutree(hc.res.centroid,k=4), pch=20,main="Hierarchical Clustering (Centroid Linkage)")
text(mydata,labels=row.names(mydata),col=cutree(hc.res.centroid,k=4),pos=1,offset=0.3,cex=0.5)
