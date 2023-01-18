#
#  OC_in_R_2008.r
#
#  Program to Run R Version of OC
#
#
#  Remove all objects just to be safe
#
rm(list=ls(all=TRUE))
#
library(pscl)
library(oc)
library(gdata)
#
#
#
nrollcall <- 679
#
hr <- readKH("c:/roll_call_displays/s111_dec_18B.ord",
       dtl=NULL,
       yea=c(1,2,3),
       nay=c(4,5,6),
       missing=c(7,8,9),
       notInLegis=0,
       desc="111th Senate Roll Call Data",
       debug=FALSE)
#
#
#  Call OC
#
#  Example:  1-Dim
#
#result <- oc(sen90, dims=1, polarity=c(1))
#  
#  Example:  2-Dim
#
result <- oc(hr, dims=2, polarity=c(1,9))
summary(result)
windows()
plot(result)
#
#  ---- Useful Commands To See What is in an Object
#
# > length(result)
# [1] 5
# > class(result)
# [1] "OCobject"
# > names(result)
# [1] "legislators" "rollcalls"   "dimensions"  "eigenvalues" "fits"       
# 
# result$legislators
# result$rollcalls
# result$dimensions
# result$eigenvalues
# result$fits
#
#  Legislators
#
write.fwf(x=format(as.data.frame(result$legislators),digits=5,width=10,
   scientific=FALSE),"c:/roll_call_displays/S111_Senators.txt")
#
#  Roll Calls
#
write.fwf(x=format(as.data.frame(result$rollcalls),digits=5,width=10,
   scientific=FALSE),"c:/roll_call_displays/S111_RollCalls.txt")
#
#
result999 <- ifelse(is.na(result$rollcalls),999,result$rollcalls)
nvotescaled <- sum(result999[,7]!=999)
#
ws <- result$rollcalls[,8]
N1 <- result$rollcalls[,6]
N2 <- result$rollcalls[,7]
#
# result$legislators$coord1D
#result9999 <- ifelse(is.na(result$legislators),999,result$legislators)
#nlegscaled <- sum(result9999[,7]!=999)
oc1 <-  result$legislators[,13]
oc2 <-  result$legislators[,14]
party <- result$legislators[,6]
state <- result$legislators[,2]
#
windows()
par(mfrow=c(1,2))
#
plot(oc1,oc2,type="n",asp=1,
       main="",
       xlab="",
       ylab="",
       xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),cex=1.2,font=2)
#
# Main title
mtext("OC Plot of 111th Senate (2009-10)\nSenator Ideal Points",side=3,line=1.50,cex=1.2,font=2)
# x-axis title
# x-axis title
mtext("D=Northern Democrat, S=Southern Democrat\nR=Republican",side=1,line=3.25,cex=1.2)
#mtext("Liberal - Conservative",side=1,line=2.75,cex=1.2)
# y-axis title
mtext("Social/Lifestyle Issues",side=2,line=2.5,cex=1.2)
#
#
#  Southern Democrats
points(oc1[party == 100 & state >= 40 & state <= 51],oc2[party == 100 & state >= 40 & state <= 51],pch='S',col="red",font=2)
points(oc1[party == 100 & state == 53],oc2[party == 100 & state == 53],pch='S',col="red",font=2)
points(oc1[party == 100 & state == 54],oc2[party == 100 & state == 54],pch='S',col="red",font=2)
#  Northern Democrats
points(oc1[party == 100 & (state < 40 | state > 54)],oc2[party == 100 & (state < 40 | state > 54)],pch='D',col="red",font=2)
points(oc1[party == 100 & state == 52],oc2[party == 100 & state == 52],pch='D',col="red",font=2)
#  Republicans
points(oc1[party == 200 & state != 99],oc2[party == 200 & state != 99],pch='R',col="blue",font=2)
#
#
# pos -- a position specifier for the text. Values of 1, 2, 3 and 4, 
# respectively indicate positions below, to the left of, above and 
# to the right of the specified coordinates 
#
# President Bush
#
#points(oc1[state == 99],oc2[state == 99],pch='P',col="black",font=2)
#
plot(N1,N2,type="n",asp=1,
       main="",
       xlab="",
       ylab="",
       xlim=c(-1.0,1.0),ylim=c(-1.0,1.0),cex=1.2,font=2)
#
# Main title
mtext("OC Plot of 111th Senate (2009-10)\nCoombs Mesh from Cutting Lines",side=3,line=1.50,cex=1.2,font=2)
# x-axis title
mtext("Liberal - Conservative",side=1,line=2.75,cex=1.2)
# y-axis title
mtext("Social/Lifestyle Issues",side=2,line=2.5,cex=1.2)
#
#
#  Set Length of Arrows off ends of Cutting Lines
#
#xlarrow <- 0.1
xlarrow <- 0.0
#
#
i <- 1
while (i <= length(ws)){
     if(result999[i,7]!=999){
 #  Plot Cutting Line
#
#  This computes the point on the Normal Vector 
#    Through which the Cutting Line passes
#
xws <- ws[i]*N1[i]
yws <- ws[i]*N2[i]
#
#  This computes the Cutting Line
#
arrows(xws,yws,xws+N2[i],yws-N1[i],length=0.0,lwd=2,col="black")
arrows(xws,yws,xws-N2[i],yws+N1[i],length=0.0,lwd=2,col="black")
#
#  TT[i,9] is the Choice Above the Cutting Point
#
#   ( xwslow, ywslow ) is a point on the
#                      normal vector above/below the cutting point
#                      by xlarrow units.  This allows the computation
#                      of points to draw the arrows to from the ends
#                      of the normal vectors
#
#if(TT[i,9] == 6){
#   xwslow <- (ws[i]- xlarrow)*N1[i]
#   ywslow <- (ws[i]- xlarrow)*N2[i]
#}
#if(TT[i,9] == 1){
#   xwslow <- (ws[i]+ xlarrow)*N1[i]
#   ywslow <- (ws[i]+ xlarrow)*N2[i]
#}
#
#  ( xws+N2[i] , yws-N1[i] ) and
#  ( xws-N2[i] , yws+N1[i] ) are the coordinates for the two points
#                            at the ends of the cutting line
#
#  ( xwslow+N2[i],ywslow-N1[i] ) and
#  ( xwslow-N2[i],ywslow+N1[i] ) are the points above/below the
#                                endpoints of the cutting line
#                                to which the arrows are drawn
#
#arrows(xws+N2[i],yws-N1[i],xwslow+N2[i],ywslow-N1[i],length=0.1,lwd=2,col="red")
#arrows(xws-N2[i],yws+N1[i],xwslow-N2[i],ywslow+N1[i],length=0.1,lwd=2,col="red")
#
}
i <- i + 1
}
#
#  Democrats
points(oc1[party == 100 & state != 99],oc2[party == 100 & state != 99],pch=16,col="red",font=2)
#  Republicans
points(oc1[party == 200 & state != 99],oc2[party == 200 & state != 99],pch=16,col="blue",font=2)
