##############################################################################
########################## CALL NECESSARY LIBRARIES ##########################
##############################################################################

library(Hmisc)
library(foreign)
library(sp)
library(rgdal)
library(raster)
options(stringsAsFactors=FALSE)

##############################################################################
########################## READ THE DATA           ##########################
##############################################################################

rast_loss  <- raster("data/resampled_loss.tif")
rast_gain  <- raster("data/resampled_gain.tif")
rast_mask  <- raster("data/gadm_ethiopia.tif")
rast_tcov  <- raster("data/gfc_ethiopia_tc.tif")

pts_loss   <- read.table("data/pts_loss.txt")
pts_gain   <- read.table("data/pts_gain.txt")

#pts_loss   <- rasterToPoints(rast_loss,fun=function(rast){rast==9})
#pts_gain   <- rasterToPoints(rast_gain,fun=function(rast){rast==9})

df_pts_loss<- as.data.frame(pts_loss)
df_pts_gain<- as.data.frame(pts_gain)

#write.table(pts_loss,"pts_loss.txt")
#write.table(pts_gain,"pts_gain.txt")

tot_nb_rand  <- 8000

# ######### Draw $tot_nb_rand points within the extension of the raster 
my_sample      <- data.frame(sampleRandom(rast_loss,tot_nb_rand,xy=TRUE))
# ######### Extract values of the mask on point locations
my_sample$mask <- extract(rast_mask,my_sample[,c(1:2)])
my_sample$tcov <- extract(rast_tcov,my_sample[,c(1:2)])

# ######### Change the names, check the structure again
names(my_sample) <- c("x_coord","y_coord","loss","mask","tcov")

# ######### Add a column with row number to get unique identifiers
my_sample$id<-row(my_sample)[,1]

shinyServer(
  # ######### Interactive data parameters
  function(input, output) {
    
################################################    
################ Reactive variable : all_points
################################################
all_points <- reactive({
                  nb_loss      <- input$sampling_rate_loss
                  nb_gain      <- input$sampling_rate_gain
                  tc_threshold <- input$treecover

                  nb_noloss_F <- min(
                                    floor(input$sampling_rate_noloss*1/3),
                                    nrow(my_sample[my_sample$tcov>=tc_threshold & my_sample$loss==0 & my_sample$mask==1,])
                                    )
                  
                  nb_noloss_NF <- input$sampling_rate_noloss -  nb_noloss_F
                  
                  # ######### Randomly select points with no_loss () 
                  # ######### and within the country mask (value 1) and TreeCover >= Threshold
                  noloss_F_pts<-my_sample[
                    my_sample$id 
                    %in% 
                      sample(my_sample[
                        my_sample$tcov>=tc_threshold 
                        & my_sample$loss==0
                        & my_sample$mask==1 
                        ,]$id,
                        nb_noloss_F)
                    ,]

                  # ######### Take $nb_noloss_NF points in the no_loss layer with no tree
                  noloss_NF_pts<-my_sample[
                    my_sample$id 
                    %in% 
                      sample(my_sample[
                        my_sample$tcov==0 
                        & my_sample$loss==0 
                        & my_sample$mask==1
                        ,]$id,
                        nb_noloss_NF)
                    ,]
                  
                  # ######### sample the loss layer
                  names(df_pts_loss) <- c("x_coord","y_coord","value")
                  df_pts_loss$id<-row(df_pts_loss)[,1]
                  loss_pts<-df_pts_loss[df_pts_loss$id %in% sample(df_pts_loss$id,nb_loss),]
                  
                  # ######### sample the gain layer
                  names(df_pts_gain) <- c("x_coord","y_coord","value")
                  df_pts_gain$id<-row(df_pts_gain)[,1]
                  gain_pts<-df_pts_gain[df_pts_gain$id %in% sample(df_pts_gain$id,nb_gain),]
                  
                  # ######### Change the column value with value 1 for "loss", 2 for "no_loss", 3 for "gains"
                  x1 <- noloss_F_pts[,c(1,2,3,6)]
                  x2 <- noloss_NF_pts[,c(1,2,3,6)]
                  x3 <- loss_pts
                  x4 <- gain_pts
                  names(x1) <- names(x2) <- names(x3) <- names(x4)<- c("x_coord","y_coord","value","id")
                  
                  x1$value <- 4
                  x2$value <- 2
                  x3$value <- 1
                  x4$value <- 3
                  
                  # ######### Combine the three datasets: gain loss and stable
                  all_points<-rbind(x1,x2)
                  all_points<-rbind(all_points,x3)
                  all_points<-rbind(all_points,x4)
                  all_points
                  })

################################################    
################ OUtput item        : map output
################################################
    
    output$map_pts <- renderPlot({
                         dfa<-all_points()
                         plot(my_sample$x_coord,my_sample$y_coord,type="n",xlab="longitude",ylab="latitude")
                         rasterImage(as.raster(rast_mask),xmin(rast_mask),ymin(rast_mask),xmax(rast_mask),ymax(rast_mask),bg="red")
                         points(dfa[dfa$value==4,]$x_coord,dfa[dfa$value==4,]$y_coord,col="green",pch=19)
                         points(dfa[dfa$value==2,]$x_coord,dfa[dfa$value==2,]$y_coord,col="grey",pch=19)
                         points(dfa[dfa$value==1,]$x_coord,dfa[dfa$value==1,]$y_coord,col="red",pch=19)
                         points(dfa[dfa$value==3,]$x_coord,dfa[dfa$value==3,]$y_coord,col="blue",pch=19)
                            })
#################################################    
################ OUtput item       : Table output
#################################################

    output$table_stable <- renderTable({
      my_sample$treed <- 0
      my_sample[my_sample$tcov >= input$treecover,]$treed <- 1
      my_table<-table(my_sample[my_sample$loss==0,]$treed)
      names(my_table) <- c("Without tree","With tree")
      my_table
                                   })
#################################################    
################ OUtput item       : Sampling 
#################################################

      output$table_final <- renderTable({
        dfa<-all_points()
        my_table_final <- table(dfa$value)
        names(my_table_final)<-c("loss","noloss_NF","gain","noloss_F")
        my_table_final
        })


##################################################   
################ Reactive variable : Vector output
##################################################

sp_df <- reactive({
        dfa<-all_points()
        sp_df<-SpatialPointsDataFrame(
          coords=dfa[,c(1,2)],
          data=data.frame(dfa[,c(4,3)]),
          proj4string=CRS("+proj=longlat +datum=WGS84")
                                     )
      })

###################################################    
################ Output item : KML for fusion table
###################################################

    output$downloadData <- downloadHandler(
      filename = function(){
        paste(input$basename,".kml",sep="")},
      content  = function(file){
        to_export<-sp_df()
        writeOGR(obj=to_export,dsn=file,layer=input$basename,driver = "KML")}
      )
  }
)
