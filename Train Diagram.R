library(xml2)
library(dplyr)
library(sf)
library(ggplot2)
library(ggsflabel)
library(httr)
library(lwgeom)
library(maptools)
library(nngeo)
library(reshape2)
library(data.table)
library(stringr)
library(plotly)
library(tmap)

tmap_mode("view")

windowsFonts(A=windowsFont("Times New Roman"))
windowsFonts(B=windowsFont("標楷體"))

# PTX api
get_ptx_data <- function (app_id, app_key, url){
  # Set the locale of Liniux
  Sys.setlocale("LC_ALL","C")
  
  # "Tue, 21 Aug 2018 01:18:42 GMT"
  xdate <- format(as.POSIXlt(Sys.time(), tz = "GMT"), "%a, %d %b %Y %H:%M:%S GMT")
  sig <- hmac_sha1(app_key, paste("x-date:", xdate)) 
  
  # hmac username="APP ID", algorithm="hmac-sha1", headers="x-date", 
  # signature="Base64(HMAC-SHA1("x-date: " + x-date , APP Key))"
  
  authorization <- paste0(
    'hmac username="', app_id, '", ',
    'algorithm="hmac-sha1", ',
    'headers="x-date", ',
    'signature="', sig, '\"', sep = '')
  
  auth_header <- c(
    'Authorization'= authorization,
    'x-date'= as.character(xdate))
  
  dat <- GET(url, 
             config = httr::config(ssl_verifypeer = 0L), 
             add_headers(.headers = auth_header))
  
  print(http_status(dat)$message)
  
  # Set back to origin locale
  Sys.setlocale(category = "LC_ALL", locale = "cht")
  
  # return(dat)
  return(content(dat))
}

app_id = '8f35504e01eb4a43abfd41c920955690'
app_key = 'H9MfljykHDeGiifyr2zKJ0XsKFQ'


# 當日時刻表資料
url="https://ptx.transportdata.tw/MOTC/v2/Rail/THSR/DailyTimetable/Today?&$format=XML"
x=get_ptx_data(app_id, app_key, url)

HSR_info=data.frame(TrainNo=xml_text(xml_find_all(x, xpath = "//d1:TrainNo")),
                    Direction=xml_text(xml_find_all(x, xpath = "//d1:Direction")),
                    StartingStationID=xml_text(xml_find_all(x, xpath = "//d1:StartingStationID")),
                    StartingStationName=xml_text(xml_find_all(x, xpath = "//d1:StartingStationName//d1:Zh_tw")),
                    EndingStationID=xml_text(xml_find_all(x, xpath = "//d1:EndingStationID")),
                    EndingStationName=xml_text(xml_find_all(x, xpath = "//d1:EndingStationName//d1:Zh_tw")))

num_of_sch=xml_length(xml_find_all(x, xpath = ".//d1:StopTimes"))

HSR_sch_temp=data.frame(StopSequence=xml_text(xml_find_all(x, xpath = "//d1:StopSequence")),
                        StationID=xml_text(xml_find_all(x, xpath = "//d1:StationID")),
                        StationName=xml_text(xml_find_all(x, xpath = "//d1:StationName//d1:Zh_tw")),
                        ArrivalTime=xml_text(xml_find_all(x, xpath = "//d1:ArrivalTime")),
                        DepartureTime=xml_text(xml_find_all(x, xpath = "//d1:DepartureTime")))

HSR_sch=data.frame()
for (i in c(1:length(num_of_sch))){
  sec_head=sum(num_of_sch[0:(i-1)])+1
  sec_tail=sum(num_of_sch[0:i])
  HSR_sch=rbind(HSR_sch, cbind(HSR_info[i,], HSR_sch_temp[c(sec_head:sec_tail),]))
}
rm(HSR_info, HSR_sch_temp, sec_head, sec_tail, num_of_sch)




# 圖資資料 (計算各站點間距離)

url="https://ptx.transportdata.tw/MOTC/v2/Rail/THSR/Station?&$format=XML"
x=get_ptx_data(app_id, app_key, url)

HSR_station=data.frame(StationID=xml_text(xml_find_all(x, xpath="//d1:StationID")),
                       StationName=xml_text(xml_find_all(x, xpath="//d1:StationName//d1:Zh_tw")),
                       PositionLon=xml_text(xml_find_all(x, xpath="//d1:PositionLon")),
                       PositionLat=xml_text(xml_find_all(x, xpath="//d1:PositionLat")))

HSR_station=mutate(HSR_station, Geometry=st_as_sfc(paste0("POINT(", PositionLon, " ", PositionLat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)

url="https://ptx.transportdata.tw/MOTC/v2/Rail/THSR/Shape?&$format=XML"
x=get_ptx_data(app_id, app_key, url)

HSR_shape_temp=data.frame(Geometry=xml_text(xml_find_all(x, xpath="//d1:THSRShape")))
HSR_shape_temp=mutate(HSR_shape_temp, Geometry=st_as_sfc(Geometry))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)


# 利用站點裁切路線 (先找尋到站點投影在路線上的點，將其擴展非常細長的buffer，做為切割元件，最後在擷取其中的linestring)
nrst=st_nearest_points(HSR_station, HSR_shape_temp)
on_line_all=st_cast(nrst, "POINT")
buf_all=st_combine(st_buffer(on_line_all, 0.1))
HSR_shape=st_collection_extract(st_split(HSR_shape_temp, buf_all), "LINESTRING")
rm(nrst, on_line_all, buf_all, HSR_shape_temp)

HSR_shape=mutate(st_sf(HSR_shape), id=c(1:25))

# 計算&檢查里程數 (由於圖資中含有端點至機廠的路段，必須將其排除，故設定路段>2000公尺者尚為營運路段)
HSR_shape=mutate(HSR_shape, length=as.numeric(st_length(HSR_shape)))%>%
  filter(length>2000)

HSR_station=mutate(HSR_station, temp_id=c(1:nrow(HSR_station)))
temp=st_nn(HSR_shape, HSR_station, k=2)
temp=data.frame(id=HSR_shape$id, t(data.frame(temp)), row.names=NULL)%>%
  mutate(id_O=ifelse(X1<X2, X1, X2),
         id_D=ifelse(X1<X2, X2, X1))%>%
  select(id, id_O, id_D)%>%
  arrange(id_O)%>%
  left_join(st_drop_geometry(HSR_station)[, c("StationName","temp_id")], by=c("id_O"="temp_id"))%>%
  left_join(st_drop_geometry(HSR_station)[, c("StationName","temp_id")], by=c("id_D"="temp_id"))

temp$id=as.character(temp$id)
HSR_shape$id=as.character(HSR_shape$id)
HSR_shape=left_join(HSR_shape, temp)%>%
  arrange(id_O)
rm(temp)

tm_shape(HSR_shape)+
  tm_lines(col="id", lwd=5)+
  tm_shape(HSR_station)+
  tm_dots()



cumdist=data.frame(StationName=c(HSR_shape$StationName.x, "左營"), cumdist=c(0, cumsum(HSR_shape$length))/1000)

HSR_sch_rev=reshape2::melt(HSR_sch, id.vars=c("TrainNo","Direction","StartingStationName","EndingStationName","StopSequence","StationID","StationName"),
                           measure.vars=c("ArrivalTime","DepartureTime"))%>%
  left_join(cumdist)%>%
  rename(A_D=variable, Time=value)%>%
  mutate(Time=as.numeric(substr(Time, 1, 2))*60+as.numeric(substr(Time, 4, 5)))


ggplotly(
  ggplot()+
    geom_line(data=HSR_sch_rev, aes(x=Time, y=cumdist, group=TrainNo, color=as.character(substr(TrainNo, 2, 2))))+
    scale_color_manual(values=c("1"="#FFB5B5", "2"="#B9B973", "3"="#B9B9FF", "5"="#00CACA", "6"="#53FF53", "8"="#FFBB77"))+
    geom_text(data=cumdist, aes(x=300, y=cumdist, label=StationName), family="B", size=5, fontface="bold")+
    scale_x_continuous(limits=c(300,1450), breaks=seq(360, 1410, 30),
                       labels=paste0(str_pad(seq(360, 1410, 30)%/%60, 2, side="left", pad="0"), ":", str_pad(seq(360, 1410, 30)%%60, 2, side="left", pad="0")))+
    scale_y_reverse()+
    theme_minimal()+
    theme(axis.text.x=element_text(size=12, family="A", angle=90, face="bold"),
          axis.text.y=element_blank(),
          axis.title=element_blank())
)





