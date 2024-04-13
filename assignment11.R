library(sf)
library(terra)
library(spData)
library(spDataLarge)
# 读取开发区的地理要素数据
hefei_eno_zone <- st_read("D:/博1/科研/应用经济学/G341022合肥经济技术开发区.txt") 
hefei_zone_1 <- st_read("D:/博1/科研/应用经济学/G342020合肥高新技术产业开发区区块一.txt")
hefei_zone_2 <- st_read("D:/博1/科研/应用经济学/G342020合肥高新技术产业开发区区块二.txt")
# 读取企业信息
hefei_enterprise <- read.table("D:/博1/科研/应用经济学/hefei.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# 转换为 sf 对象
hefei_enterprise_sf <- st_as_sf(hefei_enterprise, coords = c("lng", "lat"), crs = 4326)
# 判断企业是否在开发区内
enterprise_in_eno_zone <- st_within(hefei_enterprise_sf, hefei_eno_zone)
enterprise_in_eno_zone_lo <- sapply(enterprise_in_eno_zone, function(x) any(x == 1))
enterprise_in_zone1 <- st_within(hefei_enterprise_sf, hefei_zone_1)
enterprise_in_eno_zone1_lo <- sapply(enterprise_in_zone1, function(x) any(x == 1))
enterprise_in_zone2 <- st_within(hefei_enterprise_sf, hefei_zone_2)
enterprise_in_zone2_lo <- sapply(enterprise_in_zone2, function(x) any(x == 1))
# 计算企业在开发区内的数量
enterprise_count_in_eno_zone <- sum(sapply(enterprise_in_eno_zone, length))
enterprise_count_in_zone1 <- sum(sapply(enterprise_in_zone1, length))
enterprise_count_in_zone2 <- sum(sapply(enterprise_in_zone2, length))
# 
eno_zone <- hefei_enterprise_sf[enterprise_in_eno_zone_lo,]


#绘制
library(ggplot2)

# 创建一个空的地图对象
map <- ggplot() +
  # 添加合肥经济技术开发区的边界
  geom_sf(data = hefei_eno_zone, fill = "transparent", color = "blue") +
  
  # 添加合肥高新技术产业开发区区块一的边界
  geom_sf(data = hefei_zone_1, fill = "transparent", color = "red") +
  
  # 添加合肥高新技术产业开发区区块二的边界
  geom_sf(data = hefei_zone_2, fill = "transparent", color = "green") +
  
  # 添加在合肥经济技术开发区内的企业位置
  geom_sf(data = hefei_enterprise_sf[enterprise_in_eno_zone_lo,], color = "black", size = 1) +
  
  # 添加在合肥高新技术产业开发区区块一内的企业位置
  geom_sf(data = hefei_enterprise_sf[enterprise_in_zone1_lo,], color = "black", size = 1) +
  
  # 添加在合肥高新技术产业开发区区块二内的企业位置
  geom_sf(data = hefei_enterprise_sf[enterprise_in_zone2_lo,], color = "black", size = 1)

# 显示地图
print(map)

