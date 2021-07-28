### Loading in RDS/Rdata file

mz <- readRDS("processed_data/mouse_zones.Rdata")
names(mz) <- c("mouse1", "mouse2", "mouse3", "mouse4")

lapply(mz, head)

