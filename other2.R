pin_layout <- function(l, s) { # layout and shape
  # package management
  require(stringr)
  
  # parameters
  len_l <- length(l)
  n_pins_per_side <- (len_l - 1) * 2 + 1
  
  # bottom-right (br), bottom-half (bh) and top-half (th)
  if (shape == "1/8") {
    br_map <- matrix(as.numeric(str_split_fixed(l, " ", len_l)), len_l, len_l)
    br_map[is.na(br_map)] <- t(br_map)[is.na(br_map)]
    bl_map <- br_map[,len_l:1]
    bh_map <- cbind(bl_map[,-len_l], br_map)
    th_map <- matrix(rev(bh_map), ncol = n_pins_per_side)
    map <- rbind(th_map[-len_l,], bh_map)
    return(map)
  } else if (shape == "1/4") {
    br_map <- matrix(as.numeric(str_split_fixed(l, " ", len_l)), len_l, len_l)
    bl_map <- br_map[,len_l:1]
    bh_map <- cbind(bl_map[,-len_l], br_map)
    th_map <- matrix(rev(bh_map), ncol = n_pins_per_side)
    map <- rbind(th_map[-len_l,], bh_map)
  } else {
    msg <- paste("Error: shape", shape, "not supported.")
    return(msg)
  }
  
}
