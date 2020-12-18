# nocov start
#' @noRd
#' @title droplet_ip
#' @description This funciton returns the IP adress of a droplet
#' @param x droplet - selected droplet
droplet_ip <- function(x) {
  v4 <- x$network$v4
  if (length(v4) == 0) {
    stop("No network interface registered for this droplet\n  Try refreshing like: droplet(d$id)",
         call. = FALSE
    )
  }
  ips <- do.call("rbind", lapply(v4, as.data.frame))
  public_ip <- ips$type == "public"
  if (!any(public_ip)) {
    ip <- v4[[1]]$ip_address
  } else {
    ip <- ips$ip_address[public_ip][[1]]
  }
  ip
}
# nocov end

#' @noRd
#' @title cn
#' @description Simple helper function
#' @param x string
#' @param y string
cn <- function(x, y) if (nchar(y) == 0) y else paste0(x, y)
