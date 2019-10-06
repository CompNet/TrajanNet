#############################################################################################
# Allows to control the width of circle when plotting graphs with igraph.
# 
# Code source by Gábor Csárdi, 2009-09-11, retrieved from
# https://lists.gnu.org/archive/html/igraph-help/2009-09/msg00019.html
#############################################################################################

.igraph.shape.circle2 <- function(coords, el=NULL, v=NULL,
		mode=c("clip", "plot"),
		params, end=c("both", "from", "to")) {
	
	mode=match.arg(mode)
	end =match.arg(end)
	
	#####################################################################
	## clipping mode
	
	if (mode=="clip") {
		if (length(coords)==0) { return (coords) }
		
		vertex.size <- 1/200 * params("vertex", "size")
		
		if (end=="from") {
			phi <- atan2(coords[,4] - coords[,2], coords[,3] - coords[,1])
			vsize.from <- if (length(vertex.size)==1) {
						vertex.size
					} else {
						vertex.size[ el[,1]+1 ]
					}
			res <- cbind(coords[,1] + vsize.from*cos(phi),
					coords[,2] + vsize.from*sin(phi) )
		} else if (end=="to") {
			phi <- atan2(coords[,4] - coords[,2], coords[,3] - coords[,1])
			r <- sqrt( (coords[,3] - coords[,1])^2 + (coords[,4] - coords[,2])^2 )
			vsize.to <- if (length(vertex.size)==1) {
						vertex.size
					} else {
						vertex.size[ el[,2]+1 ]
					}
			res <- cbind(coords[,1] + (r-vsize.to)*cos(phi),
					coords[,2] + (r-vsize.to)*sin(phi) )
		} else if (end=="both") {
			phi <- atan2(coords[,4] - coords[,2], coords[,3] - coords[,1])
			r <- sqrt( (coords[,3] - coords[,1])^2 + (coords[,4] - coords[,2])^2 )
			vsize.from <- if (length(vertex.size)==1) {
						vertex.size
					} else {
						vertex.size[ el[,1]+1 ]
					}
			vsize.to <- if (length(vertex.size)==1) {
						vertex.size
					} else {
						vertex.size[ el[,2]+1 ]
					}
			res <- cbind(coords[,1] + vsize.from*cos(phi),
					coords[,2] + vsize.from*sin(phi),
					coords[,1] + (r-vsize.to)*cos(phi),
					coords[,2] + (r-vsize.to)*sin(phi) )
		}
		
		res
		
		#####################################################################
		## plotting mode
		
	} else if (mode=="plot") {
		vertex.color       <- params("vertex", "color")
		if (length(vertex.color) != 1 && !is.null(v)) {
			vertex.color <- vertex.color[v+1]
		}
		vertex.frame.color <- params("vertex", "frame.color")
		if (length(vertex.frame.color) != 1 && !is.null(v)) {
			vertex.frame.color <- vertex.frame.color[v+1]
		}
		vertex.size        <- 1/200 * params("vertex", "size")
		if (length(vertex.size) != 1 && !is.null(v)) {
			vertex.size <- vertex.size[v+1]
		}
		vertex.size <- rep(vertex.size, length=nrow(coords))
		
		vertex.frame.width <- params("vertex", "frame.width")
		vertex.frame.width <- rep(vertex.frame.width, length=nrow(coords))
		
		vertex.color <- rep(vertex.color, length=nrow(coords))
		vertex.frame.color <- rep(vertex.frame.color, length=nrow(coords))
		vertex.size <- rep(vertex.size, length=nrow(coords))
		
		for (i in seq_len(nrow(coords))) {
			symbols(x=coords[i,1], y=coords[i,2], bg=vertex.color[i],
					fg=vertex.frame.color[i],
					circles=vertex.size[i], add=TRUE, inches=FALSE,
					lwd=vertex.frame.width[i])
		}
	}
	
}

.igraph.shapes <- get( ".igraph.shapes", asNamespace("igraph"))
.igraph.shapes[["circle2"]] <- .igraph.shape.circle2
unlockBinding(".igraph.shapes", asNamespace("igraph"))
assign(".igraph.shapes", .igraph.shapes, envir=asNamespace("igraph"))

i.default.values <- get("i.default.values", asNamespace("igraph"))
i.default.values$vertex$frame.width=1
unlockBinding("i.default.values", asNamespace("igraph"))
assign("i.default.values", i.default.values, envir=asNamespace("igraph"))
