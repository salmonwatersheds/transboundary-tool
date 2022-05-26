###############################################################################
# Packages
###############################################################################

library(shiny)
library(shinyBS)
library(shinyWidgets)
library(leaflet)
library(rgdal)
library(sjmisc)
library(PNWColors)
library(tools) # for toTitleCase
library(rsconnect)
library(sp)
library(mapview)
library(htmlwidgets)
library(knitr)
library(rmarkdown)
library(maptools)
library(webshot)

webshot::install_phantomjs()



# gpclibPermit()
###############################################################################
# Load data
###############################################################################

tbrDat <- read.csv("data/TBREscapement.csv")

yrs <- c(1950:2019)
indYears <- c(which(names(tbrDat) == paste0("X", min(yrs))):which(names(tbrDat) == paste0("X", max(yrs))))

#------------------------------------------------------------------------------
# Color scheme
#------------------------------------------------------------------------------

abundCol <- rev(pnw_palette("Bay", n = 10))
abundCut <- seq(0, 15, length.out = 11)
abundCutExp <- c(1, 5, 20, 90, 400, 1800, 8100, "36k", "160k", "730k", "3.3mil")

catCol <- c(abundCol[c(1,4,7,10)])

wshdCol <- c(pnw_palette("Bay", n = 6))
#------------------------------------------------------------------------------
# Spatial data
#-----------------------------------------------------------------------------

tbrPoly <- read.csv("data/tbrPolygon.csv")

# Watersheds
wshdPolys <- read.csv("data/wshdPolygons.csv") 
	
###############################################################################
# Define user interface
###############################################################################

ui <- fluidPage(
	tags$head(
		tags$style(HTML("hr {border-top: 1px solid #000000;}"))
	),
	
	HTML("<h2 style='background: #D9D9D9; padding: 20px'>Transboundary Escapement Data</h2>"),
	
	HTML("This app was developed by the Pacific Salmon Foundation’s <a href='https://salmonwatersheds.ca/'>Salmon Watersheds Program</a> to display historical information on the number of adult salmon returning to spawn in individual rivers ('escapement data') over a ~60-year period based on data obtained from Fisheries and Oceans Canada’s <a href= 'https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6'>New Salmon Escapement Database (NuSEDS)</a>."),
	
	# Sidebar layout with input and output definitions ----
	sidebarLayout(
		
		# Sidebar panel for inputs ----
		sidebarPanel(
			
			# Input: Selector for choosing dataset ----
			# h3("Species and area of interest"),
			pickerInput(inputId = "Species",
									label = "Choose a species:",
									choices = c("Chinook", "Chum", "Coho", "Pink", "Sockeye", "Multiple species"), 
									selected = "Sockeye"),
			
			pickerInput(inputId = "Watershed",
									label = "Choose a watershed:",
									choices = c("All", unique(wshdPolys$watershedName)), 
									selected = "All"),
			
			conditionalPanel(
				condition = "input.Species == 'Multiple species'",
				checkboxGroupInput(inputId = "whichSpecies",
													 label = "Choose species to compare:",
													 choices = c("Chinook", "Chum", "Coho", "Pink", "Sockeye"),
													 selected = c("Pink", "Coho"))
			),
			
			bsTooltip("whichSpecies", "Check multiple species to comapre escapement and spawn timing.",
								"right", options = list(container = "body")),
			
		
			hr(),
			
			# pickerInput(inputId = "sort2",
			# 						label = "Sort streams by:",
			# 						choices = c("Stream name", "Number of years surveyed (most to least)")), 
			# bsTooltip("sort2", "The sorting criteria will be applied within each category of streams. Note that if multiple species are selected, then stream name and distance are the only options here, since the other criteria will differ among species within a stream.",
			# 					"right", options = list(container = "body")),

					width = 3),
		
		# Main panel for displaying outputs ----
		mainPanel(
			# Output: Leaflet map
			leafletOutput("spawnLocMap", width = "100%", height = 500),
			br(),
			br()
		)
	),
	
	tabsetPanel(type = "tabs",
							selected = "Escapement Data",
							tabPanel("Escapement Data", 
											 HTML("<h3>Escapement data for 1950 - 2019</h3>
											 		 <p style='color:grey'>*<small>indicates a history of enhancement (see 'Data Sources').</small></p>"),
											 plotOutput(outputId = "monPlot", inline = TRUE),
											 HTML("<p>The main figure shows historical escapement monitoring data from the <a href='https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6'>New Salmon Escapement Database System</a> (NuSEDS) for the period 1950 - 2020, with the points coloured according to the abundance of spawners (red/orange = low abundance, amber = medium abundance, green/blue = high abundance; see scale bar). Each row is a unique spawning population within the database. There may be more than one population within a stream if, for example, runs with early and late timing are differentiated or if multiple species are selected above. The numbered circles correspond to the stream locations on the map above (ordered alphabetically within indicator/priority categories). An asterisk (*) after the species indicates that there is a history of enhancement of that population (e.g., hatchery supplementation; see the ‘Data Sources’ for more information).</p>
 <p>The values of the sorting criteria, if applicable, are shown on the right-hand side of the figure. For details of how these values were calculated, see the ‘Data Sources’ tab.</p><br><br><br><br><br>")),
						
							tabPanel("Data Sources", 
											 HTML("<h4>Escapement and spawn timing</h4>
											 		 <p>The escapement estimates and spawn timing data are publicly available from Fisheries and Oceans Canada as part of the <a href='https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6'>New Salmon Escapement Database System</a> (NuSEDS). The escapement data displayed here are the MAX_ESTIMATE field from NuSEDS, and were downloaded May 2021 and include data up to 2020. Although there may be additional sources of data on escapement, including from First Nations and academic researchers, those data were not publicly accessible for incorporation into this Tool. Compiling escapement information from multiple sources for display may be a useful future extension of the Tool.</p>"))
	) # end tabSetPanel
	
) # end fluidpage



###############################################################################
# Define server
###############################################################################

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
	
	#-----------------
	# Subset area and species
	#-----------------
	
	# input <- list(Species = "Pink", Watershed = "All")
	# dat <- tbrDat[which(tbrDat$Species == input$Species & tbrDat$watershed == input$Watershed), ]
	
	filteredData <- reactive({
		
		if(input$Species == "Multiple species" & input$Watershed == "All"){
			tbrDat[which(tbrDat$Species %in% input$whichSpecies), ]
		} else if(input$Species == "Multiple species"){
			tbrDat[which(tbrDat$Species %in% input$whichSpecies & tbrDat$watershed == input$Watershed), ]
		} else if(input$Watershed == "All"){
			tbrDat[which(tbrDat$Species == input$Species), ]
		} else {
			tbrDat[which(tbrDat$Species == input$Species & tbrDat$watershed == input$Watershed), ]
		}
		
	})
	
	# Number of streams
	nStreams <- reactive({
		dat <- filteredData() 
		length(unique(dat$streamName))
	})
	
	# Number of rows in plots (!= number of streams if species = All species)
	nRows <- reactive({
		dat <- filteredData()
		nrow(dat)
	})
	
	# Stream numbers for mapping and display
	streamData <- reactive({
		dat <- filteredData()
		nS <- nStreams()
		nR <- nRows()
		
		
		streamNames <- unique(dat$streamName)
		uniqueStreamIndex <- match(streamNames, dat$streamName)
		datStream <- dat[uniqueStreamIndex, ]
		
		datStream <- datStream[order(datStream$streamName), c("streamName", "Latitude", "Longitude", "watershed")]
		datStream$num <- c(1:nS)
		
		datStream
	})
	
	#-----------------
	# Sorting
	#-----------------
	sortedData <- reactive({
		dat <- filteredData()
		
		if(nrow(dat) > 0){
		nS <- nStreams()
		nR <- nRows()
		o <- order(dat$streamName)
		dat <- dat[o, ]
	} 
		
		dat
		
	})
	
	# ##############################################################################
	# # Downloadable csv of selected dataset 
	# ##############################################################################
	# output$downloadLoc <- downloadHandler(
	# 	filename = function() {
	# 		paste("SpawningLocations_", input$area, input$Species, ".csv", sep = "")
	# 	},
	# 	content = function(file) {
	# 		dumDat <- filteredData()
	# 		downDat <- dumDat[, c("POP_ID", "Species", "streamName", "Latitude", "Longitude", "priority", "indicator")]
	# 		write.csv(downDat, file, row.names = FALSE)
	# 	}
	# )
	# 
	##############################################################################
	# Map
	##############################################################################
	
	# #----
	# # Function to add legend
	# #----
	# addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){
	# 	
	# 	make_shapes <- function(colors, sizes, borders, shapes) {
	# 		shapes <- gsub("circle", "50%", shapes)
	# 		shapes <- gsub("square", "0%", shapes)
	# 		paste0(colors, "; width:", sizes, "px; height:", sizes, "px; margin-top: 4px; border:3px solid ", borders, "; border-radius:", shapes)
	# 	}
	# 	make_labels <- function(sizes, labels) {
	# 		paste0("<div style='display: inline-block;height: ", 
	# 					 sizes, "px;margin-top: 4px;line-height: ", 
	# 					 sizes, "px;'>", labels, "</div>")
	# 	}
	# 	
	# 	legend_colors <- make_shapes(colors, sizes, borders, shapes)
	# 	legend_labels <- make_labels(sizes, labels)
	# 	
	# 	return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity))
	# }
	
	#----
	# Output leaflet map
	#----
	output$spawnLocMap <- renderLeaflet({
		
		dat <- filteredData()
		
		mapBounds <- c(min(wshdPolys$X, na.rm = TRUE), min(wshdPolys$Y, na.rm = TRUE), max(wshdPolys$X, na.rm = TRUE), max(wshdPolys$Y, na.rm = TRUE))
		
		myLeaf <- leaflet() %>%
			addProviderTiles(providers$Esri.WorldTopoMap,#OpenStreetMap.Mapnik,#Esri.WorldGrayCanvas, #
											 options = providerTileOptions(noWrap = TRUE)
			) %>% 
			fitBounds(mapBounds[1], mapBounds[2], mapBounds[3], mapBounds[4])
		
		# for(i in unique(tbrPoly$SID)){
		# 	myLeaf <- addPolygons(myLeaf, lng = tbrPoly$X[tbrPoly$SID == i], lat = tbrPoly$Y[tbrPoly$SID == i], color="#000000", fillOpacity = 0, weight = 2, opacity = 0.9)
		# }
		
		for(i in 1:6){ # For each watershed
			for(j in unique(wshdPolys$SID[wshdPolys$PID == i])){ # For each polygon in that watershed
				
				# Add polygon with watershedName label if it's polygon #1
				# Except for Taku, which the second polygon is the main one!
				if((i != 4 & j == 1) | (i == 4 & j == 2)){
					myLeaf <- addPolygons(
					myLeaf, 
					lng = wshdPolys$X[wshdPolys$PID == i & wshdPolys$SID == j], 
					lat = wshdPolys$Y[wshdPolys$PID == i & wshdPolys$SID == j], 
					color = wshdCol[i], 
					fillOpacity = 0.4, 
					weight = 1, 
					opacity = 0.9, 
					fillColor = wshdCol[i], 
					label = unique(wshdPolys$watershedName[wshdPolys$PID == i & wshdPolys$SID == j]),
					labelOptions = labelOptions(
						noHide = TRUE, 
						direction = "center", 
						textOnly = TRUE, 
						style = list("font-style" = "bold",
												 "font-size" = "14px")))
				} else { # Add extra polygons for watershed i without a label
					myLeaf <- addPolygons(
						myLeaf, 
						lng = wshdPolys$X[wshdPolys$PID == i & wshdPolys$SID == j], 
						lat = wshdPolys$Y[wshdPolys$PID == i & wshdPolys$SID == j], 
						color = wshdCol[i], 
						fillOpacity = 0.4, 
						weight = 1, 
						opacity = 0.9, 
						fillColor = wshdCol[i])
				}
			}
		}
		
		myLeaf
	})
	
	
	#----
	# Add circle markers depending on order according to sort2
	#----
	observe({
		dat <- filteredData()
		
		if(nrow(dat) == 0){
			
			# boundsLL <- c(min(tbrDat$Longitude, na.rm = TRUE), min(tbrDat$Latitude, na.rm = TRUE), max(tbrDat$Longitude, na.rm = TRUE), max(tbrDat$Latitude, na.rm = TRUE))
			
			myLeaf <- leafletProxy("spawnLocMap") %>%
				# fitBounds(boundsLL[1], boundsLL[2], boundsLL[3], boundsLL[4]) %>%
				clearMarkers() 
			
		} else {
			
		datStream <- streamData()	
		
		# boundsLL <- c(min(datStream$Longitude, na.rm = TRUE), min(datStream$Latitude, na.rm = TRUE), max(datStream$Longitude, na.rm = TRUE), max(datStream$Latitude, na.rm = TRUE))

		myLeaf <- leafletProxy("spawnLocMap") %>%
			# fitBounds(boundsLL[1], boundsLL[2], boundsLL[3], boundsLL[4]) %>%
			clearMarkers() 
		
		myLeaf <- addCircleMarkers(myLeaf, 
															 lat = datStream$Latitude[!is.na(datStream$Latitude)],
															 lng = datStream$Longitude[!is.na(datStream$Latitude)], 
															 popup = datStream$streamName[!is.na(datStream$Latitude)],
															 label = datStream$num[!is.na(datStream$Latitude)], 
															 labelOptions = labelOptions(
															 	noHide = TRUE, 
															 	direction = "center", 
															 	textOnly = TRUE, 
															 	style = list("font-style" = "bold",
															 							 "font-size" = "10px")),#"color" = "white",
															 color = wshdCol[match(datStream$watershed[!is.na(datStream$Latitude)], unique(wshdPolys$watershedName))],
															 fillOpacity = 0.5,
															 opacity = 1,
															 radius = 8, #ifelse(dat$category[!is.na(dat$Latitude)] == 4, 6, 8),
															 stroke = TRUE,
															 weight = 1,
															 fillColor = wshdCol[match(datStream$watershed[!is.na(datStream$Latitude)], unique(wshdPolys$watershedName))]
		)
		
		myLeaf
		}
	})
	
	
	##############################################################################
	# Plotting escapement data
	##############################################################################
	observe({ # Observe for reactive plot height
		
		output$monPlot <- renderPlot({
			
			#---------------
			# Sorted data
			dat <- sortedData()
			
			if(nrow(dat) > 0){
			nS <- nStreams()
			nR <- nRows()
			
			streamNames <- unique(dat$streamName)
			streamMatch <- match(dat$streamName, streamNames)
		  uniqueStreamIndex <- match(streamNames, dat$streamName)
			
			#---------------
			# Stream data
			datStream <- streamData()
			datStream$match <- match(datStream$streamName, dat$streamName) # length nS
			
			#---------------
			# Plotting
			# layout(mat = matrix(c(1,1,1,2), nrow = 1))
			par(mar = c(10,20,4,2), cex = 1, cex.lab = 1)
			
			plot(c(min(yrs), max(yrs)), c(0.5, nR + 0.5), "n", yaxt = "n", xlab = "", ylab = "", yaxs= "i", bty = "n")
			axis(side = 3)
			abline(v = seq(1950, 2019, 10) - 0.5)
			segments(x0 = 1800, x1 = 2021, y0 = nR + 0.5, y1 = nR + 0.5, xpd = NA)
			segments(x0 = 1800, x1 = 2021, y0 = 0.5, y1= 0.5, xpd = NA)
			
			u <- par('usr')
			
			for(i in 1:nR){
				if(is_even(streamMatch[i])) polygon(x = rep(c(1800, 2080), each = 2), y = nR - i + 1 + c(-0.5, 0.5, 0.5, -0.5), border = NA, col = "#00000020", xpd = NA)
				
				points(yrs, rep(nR - i + 1, length(yrs)), pch = c(19, NA)[as.numeric(is.na(dat[i, indYears])) + 1], col = abundCol[findInterval(log(dat[i, indYears]), abundCut)]) 
				
			}
			
			
			axis(side = 2, at = nR - uniqueStreamIndex + 1, labels = streamNames, las =1, tck = 0, lwd = NA, line = 2.5, font = 2)
			axis(side = 2, at = nR + 1, labels = "Stream name", las =1, tck = 0, lwd = NA, line = 2.5, font = 2, xpd = NA)
			
			text(1948, 1:nR, pos = 2, rev(dat$Species), xpd = NA, adj = 2, cex = 0.8, col = grey(0.6))
			text(1948, nR + 1, pos = 2, "Species", xpd = NA, adj = 2, cex = 0.8, col = grey(0.6))
			


			# Number each stream according to category
			xNum <- 1928
			cexNum <- 2.7
			cexText <- 0.9
			points(rep(xNum, nS), 
						 (nR - datStream$match + 1), 
						pch = 21, 
						bg = paste0(wshdCol[match(datStream$watershed, unique(wshdPolys$watershedName))], 60), 
						col = wshdCol[match(datStream$watershed, unique(wshdPolys$watershedName))], 
						xpd = NA, cex = cexNum)
					
					text(rep(xNum, nS), 
							 (nR - datStream$match + 1), 
							 datStream$num, 
							 cex = cexText,  xpd = NA)

			text(xNum-2, nR + 1, "Number on map", xpd = NA, adj = 2, cex = 1, pos = 4)
			
			
			# Legend
			posL <- c(4, 5)
			for(i in 1:length(abundCol)){
				polygon(x = 1949.5 + c(i-1, i, i, i-1)*(2019-1950)/length(abundCol), y = u[3] - posL[c(1, 1, 2, 2)], col = abundCol[i], border= NA, xpd = NA)
			}
			segments(x0 = 1949.5 + (c(0:length(abundCol)))*(2019-1950)/length(abundCol), 
							 x1 = 1949.5 + (c(0:length(abundCol)))*(2019-1950)/length(abundCol),
							 y0 = u[3] - posL[1], y1 = u[3] - posL[2], xpd = NA)
			text(1949.5 + (c(0:length(abundCol)))*(2019-1950)/length(abundCol), u[3] - posL[2] + 1, round(abundCut, 1), pos = 3, xpd = NA)
			text(1949.5 + (c(0:length(abundCol)))*(2019-1950)/length(abundCol), u[3] - posL[1] - 1, abundCutExp, pos = 1, xpd = NA)
			
			polygon(x = c(1949.5, 2019.5, 2019.5, 1949.5), y =  u[3] - posL[c(1, 1, 2, 2)], xpd = NA)
			
			text(1948, u[3] - posL[1] + 0.2, xpd = NA, "Log spawner abundance", pos = 2)
			text(1948, u[3] - posL[2] - 0.2, xpd = NA, "Spawner abundance", pos = 2)
			
		} else {
			layout(mat = matrix(c(1,1,1,2), nrow = 1))
			par(mar = c(4,4,2,1), cex = 1, cex.lab = 1)
			
			plot(1, 1, "n", ylab = "", xlab = "", bty = "n", xaxt = "n", yaxt = "n")
			text(1, 1, "No data to plot")
		} }, 
		width = 1200, height = max(8, nRows()) * 22 + 90)
		
		
		
	}) # end observe

}

###############################################################################
# Execute
###############################################################################

shinyApp(ui = ui, server = server)
