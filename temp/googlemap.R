library(googleway)

## using a valid Google Maps API key (Direction)
key <- "api key"

## Using the first and last coordinates as the origin/destination
origin <- c(17.48693, 78.38945)
destination <- c(17.47077, 78.35874)

## and the coordinates in between as waypoints
waypoints <- list(via = c(17.49222, 78.39643),
                  via = c(17.51965, 78.37835),
                  via = c(17.49359, 78.40079),
                  via = c(17.49284, 78.40686))
## use 'stop' in place of 'via' for stopovers
res <- google_directions(origin = origin,
                         destination = destination,
                        # waypoints = waypoints,
                         key = key) ## include simplify = F to return data as JSON


df_polyline <- decode_pl(res$routes$overview_polyline$points)
head(df_polyline)
#        lat      lon
# 1 17.48698 78.38953
# 2 17.48708 78.38946
# 3 17.48745 78.39008
# 4 17.48729 78.39052
# 5 17.48707 78.39110
# 6 17.48754 78.39128

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addPolylines(data = df_polyline, lat = ~lat, lng = ~lon)

