##############################################################################

##### dives and intervals split between 2 intervals evenly ###################

# duplicated dives
dup_dives <- anyDuplicated(unlist(GPS_turtle$Dives))

# for each duplicated dive
for (dd in dup_dives) {
  
  # identify the GPS intervals that contain the duplicates
  intervals <- GPS_turtle$Dives[sapply(GPS_turtle$Dives, `%in%`, x = dd)]
  
  # remove dive from not minimum value
  shortcut <- GPS_turtle$Dives[max(intervals)]            # set dives shortcut
  GPS_turtle$Dives[max(intervals)] <- shortcut[shortcut != dd]
  
}

##############################################################################
