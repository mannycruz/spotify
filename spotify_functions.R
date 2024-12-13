# Spotify functions

# load in required packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(knitr)
library(gghighlight)
library(lubridate)
library(jsonlite)
library(spotifyr)
library(glue)
library(viridis)
library(httr)
library(httpuv)
library(tidyr)

# load in spotify data:

# Set working directory to directory where Spotify MyData folder is saved, this is where plots will be stored

setwd() <- "/path/to/directory"
data_file <- "/path/to/spotify/data"

## Create dataframe from all StreamingHistory files
# StreamingHistory files identified and merged

create_sh_df = function(results_dir){
  StreamingHistory_files <- list.files(path=results_dir, pattern="StreamingHistory")
  all_files <- StreamingHistory_files %>% map_df(~fromJSON(file.path(results_dir, .), flatten=TRUE))
  return(all_files)
}

# filter out skipped songs and podcasts
totalHistory <- totalHistory %>% filter(msPlayed>10000) %>% filter(msPlayed<=600000)

# Set up Spotify API
# You need to set up a developer account with Spotify and create an "app" to get client ID and client secret code
Sys.setenv(SPOTIFY_CLIENT_ID="")
Sys.setenv(SPOTIFY_CLIENT_SECRET="")
access_token <- get_spotify_access_token()

# Quick functions to get top tracks or artists/recently played:
top_artists <- get_my_top_artists_or_tracks(type = "artists", limit=50, time_range = "short_term", authorization=get_spotify_authorization_code(scope=c('user-read-recently-played', 'user-top-read', 'user-library-read')))

top_tracks <- get_my_top_artists_or_tracks(type="tracks", limit=50, time_range = "short_term", authorization = get_spotify_authorization_code(scope=c('user-read-recently-played','user-top-read', 'user-library-read')))
for (i in seq_len(nrow(top_tracks))) {
  top_tracks$artist[i] <- toString(top_tracks$artists[[i]]$name)
}

recently_played <- get_my_recently_played(limit = 50, authorization = get_spotify_authorization_code(scope=c("user-read-recently-played")))



# Create new data frame that includes month, day, valence values
final_table <- expand_spotify_tracks(totalHistory)


# map distribution of genres
# this function is not finished and doesn't do anything :-)
genre_distributions <- map_genres(final_table)

#This generates a valence heatmap for the year
year_valence_map <- year_heatmap(final_table)

# This generates a valence heatmap for each month
month_valence_map <- month_heatmaps(final_table)

# This generates 3 pie/donut charts for each Month:
# 1. Top artists of the month
# 2. Top songs of the month
# 3. Valence trends of the month
top_songs_month <- top_song_month(final_table)








######### functions ##############
#'@function make function for total minutes listened :> then graph along year

#' Get the top tracks, artists, or recently played
#' 
#' @param type        "tracks", "artists", or "recently_played"
#' @param time_range  "short_term": 30 days, "medium_term": six months, "long_term": several years
#' 

quick_search = function(type, time_range) {
  access_token <- get_spotify_access_token()
  if (type!="recently_played"){
    quick_df = get_my_top_artists_or_tracks(type = type, limit=50, offset = 0, time_range = time_range, authorization = get_spotify_authorization_code(scope = c('user-read-recently-played', 'user-top-read', 'user-library-read')))
    for (s in seq_len(nrow(quick_df))){
      artist = quick_df[[1]][[s]]$name[1]
      quick_df$artist[s] = artist
    }
    quick_df = select(quick_df, artist, name, popularity, album.release_date)
  } else {
    quick_df = get_my_recently_played(limit = 50, authorization = get_spotify_authorization_code(scope = c(c("user-read-recently-played"))))
    for (s in seq_len(nrow(quick_df))){
      artist = quick_df[[2]][[s]]$name[1]
      quick_df$artist[s] = artist
    }
    quick_df = quick_df %>% separate(col = played_at, into = c("date", "time"), sep="T")
    quick_df = quick_df %>% mutate(time = gsub("Z", "", time)) %>% select(track.name, artist, date, time, track.popularity)
  }
  return(quick_df)
}

#' Attach genre, valence, uri, and album art to history dataframe using Spotify's API
#'
#' @param aggregated_history  dataframe containing listening history downloaded from Spotify
#'

expand_spotify_tracks <- function(aggregated_history) {
  unique_history <- aggregated_history %>% select(artistName, trackName) %>% unique()
  count = 0
  for (i in seq_len(nrow(unique_history))) {
    count <- count + 1
    if (count %in% seq.int(from = 1, to = nrow(unique_history), by = as.integer(nrow(unique_history)/10))) {
      print(glue("{as.integer(count/nrow(unique_history) * 100)}% done"))
    }
    query = tolower(paste(unique_history$artistName[i], unique_history$trackName[i]))
    track_info = search_spotify(query, "track")
    if (is_empty(track_info)) {
      print(glue("Could not find track info for query {query}... Reformatting..."))
      if (grepl("-", unique_history$trackName[i])) {
        new_song <- trimws(strsplit(strsplit(unique_history$trackName[i], split="-")[[1]], split='" "')[1], which = "both")
        new_query = tolower(paste(unique_history$artistName[i], new_song))
        new_track_info = search_spotify(new_query, "track")
        if (is_empty(new_track_info)) {
          print(glue("Could not find track info for original query {query} or new query {new_query}. Skipping..."))
          next
        } else {
          unique_history$trackName[i] <- new_song
          track_info <- new_track_info
        }
      }
    }
    # get genres 
    if (ncol(track_info)==0) {
      print(glue("No track info for query {query}."))
      next
    }
    artist_info <- track_info[[1]][[1]]
    artist_id <- artist_info$id[1]
    artist_catalog <- get_artist(artist_id)
    genres <- artist_catalog[["genres"]] %>% toString()
    uri <- track_info$uri[1]
    # get release date
    release_info <- track_info$album.release_date[1]
    # get album art url
    album_art <- track_info[[20]][[1]]$url[1]
    if (is_empty(album_art)) {
      print(paste(count, "count has empty album art"))
      album_art <- "NA"
    }
    # get valence
    track_id <- track_info$id[1]
    track_analysis <- get_track_audio_features(track_id)
    valence_value <- track_analysis$valence
    if (is_empty(valence_value)) {
      valence_value <- "NA"
    }
    # fill in values
    unique_history$genres[i] <- genres
    unique_history$release_date[i] <- release_info
    unique_history$album_art[i] <- album_art
    unique_history$valence[i] <- valence_value
    unique_history$uri[i] <- uri
  }
  # Join original table with filled in values
  aggregated_history <- left_join(aggregated_history, unique_history, by=c("artistName","trackName"))
  aggregated_history <- mutate(aggregated_history, month = toString(month(date(endTime))), day = day(endTime))
  return(aggregated_history)
}

#' Tabulate how often different genres appear in history over past year (can compare between skipped and listed to songs too!)
#' 
#' @param aggregated_table  The data table containing song info (and genres column)
#' 
count_genres <- function(aggregated_table){
  # Put all genres in a column of a dataframe
  genre_df <- data.frame(genre = aggregated_table$genres)
  # Split genres into multiple rows based on "," separator
  genre_df <- separate_rows(genre_df, genre, sep = ", ")
  genre_df <- genre_df %>% group_by(genre) %>% add_count(name = "genre_occurrence") %>% unique()
}

#' Create a heatmap of valence values across the year
#' 
#' @param aggregated_table  Dataframe containing listening history and valence
#' @param out_file          Output file for plot
#' @param viridis_color     Viridis gradient colour for tiles. "magma", "plasma", "inferno", "civids", "mako", "rocket", "rainbow"
#' @param year              Year to use as plot title. Defaults to current year as defined by system.

draw_year_heatmap = function(aggregated_table, out_file=paste(getwd(), "MyYearValence.png", sep="/"), viridis_color="magma", year=format(Sys.Date(), "%Y")) {
  reformated_table = aggregated_table %>% filter(!is.na(valence), valence!="NA") %>% mutate(valence_num = as.numeric(valence)) %>% filter(valence!=0) %>% arrange(valence)
  # Get songs with lowest and highest values
  minimum_valence_song = reformated_table %>% slice_head()
  maximum_valence_song = reformated_table %>% slice_tail()
  medium_valence_song = reformated_table[nrow(reformated_table)/2,]
  
  year_valence = data.frame(dates = date(reformated_table$endTime),
                          valence = as.numeric(reformated_table$valence),
                          month = reformated_table$month,
                          day = reformated_table$day)
  year_valence = year_valence %>% 
                  group_by(dates, month, day) %>% 
                  filter(!is.na(valence)) %>% 
                  mutate(avg_valence = mean(as.numeric(valence))) %>% 
                  filter(valence != 0) %>% select(-valence) %>% unique()
  
  # Get labels for minimum, median, and maximum valence songs
  min_song_label = paste(minimum_valence_song$trackName, minimum_valence_song$artistName, minimum_valence_song$valence, sep="\n")
  med_song_label = paste(medium_valence_song$trackName, medium_valence_song$artistName, medium_valence_song$valence, sep="\n")
  max_song_label = paste(maximum_valence_song$trackName, maximum_valence_song$artistName, maximum_valence_song$valence, sep="\n")
  
  # Reformat the month to a string
  for (d in seq_len(nrow(year_valence))) {
    month_abr = toString(month(as.numeric(year_valence$month[d]), label=TRUE))
    year_valence$month_abr[d] = month_abr
  }
  # Create the plot
  valence_plot = ggplot(
    year_valence,
    aes(x=day, y=month_abr)) +
    geom_tile(aes(fill=avg_valence)) +
    scale_fill_viridis(
      limits = c(0,1),
      breaks = c(as.numeric(minimum_valence_song$valence), as.numeric(medium_valence_song$valence), as.numeric(maximum_valence_song$valence)),
      labels = c(min_song_label, med_song_label, max_song_label),
      begin = 0,
      end = 1,
      option = viridis_color
    ) +
    scale_y_discrete(limits = month.abb) +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle(label = year) +
    theme(
      axis.text.y = element_text(size = 11, colour = "white"),
      axis.text.x = element_blank(),
      text = element_text(family = "Helvetica", size=13, color = "white"),
      plot.title = element_text(face="bold",hjust=0.5,vjust=1.6,size=16),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "#212121"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.key.width = unit(4, "cm"),
      plot.background = element_rect(fill = "#212121"),
      panel.background = element_rect(fill = "#212121"),
      axis.ticks = element_blank(),
      plot.margin = margin(0.8, 0.8, 0.8, 0.8, "cm"),
      legend.title = element_blank()
    )
  ggsave(out_file, valence_plot, device = "png", units = "in", width = 11, height = 6.97)
  print(glue("Saving to {out_file}"))
  print("Done! ╰(*´︶`*)╯♡")
  return(valence_plot)
}

##
year_heatmap = function(filtered_aggregated_table) {
  dates_available <- c()
  filtered_aggregated_table <- filtered_aggregated_table %>% filter(!(is.na(valence)))
  for (i in seq_len(nrow(filtered_aggregated_table))) {
    endTime <- filtered_aggregated_table$endTime[i]
    endDate <- date(endTime)
    dates_available <- c(dates_available,endDate)
  }
  # get a list of every day music was listened to
  dates_available <- unique(dates_available)
  
  # create a new dataframe with average valence values for each day per year
  year_df <- data.frame(matrix(ncol=3,nrow=length(dates_available)))
  year_names <- c("month","day","avg_valence")
  colnames(year_df) <- year_names
  entry <- 0
  for (available_date in dates_available) {
    avg_valence <- 0
    entry <- entry + 1
    # subset table per day
    date_entries <- filtered_aggregated_table %>% filter(date(endTime)==available_date)
    num_entries <- nrow(date_entries)
    # sum up valence values
    total_valence <- sum(date_entries$valence)
    # get avg valence
    avg_valence <- total_valence/num_entries
    month <- toString(month(as_date(available_date),label=TRUE))
    day <- day(as_date(available_date))
    year_df$month[entry] <- month
    year_df$day[entry] <- day
    year_df$avg_valence[entry] <- avg_valence
  }
  # There may be some duplicate days depending on what time Spotify data was requested
  # Following code will just merge the two days instead of dropping them
  final_year_df <- year_df %>% group_by(month,day) %>% summarise_each(funs(avg_valence=mean(., na.rm=TRUE)))
  final_year_df <- as.data.frame(final_year_df)
  
  # minimum valence of the year
  min_year_valence <- min(filtered_aggregated_table$valence)
  min_song <- filtered_aggregated_table %>% filter(valence==min_year_valence)
  if (nrow(min_song) > 1) {
    min_song_name <- head(names(sort(table(min_song$trackName),decreasing=TRUE)),n=1)
    min_song <- min_song %>% filter(trackName==min_song_name)
  }
  min_year_label <- paste(min_song$trackName[1],min_song$artistName[1],min_song$valence[1],sep="\n")
  # median valence of the year
  med_year_valence <- round(median(filtered_aggregated_table$valence),3)
  med_song <- filtered_aggregated_table %>% filter(valence==med_year_valence)
  if (nrow(med_song) > 1) {
    med_song_name <- head(names(sort(table(med_song$trackName),decreasing = TRUE)),n=1)
    med_song <- med_song %>% filter(trackName==med_song_name)
  }
  med_year_label <- paste(med_song$trackName[1],med_song$artistName[1],med_song$valence[1],sep="\n")
  # max valence of the year
  max_year_valence <- max(filtered_aggregated_table$valence)
  max_song <- filtered_aggregated_table %>% filter(valence == max_year_valence)
  if (nrow(max_song) > 1) {
    max_song_name <- head(names(sort(table(max_song$trackName),decreasing=TRUE)),n=1)
    max_song <- max_song %>% filter(trackName == max_song_name)
  }
  max_year_label <- paste(max_song$trackName[1],max_song$artistName[1],max_song$valence[1],sep="\n")
  # Create a plot to visualize
  heatmap_year <- ggplot(
    final_year_df,
    aes(x=day,y=month)) +
    geom_tile(aes(fill=avg_valence)) +
    scale_fill_viridis(
      limits=c(0,1),
      breaks=c(min_year_valence,med_year_valence,max_year_valence),
      labels=c(min_year_label,med_year_label,max_year_label),
      begin=0,
      end=1,
      option="magma") +
    scale_y_discrete(limits=month.abb) +
    # x label on x axis
    xlab(NULL) +
    # y label on y axis
    ylab(NULL) +
    ggtitle(label = "2021") +
    theme(
      axis.text.y = element_text(size=11,color="white"),
      text=element_text(family="Helvetica",size=13,color="white"),
      plot.title = element_text(face="bold",hjust=0.5,vjust=1.6,size=16),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill="#212121"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.key.width = unit(4,"cm"),
      plot.background = element_rect(fill="#212121"),
      panel.background = element_rect(fill="#212121"),
      axis.ticks = element_blank(),
      plot.margin = margin(0.8,0.8,0.8,0.8,"cm"),
      legend.title = element_blank(),
      # remove day labels
      axis.text.x = element_blank())
  year_file <- "year_valence_test.png"
  ggsave(year_file,heatmap_year,device=png,units="in",width=11,height=6.97)
  print("Done! ╰(*´︶`*)╯♡")
  return(heatmap_year)
}

month_heatmaps <- function(filtered_aggregated_table) {
  filtered_aggregated_table <- filtered_aggregated_table %>% filter(!(is.na(valence)))
  months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
  for (selected_month in months) {
    month_songs <- filtered_aggregated_table %>% filter(month(endTime,label=TRUE,abbr=FALSE) == selected_month)
    for (i in seq_len(nrow(month_songs))) {
      time <- month_songs$endTime[i]
      timesplit <- strsplit(time," ")
      timesplit <- strsplit(timesplit[[1]]," ")
      hmsec <- timesplit[[2]]
      month_songs$time[i] <- hmsec
    }
    min_valence <- min(month_songs$valence)
    min_song <- month_songs %>% filter(valence==min_valence)
    if (nrow(min_song) > 1) {
      min_song_name <- head(names(sort(table(min_song$trackName),decreasing=TRUE)),n=1)
      min_song <- min_song %>% filter(trackName==min_song_name)
    }
    min_label <- paste(min_song$trackName[1],min_song$artistName[1],min_song$valence[1],sep="\n")
    med_valence <- round(median(month_songs$valence),3)
    med_song <- month_songs %>% filter(valence==med_valence)
    if (nrow(med_song) > 1 ) {
      med_song_name <- head(names(sort(table(med_song$trackName),decreasing=TRUE)),n=1)
      med_song <- med_song %>% filter(trackName==med_song_name)
    }
    med_label <- paste(med_song$trackName[1],med_song$artistName[1],med_song$valence[1],sep="\n")
    max_valence <- max(month_songs$valence)
    max_song <- month_songs %>% filter(valence==max_valence)
    if (nrow(max_song) > 1) {
      max_song_name <- head(names(sort(table(max_song$trackName),decreasing=TRUE)),n=1)
      max_song <- max_song %>% filter(trackName==max_song_name)
    }
    max_label <- paste(max_song$trackName[1],max_song$artistName[1],max_song$valence[1],sep="\n")
    
    times_available <- c()
    # get each day that music was listened to 
    for (i in seq_len(nrow(month_songs))) {
      endTime <- month_songs$endTime[i]
      endDate <- date(endTime)
      times_available <- c(times_available,endDate)
    }
    times_available <- unique(times_available)
    
    hour_avg_valence <- data.frame(matrix(ncol=4,nrow=(length(times_available) * 24)))
    names_2 <- c("month","day","hour","avg_valence")
    colnames(hour_avg_valence) <- names_2
    
    counter <- 0
    for (available_time in times_available) {
      day_songs <- month_songs %>% filter(day==day(as_date(available_time)))
      for (hour in seq(from=0,to=23)) {
        counter <- counter + 1
        month <- toString(month(as_date(available_time),label=TRUE,abbr=TRUE))
        day <- day(as_date(available_time))
        hour_songs <- day_songs %>% filter(hour(endTime)==hour)
        num_entries <- nrow(hour_songs)
        total_valence <- sum(hour_songs$valence)
        avg_valence <- total_valence/num_entries
        hour_avg_valence$month[counter] <- month
        hour_avg_valence$day[counter] <- day
        hour_avg_valence$hour[counter] <- hour
        hour_avg_valence$avg_valence[counter] <- avg_valence
      }
    }
    
    hour_avg_valence_filtered <- hour_avg_valence %>% filter(!avg_valence=="NaN")
    
    # visualize with heatmap
    
    month_heatmap <- ggplot(
      hour_avg_valence_filtered,
      aes(x=day,y=hour)) +
      geom_tile(aes(fill=avg_valence)) +
      scale_x_continuous(
        limits=c(0.5,31.5),
        breaks=c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31)) +
      scale_fill_viridis(
        limits=c(0,1),
        breaks=c(min_valence,med_valence,max_valence),
        labels=c(min_label,med_label,max_label),
        begin=0,
        end=1,
        option="magma") +
      xlab("Day") + ylab("Hour") +
      ggtitle(label = selected_month) +
      scale_y_reverse(
        limits=c(24.5,-0.5),
        breaks=c(0,5,10,15,20,24),
        labels=c("12:00","05:00","10:00","15:00","20:00","24:00")) +
      theme(
        axis.text.y = element_text(size=8,color="white"),
        text = element_text(family="Helvetica",size=13,color="white"),
        plot.title = element_text(face="bold",hjust=0.5,vjust=1.6,size=16),
        panel.grid.major = element_line(colour="#616161"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="#212121"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.key.width = unit(4,"cm"),
        plot.background = element_rect(fill="#212121"),
        panel.background = element_rect(fill="#212121"),
        axis.ticks=element_blank(),
        legend.title = element_blank(),
        plot.margin = margin(0.8,0.8,0.8,0.8,"cm"),
        axis.text.x = element_text(size=10,color="white")
      )
    month_file_name <- paste0(selected_month,"_valence_test.png")
    ggsave(month_file_name,month_heatmap,device=png,units="in",width=11,height=6.97)
  }
  print("Done! ╰(*´︶`*)╯♡")
  return(month_heatmap)
}

top_song_month <- function(filtered_aggregated_table) {
  counter <- 0
  months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  month_dataframe <- data.frame(matrix(ncol=8,nrow=12))
  columnnames <- c('month','top_artist','artist_count','top_artist_songs','top_song','song_count','song_artist','song_valence')
  colnames(month_dataframe) <- columnnames
  out <- list()
  for (available_month in months) {
    counter <- counter + 1
    month_table <- filtered_aggregated_table %>% filter(month==available_month) 
    top_song <- head(sort(table(month_table$trackName),decreasing = TRUE), n=1)
    top_song_name <- names(top_song)
    top_song_count <- top_song[[1]][[1]]
    song_info <- filtered_aggregated_table %>% filter(trackName==top_song_name) %>% select(artistName,valence) %>% unique()
    artist_name <- song_info$artistName[1]
    valence_info <- song_info$valence[1]
    top_artist_info <- head(sort(table(month_table$artistName),decreasing=TRUE), n = 1)
    top_artist <- names(top_artist_info)
    top_artist_count <- top_artist_info[[1]][[1]]
    top_artist_songs <- month_table %>% filter(artistName==top_artist) %>% select(trackName) %>% unique()
    top_artist_songs <- toString(top_artist_songs)
    if (length(artist_name) > 1) {
      print(glue("More than one artist for {top_song_name}"))
      break
    }
    
    # URI is the a alphanumerical string that spotify uses to create the Spotify "codes", 
    # if you wanna use this uncomment out the lines and change month_dataframe <- matrix col numsm to 10,
    # add 'song_uri' and 'spotify_code' to columnnames
    # get uri
    #query <- paste(top_song_name, artist_name)
    #query <- tolower(query)
    #top_track_info <- search_spotify(query, type="track")
    #top_song_uri <- top_track_info$uri[1]
    
    #spotify_code <- paste0("https://scannables.scdn.co/uri/plain/jpeg/24e07d/black/640/",top_song_uri)
    month_dataframe$month[counter] <- available_month
    month_dataframe$top_artist[counter] <- top_artist
    month_dataframe$artist_count[counter] <- top_artist_count
    month_dataframe$top_artist_songs[counter] <- top_artist_songs
    month_dataframe$top_song[counter] <- top_song_name
    month_dataframe$song_count[counter] <- top_song_count
    month_dataframe$song_artist[counter] <- artist_name
    month_dataframe$song_valence[counter] <- valence_info
    #month_dataframe$song_uri[counter] <- top_song_uri
    #month_dataframe$spotify_code[counter] <- spotify_code
    
    # create donut plot of top songs/top artist
    
    total_plays <- nrow(month_table)
    total_songs_dataframe <- month_table %>% select(trackName,artistName) %>% unique()
    total_songs_count <- total_songs_dataframe %>% summarise(count=n())
    total_songs_count <- total_songs_count[[1]]
    
    song_fraction_df <- data.frame(matrix(ncol=4,nrow=total_songs_count))
    song_fraction_df_names <- c('song','artist','plays','fraction')
    colnames(song_fraction_df) <- song_fraction_df_names
    
    total_artists_dataframe <- month_table %>% select(artistName) %>% unique()
    total_artists_count <- nrow(total_artists_dataframe)
    total_artists_df <- data.frame(matrix(ncol=3,nrow=total_artists_count))
    total_artists_name <- c('artist','plays','fraction')
    colnames(total_artists_df) <- total_artists_name
    
    for (i in seq(total_artists_count)) {
      artist_artist <- total_artists_dataframe$artistName[i]
      artist_plays <- month_table %>% filter(artistName == artist_artist) %>% nrow()
      artist_fraction <- (artist_plays/total_artists_count)
      total_artists_df$artist[i] <- artist_artist
      total_artists_df$plays[i] <- artist_plays
      total_artists_df$fraction[i] <- artist_fraction
    }
    total_artists_df$ymax <- cumsum(total_artists_df$fraction)
    total_artists_df$ymin <- c(0,head(total_artists_df$ymax,n=-1))
    total_artists_df$labelPosition <- (total_artists_df$ymax + total_artists_df$ymin) / 2
    total_artists_df$label <- paste0(total_artists_df$artist,"\n",total_artists_df$plays," plays")
    
    artist_fraction_donut <- ggplot(
      total_artists_df,
      aes(ymax=ymax,
          ymin=ymin,
          xmax=4,
          xmin=3,
          fill=artist,
          #alpha=ifelse(fraction>0.1,1,0.95))) +
          alpha = fraction)) +
      geom_rect() + theme_void() + theme(legend.position="none") + coord_polar(theta="y") + xlim(c(2,4)) +
      geom_text(x=3.5,aes(y=labelPosition,label=ifelse(fraction>0.025,label,''), fontface="bold")) +
      ggtitle(label = available_month) + theme(plot.title = element_text(face="bold",hjust=0.5,vjust=1.6,size=16,colour = "black"), panel.background = element_rect(fill="white"), plot.background = element_rect(fill="white"))
    
    artist_fraction_file_name <- glue("{available_month}_artist_fraction.png")
    ggsave(artist_fraction_file_name, artist_fraction_donut,device=png,units="in",width=11,height=6.97)
      

    for (i in seq(total_songs_count)) {
      song_name <- total_songs_dataframe$trackName[i]
      artist_name <- total_songs_dataframe$artistName[i]
      song_count <- month_table %>% filter(trackName==song_name & artistName==artist_name) %>% summarise(count=n())
      song_count <- song_count$count
      song_fraction <- (song_count / total_plays)
      song_fraction_df$song[i] <- song_name
      song_fraction_df$artist[i] <- artist_name
      song_fraction_df$plays[i] <- song_count
      song_fraction_df$fraction[i] <- song_fraction
    }
    song_fraction_df$ymax <- cumsum(song_fraction_df$fraction)
    song_fraction_df$ymin <- c(0,head(song_fraction_df$ymax,n=-1))
    song_fraction_df$labelPosition <- (song_fraction_df$ymax + song_fraction_df$ymin) / 2
    song_fraction_df$label <- paste0(song_fraction_df$song,"\n",song_fraction_df$artist,"\n",song_fraction_df$plays," plays")
        
    # make plot for song fractions
    song_fraction_donut <- ggplot(
      song_fraction_df,
      aes(ymax=ymax,
          ymin=ymin,
          xmax=4,
          xmin=3,
          fill=song,
          alpha=fraction)) +
          #alpha=ifelse(song==top_song_name,1,0.99))) +
      geom_rect() + theme(axis.ticks = element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(), plot.title = element_text(face="bold"), plot.background=element_rect(color="white"), panel.background = element_rect(fill="white"), legend.position = "none") + theme_void() + theme(legend.position="none") + coord_polar(theta="y") + xlim(c(2, 4)) +
      geom_text(x=4.2, aes(y=labelPosition, size=fraction, label=ifelse(fraction>0.005 | song==top_song_name,label,''),fontface="bold")) + lims(size = c(0.5,1)) +
      ggtitle(label = available_month) + theme(plot.title = element_text(face="bold",hjust=0.5,vjust=1.6,size=16,colour = "black"), panel.background = element_rect(fill="white"), plot.background = element_rect(fill="white"))
      
    fraction_file_name <- glue("{available_month}_fraction.png")
    ggsave(fraction_file_name, song_fraction_donut,device=png,units="in",width=16,height=10)
      
    late_night_songs <- month_table %>% filter(hour(endTime)>=0 & hour(endTime)<=5)
    total_night_plays <- nrow(late_night_songs)
    num_songs_dataframe <- late_night_songs %>% select(artistName,trackName,valence) %>% unique()
    num_songs <- late_night_songs %>% nrow()
    late_night_dataframe <- data.frame(matrix(ncol=5,nrow=num_songs))
    late_night_names <- c('song','artist','valence','plays','fraction')
    colnames(late_night_dataframe) <- late_night_names
    
    for (n in seq(num_songs)) {
      artist <- num_songs_dataframe$artistName[n]
      song <- num_songs_dataframe$trackName[n]
      valence <- num_songs_dataframe$valence[n]
      plays <- late_night_songs %>% filter(trackName==song & artistName==artist) %>% nrow()
      fraction <- (plays / total_night_plays)
      late_night_dataframe$song[n] <- song
      late_night_dataframe$artist[n] <- artist
      late_night_dataframe$valence[n] <- valence
      late_night_dataframe$plays[n] <- plays
      late_night_dataframe$fraction[n] <- fraction
    }
    late_night_dataframe$ymax <- cumsum(late_night_dataframe$fraction)
    late_night_dataframe$ymin <- c(0,head(late_night_dataframe$ymax,n=-1))
    late_night_dataframe$labelPosition <- (late_night_dataframe$ymax + late_night_dataframe$ymin) / 2
    late_night_dataframe$label <- paste0(late_night_dataframe$song,"\n",late_night_dataframe$artist,"\n",late_night_dataframe$plays," late plays")
    
    lowest_valence <- min(late_night_songs$valence)
    lowest_valence_song <- late_night_songs %>% filter(valence==lowest_valence)
    if (nrow(lowest_valence_song) > 1) {
      lowest_name <- head(names(sort(table(lowest_valence_song$trackName),decreasing=TRUE)),n=1)
      lowest_valence_song <- lowest_valence_song %>% filter(trackName==lowest_name)
    }
    lowest_label <- paste(lowest_valence_song$trackName[1],lowest_valence_song$artistName[1],lowest_valence_song$valence[1],sep="\n")
    
    highest_valence <- max(late_night_songs$valence)
    highest_valence_song <- late_night_songs %>% filter(valence==highest_valence)
    if (nrow(highest_valence_song) > 1) {
      highest_name <- head(names(sort(table(highest_valence_song$trackName),decreasing=TRUE)),n=1)
      highest_valence_song <- highest_valence_song %>% filter(trackName==highest_name)
    }  
    highest_label <- paste(highest_valence_song$trackName[1],highest_valence_song$artistName[1],highest_valence_song$valence[1],sep="\n")
    
    median_valence <- round(median(late_night_songs$valence),3)
    median_valence_song <- late_night_songs %>% filter(valence==median_valence)
    if (nrow(median_valence_song) > 1) {
      median_name <- head(names(sort(table(median_valence_song$trackName),decreasing=TRUE)),n=1)
      median_valence_song <- median_valence_song %>% filter(trackName==median_name)
    }
    median_label <- paste(median_valence_song$trackName[1],median_valence_song$artistName[1],median_valence_song$valence[1],sep="\n")
    
    late_night_donut <- ggplot(
      late_night_dataframe,
      aes(ymax = ymax,
          ymin = ymin,
          xmax = 4,
          xmin= 3,
          fill=valence)) +
          #alpha=fraction)) +
      scale_fill_viridis(breaks = c(lowest_valence,median_valence,highest_valence), labels=c(lowest_label,median_label,highest_label), option="mako",limits = c(0,1)) +
      geom_rect() + coord_polar(theta = "y") + theme_void() + xlim(c(2,4)) + theme(legend.margin = margin(0.5,0.5,0.5,0.5,"cm"), legend.background = element_rect(fill="#212121"), legend.position = "bottom", legend.justification = "center", legend.key.width = unit(4,"cm")) +
      geom_text(x=4.2,aes(y=labelPosition,colour="white",label=ifelse(plays>5,label,''),fontface="bold")) +
      ggtitle(label = available_month) + theme(text=element_text(colour="white"),plot.title = element_text(face="bold",hjust=0.5,vjust=1.6,size=16,colour = "white"),
                                               panel.background = element_rect(fill="#212121"),
                                               plot.background = element_rect(fill="#212121"))
    late_night_name <- glue("late_night_{available_month}.png")
    ggsave(late_night_name,late_night_donut,device=png,units="in",width=16,height=10)
  }
  return(month_dataframe)
}

summer_trends <- function(filtered_aggregated_table) {
  months <- c("May","Jun","Jul","Aug")
  summer_songs <- filtered_aggregated_table %>% filter(month %in% months)
  unique_songs <- summer_songs %>% select(trackName,artistName,valence) %>% unique()
  summer_dataframe <- data.frame(matrix(ncol=4,nrow=nrow(summer_songs)))
  sum_cols <- c('song','artist','valence','plays')
  colnames(summer_dataframe) <- sum_cols
  for (i in seq(nrow(unique_songs))) {
    song <- unique_songs$trackName[i]
    artist <- unique_songs$artistName[i]
    valence <- unique_songs$valence[i]
    num_of_plays <- summer_songs %>% filter(trackName==song & artistName==artist) %>% summarise(count=n())
    num_of_plays <- num_of_plays$count
    summer_dataframe$song[i] <- song
    summer_dataframe$artist[i] <- artist
    summer_dataframe$valence[i] <- valence
    summer_dataframe$plays[i] <- num_of_plays
  }
  return(summer_dataframe)
}

# Use Spotify API to get top tracks and artists of last four weeks
recently_played <- get_my_recently_played(limit = 50, authorization = get_spotify_authorization_code(scope = "user-read-recently-played"))
top_tracks <- get_my_top_artists_or_tracks(type = "tracks", limit=50, time_range = "short_term", authorization=get_spotify_authorization_code(scope=c("user-read-recently-played","user-top-read","user-library-read")))
top_artists <- get_my_top_artists_or_tracks(type = "artists", limit=50, time_range = "short_term", authorization=get_spotify_authorization_code(scope=c("user-read-recently-played","user-top-read","user-library-read")))

for (i in seq(nrow(top_tracks))) {
  top_tracks$artist[i] = toString(top_tracks$artists[[i]]$name)
}

top_tracks <- top_tracks %>% select(name, artist,popularity)

# Popularity graphs:

top_tracks <- top_tracks %>% mutate(short_name = gsub("\\s*\\([^\\)]+\\)","", name))
top_tracks <- top_tracks %>% mutate(short_name = gsub("\\s*\\[[^\\)]+\\]","", short_name))
top_tracks$x_pos = 1:50
top_tracks <- top_tracks %>% mutate(pop_class = case_when(
  popularity >= 80 ~ "highest",
  popularity < 80 & popularity >= 60 ~ "high",
  popularity < 60 & popularity >= 40 ~ "medium",
  popularity < 40 & popularity >= 20 ~ "low",
  popularity < 20 & popularity >= 0 ~ "lowest")
)
rect_data <- data.frame(ystart=seq(0,80,20), yend=seq(20,100,20), popularity=c("lowest","low","medium","high","highest"))
ggplot() + geom_rect(data = rect_data, aes(ymin=ystart, ymax=yend, xmin = -Inf, xmax = Inf, fill=popularity), alpha=0.2) + geom_point(data = top_tracks, aes(y = popularity, x=1:50, colour = pop_class)) + guides(colour = "none") + geom_text(data=top_tracks, x=top_tracks$x_pos+6, y=top_tracks$popularity, label=ifelse(top_tracks$pop_class=="highest",top_tracks$short_name,""), size=2.5)
ggplot() + geom_rect(data = rect_data, aes(ymin=ystart, ymax=yend, xmin = -Inf, xmax = Inf, fill=popularity), alpha=0.2) + geom_point(data = top_tracks_2, aes(y = popularity, x=1:49, colour = pop_class), alpha=ifelse(top_tracks_2$pop_class=="medium",1,0.3)) + guides(colour = "none") + geom_text(data=top_tracks_2, aes(x=top_tracks_2$x_pos, y=top_tracks_2$popularity), label=ifelse(top_tracks_2$pop_class=="medium",top_tracks_2$short_name,""), size=2.5, position=position_jitter(width=1.5,height=5.4))

ggplot() + 
  geom_rect(data = rect_data, 
            aes(ymin=ystart, 
                ymax=yend, 
                xmin = -Inf, 
                xmax = Inf, 
                fill=popularity), 
            alpha=0.2) + 
  geom_point(data = top_tracks, 
             aes(y = popularity, 
                 x=1:49, 
                 colour = pop_class), 
             alpha=ifelse(top_tracks$pop_class=="medium",1,0.3)) + 
  guides(colour = "none") + 
  geom_text(data=top_tracks, 
            aes(x=top_tracks$x_pos, 
                y=top_tracks_2$popularity), 
            label=ifelse(top_tracks$pop_class=="medium",top_tracks$short_name,""), 
            size=2.5, 
            position=position_jitter(width=1.5,height=1.01))

top_tracks <- top_tracks %>% head(n=40)
ggplot() + 
  geom_rect(data = rect_data, 
            aes(ymin=ystart, 
                ymax=yend, 
                xmin = -Inf, 
                xmax = Inf, 
                fill=popularity), 
            alpha=0.2) + 
  geom_point(data = top_tracks, 
             aes(y = popularity, 
                 x=1:40, 
                 colour = pop_class), 
             alpha=ifelse(top_tracks$pop_class=="highest",1,0.3)) + 
  guides(colour = "none") + 
  geom_text(data=top_tracks, 
            aes(x=top_tracks$x_pos+3, 
                y=top_tracks$popularity+2), 
            label=ifelse(top_tracks$pop_class=="highest",top_tracks$short_name,""), 
            size=2.5, 
            position=position_jitter(width=1,height=1)) +
  scale_fill_discrete(limits = c("highest","high","medium","low","lowest"))

pop_level = "lowest"
plot_label <- paste(toupper(pop_level), "Popularity Songs in June")

ggplot() + 
  geom_rect(data = rect_data, 
            aes(ymin=ystart, 
                ymax=yend, 
                xmin = -Inf, 
                xmax = Inf, 
                fill=popularity), 
            alpha=0.2) + 
  geom_point(data = top_tracks, 
             aes(y = popularity, 
                 x=1:40, 
                 colour = top_tracks$pop_class),
             alpha = ifelse(top_tracks$pop_class==(as.name(pop_level)),0.5,0.2))+ 
  guides(colour = "none") + 
  geom_text(data=top_tracks, 
            aes(x=top_tracks$x_pos, 
                y=top_tracks$popularity), 
            label=ifelse(top_tracks$pop_class==(as.name(pop_level)),top_tracks$short_name,""), 
            size=2.5, 
            position=position_jitter(width=1.01,height=1.03)) +
  scale_fill_discrete(limits = c("highest","high","medium","low","lowest")) +
  labs(x = "Most Listened --> Least Listened") +
  ggtitle(label = plot_label) +
  theme(plot.title=element_text(size=10))
  
# top 10 most listened to songs
ggplot() + 
  geom_rect(data = rect_data, 
            aes(ymin=ystart, 
                ymax=yend, 
                xmin = -Inf, 
                xmax = Inf, 
                fill=popularity), 
            alpha=0.2) + 
  geom_point(data = top_tracks, 
             aes(y = popularity, 
                 x=1:40, 
                 colour = top_tracks$pop_class),
             alpha = ifelse(top_tracks$x_po<=10,0,0.2))+ 
  guides(colour = "none") + 
  geom_text(data=top_tracks, 
            aes(x=top_tracks$x_pos+6.8, 
                y=top_tracks$popularity + 0.8,
                colour=top_tracks$pop_class), 
            label=ifelse(top_tracks$x_pos<=10,top_tracks$short_name,""), 
            size=2.5, 
            position=position_jitter(width=1.01,height=1.01)) +
  scale_fill_discrete(limits = c("highest","high","medium","low","lowest")) +
  labs(x = "Most Listened --> Least Listened") +
  ggtitle(label = "10 most listened to!!!") +
  theme(plot.title=element_text(size=10))
