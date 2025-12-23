#' NORMALISE_PP_to_SP
#'
#' This function helps selecting the type of normalisation for paired-pulse data
#'
#' The functions goes over the 'data' file.
#' We expect 'data' to have one column for each subject, with ID on top.
#' We handle also the option where the format is wide
#' The first row has subject's ID ("ID")
#' All the other rows are raw excitability measures, which alternate between single-pulse and paired-pulse (e.g., ID, SP_t0, PP_t0, SP_t1, PP_t1, SP_t2 etc.)
#'
#' @param data Single participant data, including their ID
#' @param format How are the data shaped? "Long" or "Wide"
#' @param number_of_triggers How many total measures do you have? For example, 4 means two single-pulse and two paired-pulse
#' @param normalise What type of normalisation do you want for the paired-pulse data? Can choose from five options: (1) (PP_tX / SP_tX); (2) (PP_tX - SP_tX) / SP_tX; (3) (PP_tX/SP_tX) / (PP_t0 / SP_t0); (4) natural_log(PP_tX / SP_tX); (5) (PP_tX / SP_t0). 'tX' means that this values changes based on the time-point (it could be t_1, t_2, T_3 etc.). 't0' means that this value is fixed at the baseline/starting value
#' @return A file in wide format with all the single- and paired-pulse data normalised
#' @export
#' @examples
#' # Basic usage
#' data_wide <- data.frame(id = 999, t(rnorm(10, mean = 1, sd = 0.1)))
#' NORMALISE_PP_to_SP(data_wide, format = "Wide", number_of_triggers = 10 , normalise = "C")
#'
#' data_long <- data.frame(value = c(999, rnorm(10, mean = 1, sd = 0.1)))
#' NORMALISE_PP_to_SP(data_long, format = "Long", number_of_triggers = 10 , normalise = "A")
NORMALISE_PP_to_SP <- function(data,
                               format,
                                number_of_triggers,
                                normalise
                               ){



  if (format == "Long"){
    data <- as.data.frame(t(data))
    }


  current_ID <- data[1]


  expected_cols <- number_of_triggers + 1  # +1 for ID column
  if (ncol(data) != expected_cols) {
    stop(paste0("Data has ", ncol(data), " columns but expected ", expected_cols,
                " (ID + ", number_of_triggers, " triggers)"))
  }



  TS_alone_list <- list()
  CS_TS_list <- list()


  for (i in 1:number_of_triggers){

    if (i %% 2 != 0 ){
      next
    }

    #divide the trigger by groups
    TS_alone_list[[paste0("TS_alone", i)]] <- data[i]
    CS_TS_list[[paste0("CS_TS", i)]] <- data[i+1]

  }

  TS_alone <- as.data.frame ( unlist(TS_alone_list) )
  colnames(TS_alone)[1] <- "value"
  TS_alone$value <- as.numeric(TS_alone$value)


  CS_TS <- as.data.frame (unlist(CS_TS_list) )
  colnames(CS_TS)[1] <- "value"
  CS_TS$value <- as.numeric(CS_TS$value)


  # Check for valid data
  if (any(is.na(TS_alone$value)) || any(is.na(CS_TS$value))) {
    warning("NA values detected in data")
  }


  if (normalise == "byTimePoint" | normalise == 1 | normalise == "A"){

    df_TS <- TS_alone / TS_alone
    df_CS_TS <- CS_TS / TS_alone

    combined <- rbind(df_TS, df_CS_TS)


  }

  else if (normalise == "byTimePoint_with_subtraction" | normalise == "B" | normalise == 2){

    df_TS <- as.data.frame(rep(0, nrow(TS_alone)))
    colnames(df_TS) <- "value"


    #df_TS <- (TS_alone - TS_alone) / TS_alone
    df_CS_TS <- (CS_TS - TS_alone) / TS_alone

    combined <- rbind(df_TS, df_CS_TS)


  }

  else if (normalise == "PP_SP_ratio" | normalise == "C" | normalise == 3){

    df_TS <- (CS_TS / TS_alone) / (CS_TS[1,1] / TS_alone[1,1])
    df_CS_TS <- (CS_TS / TS_alone) / (CS_TS[1,1] / TS_alone[1,1])

    combined <- rbind(df_TS, df_CS_TS)

     }


  else if (normalise == "byTimePoint_with_NatLog" | normalise == "D" | normalise == 4){

    df_TS <- log(TS_alone / TS_alone)
    df_CS_TS <- log(CS_TS / TS_alone)

    combined <- rbind(df_TS, df_CS_TS)


  }



  else if (normalise == "to_SP_t0" | normalise == "E" | normalise == 5){

    baseline <- TS_alone[1, 1]
    df_TS <- TS_alone / baseline
    df_CS_TS <- CS_TS / baseline


    } else {
      stop(paste0("Unknown normalization method: ", normalise))
    }


  # Combine and interleave
  combined <- rbind(df_TS, df_CS_TS)
  n <- nrow(df_TS)
  indices <- c(rbind(1:n, (n + 1):(2 * n)))
  result <- as.data.frame(combined[indices, ])
  rownames(result) <- NULL

  # Transpose and add ID
  result <- as.data.frame(t(result))
  result <- cbind(current_ID, result)

  # Create column names (FIXED: consistent naming)
  n_cols <- ncol(result)
  n_pairs <- (n_cols - 1) / 2
  new_names <- c("ID")

  for (i in 1:n_pairs) {
    new_names <- c(new_names, paste0("TS_alone", i), paste0("CS_TS_", i))
  }

  colnames(result) <- new_names

  return(result)

  }




#' NORMALISE_PP_to_SP
#'
#' This function is a wrapper for 'SRTT_analysis' to handle dataframes as input data
#'
#'
#' The functions goes over the 'data' file.
#' We expect 'data' to have one column for each subject, with ID on top.
#' We handle also the option where the format is wide
#' The first row has subject's ID ("ID")
#' All the other rows are raw excitability measures, which alternate between single-pulse and paired-pulse (e.g., ID, SP_t0, PP_t0, SP_t1, PP_t1, SP_t2 etc.)
#'
#' @param data Single participant data, including their ID
#' @param format How are the data shaped? "Long" or "Wide"
#' @param number_of_triggers How many total measures do you have? For example, 4 means two single-pulse and two paired-pulse
#' @param normalise What type of normalisation do you want for the paired-pulse data? Can choose from five options: (1) (PP_tX / SP_tX); (2) (PP_tX - SP_tX) / SP_tX; (3) (PP_tX/SP_tX) / (PP_t0 / SP_t0); (4) natural_log(PP_tX / SP_tX); (5) (PP_tX / SP_t0). 'tX' means that this values changes based on the time-point (it could be t_1, t_2, T_3 etc.). 't0' means that this value is fixed at the baseline/starting value
#' @return A file in wide format with all the single- and paired-pulse data normalised
#' @export
#' @examples
#' # Basic usage
#' ID <- LETTERS[1:5]
#' dataframe_wide <- data.frame(ID = ID, matrix(stats::rnorm(5 * 16, mean = 1, sd = 0.1), nrow = 5, ncol = 16) )
#' NORMALISE_PP_to_SP_forDataframe(data = dataframe_wide, format = "Wide", number_of_triggers = 16 , normalise = 3)
#'
#' dataframe_long <- as.data.frame(t(dataframe_wide))
#' NORMALISE_PP_to_SP_forDataframe(data = dataframe_long, format = "Long", number_of_triggers = 16 , normalise = 4)
NORMALISE_PP_to_SP_forDataframe <- function(data,
                                            format,
                                            number_of_triggers,
                                            normalise
                                            ){



  # initialise empty dataframe
  new <- NULL


  if (format == "Long"){
    data <- as.data.frame(t(data))
  }


  format <- "Wide"


    for (i in 1:nrow(data)){
      new <- rbind(new, NORMALISE_PP_to_SP(data[i, ],
                                      format = format,
                                      number_of_triggers = number_of_triggers,
                                      normalise = normalise))
    }

  rownames(new) <- NULL

  return(new)



}


#' plot_data_for_Dataframes
#'
#' This function handles dataframes with one or multiple IDs as input data
#' this is a function to visualize excitability measures
#' each dot is a response
#' X-axis = type of excitability measures (TS_alone or CS & TS)
#' Y-axis = excitability amplitude
#'
#' @param data Dataframe including participant data, and their ID
#' @param format How are the data shaped? "Long" or "Wide"
#' @param col_line What color do you want to use for the line? (e.g., "black")
#' @param col_dots What color do you want to use for the dots? (e.g., "grey")
#' @param error_measure How do you want to calculate the group-wise error measure? "sd" for standard deviation or "se" for standard error of the mean
#' @return Three figures returned in a list and modifiable
#' @export
#' @examples
#' # Basic usage
#' ID <- LETTERS[1:5]
#' dataframe_wide <- data.frame(ID = ID, matrix(stats::rnorm(5 * 16, mean = 1, sd = 0.5), nrow = 5, ncol = 16) )
#' data <- NORMALISE_PP_to_SP_forDataframe(data = dataframe_wide, format = "Wide", number_of_triggers = 16 , normalise = 5)
#' plots <- plot_data_for_Dataframes(data, format = "Wide", col_line = "grey", col_dots = "grey", error_measure = "se")
#' plot1 <- plots$plot1
#' plot1 + ggplot2::geom_hline(yintercept = 1, col = "red")
#' plots$plot2
#' plots$plot3
#' data_wide <- data[1, ]
#' plots <- plot_data_for_Dataframes(data_wide, format = "Wide", col_line = "grey", col_dots = "grey", error_measure = "se")
#' plots$plot3
plot_data_for_Dataframes <- function(data,
                                       format = "Wide",
                                       col_line = "grey",
                                       col_dots = "grey",
                                       error_measure = "se"
){


  # Make sure data is a data.frame
  data <- as.data.frame(data)


  # if format == "Long" , we need to save ID
  if (format == "Long"){

    data <- as.data.frame( t (data))

  }


  colnames(data)[1] <- "ID"
  ID <- data$ID

  #data <- subset(data, select=-ID)

  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  # filter whether it is Skill or average RT data

  TS_alone <- dplyr::select(data, starts_with("TS"))

  TS_alone <- cbind(ID , TS_alone)

  CS_TS <- dplyr::select(data, !starts_with("TS"))

  CS_TS <- cbind(ID , CS_TS)


  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  # pivot_longer
  TS_alone <- tidyr::pivot_longer(TS_alone,
                                  cols=-ID,
                                  names_to='var',
                                  values_to='value')

  TS_alone$time <- substr(TS_alone$var, nchar(TS_alone$var), nchar(TS_alone$var))

  CS_TS <- tidyr::pivot_longer(CS_TS,
                               cols=-ID,
                               names_to='var',
                               values_to='value')

  CS_TS$time <- substr(CS_TS$var, nchar(CS_TS$var), nchar(CS_TS$var))




  TS_alone_and_CS_TS <- tidyr::pivot_longer(data,
                                            cols=-ID,
                                            names_to='var',
                                            values_to='value')

  TS_alone_and_CS_TS$time <- substr(TS_alone_and_CS_TS$var, nchar(TS_alone_and_CS_TS$var), nchar(TS_alone_and_CS_TS$var))


  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


  # order the x-axis variables as factors

  TS_alone$time <- as.numeric(TS_alone$time)
  CS_TS$time <- as.numeric(CS_TS$time)
  TS_alone_and_CS_TS$time <- as.numeric(TS_alone_and_CS_TS$time)


  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  TS_alone$value <- as.numeric(TS_alone$value)

  CS_TS$value <- as.numeric(CS_TS$value)

  TS_alone$ID <- as.character(TS_alone$ID)

  CS_TS$ID <- as.character(CS_TS$ID)


  TS_alone_and_CS_TS$value <- as.numeric(TS_alone_and_CS_TS$value)

  TS_alone_and_CS_TS$value <- as.numeric(TS_alone_and_CS_TS$value)



  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  TS_alone$group <- "A"
  CS_TS$group <- "A"
  TS_alone_and_CS_TS$group <- "A"

  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


  # use the summary function to find mean and se/sd

  TS_alone_mean <- Rmisc::summarySE(TS_alone, measurevar = "value", groupvars = "var", na.rm=T)

  TS_alone_mean$group <- "A"

  CS_TS_mean <-  Rmisc::summarySE(CS_TS, measurevar = "value", groupvars = "var", na.rm=T)

  CS_TS_mean$group <- "A"



  TS_alone_and_CS_TS_mean <-  Rmisc::summarySE(TS_alone_and_CS_TS, measurevar = "value", groupvars = "var", na.rm=T)

  TS_alone_and_CS_TS_mean$group <- "A"



  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  # need to set the right order for the x-axis variables

  TS_alone_and_CS_TS_mean <- TS_alone_and_CS_TS_mean  |>
    dplyr::mutate(
      num = as.numeric(stringr::str_extract(var, "\\d+$")),
      is_ts_alone = stringr::str_detect(var, "^TS_alone")
    )  |>
    dplyr::arrange(num, dplyr::desc(is_ts_alone))  |>
    dplyr::mutate(var = factor(var, levels = unique(var)))  |>
    dplyr::select(-num, -is_ts_alone)

  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



  p1 <- ggplot2::ggplot(TS_alone,
                        ggplot2::aes(var, value))+
    ggplot2::geom_point(ggplot2::aes(col = ID), size = 0.5)+
    ggplot2::geom_line(ggplot2::aes(group = ID , col = ID), linewidth  = 0.2, alpha = 0.4)+
    ggplot2::geom_line(data = TS_alone_mean, ggplot2::aes(group = group ) , col = col_dots, linewidth  = 1, alpha = 1)+
    ggplot2::geom_errorbar(data = TS_alone_mean,
                           ggplot2::aes(x = var, ymin = value - get(error_measure), ymax = value + get(error_measure)), linewidth = 2 , width = .1, colour = col_line) +
    ggplot2::geom_point(data = TS_alone_mean, col = col_dots, size = 4)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "none")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))+
    ggplot2::theme(axis.title.x=ggplot2::element_blank())+
    ggplot2::ggtitle("TS Alone")



  p2 <- ggplot2::ggplot(CS_TS,
                  ggplot2::aes(var, value))+
    ggplot2::geom_point(ggplot2::aes(col = ID), size = 0.5)+
    ggplot2::theme_bw()+
    ggplot2::geom_line(ggplot2::aes(group = ID, col = ID), linewidth  = 0.1, alpha =0.4)+
    ggplot2::geom_line(data = CS_TS_mean, ggplot2::aes(group = group ) , col = col_dots, linewidth  = 1, alpha = 1)+
    ggplot2::geom_errorbar(data = CS_TS_mean,
                           ggplot2::aes(x = var, ymin = value - get(error_measure), ymax = value + get(error_measure)), linewidth = 2 , width = .1, colour = col_line) +
    ggplot2::geom_point(data = CS_TS_mean, col = col_dots, size = 4)+
    ggplot2::theme(legend.position = "none")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))+
    ggplot2::theme(axis.title.x=ggplot2::element_blank())+
    ggplot2::theme(axis.title.y=ggplot2::element_blank())+
    ggplot2::ggtitle("CS + TS")



  p3 <- ggplot2::ggplot(TS_alone_and_CS_TS,
                       ggplot2::aes(var, value))+
    ggplot2::geom_point(ggplot2::aes(col = ID), size = 0.5)+
    ggplot2::theme_bw()+
    ggplot2::geom_line(ggplot2::aes(group = ID, col = ID), linewidth  = 0.1, alpha =0.4)+
    ggplot2::geom_errorbar(data = TS_alone_and_CS_TS_mean,
                           ggplot2::aes(x = var, ymin = value - get(error_measure), ymax = value + get(error_measure)), linewidth = 2 , width = .1, colour = col_line) +
    ggplot2::geom_point(data = TS_alone_and_CS_TS_mean, col = col_dots, size = 4)+
    ggplot2::geom_line(data = TS_alone_and_CS_TS_mean, ggplot2::aes(group = group ) , col = col_dots, linewidth  = 1, alpha = 1)+
    ggplot2::theme(legend.position = "none")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))+
    ggplot2::theme(axis.title.x=ggplot2::element_blank())+
    ggplot2::theme(axis.title.y=ggplot2::element_blank())+
    ggplot2::ggtitle("TS Alone & CS + TS")


  return(list(plot1 = p1, plot2 = p2, plot3 = p3))



}
