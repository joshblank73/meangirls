#' Announces the number of candygrams for a person.
#'
#' @param person A vector of the candygram recipients
#' @param number A vector of how many grams they got
#' @param extra_message A vector of string giving extra commentary.
#'
#' @return Multiple candy gram announcements
#'
#' @importFrom stringr str_detect str_to_title str_trim
#' @importFrom english as.english
#'
#' @export
give_many_candygrams <- function(person, number, extra_message) {

  output <- rep(NA, length(person))


  for (i in 1:length(person)) {

    stopifnot(number[i] > 0)


    if (str_detect(person[i], "Gretchen")) {

      output[i] <- "None for Gretchen Weiners."

    }


    if (is.na(extra_message[i])) {

      current_extra_message <- add_commentary(person[i], number[i])

    }

    else current_extra_message <- extra_message[i]


    if (is.na(output[i])) {

      current_number <- str_to_title(as.english(number[i]))

      output[i] <- stringr::str_trim(glue::glue("{current_number} for {person[i]}. {current_extra_message}"))

    }

  }

  return(output)

}
