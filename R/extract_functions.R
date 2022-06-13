

#' Extract a single region of the alignment
#'
#' @param query A DNAStringSet Object. It must be set.
#' @param start A vector indicating each start position. Defaults to 1.
#' @param end A vector indicating each end position. Defaults to 1.
#'
#' @return The function returns a DNAStringSet of the selected region of the alignment according to the
#'         Start and End parameters.
#'
#' @export
#' @examples
#' extract_seq("acccataaaa",start=2,end=8)
#' extract_seq("acccataaaa",1,4)

extract_seq<-function(query,start,end){
  stopifnot("`start` must be lower or equal to `end`
            for each pair " = (start <= end))
  stopifnot("`query` must be larger or equal to the region width
            to extract" = (Biostrings::width(query[1]) >= (end-start)))

  subsequence<-Biostrings::subseq(query,start,end)
  return(Biostrings::DNAStringSet(subsequence))
}




#' Extract multiple regions of an alignment concatenated
#'
#' Use this function to extract specific regions of interest from
#' an alignment concatenated sequentially.
#'
#' @param query A DNAStringSet Object. It must be set.
#' @param start A vector indicating each start position for each region. Defaults to 1.
#' @param end A vector indicating each end position for each region. Defaults to 1.
#'
#' @return The function returns a DNAStringSet of the selected regions of the alignment according to the
#'         Start and End parameters.
#' @export
#'
#' @examples
#' extract_multi_gap(c("acccataaaa","agtaatagat"),
#'                  start=2,
#'                  end=8)
#'
#' extract_multi_gap(c("acccataaaa","agtaatagat"),
#'                  start=2,
#'                  end=8)

extract_multi_gap<-function(query,start=1,end=1){
  stopifnot("query must be set" =
              !missing(query))
  stopifnot("`start` must be a numeric" =
              is.numeric(start))
  stopifnot("`start` must be of same length than `end` " =
              (length(start) == length(end)))


  subsequence<-extract_seq(query,start[1],end[1])

  if (length(start)>1){

    for (i in 2:length(start)) {
      subsequence_add<- extract_seq(query,start[i],end[i])
      final_sequence<-Biostrings::xscat(subsequence,subsequence_add)

    }
  }else{
    final_sequence<-subsequence
  }
  names(final_sequence)<-names(query)

  return(final_sequence)

}

