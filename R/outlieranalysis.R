#' @title OUTLIER ANALYSIS
#'
#' @description Package is useful for outlier detection and missing value analysis
#'
#'
#' @param data
#'
#'
#' @return Detailed table is printed in the console
#'
#' @examples outlieranalysis(data)
#'
#' @export

outlieranalysis <- function(data)
{
  if (!is.data.frame(data))
  {
    print('Please input data frame')
    return(NULL)
  }
  #creating a local function
  pmiss <- function(x)
  {
    return(round((sum(is.na(x))/length(x)*100),2))
  }

  miss <- apply(data, 2, pmiss)
  miss <- as.matrix(miss)
  colnames(miss) <- "% Missing"

  print(miss)

  uppercount = c()
  lowercount = c()
  name = c()
  data = Filter(is.numeric,data)
  j=1
  for(i in 1:ncol(data))
  {


    if (length(unique(data[,i])) > 10)
    {
      name[j] = names(data[i])
      x = data[i][!is.na(data[i])]

      upperbound = quantile(data[,i],0.75,na.rm = T) + 1.5 * IQR(data[,i],na.rm = T)
      lowerbound = quantile(data[,i],0.25,na.rm = T) - 1.5 * IQR(data[,i],na.rm = T)

      uppercount[j] = sum(x>upperbound)
      lowercount[j] = sum(x<lowerbound)
      j = j+1

    }

  }

  outlier = matrix(data = c(uppercount,lowercount), nrow = length(name), dimnames = list(name,c(' upper outlier','lower outlier')))

  print(outlier)
}
