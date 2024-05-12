# Initialize English alphabet
englishLettersList <- function()
{
  letters
}

# Sort list
sortList <- function(lst)
{
  return(sort(lst))
}

# Find a first element of the list
getFirstElement <- function(lst) 
{
  return(lst[1])
}

# Find a last element of the list
getLastElement <- function(lst)
{
  return(lst[length(lst)])
}

# Generate segment ends needed for exponential distribution
equalSegmentEnds <- function(min, max, count)
{
  segmentLength <- (max - min) / count
  ends <- seq(min, max, by = segmentLength)[2:count]
  return(c(ends, max))
}

# Exponential distribution
exponentialDistribution <- function(lambda, power)
{
  exponentialPdf <- function(x, lambda)
  {
    lambda * exp(-lambda * x)
  }
  
  xValues <- seq(0.01, 1, length.out = power)
  yValues <- sapply(xValues, function(x) exponentialPdf(x, lambda))
  
  return(list(xValues, yValues))
}

multiplyItemsInList <- function(a, b) 
{
  return(a * b)
}

insertAtEnd <- function(lst, item) 
{
  if (length(lst) == 0) 
  {
    return(list(item))
  } 
  else 
  {
    return(c(lst, item))
  }
}

# Classify numbers according to segments
segmentIndices <- function(numbers, segments) 
{
  findSegmentIndex <- function(num) 
    {
    for (i in 1:(length(segments) - 1)) 
    {
      if (num >= segments[i] && num < segments[i + 1]) 
      {
        return(i)
      }
    }
    return(length(segments))
  }
  
  return(sapply(numbers, function(num) findSegmentIndex(num)))
}

# Transform numbers to letters using english alphabet
numbersToLetters <- function(numbers) 
{
  return(letters[numbers])
}

# Building a matrix 
getCrossOfMatrix <- function(currentItem, innerClassifiedListWithoutFirstItem, innerCurrentClass, innerNextClass) 
{
  if (length(innerClassifiedListWithoutFirstItem) == 0) 
  {
    return(0)
  }
  else
  {
    if (innerCurrentClass == currentItem && innerNextClass == innerClassifiedListWithoutFirstItem[1]) 
    {
      return(1 + getCrossOfMatrix(innerClassifiedListWithoutFirstItem[1], innerClassifiedListWithoutFirstItem[-1], innerCurrentClass, innerNextClass))
    }
    else
    {
      return(getCrossOfMatrix(innerClassifiedListWithoutFirstItem[1], innerClassifiedListWithoutFirstItem[-1], innerCurrentClass, innerNextClass))
    }
  }
}

getLineOfMatrix <- function(classifiedList, numOfClasses, currentClass)
{
  sapply(0:(numOfClasses - 1), function(current) 
  {
    if (current == numOfClasses) 
    {
      return("|")
    }
    else
    {
      return(getCrossOfMatrix(classifiedList[1], classifiedList[-1], currentClass, current))
    }
  })
}

displayAllLinesOfMatrix <- function(list, numOfClasses, letters) 
{
  cat("  ", paste(letters[1:numOfClasses], collapse = " "), "+\n")
  for (current in 1:numOfClasses) 
  {
    cat(letters[current], " ", paste(getLineOfMatrix(list, numOfClasses, current - 1), collapse = " "), "\n")
  }
}

# Generate list of random numbers from 1 to 500 
initialList <- replicate(100, sample(1:500, 1))

# Define alphabet power
alphabetPower <- 20

print("English Alphabet:")
print(englishLettersList())

print("Input list:")
print(initialList)

print("Sorted list:")
print(sortList(initialList))

print("First element of sorted list:")
print(getFirstElement(sortList(initialList)))

print("Last element of sorted list:")
print(getLastElement(sortList(initialList)))

print("Segment ends:")
print(equalSegmentEnds(getFirstElement(sortList(initialList)), getLastElement(sortList(initialList)), alphabetPower))

lambda <- 2
points <- exponentialDistribution(lambda, alphabetPower + 1)

print("Segments for exponential distribution:")
print(points)

exponentialSegment <- multiplyItemsInList(points[[2]], equalSegmentEnds(getFirstElement(sortList(initialList)), getLastElement(sortList(initialList)), alphabetPower))
exponentialSegment <- multiplyItemsInList(exponentialSegment,  rep(2.3, alphabetPower + 1))
exponentialSegmentEnds <- insertAtEnd(exponentialSegment, getLastElement(sortList(initialList)))

print("Segment indices:")
print(segmentIndices(initialList, exponentialSegmentEnds))

print("Transform number to letters:")
print(numbersToLetters(segmentIndices(initialList, exponentialSegmentEnds)))

print("Matrix:")
displayAllLinesOfMatrix(segmentIndices(initialList, exponentialSegmentEnds), length(exponentialSegmentEnds), letters)

