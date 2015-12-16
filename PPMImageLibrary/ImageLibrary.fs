//
// F#-based PPM image library.
//
// Harsh Patel
// U. of Illinois, Chicago
// CS341, Fall2015
// Homework 6
//
module PPMImageLibrary

#light


//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage(image:int list list) = 
  if image = [] then
    printfn "**END**"
  else
    printfn "%A" image.Head
    OutputImage(image.Tail)
           
let DebugOutput(width:int, height:int, depth:int, image:int list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage(image)

// This function removes the first pixel of the image and return the image.
let rec removeHead (L: int list list) = 
  if L = [] then []
  else L.Head.Tail.Tail.Tail :: removeHead L.Tail

// This function takes in a list and returns the head of the list.
let rec getHead (L: int list) = 
 let a =  [L.Head; L.Tail.Head; L.Tail.Tail.Head]
 a

//
// TransformFirstRowWhite:
//
// An example transformation: replaces the first row of the given image
// with a row of all white pixels.
//
let rec BuildRowOfWhite cols white = 
  if cols = 0 then
    []
  else 
    // 1 pixel, i.e. RGB value, followed by remaining pixels:
    white :: white :: white :: BuildRowOfWhite (cols-1) white

let FirstRowWhite(width:int, height:int, depth:int, image:int list list) = 
  // first row all white :: followed by rest of original image
  (BuildRowOfWhite width depth) :: image.Tail

// Helper function that converts a list list of int to a list of string.   
let rec save (image: int List List, newList: string list) = 
  if image = [] then
    newList
  else
    let L = List.map (fun x -> x.ToString()) image.Head @ newList
    save(image.Tail, L)

//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let WriteP3Image(filepath:string, width:int, height:int, depth:int, image:int list list) = 
  //
  // Here's one possible strategy: build a list of strings, then WriteAllLines.
  // Each string appears on a separate line. 
  //
  let header = ["P3" ; (string width) + " " + (string height); (string depth)]
  let imgStr = [""]
  let newImage = List.rev image
  let imageString = save(newImage, imgStr)
  let L = header @ imageString
  System.IO.File.WriteAllLines(filepath, L)
  //
  true  // success

//This function takes in a list of integers, and then calculates
//the average of each pixel, and replaces that pixel with the average.
//It acts as a helper function for the Grayscale function.
let rec avgMaker (L : int list)  = 
// PRE: L must be initialized
// POST: an average list of the list entered.
    if L = [] then
      []
    else 
      let avg = ((L.Head + L.Tail.Head + L.Tail.Tail.Head) / 3)
      let avgLst = [avg;avg;avg]
      avgLst @ avgMaker L.Tail.Tail.Tail

// This function takes in an image and makes it grayscale.
let rec Grayscale(width:int, height:int, depth:int, image:int list list) =
// PRE: width, height, depth and image must be initialized.
// POST: a grayscale image of the image entered. 
  let L = []
  // Map the average function on each list in the list list.
  let grayImg = List.map (fun x -> avgMaker (x))image
  grayImg


// A helper function to help treshold
let tresholdConverter(L: int list, treshold:int) =
// PRE: L and treshold must be initialized.
// POST: A list wiht tresholded values. 
// If the number is greated than treshold then make the pixel have the value of 255
// if it is less than treshold, then let it have the value of 0.
  let newImg = List.map(fun x -> if x >= treshold then 255 else if x < treshold then 0 else x) L
  newImg

// This function takes in an image and returns an image with tresholded values.
let rec Threshold(width:int, height:int, depth:int, image:int list list, threshold:int) =
// PRE: width, height, depth, image and treshold must be initialized.
// POST: An image that is tresholded to the treshold values.
// For all the list in the list list, run tresholdConverter on it.
  let tesholdImage = List.map(fun x -> tresholdConverter(x,threshold)) image
  tesholdImage

// Helper function for flip horizontal
let rec reverse (L:int list , newLst: int list) =
// PRE: L and newLst must be initialized.
// POST: reverses the list. 
// if empty then return the list, else take the first three values and put it 
  if L = [] then
    newLst
  else
  // append to the reverse list and send it in with the function call
    let revLst = [L.Head; L.Tail.Head; L.Tail.Tail.Head] @ newLst
    reverse (L.Tail.Tail.Tail, revLst)
    
// The function that flips the picture horizontally.
let rec FlipHorizontal(width:int, height:int, depth:int, image:int list list) = 
// PRE: width, height, depth, and image must be initialized
// POST: flipps the image that was entered and returns it.
  let emtLst = [];
  let flippedImg = List.map (fun x -> reverse (x,emtLst)) image
  flippedImg

  // ---------- Zooming in -------------------- //
// A helper function for the zoom function.
let rec rep (L:int list, zf) = 
// PRE: L and zf must be initialized.
// POST: a list with replicated pixel values so it looks like we zoomed in.
  if L = []  then [] else
  let head = getHead(L)
  // get the head pixel and replicate it zoom factor times.
  let repHead = List.replicate zf head
  repHead :: rep (L.Tail.Tail.Tail, zf)
// Zoom in.
let rec Zoom(width:int, height:int, depth:int, image:int list list, factor:int) =
// PRE: width, height, depth, image and factor must be initialized.
// POST: An image that is zoomed in.
// zoom in to each list and then combine the list list list list so it is a list list.
 let b = List.map (fun x -> rep (x,factor)) image                  
 let c = List.map (fun x -> List.concat x) b
 let c = List.map (fun x -> List.concat x) c
 let c = List.map (fun x -> List.replicate factor x) c
 let zoomedImg = List.collect (fun x -> x) c
 zoomedImg

// This function takes in a list list, and tranposes the list, so that all the 
// month info are grouped in one row of the list.
let rec transpose (L: int list list) = 
// PRE: L must be initialized
// POST: transposes the array of pixels and returns the transposed array.
// boiler plate check
  if L = [] then []
// if the list is empty then done!
  else if L.Head = []  then []
  else
// create a new list with one less column and concat the list
// with the new list...
  // let returnList = 
  let passList = List.map getHead L
  let newList = removeHead L
  passList :: transpose (newList)


let rec RotateRight90(width:int, height:int, depth:int,image:int list list) =
// PRE: width, height, depth and image must be initialized.
// POST: Rotates the image by 90 deg CW and returns the image.
  let finalLst  = []
  // To rotate, tanspose and reverse.
  let newImage = transpose(image)
  let rotatedImage =  List.map (fun x -> List.concat x) newImage
  let emtLst = []
  let rotatedImg = List.map (fun x -> reverse (x,emtLst)) rotatedImage
  rotatedImg
  