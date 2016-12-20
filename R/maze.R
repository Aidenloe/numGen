#' @export
#' @import igraph
#' @importFrom grDevices dev.off
#' @importFrom grDevices png
#' @importFrom graphics grconvertX
#' @importFrom graphics grconvertY
#' @importFrom graphics plot
#' @param rank This is the Rank of the maze.
#' @param saturation The number of black dots created for a given grid.
#' @param seed To make sure that the randomness of the created black dots is captured and not repeated.
#' @param grid is the grid of the maze
#' @param wd is the working directory to save the HTML source code in. If not given, the file will be saved in the default working directory.
#' @param background The background colour of the page.
#' @param boxBackground The background colour of the box.
#' @param fontColour The font colour of the instructions.
#' @param Timer If True, a time limit of 4 mintues is given per question.
#' @description This function generates an Elithorn Maze
#' @details This function creates a maze into your folder.
#' A grid object needs to be called out first before runing the maze function.
#' The grid object needs to be the same as the rank given.
#' @author Aiden Loe
#' @title maze
#' @examples
#'
#' rank <- 3
#' i <- 2
#' saturation <- 0.5
#'
#' #Grid must be same as rank
#' grid <- gridThreeUp
#'
#' #Folder to save html/
#' setwd("~/desktop")
#' filePath<- getwd()
#'
#' #Generate item
#' maze(rank= 3,seed=5,saturation,grid = gridThreeUp,wd=filePath,
#' background="#7abcff",boxBackground="#66CDAA", fontColour="white ",
#' Timer=TRUE)
#'
#'
#'

maze <- function(rank = 3,
                 saturation = 0.5,
                 seed = 1,
                 grid = NULL,
                 wd = NULL,
                 background="#7abcff",
                 boxBackground = "#66CDAA",
                 fontColour="white",
                 Timer=TRUE){

  if(is.null(grid)){
    stop("Please select a grid of a specific rank to construct the maze.")
  }

  if(is.null(wd)){
    warning("HTML file is saved in default working directory.")
  }
  G <- graph(genMaze(rank), directed = TRUE )

  set.seed<- seed
  saturation <- saturation

  #Node length
  topNodes <- topNodes(rank)
  topNodes

  #lowerGrid
  lowerGridCombind<- lowerGrid(rank)


  #### Calculate Path to node length ####
  allPaths <- all_simple_paths(G, 1,topNodes)
  allPaths

  #saturation and node Position
  nodePosition <- colourNodePosition(rank, saturation,set.seed)
  nodePosition

  # max score
  maxscore <- maxScore(rank, nodePosition)
  maxScore <- max(maxscore$totalScore)
  maxscore

  #minimum steps to achieve maximum score
  #minStep(rank, topNodes)

  # number of optimised Routes
  #maxScoreRoutes(rank, nodePosition)

  #Complete solution
  #solution(rank, saturation,set.seed)

  ##### From Here (HTML) ####
  if(is.null(wd)){
    wd = getwd()
  }

##### From Here (HTML) ####
htmlfile = file.path(paste0(wd, "/seed",seed,".html"))
##### From Here (HTML) ####
cat("\n<html><head>",file=htmlfile)
# CSS
cat(paste0("
    <style>
    html {height: 100%}

    #center {
    width: 50%;
    margin: auto;

}
body, td {
font-family: serif;
background:", background, ";/* Old browsers */
/* background: -moz-linear-gradient(top, #7abcff 0%, #60abf8 44%, #4096ee 100%);  FF3.6-15 */
/* background: -webkit-linear-gradient(top, #7abcff 0%,#60abf8 44%,#4096ee 100%);  Chrome10-25,Safari5.1-6 */
/* background: linear-gradient(to bottom, #7abcff 0%,#60abf8 44%,#4096ee 100%);*/
font-size: 13 px;
}\n


.myButton{
width:0.001px;
height:0.001px;
background-color:#DBFEF8;
-moz-border-radius:28px;
-webkit-border-radius:28px;
border-radius:28px;
border:1px solid #18ab29;
cursor:pointer;
color:#000000;
font-family:Arial;
font-size:10px;
padding:12px 10px;
margin: -12px -12px;
text-decoration:none;
text-shadow:none;
position:absolute;

}

.myButton:hover {
background-color:#DBFEF8;
}
.myButton:active {
background-color:#DBFEF8;
}

.myButtonTwo{
width:0.001px;
height:0.001px;
background-color:gold;
-moz-border-radius:28px;
-webkit-border-radius:28px;
border-radius:28px;
border:1px solid #18ab29;
cursor:pointer;
color:#000000;
font-family:Arial;
font-size:10px;
padding:12px 10px;
margin: -12px -12px;
text-decoration:none;
text-shadow:none;
position:absolute;

}
.colour{
opacity:0.0;
}
/* .myButtonTwo:hover {
background-color:#FFCF75;
} */
/*.myButtonTwo:active {
background-color:#FFCF75;
}*/


.buttonPosition{
position:absolute;
}

.box {
margin-top:10px;
margin-bottom:auto;
border:10px solid white;
max-width:900px;
padding:20px;
border-radius: 20px;
-webkit-border-radius: 15px;
-moz-border-radius: 15px;
background:", boxBackground,";
color:white;
font-weight:bold;
margin:50px auto;
/*height:550px;*/
/*width: 600px;*/
height:850px;
width: 920px;
}
</style>
"), append= TRUE, file = htmlfile)
cat("\n</head>", append=TRUE, file = htmlfile)
cat("\n<br>", append=TRUE, file = htmlfile)
cat(paste0("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;\"><span style=\"color: ",fontColour,";font-size:25px\">Level {{level}} out of {{t_question}}.</span></p>"),append=TRUE, file = htmlfile)
cat("\n<body>", append = TRUE, file = htmlfile)

cat(paste0("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:20px;\"><font color=\"",fontColour,"\">The goal is to collect as many gold coins as possible as you plan your route up to the top.</font></p>"), append=TRUE, file=htmlfile)
cat(paste0("\n<p align=\"center\" style=\"font-family:lucida sans unicode,lucida grande,sans-serif;font-size:20px;\"><font color=\"",fontColour,"\">To start, click on the first node at the bottom of the maze.</font></p>"), append=TRUE, file=htmlfile)
if(Timer==TRUE){
cat("\n<input id=\"countdown\" name=\"timeLeft\" type=\"hidden\" />", append=TRUE, file=htmlfile)
cat(paste0("\n<div style=\"text-align: center; font-size:35px\"><font color=\"",fontColour,"\">Timer: <span id=\"countdown2\">&nbsp;</span></font></div>"), append=TRUE, file=htmlfile)
}

#Must normalise coordinates
#grconvertX and gconvertY create the right pixel coordinates.

coordinates.1 <- grid
plot(coordinates.1)
coordinates.2<- coordinates.1[which(1:nrow(coordinates.1) %in% lowerGridCombind),]
plot(coordinates.2)


# Plot Graph
#save empty .png
png(filename="map.png", height=1000, width=1000)
#plot graph, png must always be forced.
#AIG:::plot.logic.map(G, png=FALSE, layout = coordinates.2, height=1000, width=1000, v.size=10, vertex.label.cex=0.5,vertex.shape="square", xlab=".", ylab='.',cex.lab=0.1) # plot using desired coordinates)
#AIG:::plot.logic.map(G, layout=coordinates.1,height=1000, width=1000)
plot(G, layout=coordinates.1)
# combind vector of coordinates
#coord. <- cbind(grconvertX(coordinates.2[, 1], "user", "device"), cbind(grconvertY(coordinates.2[, 2], "user", "device")))
coord.node <- cbind(grconvertX(coordinates.1[, 1], "user", "device"), cbind(grconvertY(coordinates.1[, 2], "user", "device")))
coord. <- cbind(grconvertX(coordinates.1[lowerGridCombind, 1], "user", "device"), cbind(grconvertY(coordinates.1[lowerGridCombind, 2], "user", "device")))
coord.1 <- apply(coord., 1:2, function(x) x/1.2) #adjust node coordinates
dev.off()
coordinates.1

#### TO here (CSS) ####
cat("\n<div align=center>", append = TRUE, file=htmlfile)
cat("<div class=box>", append=TRUE, file=htmlfile)

#margin:0 auto puts container in the center when it is a fixed width.
cat("\n<div style= 'position:relative;width:auto; height:auto;margin:0 auto' id = 'graphContainer'>", append=TRUE, file=htmlfile)  #position:relative(parent)

#Position of the buttons is relative to the div position
#z index puts the div tag at the top, so the nodes are above the edges.
colouredPoint <- ifelse(lowerGridCombind %in% nodePosition,"black","non-black")
n.name <-  lowerGridCombind
lowerGridCombind
nodePosition
colouredPoint
buttons = ""
for (j in 1:nrow(coord.)){
  buttons <- paste0(buttons,"\n<div onClick='nodeClick(this)' id = '", n.name[j],"'", " class =",if(colouredPoint[j]=="non-black"){'\"myButton\"'}else{'\"myButtonTwo\"'}, " style = 'z-index:1; left:",(coord.1[j,1]),"px;top:",(coord.1[j,2]),"px'> <span class=colour>",n.name[j],"</span></div>")
}

# for (j in 1:nrow(coord.)){
#   buttons <- paste0(buttons,"\n<div onClick='nodeClick(this)' id = '", j,"'", " class ='myButton' style = 'z-index:1; left:",(coord.1[j,1]),"px;top:",(coord.1[j,2]),"px'>",n.name[j],"</div>")
# }

buttons
cat(buttons, append=TRUE, file=htmlfile)

ed<- ends(G, E(G), names=FALSE)
lowerGridCombind
ed <- as.data.frame(ed)
ed.1 <- ed[ed$V1 %in% lowerGridCombind,]
ed.2 <- ed.1[ed.1$V2 %in% lowerGridCombind,]
ed.2

##### Extract start node coordinates.
#Number of rows we need from vector.
start.index <- ed.2[,1]

#Number of columns we need for matrix.
start.coord <- ncol(coord.)

#Empty Matrix
start.node.coord <- matrix(NA, nrow = length(start.index), ncol = start.coord)
length(start.index)
for(i in 1:length(start.index)){
  start.node.coord[i,]<- coord.node[start.index[i],]
}
start.node.coord.1 <- apply(start.node.coord,1:2, function(x) x/1.2)
start.node.coord

##### Extract end node coordinates.
end.index<- ed.2[,2]
end.coord <- ncol(coord.)

#Create empty matrix
end.node.coord <- matrix(NA, nrow = length(end.index), ncol= end.coord)
for(i in 1:length(end.index)){
  end.node.coord[i,] <- coord.node[end.index[i],]
}
end.node.coord.1 <- apply(end.node.coord,1:2, function(x) x/1.2) #adjust edge coord

end.node.coord.1


# This needs to change for the java script so that the arrows are highlighted.
x <- nrow(ed.2)
uniDirection = 2
direction <- rep(2,times=x)
arrowDirect <- cbind.data.frame(ed.2,direction)
arrowDirect
#### Edges of Nodes
connections = ""
for (i in 1:nrow(ed.2)){
  if(arrowDirect$direction[i]==2){
    connections <- paste0(connections,"
                          <defs>
                          <marker id=\"arrow\" markerWidth=\"100\" markerHeight=\"50\" refx=\"15\" refy=\"6\" orient=\"auto\">
                          <path id=\"colourArrow\" d=\"M2,1 L2,10 L10,6 L2,2\" style=\"fill:blue\" />
                          </marker>
                          </defs>
                          <path id=",paste0('"',ed.2[i,1],'_',ed.2[i,2],'"'), " d=","\"M",start.node.coord.1[i,1],',',start.node.coord.1[i,2],',L',end.node.coord.1[i,1],',',end.node.coord.1[i,2],"\" style=\"stroke:black; stroke-width: 3.25px; fill: none ;marker-end: url(#arrow",i,");\" >
                          </path> ")
  }else{
    connections <- paste0(connections,"
                          <path id=",paste0('"',ed.2[i,1],'_',ed.2[i,2],'"'), " d=","\"M",start.node.coord.1[i,1],',',start.node.coord.1[i,2],',L',end.node.coord.1[i,1],',',end.node.coord.1[i,2],"\" style=\"stroke:black; stroke-width: 3.25px; fill: none ;\" >
                          </path> ")
  }
  }
connections


cat("\n<div>", append = TRUE, file=htmlfile)
cat("\n <svg height=\"1000\" width=\"1000\">", append=TRUE, file=htmlfile)
cat(connections, append=TRUE, file=htmlfile)
cat("\n </svg>", append=TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n<div id=\"hidden\">&nbsp;</div>", append=TRUE, file=htmlfile) #saves all the info into the id=hidden. concerto will automatically save all this info in a form.
cat("\n<div id=\"hidden2\">&nbsp;</div>", append=TRUE, file=htmlfile)
cat("\n<div id=\"hidden3\">&nbsp;</div>", append=TRUE, file=htmlfile)
cat("\n<input name=\"next\" style=\"display: none;\" type=\"Submit\" value=\"next\" />",append=TRUE, file=htmlfile) # For concerto
#cat("\n<input type = 'submit' value = 'next' style='display: none'> ", append=TRUE, file = htmlfile)
cat("\n</div>", append = TRUE, file=htmlfile)
cat("\n<p style =\"width:150px; text-align: center; height:20px; background-color:#fff; border: 1px solid #999\" id=\"output\" hidden></p>", append=TRUE, file=htmlfile)




#jsfile = file.path('~/Dropbox/Cambridge/PhD/ICAR/AIG/Maze/Maze example/', "script.js")
#cat("prompt('please state your name')\n", file= jsfile)
#cat("confirm('Are you sure');\n", append=TRUE, file= jsfile)

# Step 1 Break into col vectors
start.node <- ed[,1]
start.node<- cbind(start.node)

end.node<- ed[,2]
end.node <- cbind(end.node)

travelledFirstNode = 0
travelledBlackNode = 0
# Include the position of which row to be reverse directions or unidirections
##### Step 2 for loop across number of col vectors

  edge.list <- "\n var edgeArray=["
arrowDirect
start.index
for (i in 1: 1:nrow(ed.2)){
  if (i != 1) {
    edge.list <- paste0(edge.list,",[",start.index[i],",",end.index[i],",", travelledFirstNode[],",", travelledBlackNode[],",",direction[i], "]")
  } else {
    edge.list <- paste0(edge.list,"[",start.index[i],",",end.index[i],",", travelledFirstNode[],",", travelledBlackNode[],",",direction[i],"]")
  }
}



edge.list <- paste0(edge.list,"];")
edge.list
cat("\n<script>", append = TRUE, file = htmlfile)
cat(edge.list, append=TRUE, file=htmlfile)
#### TO GET NODE POSITIION ####
v <- paste0("if(")
e <- NULL
for(i in 1:(length(nodePosition)-1)){
  e <- paste0(e,paste0("(nodeclicked.id == ",nodePosition[i],") || "))
}
colourNodePosition <- paste0(v,e,paste0("(nodeclicked.id == ",nodePosition[length(nodePosition)],"))"))
##### TO CALUCLATE SUM SCORE ####
end.index.df<- as.data.frame(end.index)
index <- 1:nrow(end.index.df)
end.index.df <- cbind.data.frame(index,end.index.df)
topNodes
which(end.index.df$end.index == topNodes)

test1 <- NULL
for(i in 1:length(topNodes)){
  test1[[i]] <- end.index.df[which(end.index.df$end.index == topNodes[i]),]
}
lastRow <- do.call("rbind",test1)

v <- paste0("if(")
e<- NULL
for(i in 1:(nrow(lastRow)-1)){
  e <- paste0(e,paste0("(nodeclicked.id == edgeArray[",lastRow$index[i]-1,"][1]) || "))
}
e
finalRow <- paste0(v,e,paste0("(nodeclicked.id == edgeArray[",lastRow$index[nrow(lastRow)]-1,"][1]))"))
finalRow

##### javaScript1 ####
if(Timer==TRUE){
javaScript<- paste0("
                    ///////////// Count Down Timer Begin ////////////
                    function countdown( elementName, minutes, seconds )
                    {
                    var element, endTime, hours, mins, msLeft, time;

                    function twoDigits( n )
                    {
                    return (n <= 9 ? \"0\" + n : n);
                    }
                    function updateTimer()
                    {
                    msLeft = endTime - (+new Date);
                    if (msLeft < 10000*6) {
                    document.getElementById(\"countdown2\").style.color = \"red\";
                    }
                    if ( msLeft < 1000 ) {
                    //element.innerHTML = \"countdown's over!\";
                    //document.getElementById(\"countdown\");
                    //document.countdown.submit();
                    setTimeout('document.countdown2.submit()', 1);
                    test.submit();
                    } else {
                    time = new Date( msLeft );
                    hours = time.getUTCHours();
                    mins = time.getUTCMinutes();
                    element.innerHTML = (hours ? hours + ':' + twoDigits( mins ) : mins) + ':' + twoDigits( time.getUTCSeconds() );
                    timeStamp =   twoDigits( mins )  + ':' + twoDigits( time.getUTCSeconds()) ;
                    setTimeout( updateTimer, time.getUTCMilliseconds() + 500 );
                    }
                    }

                    element = document.getElementById( elementName );
                    endTime = (+new Date) + 1000 * (60*minutes + seconds) + 500;
                    updateTimer();
                    }

                    countdown( \"countdown\", 04, 00 );
                    countdown( \"countdown2\", 04, 00 );


                    var pageInput = document.getElementsByTagName(\"input\");
                    for (i=0; i<pageInput.length; i++){
                    console.log(pageInput[i]);
                    pageInput[i].addEventListener('change', function()
                    {
                    console.log('test')
                    document.getElementById(\"submitButton\").disabled = false;
                    });
                    }


                    ///////////// Timer End ////////////

                    var isFirstNodeClicked=true;
                    var prevNodeId=null;
                    var nodeArray = [];
                    var timerArray = [];
                    var responseArray =[];
                    var node = [];
                    var clickedNodes=[];
                    function captureActionData(node, nodeArray, response, timeStamp){
                    var captureObject = {}; // Create empty JSON array
                    captureObject.node = node
                    captureObject.nodeArray = nodeArray;
                    captureObject.time = timeStamp;
                    captureObject.response = response;
                    responseArray.push(captureObject);
                    console.log(JSON.stringify(responseArray));
                    }



                    function nodeClick(nodeclicked) {
                    console.log('clickedNode = ' + nodeclicked.id + '; previous node = ' + prevNodeId);
                    //console.log(nodeclicked.id)
                    //console.log(edgeArray[0][1]);

                    var response ; // Recorded as right or wrong.

                    // Record first node ///////////
                    if (isFirstNodeClicked == true){
                    if(nodeclicked.id == edgeArray[1][0] && edgeArray[2][0]){
                    node = nodeclicked.id
                    response = \"correct\";
                    timerArray.push(timeStamp);
                    console.log(timerArray);
                    var nodeArray ;

                    captureActionData(node, 'NULL' ,response, timeStamp);

                    // create hidden layer (Dynamic)
                    var form = document.getElementById('hidden');
                    var div = document.createElement('div');
                    var inputElements1 = document.createElement('input');
                    inputElements1.setAttribute(\"type\", \"hidden\");
                    inputElements1.setAttribute(\"name\", \"node\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements1.setAttribute(\"value\", node);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements1);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) 'timestamp'
                    var inputElements2 = document.createElement('input');
                    inputElements2.setAttribute(\"type\", \"hidden\");
                    inputElements2.setAttribute(\"name\", \"timeStamp\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements2.setAttribute(\"value\", timeStamp);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements2);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) 'responses'
                    var inputElements3 = document.createElement('input');
                    inputElements3.setAttribute(\"type\", \"hidden\");
                    inputElements3.setAttribute(\"name\", \"response\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements3.setAttribute(\"value\", response);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements3);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic)
                    var inputElements4 = document.createElement('input');
                    inputElements4.setAttribute(\"type\", \"hidden\");
                    inputElements4.setAttribute(\"name\", \"nodePosition\");
                    inputElements4.setAttribute(\"value\", node);

                    div.appendChild(inputElements4);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //


                    }else{
                    alert(\"Please start from the first node at the bottom\");
                    node = nodeclicked.id
                    response = \"notBottom\";
                    timerArray.push(timeStamp);
                    console.log(timerArray);
                    var nodeArray ;

                    captureActionData(node, 'NULL' ,response, timeStamp);

                    // create hidden layer (Dynamic)
                    var form = document.getElementById('hidden');
                    var div = document.createElement('div');
                    var inputElements1 = document.createElement('input');
                    inputElements1.setAttribute(\"type\", \"hidden\");
                    inputElements1.setAttribute(\"name\", \"node\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements1.setAttribute(\"value\", node);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements1);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) 'timestamp'
                    var inputElements2 = document.createElement('input');
                    inputElements2.setAttribute(\"type\", \"hidden\");
                    inputElements2.setAttribute(\"name\", \"timeStamp\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements2.setAttribute(\"value\", timeStamp);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements2);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) 'responses'
                    var inputElements3 = document.createElement('input');
                    inputElements3.setAttribute(\"type\", \"hidden\");
                    inputElements3.setAttribute(\"name\", \"response\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements3.setAttribute(\"value\", response);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements3);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic)
                    var inputElements4 = document.createElement('input');
                    inputElements4.setAttribute(\"type\", \"hidden\");
                    inputElements4.setAttribute(\"name\", \"nodePosition\");
                    inputElements4.setAttribute(\"value\", node);

                    div.appendChild(inputElements4);
                    form.appendChild(div);
                    return;
                    }

                    }


                    /// Second node onwards ////////
                    if (isFirstNodeClicked == false) {
                    var pathExists = false;
                    for(var i=0; i<edgeArray.length; i++) {
                    //alert(edgeArray[i][0]);
                    //alert(edgeArray[i][1]);
                    if((edgeArray[i][0] == nodeclicked.id && edgeArray[i][1] == prevNodeId) || (edgeArray[i][0] == prevNodeId && edgeArray[i][1] == nodeclicked.id)) {
                    if ((edgeArray[i][2] == 0)) {
                    edgeArray[i][2] = 1;",
                    colourNodePosition, "{
                    edgeArray[i][3] = 1;  // count total score
                    console.log(edgeArray[i]);
                    }

                    // JSON array
                    node = nodeclicked.id
                    response = \"correct\";
                    timerArray.push(timeStamp);
                    console.log(timerArray);
                    captureActionData(node, edgeArray[i], response, timeStamp);

                    // create hidden layer (Dynamic)
                    var form = document.getElementById('hidden');
                    var div = document.createElement('div');
                    var inputElements1 = document.createElement('input');
                    inputElements1.setAttribute(\"type\", \"hidden\");
                    inputElements1.setAttribute(\"name\", \"node\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements1.setAttribute(\"value\", edgeArray[i]);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements1);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) 'time stamp'
                    var inputElements2 = document.createElement('input');
                    inputElements2.setAttribute(\"type\", \"hidden\");
                    inputElements2.setAttribute(\"name\", \"timeStamp\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements2.setAttribute(\"value\", timeStamp);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements2);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) 'responses'
                    var inputElements3 = document.createElement('input');
                    inputElements3.setAttribute(\"type\", \"hidden\");
                    inputElements3.setAttribute(\"name\", \"response\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements3.setAttribute(\"value\", response);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements3);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic)
                    var inputElements4 = document.createElement('input');
                    inputElements4.setAttribute(\"type\", \"hidden\");
                    inputElements4.setAttribute(\"name\", \"nodePosition\");
                    inputElements4.setAttribute(\"value\", node);

                    div.appendChild(inputElements4);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    pathExists = true; //hooray path exists :P
                    // prevMyNode = document.getElementById(prevNodeId); // Put into new variable
                    //prevMyNode.style.backgroundColor = '#FFCF75';  // changes the color of the element of previous node ID

                    }

                    //unidirection
                    if(edgeArray[i][4] == 2 && Number(nodeclicked.id) < prevNodeId){
                    edgeArray[i][2] = 0;
                    node = nodeclicked.id
                    timerArray.push(timeStamp);
                    console.log(timerArray);
                    alert(\"Wrong direction\");
                    response = \"wrongDirection\";
                    captureActionData(node, edgeArray[i], response, timeStamp);
                    // create hidden layer (Dynamic)
                    var form = document.getElementById('hidden');
                    var div = document.createElement('div');
                    var inputElements1 = document.createElement('input');
                    inputElements1.setAttribute(\"type\", \"hidden\");
                    inputElements1.setAttribute(\"name\", \"node\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements1.setAttribute(\"value\", edgeArray[i]);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements1);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) \"time stamp\"
                    var inputElements2 = document.createElement('input');
                    inputElements2.setAttribute(\"type\", \"hidden\");
                    inputElements2.setAttribute(\"name\", \"timeStamp\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements2.setAttribute(\"value\", timeStamp);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements2);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) 'responses'
                    var inputElements3 = document.createElement('input');
                    inputElements3.setAttribute(\"type\", \"hidden\");
                    inputElements3.setAttribute(\"name\", \"response\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements3.setAttribute(\"value\", response);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements3);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic)
                    var inputElements4 = document.createElement('input');
                    inputElements4.setAttribute(\"type\", \"hidden\");
                    inputElements4.setAttribute(\"name\", \"nodePosition\");
                    inputElements4.setAttribute(\"value\", node);

                    div.appendChild(inputElements4);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //


                    //prevMyNode.style.backgroundColor = '#218868';
                    return false;
                    }

                    }

                    }



                    //check if the person tried to take a path that don't exists or repeated
                    if (pathExists == false) {
                    for(var i=0; i<edgeArray.length; i++) {
                    if((edgeArray[i][0] !== nodeclicked.id && edgeArray[i][1] !== prevNodeId) || (edgeArray[i][0] !== prevNodeId && edgeArray[i][1] !== nodeclicked.id)) {
                    if ((edgeArray[i][2] == 0)) { //checks that the user hasn't taken this path already
                    edgeArray[i][2] = 0; //makes it so that they can't take that path again
                    console.log(edgeArray[i]);
                    console.log(clickedNodes.length);


                    // Repeated nodes
                    if(nodeclicked.id == prevNodeId) {
                    timerArray.push(timeStamp);
                    console.log(timerArray);
                    alert(\"This is repeated\");
                    response = \"repeated\";
                    captureActionData(clickedNodes[i], clickedNodes[i] ,response, timeStamp);

                    // create hidden layer (Dynamic)
                    var form = document.getElementById('hidden');
                    var div = document.createElement('div');
                    var inputElements1 = document.createElement('input');
                    inputElements1.setAttribute(\"type\", \"hidden\");
                    inputElements1.setAttribute(\"name\", \"node\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements1.setAttribute(\"value\", nodeclicked.id);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements1);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) \"time stamp\"
                    var inputElements2 = document.createElement('input');
                    inputElements2.setAttribute(\"type\", \"hidden\");
                    inputElements2.setAttribute(\"name\", \"timeStamp\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements2.setAttribute(\"value\", timeStamp);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements2);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) 'responses'
                    var inputElements3 = document.createElement('input');
                    inputElements3.setAttribute(\"type\", \"hidden\");
                    inputElements3.setAttribute(\"name\", \"response\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements3.setAttribute(\"value\", response);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements3);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic)
                    var inputElements4 = document.createElement('input');
                    inputElements4.setAttribute(\"type\", \"hidden\");
                    inputElements4.setAttribute(\"name\", \"nodePosition\");
                    inputElements4.setAttribute(\"value\", node);

                    div.appendChild(inputElements4);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //
                    return false;

                    }

                    // JSON array
                    node = nodeclicked.id
                    timerArray.push(timeStamp);
                    console.log(timerArray);
                    alert('You cant go here!');
                    response = \"incorrect\";
                    captureActionData(node, edgeArray[i], response, timeStamp);

                    // create hidden layer (Dynamic)
                    var form = document.getElementById('hidden');
                    var div = document.createElement('div');
                    var inputElements1 = document.createElement('input');
                    inputElements1.setAttribute(\"type\", \"hidden\");
                    inputElements1.setAttribute(\"name\", \"node\");
                    //inputElements1.setAttribute(\"name\",\"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements1.setAttribute(\"value\", edgeArray[i]);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements1);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) \"time stamp\"
                    var inputElements2 = document.createElement('input');
                    inputElements2.setAttribute(\"type\", \"hidden\");
                    inputElements2.setAttribute(\"name\", \"timeStamp\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements2.setAttribute(\"value\", timeStamp);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements2);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic) 'responses'
                    var inputElements3 = document.createElement('input');
                    inputElements3.setAttribute(\"type\", \"hidden\");
                    inputElements3.setAttribute(\"name\", \"response\");
                    //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                    inputElements3.setAttribute(\"value\", response);
                    // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                    div.appendChild(inputElements3);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    // create hidden layer (Dynamic)
                    var inputElements4 = document.createElement('input');
                    inputElements4.setAttribute(\"type\", \"hidden\");
                    inputElements4.setAttribute(\"name\", \"nodePosition\");
                    inputElements4.setAttribute(\"value\", node);

                    div.appendChild(inputElements4);
                    form.appendChild(div);
                    // create hidden layer (Dynamic) [End] //

                    return false;




                    }
                    }
                    }
                    }

                    }

                    // we successfully clicked on something, so change colour of stuff (line and node)
                    if (isFirstNodeClicked == true){
                    nodeclicked.style.backgroundColor = 'red';
                    }

                    // Create getChildren(gets all the nodes including the node you click)
                    // changes the colour of everything except the node you click on
                    function getChildren(n, skipMe){
                    var r = [];
                    for ( ; n; n = n.nextSibling )
                    if ( n.nodeType == 1 && n != skipMe)
                    r.push( n );
                    return r;
                    };
                    // Create getsiblings(gets all the nodes except the node you click)
                    function getSiblings(n) {
                    return getChildren(n.parentNode.firstChild, n);
                    }


                    if (isFirstNodeClicked == false) { // the line doesnt change colour the first time

                    if (Number(nodeclicked.id) > Number(prevNodeId)) {
                    var elementname = prevNodeId+'_'+nodeclicked.id;
                    } else {
                    var elementname = nodeclicked.id+'_'+prevNodeId;
                    }

                    console.log('element_name = ' + elementname + 'clicked node = ' + nodeclicked.id + '; previous node = ' + prevNodeId);

                    // this breaks if there is no if-statement on line 302. Because it doesn't continue onwards and reset nodeclicked into prevNode.
                    // So you need the if statement. Once that is true, then it continues on with the code.
                    myelement = document.getElementById(elementname);
                    myelement.style.stroke = '#FF0000';


                    }

                    // set things up for next time
                    prevNodeId = nodeclicked.id;
                    isFirstNodeClicked=false;

                    if (isFirstNodeClicked == false){
                    nodeclicked.style.borderColor = 'red';
                    }


                    // suming up the score of the person based on the black dotes crossed
                    var sum = 0;
                    for(i =0; i < edgeArray.length; i++){
                    //console.log(edgeArray[i][3]);
                    sum  += edgeArray[i][3];
                    }
                    console.log(sum); // 6


                    // check stopping rules
                    var completedGame = true;
                    if(sum == ",maxScore,"){ ")
}

if(Timer==FALSE){
  javaScript<- paste0("

                      var isFirstNodeClicked=true;
                      var prevNodeId=null;
                      var nodeArray = [];
                      var timerArray = [];
                      var responseArray =[];
                      var node = [];
                      var clickedNodes=[];
                      function captureActionData(node, nodeArray, response, timeStamp){
                      var captureObject = {}; // Create empty JSON array
                      captureObject.node = node
                      captureObject.nodeArray = nodeArray;
                      captureObject.time = timeStamp;
                      captureObject.response = response;
                      responseArray.push(captureObject);
                      console.log(JSON.stringify(responseArray));
                      }



                      function nodeClick(nodeclicked) {
                      console.log('clickedNode = ' + nodeclicked.id + '; previous node = ' + prevNodeId);
                      //console.log(nodeclicked.id)
                      //console.log(edgeArray[0][1]);

                      var response ; // Recorded as right or wrong.

                      // Record first node ///////////
                      if (isFirstNodeClicked == true){
                      if(nodeclicked.id == edgeArray[1][0] && edgeArray[2][0]){
                      node = nodeclicked.id
                      response = \"correct\";
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      var nodeArray ;

                      captureActionData(node, 'NULL' ,response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", node);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'timestamp'
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //


                      }else{
                      alert(\"Please start from the first node at the bottom\");
                      node = nodeclicked.id
                      response = \"notBottom\";
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      var nodeArray ;

                      captureActionData(node, 'NULL' ,response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", node);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'timestamp'
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      return;
                      }

                      }


                      /// Second node onwards ////////
                      if (isFirstNodeClicked == false) {
                      var pathExists = false;
                      for(var i=0; i<edgeArray.length; i++) {
                      //alert(edgeArray[i][0]);
                      //alert(edgeArray[i][1]);
                      if((edgeArray[i][0] == nodeclicked.id && edgeArray[i][1] == prevNodeId) || (edgeArray[i][0] == prevNodeId && edgeArray[i][1] == nodeclicked.id)) {
                      if ((edgeArray[i][2] == 0)) {
                      edgeArray[i][2] = 1;",
                      colourNodePosition, "{
                      edgeArray[i][3] = 1;  // count total score
                      console.log(edgeArray[i]);
                      }

                      // JSON array
                      node = nodeclicked.id
                      response = \"correct\";
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      captureActionData(node, edgeArray[i], response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", edgeArray[i]);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'time stamp'
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      pathExists = true; //hooray path exists :P
                      // prevMyNode = document.getElementById(prevNodeId); // Put into new variable
                      //prevMyNode.style.backgroundColor = '#FFCF75';  // changes the color of the element of previous node ID

                      }

                      //unidirection
                      if(edgeArray[i][4] == 2 && Number(nodeclicked.id) < prevNodeId){
                      edgeArray[i][2] = 0;
                      node = nodeclicked.id
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      alert(\"Wrong direction\");
                      response = \"wrongDirection\";
                      captureActionData(node, edgeArray[i], response, timeStamp);
                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", edgeArray[i]);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) \"time stamp\"
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //


                      //prevMyNode.style.backgroundColor = '#218868';
                      return false;
                      }

                      }

                      }



                      //check if the person tried to take a path that don't exists or repeated
                      if (pathExists == false) {
                      for(var i=0; i<edgeArray.length; i++) {
                      if((edgeArray[i][0] !== nodeclicked.id && edgeArray[i][1] !== prevNodeId) || (edgeArray[i][0] !== prevNodeId && edgeArray[i][1] !== nodeclicked.id)) {
                      if ((edgeArray[i][2] == 0)) { //checks that the user hasn't taken this path already
                      edgeArray[i][2] = 0; //makes it so that they can't take that path again
                      console.log(edgeArray[i]);
                      console.log(clickedNodes.length);


                      // Repeated nodes
                      if(nodeclicked.id == prevNodeId) {
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      alert(\"This is repeated\");
                      response = \"repeated\";
                      captureActionData(clickedNodes[i], clickedNodes[i] ,response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", nodeclicked.id);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) \"time stamp\"
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //
                      return false;

                      }

                      // JSON array
                      node = nodeclicked.id
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      alert('You cant go here!');
                      response = \"incorrect\";
                      captureActionData(node, edgeArray[i], response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\",\"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", edgeArray[i]);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) \"time stamp\"
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      return false;




                      }
                      }
                      }
                      }

                      }

                      // we successfully clicked on something, so change colour of stuff (line and node)
                      if (isFirstNodeClicked == true){
                      nodeclicked.style.backgroundColor = 'red';
                      }

                      // Create getChildren(gets all the nodes including the node you click)
                      // changes the colour of everything except the node you click on
                      function getChildren(n, skipMe){
                      var r = [];
                      for ( ; n; n = n.nextSibling )
                      if ( n.nodeType == 1 && n != skipMe)
                      r.push( n );
                      return r;
                      };
                      // Create getsiblings(gets all the nodes except the node you click)
                      function getSiblings(n) {
                      return getChildren(n.parentNode.firstChild, n);
                      }


                      if (isFirstNodeClicked == false) { // the line doesnt change colour the first time

                      if (Number(nodeclicked.id) > Number(prevNodeId)) {
                      var elementname = prevNodeId+'_'+nodeclicked.id;
                      } else {
                      var elementname = nodeclicked.id+'_'+prevNodeId;
                      }

                      console.log('element_name = ' + elementname + 'clicked node = ' + nodeclicked.id + '; previous node = ' + prevNodeId);

                      // this breaks if there is no if-statement on line 302. Because it doesn't continue onwards and reset nodeclicked into prevNode.
                    // So you need the if statement. Once that is true, then it continues on with the code.
                    myelement = document.getElementById(elementname);
                    myelement.style.stroke = '#FF0000';


                      }

                      // set things up for next time
                      prevNodeId = nodeclicked.id;
                      isFirstNodeClicked=false;

                      if (isFirstNodeClicked == false){
                      nodeclicked.style.borderColor = 'red';
                      }


                      // suming up the score of the person based on the black dotes crossed
                      var sum = 0;
                      for(i =0; i < edgeArray.length; i++){
                      //console.log(edgeArray[i][3]);
                      sum  += edgeArray[i][3];
                      }
                      console.log(sum); // 6


                      // check stopping rules
                      var completedGame = true;
                      if(sum == ",maxScore,"){ ")

}

##### javascript 2 #####
javaScript2<- paste0(
  finalRow,"{
  console.log(edgeArray[4][1]);
  alert('Congratulations! You solved the puzzle!');

  var form = document.getElementById('hidden2');
  var div = document.createElement('div');
  var inputElements4 = document.createElement('input');
  inputElements4.setAttribute(\"type\", \"hidden\");
  inputElements4.setAttribute(\"name\", \"completedGame\");
  inputElements4.setAttribute(\"value\", 1);
  div.appendChild(inputElements4);
  form.appendChild(div);

  var form = document.getElementById('hidden3');
  var div = document.createElement('div');
  var inputElements5 = document.createElement('input');
  inputElements5.setAttribute(\"type\", \"hidden\");
  inputElements5.setAttribute(\"name\", \"totalGoldScore\");
  inputElements5.setAttribute(\"value\", sum);
  div.appendChild(inputElements5);
  form.appendChild(div);



  test.submit('next'); // concerto
  return true; // stop the script.
                    }

                    }else{",
                      finalRow,"{
  console.log(edgeArray[4][1]);
  alert(\"Sorry, that was not the maximum score\");

  var form = document.getElementById('hidden2');
  var div = document.createElement('div');
  var inputElements4 = document.createElement('input');
  inputElements4.setAttribute(\"type\", \"hidden\");
  inputElements4.setAttribute(\"name\", \"completedGame\");
  inputElements4.setAttribute(\"value\", 0);
  div.appendChild(inputElements4);
  form.appendChild(div);

  var form = document.getElementById('hidden3');
  var div = document.createElement('div');
  var inputElements5 = document.createElement('input');
  inputElements5.setAttribute(\"type\", \"hidden\");
  inputElements5.setAttribute(\"name\", \"totalGoldScore\");
  inputElements5.setAttribute(\"value\", sum);
  div.appendChild(inputElements5);
  form.appendChild(div);
  test.submit('next'); // concerto
  return true; // stop the script.

                      }
                    }

  clickedNodes.push(nodeclicked.id); // array for repeated logic



                    }




  ")

cat(javaScript, append=TRUE, file=htmlfile)
cat(javaScript2, append=TRUE, file=htmlfile)
cat("\n</script>", append = TRUE, file = htmlfile)
cat("\n</body>", append = TRUE, file = htmlfile)
cat("\n</html>", append = TRUE, file = htmlfile)



}
