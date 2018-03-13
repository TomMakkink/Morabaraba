open System;

//------------------------------------------------ DATA STUCTURES ------------------------------------------------ //

type Cow = {
    Name: string
    Position: string
    isOP : bool
    isOnBoard : bool
    isAlive: bool
    inMill: int
    } 

type Player = 
     {
      Name: string
      cowsLeft : Cow List
      isTurn: bool
      cowsOnField: Cow List
      Team : int
      }

type Point = {
    xCoord: int
    yCoord: string
    }
(* team 0 = no team
        1 = player 1 team
        2 = player 2 team
        *)
type node = {
    Name: string
    Position : Point
    Occupied: bool
    team: int 
    neighbours: string List
    number: int
    cow : Cow
    }

type gameState = 
    | PLACING
    | MOVING 
    | FLYING
    | END

type Mills = 
    {
    storedMills : Cow list list
    }


//--------------------------------------------------- METHODS --------------------------------------------------- //
let getPlayerMove (currentPlayer:Player) turns = 
    printfn "%s    place cow on which node.      turn = %d" currentPlayer.Name turns
    System.Console.ReadLine()

let GetNodeFromName nodeName nodeList =
// Can use List.trfFind
     let rec getNode (tempNodeList: node List) = 
          match tempNodeList with 
          | [] -> failwith "This is not the node your looking for"
          | h::rest -> 
               match h.Name = nodeName with  
               | true -> h
               | false -> getNode rest
     getNode nodeList

let checkNodeExists nodeName field =
    List.exists (fun x -> x.Name = nodeName) field

// ----- MOVING COWS ----- //


let flyingIntroMessage (currentPlayer:Player) = 
    printfn ""
    printfn "Stage 3: "
    printfn "%ss cows can fly to any free node now its like moving a cow 
    but they can go to any free node" currentPlayer.Name


let isValidEndNode startNode endNode = 
    List.exists (fun x -> x = endNode.Name) startNode.neighbours 


let moveCowToNewPos startingNode endNode field =
    let newNode = {endNode with Occupied = true; team = startingNode.team; cow = {startingNode.cow with Position = endNode.Name} }
    let oldNode = {startingNode with Occupied = false; team = 0; cow =  {Name = "[]";Position = "NP"; isOP = false; isOnBoard = false; isAlive = false; inMill = 0 }}
    let newField = 
        List.map (fun x -> 
        match x.number = startingNode.number with 
        | true -> oldNode
        | false -> 
            match x.number = endNode.number with
            | true -> newNode
            | false -> x 
        ) field
    newField


let moveCows startingNode endNode (field: node List) poweredCow =
    match poweredCow.isOP with
    | true -> 
        match endNode.Occupied with
        | false -> moveCowToNewPos startingNode endNode field
        | _ -> field
    | false ->
        match isValidEndNode startingNode endNode && endNode.Occupied = false with 
        | true -> moveCowToNewPos startingNode endNode field
        | _ -> field


let chooseWhereToMove () = 
    let splitLine = (fun (line : string) -> Seq.toList (line.Split ' '))
    let playerMove = System.Console.ReadLine()
    splitLine playerMove


let movingIntroMessage (currentPlayer:Player) = 
    printfn "Stage 2 - You will now move cows on to any adjacent, available place.
    Please specific the node you want to move first, and then the place 
    you want to move it to. 
                        
    e.g. A1 B3
                          
    its %s turn" currentPlayer.Name


let numOfCowsPlayerHasOnBoard fieldList player = 
    List.fold(fun state fieldNode -> 
        match fieldNode.team = player with 
        | true -> (state + 1)
        | _ -> state
    ) 0 fieldList


// ----- MILLS AND SHOOTING ----- //


// this will change the node of the now dead cow to unOccupied and also the name of the cow to default
let updateDeadCowNode newNodes oldNodes = 
     let rec check inList outList =
          match inList with 
          | [] -> List.rev outList
          | h::rest -> 
               match h.Name = newNodes.Name with 
               | true -> 
                    let newNode = {newNodes with team= 0; Occupied =  false; cow =  {Name = "[]";Position = "NP"; isOP = false; isOnBoard = false; isAlive = false; inMill = 0 }}
                    check rest (newNode::outList)
               | false -> check rest (h::outList)
     check oldNodes []  

// this is the method that returns the updated lists.      
let shooting (deadCow:Cow) (herdLeft: Cow List) =
     let rec check (inList: Cow List) outList = 
          match inList with 
          | [] -> List.rev outList 
          | h::rest -> 
               match h.Name=deadCow.Name with 
               | true -> check rest outList
               | false -> check rest (h::outList)
     check herdLeft []


// This will return an updated enemy cowsleft and cowsOnField and will not allow friendly fire also returns an updated nodeList
// still need to call the board update as well as make sure the cow is dead
let shootCow nodeName mainNodeList enemy = 
     let (actualNode:Cow option)  = List.tryFind (fun x -> nodeName = x.Position) enemy.cowsOnField
     match actualNode with 
     | None -> printfn "you only get one shot gg nood get gub scrub"
               enemy,mainNodeList
     | Some cow -> 
          match cow.inMill <> 1 with 
          | false -> enemy,mainNodeList
          | true ->
               let newCowsOnField = shooting cow enemy.cowsOnField
               let updateEnemy = {enemy with cowsOnField = newCowsOnField}
               let NewMainNode = updateDeadCowNode (GetNodeFromName nodeName mainNodeList) mainNodeList
               updateEnemy,NewMainNode


// this will take in a string List (cow List) and also the players filedcows and the new mill value and return a list of cows
let changeCowMill millRow fieldCows newMill= 
     let rec check inList (cowList: Cow List) outList millList = 
          match inList with 
          | [] -> (List.rev outList), millList
          | h::rest -> //this is the string node list 
               let head::tail = cowList
               match h = head.Position with 
               | false -> check inList tail (head::outList) millList
               | true -> 
                let newCow = {head with inMill = newMill}
                check rest tail (newCow::outList) (newCow::millList)
     check millRow  (List.rev fieldCows) [] []


 // this will now call changeCowMill from with a rec function and it will still only return a updated Cowfield list :)
let filterCowMillList millrow fieldCows newMill = 
    let rec check nodeInMillNames oldHerd outlist millList = 
        match nodeInMillNames with 
            | [] -> outlist,millList
            | h::rest -> 
                let newherd,finalFormCowList = changeCowMill (List.sortDescending h) (List.sortBy (fun s-> s.Position) oldHerd) newMill
                check rest newherd newherd finalFormCowList
    check millrow fieldCows [] []


// this method will check each cows mill number in the given row to see if they are allowed to form a mill
let CheckMillNumber listOfRows cowsToCheck = 
  let h::m::t::_ = listOfRows
  let cow1 = List.find (fun (x:Cow) -> x.Position = h ) cowsToCheck
  let cow2 = List.find (fun (x:Cow) -> x.Position = m ) cowsToCheck
  // Index problem is here
  let cow3 = List.find (fun (x:Cow) -> x.Position = t ) cowsToCheck
  match (cow1.inMill = 0 && cow2.inMill = 0) && cow3.inMill =0 with 
  | true -> true, listOfRows
  | _ -> false,listOfRows


// this method will return a (bool*string List) which is a bool to show if a mill formed or not and the row.
let checkCowsInRow millNodeLists mainNodeList cowsOnField  =
     let rec check (nodeList: string List List) index outList =
          match nodeList with 
          | [] -> false,outList
          | h::rest ->
               let fir::sec::third::_ = h
               let firNode = GetNodeFromName fir mainNodeList
               let secNode = GetNodeFromName sec mainNodeList
               let thirdNode = GetNodeFromName third mainNodeList
               match firNode.Occupied && secNode.Occupied && thirdNode.Occupied with 
               | false -> check rest (index + 1) outList
               | true ->
                    match firNode.team = secNode.team && thirdNode.team = secNode.team with 
                    | false ->  check rest (index + 1) outList
                    | true -> 
                        match firNode.cow.inMill<>1 && secNode.cow.inMill<>1 && thirdNode.cow.inMill <>1 with // <---- this is a problem
                        | false -> check rest (index+1) outList
                        | true ->
                             match CheckMillNumber h cowsOnField, index with 
                             | (false,_), _ -> check rest (index + 1) outList
                             | (true,x), 2 -> 
                                match List.length millNodeLists = 2 with 
                                    | false -> check rest (index+1) (x::outList)
                                    | true -> true,(x::outList) 
                             | (true,x), _ -> check rest (index+1) (x::outList)
     check millNodeLists 0 []


// this funtion will return a list of string lists and these will hold all the combos with that paricular node the cow is on
let nodesInARow nodeName =
     match nodeName with 
     | "a0" -> [["a0";"a3";"a6"];["a0";"d0";"g0"];["a0";"b1";"c2"]]
     | "a3" -> [["a0";"a3";"a6"];["a3";"b3";"c3"]]
     | "a6" -> [["a0";"a3";"a6"];["a6";"g6";"d6"];["a6";"b5";"c4"]]
     | "b1" -> [["b1";"b3";"b5"];["a0";"b1";"c2"];["b1";"d1";"f1"]]
     | "b3" -> [["b1";"b3";"b5"];["a3";"b3";"c3"]]
     | "b5" -> [["b1";"b3";"b5"];["b5";"a6";"c4"];["b5";"f5";"d5"]]
     | "c2" -> [["c2";"c3";"c4"];["c2";"d2";"e2"];["a0";"b1";"c2"]]
     | "c3" -> [["c2";"c3";"c4"];["a3";"b3";"c3"]]
     | "c4" -> [["c2";"c3";"c4"];["c4";"d4";"e4"];["c4";"b5";"a6"]]
     | "d0" -> [["d0";"d1";"d2"];["a0";"d0";"g0"]]
     | "d1" -> [["d0";"d1";"d2"];["b1";"d1";"f1"]]
     | "d2" -> [["d0";"d1";"d2"];["d2";"e2";"c2"]]
     | "d4" -> [["d4";"d5";"d6"];["d4";"c4";"e4"]]
     | "d5" -> [["d4";"d5";"d6"];["d5";"b5";"f5"]]
     | "d6" -> [["d4";"d5";"d6"];["a6";"d6";"g6"]]
     | "e2" -> [["e2";"e3";"e4"];["e2";"f1";"g0"];["e2";"d2";"c2"]]
     | "e3" -> [["e2";"e3";"e4"];["e3";"f3";"g3"]]
     | "e4" -> [["e2";"e3";"e4"];["e4";"f5";"g6"];["e4";"d4";"c4"]]
     | "f1" -> [["f1";"f3";"f5"];["f1";"e2";"g0"];["f1";"d1";"b1"]]
     | "f3" -> [["f1";"f3";"f5"];["g3";"e3";"f3"]]
     | "f5" -> [["f1";"f3";"f5"];["f5";"e4";"g6"];["f5";"d5";"b5"]]
     | "g0" -> [["g0";"g3";"g6"];["g0";"f1";"e2"];["g0";"a0";"d0"]]
     | "g3" -> [["g0";"g3";"g6"];["g3";"f3";"e3"]]
     | "g6" -> [["g0";"g3";"g6"];["g6";"d6";"a6"];["g6";"f5";"e4"]]
     | _ -> failwith "This cow is all alone"


// this method will use the method checkCowsInRow to check if any of the combo rows are true and return (bool*string List)
let CheckinMill cow nodeList player = 
    match cow with 
    |None -> 0,[]
    |Some cows ->
        match cows.isAlive && cows.isOnBoard with 
        | false -> 0,[]
        | true -> 
            match player.cowsOnField.Length > 2 with 
            | false -> 0,[]
            | true -> 
                let millNodes = nodesInARow cows.Position
                let ifCows= checkCowsInRow millNodes nodeList player.cowsOnField
                match ifCows with 
                | false,[] -> 0,[]
                | false,rowMill -> 
                    printfn "A mill was formed \n"
                    1,rowMill
                | true,millrow ->
                    printfn "A mill was formed \n"
                    1,millrow


// ----- PLACING ----- //


let updatePlayer Player millRow = 
    let newHerd,_ = filterCowMillList millRow Player.cowsOnField 1
    let updatedPlayer = {Player with cowsOnField = newHerd}
    updatedPlayer


let updateFieldList tNode inList outList player =
    let rec update inList outList  =
        match inList with 
        | [] -> List.rev outList
        | head::tail ->
            match tNode.Name = head.Name with
            | true -> let newOut = {head with Occupied = true; team = player.Team; cow = List.head player.cowsLeft}::outList
                      update tail newOut 
            | false -> update tail (head::outList) 
    update inList outList


 // this places the cows on the field
let rec placeCow position fieldList player turns =
    let targetNode = List.tryFind (fun x -> x.Name = position) fieldList
    match targetNode with 
    | Some t -> 
            match t.team with
            | 0 -> updateFieldList t fieldList [] player
            | _ -> failwith "Node is broken."
    | _ -> 
        printfn " not valid node"
        let newPos = getPlayerMove player turns
        placeCow newPos fieldList player turns

// this just prints the game boards.
let printField (nodeList: node List) =
    Console.ForegroundColor<-ConsoleColor.Cyan
    printfn "   0  1  2        3       4  5  6"
    printf "a  "
    List.iteri ( fun i x -> 
            let printColor team prefix suffix =
                match x.cow.inMill = 1 with
                | true -> printf "%s" prefix
                          Console.ForegroundColor<-ConsoleColor.White
                          printf "%s" x.cow.Name
                          Console.ForegroundColor<-ConsoleColor.Cyan
                          printf "%s" suffix
                | false ->
                    match team with 
                        | 0 -> printf "%s" prefix 
                               Console.ForegroundColor<-ConsoleColor.Cyan
                               printf "%s%s" x.cow.Name suffix
                        | 1 -> printf "%s" prefix 
                               Console.ForegroundColor<-ConsoleColor.Yellow
                               printf "%s" x.cow.Name
                               Console.ForegroundColor<-ConsoleColor.Cyan
                               printf "%s" suffix
                        | 2 -> printf "%s" prefix 
                               Console.ForegroundColor<-ConsoleColor.Green
                               printf "%s" x.cow.Name
                               Console.ForegroundColor<-ConsoleColor.Cyan
                               printf "%s" suffix
            match i with 
            | 0 | 1 -> printColor x.team "" "------------"
            | 2 -> printColor x.team "" "   "
                   printfn ""
                   printfn "   | \            |           / |"
            | 3 -> printColor x.team "b  |  " "---------" 
            | 4 -> printColor x.team "" "---------"
            | 5 -> printColor x.team "" "  |" 
                   printfn ""
                   printfn "   |  | \         |        / |  |"
            | 6 -> printColor x.team "c  |  |  " "------"
            | 7 -> printColor x.team "" "------"
            | 8 -> printColor x.team "" "  |  |"
                   printfn ""
                   printfn "   |  |  |                |  |  |"
                   printf "d  "
            | 9 | 10 -> printColor x.team "" "-"
            | 11 -> printColor x.team "" "              "
            | 12 | 13 -> printColor x.team "" "-"
            | 14 -> printColor x.team "" ""
                    printfn "" 
                    printfn "   |  |  |                |  |  |"
            | 15 -> printColor x.team "e  |  |  " "------"  
            | 16 -> printColor x.team "" "------" 
            | 17 -> printColor x.team "" "  |  |" 
                    printfn ""
                    printfn "   |  | /         |        \ |  |"
            | 18 -> printColor x.team "f  |  " "---------"
            | 19 -> printColor x.team "" "---------"  
            | 20 -> printColor x.team "" "  |"  
                    printfn ""
                    printfn "   | /            |           \ |"
                    printf "g  "
            | 21 | 22 -> printColor x.team "" "------------"  
            | 23 -> printColor x.team "" ""
                    printfn ""
    ) nodeList
    Console.ForegroundColor<-ConsoleColor.White
 

let chooseCowToShoot () =
    printfn "Mill was formed"
    printfn "Which enemy cow do you choose to shoot? (write the node)"
    System.Console.ReadLine()


let matchMill mill updatedPlayer updatedField enemy = 
    match mill with
    | 1,millRow -> 
        let chosen = chooseCowToShoot()
        let newHerd,_ = filterCowMillList millRow updatedPlayer.cowsOnField 1
        let newPlayer = {updatedPlayer with cowsOnField = newHerd}
        let updateEnemy,newfield = shootCow chosen updatedField enemy
        printfn " "
        printField newfield
        printfn " "
        1
    //stateMachine state enemy currentPlayer updatedField (turns + 1)
    | 0,_ -> 
        printfn " "
        0
    | _ -> failwith "something went horribly wrong"


let createNodeNieghbours nodeName =
     match nodeName with 
     | "a0" -> ["a3";"d0";"b1"]
     | "a3" -> ["a0"; "b3"; "a6"]
     | "a6" -> ["a3"; "d6"; "b5"]
     | "b1" -> ["a0"; "c2"; "d1"; "b3"]
     | "b3" -> ["a3"; "c3"; "b1"; "b5"]
     | "b5" -> ["a6"; "c4"; "b3"; "d5"]
     | "c2" -> ["b1"; "c3"; "d2"]
     | "c3" -> ["c2"; "c4"; "b3"]
     | "c4" -> ["c3"; "b5"; "d4"]
     | "d0" -> ["d1"; "a0"; "g0"]
     | "d1" -> ["d0"; "b1"; "d2"; "f1";]
     | "d2" -> ["d1"; "c2"; "e2"]
     | "d4" -> ["d5"; "c4"; "e4"]
     | "d5" -> ["d4"; "d6"; "b5"; "f5"]
     | "d6" -> ["d5"; "a6"; "g6"]
     | "e2" -> ["f1"; "e3"; "d2"]
     | "e3" -> ["e2"; "e4"; "f3"]
     | "e4" -> ["e3"; "f5"; "d4"]
     | "f1" -> ["d1"; "f3"; "e2"; "g0"]
     | "f3" -> ["f1"; "e3"; "g3"; "f5"]
     | "f5" -> ["g6"; "d5"; "e4"]
     | "g0" -> ["g3"; "f1"; "d0"]
     | "g3" -> ["g0"; "g6"; "f3"]
     | "g6" -> ["g3"; "f5"; "d6"]
     | _ -> failwith "What did we do wrong..."


let createNewNode node: node = 
    let newPoint =
        match node.Position.yCoord with
        | "a" -> 
            match node.Position.xCoord with
            | 0 | 3 -> {xCoord = node.Position.xCoord + 3; yCoord = node.Position.yCoord;}
            | 6 -> {xCoord = 1; yCoord = "b";}
        | "b" -> 
            match node.Position.xCoord with
            | 1 | 3 ->  {xCoord = node.Position.xCoord + 2; yCoord = node.Position.yCoord;}
            | 5 -> {xCoord = 2; yCoord = "c";}
        | "c" -> 
            match node.Position.xCoord with
            | 2 | 3 -> {xCoord = node.Position.xCoord + 1; yCoord = node.Position.yCoord;}
            | 4 -> {xCoord = 0; yCoord = "d";}
        | "d" -> 
            match node.Position.xCoord with
            | 0 | 1 | 4 | 5 -> {xCoord = node.Position.xCoord + 1; yCoord = node.Position.yCoord;}
            | 2 -> {xCoord = 4; yCoord = node.Position.yCoord;}
            | 6 -> {xCoord = 2; yCoord = "e";}
        | "e" -> 
            match node.Position.xCoord with
            | 2 | 3 -> {xCoord = node.Position.xCoord + 1; yCoord = node.Position.yCoord;}
            | 4 -> {xCoord = 1; yCoord = "f";}
        | "f" ->
            match node.Position.xCoord with
            | 1 | 3 ->  {xCoord = node.Position.xCoord + 2; yCoord = node.Position.yCoord;}
            | 5 -> {xCoord = 0; yCoord = "g";}
        | "g" -> 
            match node.Position.xCoord with
            | 0 | 3 ->  {xCoord = node.Position.xCoord + 3; yCoord = node.Position.yCoord;}
            | 6 -> node.Position
        | _ -> failwith " something went wrong "
    
    let newName = newPoint.yCoord + string newPoint.xCoord

    {Name = newName
     Position = newPoint
     Occupied = false
     team = 0
     neighbours = createNodeNieghbours newName
     number = node.number + 1
     cow = {Name = "[]";Position = "NP"; isOP = false; isOnBoard = false; isAlive = false; inMill = 0 }
     }
// this function returns the nieghbours of the node name put in it returns a string list though.


let rec createField node listNode acc =
        match acc with 
        | 23 -> listNode
        | _ -> 
            let newNode = createNewNode node
            createField newNode (newNode::listNode) (acc + 1)


let createNodeList =
    let initPoint = {xCoord=0; yCoord="a";}
    let initName = initPoint.yCoord + string initPoint.xCoord

    let initNode = {
        Name = initName
        Position = initPoint
        Occupied = false
        team = 0
        neighbours = createNodeNieghbours initName
        number = 0
        cow = {Name = "[]";Position = "NP"; isOP = false; isOnBoard = false; isAlive = true; inMill = 0 }
        }
    
    //let rec createField node listNode acc =

    createField initNode [initNode] 0


// this gives both players their cows
let givePlayerCows  (myList: Cow list) team=
    let rec giveCows (playerFieldCows: Cow list) acc =
        match playerFieldCows.Length with
        | 12 ->List.rev playerFieldCows
        | _ -> 
            match team with 
            | 1 ->
                let newCow = {Name = ("R" + (string acc));Position = "NP"; isOP = false; isOnBoard = false; isAlive = true; inMill = 0 }
                let name = newCow.Name
                let fieldCows = newCow::playerFieldCows
                giveCows fieldCows (acc+1)
            | 2 ->
                let newCow = {Name = ("B" + (string acc));Position = "NP"; isOP = false; isOnBoard=false; isAlive = true; inMill = 0}
                let name = newCow.Name
                let fieldCows = newCow::playerFieldCows
                giveCows fieldCows (acc+1)
            | _ -> failwith "lol thats not going to work"
    giveCows myList 0


let createPlayer (name:string) team = 
     let player1 = {Name = name; cowsLeft = givePlayerCows ([]) team ; isTurn = false; cowsOnField = []; Team = team}
     player1


let gameController () =
    System.Console.Clear()
    let player1 = createPlayer "player 1" 1
    let player2 = createPlayer  "player 2" 2
    let fieldList = List.rev createNodeList
    printfn " "
    let print = printField fieldList

    let rec stateMachine state (p1:Player) (p2:Player) field turns millList = 
        let playerTurn = p1,p2
        let currentPlayer,enemy = playerTurn
        match state with 
        | PLACING -> //0 is the placing stage
            match turns < 25 with
            | false -> stateMachine MOVING p1 p2 field turns millList
            | _ ->
                //let p = placeCow currentPlayer field turns 
                let place = getPlayerMove currentPlayer turns
                let newPlayerField = placeCow place field currentPlayer turns
                let h::CowsLeft =  currentPlayer.cowsLeft   // cowsleft list is the cows that are left in the players hand
                let newCow = {Name =h.Name; Position = place; isAlive = h.isAlive; isOnBoard = true; isOP=h.isOP; inMill = h.inMill}
                let updatedPlayer = {currentPlayer with cowsLeft = CowsLeft; cowsOnField = newCow::currentPlayer.cowsOnField}
                printField newPlayerField
                
                let q = CheckinMill (List.tryHead updatedPlayer.cowsOnField) (newPlayerField) (updatedPlayer) 
                match q with 
                | 1,millRow -> 
                        let chosen = chooseCowToShoot()
                        let newHerd,supremeUltimateCowList = filterCowMillList millRow updatedPlayer.cowsOnField 1 

                        let newPlayer = {updatedPlayer with cowsOnField = newHerd}
                        let updateEnemy,newfield = shootCow chosen newPlayerField enemy
                        printfn " "
                        printField newfield
                        printfn " "
                        stateMachine state updateEnemy newPlayer newfield (turns+1) (supremeUltimateCowList::millList)
                | 0,_ -> 
                    printfn " "
                    stateMachine state enemy updatedPlayer newPlayerField (turns + 1) millList
                | _ -> printfn "something went horribly wrong"
        | MOVING -> 
            movingIntroMessage currentPlayer
            match (numOfCowsPlayerHasOnBoard field currentPlayer.Team) <= 3 with 
            | true -> stateMachine FLYING currentPlayer enemy field turns millList
            | _ ->
                let move = chooseWhereToMove ()
                match move with 
                | [startPoint;endPoint] -> 
                        let startNode = GetNodeFromName startPoint field
                        let endNode = GetNodeFromName endPoint field
                        let cowToMove = List.find (fun (x:Cow)-> startNode.Name = x.Position) currentPlayer.cowsOnField


                        let movedCow = //{cowToMove with Position = endNode.Name}
                            match cowToMove.inMill with 
                            | 0 -> {cowToMove with Position = endNode.Name}
                            | 1 -> {cowToMove with Position = endNode.Name; inMill = 2}
                            | 2 -> {cowToMove with Position = endNode.Name; inMill = 0}
                        let updatedField = moveCows startNode endNode field movedCow

                        let updatedCowsOnField = 
                            List.map ( fun (x:Cow) ->
                                match x.Name = movedCow.Name with
                                | false -> x
                                | true -> movedCow
                            ) currentPlayer.cowsOnField
                        let updatedPlayer = {currentPlayer with cowsOnField = updatedCowsOnField}
                        
                        printField updatedField                    

                        let mill = CheckinMill (Some movedCow) updatedField updatedPlayer

                        match mill with
                        | 1,millRow -> 
                            let chosen = chooseCowToShoot()
                            let newHerd,supremeUltimateCowList = filterCowMillList millRow updatedPlayer.cowsOnField 1 

                            let newPlayer = {updatedPlayer with cowsOnField = newHerd}
                            let updateEnemy,newfield = shootCow chosen updatedField enemy
                            printfn " "
                            printField newfield
                            printfn " "
                            stateMachine state updateEnemy newPlayer newfield (turns+1) (supremeUltimateCowList::millList)
                        | 0,_ -> 
                            printfn " "
                            stateMachine state enemy updatedPlayer updatedField (turns + 1) millList
                        | _ -> printfn "something went horribly wrong"
                         
                | _ -> failwith "That is not a valid move."
        | FLYING -> 
            flyingIntroMessage currentPlayer
            match List.length currentPlayer.cowsOnField < 3 with 
            | true -> stateMachine END currentPlayer enemy field turns millList
            | false ->
                printfn "its %s turn" currentPlayer.Name
                //let powerPlayer =
                match List.length currentPlayer.cowsOnField = 3 with
                | true ->
                    let EnlightedTheBeasts = List.map ( fun (x:Cow) -> {x with isOP = true}) currentPlayer.cowsOnField 
                    printfn " "

                    let powerdPlayer = {currentPlayer with cowsOnField = EnlightedTheBeasts}

                    let splitLine = (fun (line : string) -> Seq.toList (line.Split ' '))
                    let playerMove = System.Console.ReadLine()
                    let move = splitLine playerMove
                    match move with 
                    | [startPoint;endPoint] -> 
                            let startNode = GetNodeFromName startPoint field
                            let endNode = GetNodeFromName endPoint field
                            let cowToMove = List.find (fun (x:Cow)-> startNode.Name = x.Position) powerdPlayer.cowsOnField
                            let movedCow = {cowToMove with Position = endNode.Name}
                            let updatedField = moveCows startNode endNode field movedCow

                            let updatedCowsOnField = 
                                List.map ( fun (x:Cow) ->
                                    match x.Name = movedCow.Name with
                                    | false -> x
                                    | true -> movedCow
                                ) powerdPlayer.cowsOnField
                            let updatedPlayer = {powerdPlayer with cowsOnField = updatedCowsOnField}
                        
                            printField updatedField                    

                            let mill = CheckinMill (Some movedCow) updatedField updatedPlayer

                            match mill with
                            | 1,millRow -> 
                                let chosen = chooseCowToShoot()
                                let newHerd,supremeUltimateCowList = filterCowMillList millRow updatedPlayer.cowsOnField 1
                                let newPlayer = {updatedPlayer with cowsOnField = newHerd}
                                let updateEnemy,newfield = shootCow chosen updatedField enemy
                                printfn " "
                                printField newfield
                                printfn " "
                                stateMachine state updateEnemy newPlayer newfield (turns+1) (supremeUltimateCowList::millList)
                                //stateMachine state enemy currentPlayer updatedField (turns + 1)
                            | 0,_ -> 
                                printfn " "
                                stateMachine state enemy currentPlayer updatedField (turns + 1) millList
                            | _ -> printfn "something went horribly wrong"
                | false -> 
                    printfn "%s cows cant fly yet" currentPlayer.Name
                    let splitLine = (fun (line : string) -> Seq.toList (line.Split ' '))
                    let playerMove = System.Console.ReadLine()
                    let move = splitLine playerMove
                    match move with 
                    | [startPoint;endPoint] -> 
                    // Think about storing this in a tuple
                            let startNode = GetNodeFromName startPoint field
                            let endNode = GetNodeFromName endPoint field
                            let cowToMove = List.find (fun (x:Cow)-> startNode.Name = x.Position) currentPlayer.cowsOnField
                            let movedCow = {cowToMove with Position = endNode.Name}

                            let updatedField = moveCows startNode endNode field movedCow

                            let updatedCowsOnField = 
                                List.map ( fun (x:Cow) ->
                                    match x.Name = movedCow.Name with
                                    | false -> x
                                    | true -> movedCow
                                ) currentPlayer.cowsOnField
                            let updatedPlayer = {currentPlayer with cowsOnField = updatedCowsOnField}
                        
                            printField updatedField                    

                            let mill = CheckinMill (Some movedCow) updatedField updatedPlayer

                            match mill with
                            | 1,millRow -> 
                                let chosen = chooseCowToShoot()
                                let newHerd, supremeUltimateCowList = filterCowMillList millRow updatedPlayer.cowsOnField 1
                                let newPlayer = {updatedPlayer with cowsOnField = newHerd}
                                let updateEnemy,newfield = shootCow chosen updatedField enemy
                                printfn " "
                                printField newfield
                                printfn " "
                                stateMachine state updateEnemy newPlayer newfield (turns+1) (supremeUltimateCowList::millList)
                                //stateMachine state enemy currentPlayer updatedField (turns + 1)
                            | 0,_ -> 
                                printfn " "
                                stateMachine state enemy updatedPlayer updatedField (turns + 1) millList
                            | _ -> printfn "something went horribly wrong"
                         
                    | _ -> failwith "That is not a valid move."
        | END -> printfn "%s lost in %d turns" enemy.Name turns
    stateMachine PLACING player1 player2 fieldList 1 []


let rules () = printfn "The aim of Morabaraba is to get your opponent down to only 2 cows. 

There are three main phases to the game: 
    1) Placing the cows. 
    2) Moving the cows. 
    3) Flying the cows. 

PLACING THE COWS:
    Each player starts off with 12 cows that they can place anywhere on the board. 
    Each turn one cows is placed, until all 12 cows have been placed on the board, then 
    the game moves on to the next phase: moving the cows. 

    Three cows in a row on the board (diagonal, horizontal or vertical) represents a mill. 
    When a mill is formed, mob syndrome immediately kicks in and the player who formed the mill 
    can 'shoot' or remove one of the cows from the other players team. Even if two mills are 
    formed, only one cow may be shot each round. 

    Cows in a mill can not be shot. 

    A cow once outside of a mill must wait a turn, before it is placed back into that same mill. 

MOVING THE COWS 
    Once all the cows have been placed on the board, cows can be moved to any empty adjacent intersection. 

FLYING THE COWS
    When a player only has three cows left, the cows gain super powers, and can 'fly' or be placed anywhere 
    on the board. 

    Only the player will three cows will be allowed to fly. 

    A win occurs if one opponent has no possible moves.
    A win occurs if a player has just two cows.
    If either player has only three cows and neither player shoots a cow within ten moves, the game is drawn.
    
                                                                                Source: Wikipedia Morabaraba
                                                                    https://en.wikipedia.org/wiki/Morabaraba
                                                                    
    Type Begin to start, or flee for COWards.                                                                "


let startMessage () = printfn "     Greetings fellow humans.

            For many years the agressive and militant nature of cow has been documented 
            and studied, and through the generations these studies have been condensed 
            and widdled into a game played with stones and lines on dirt.
      
            In the new age of harnessed electricity and 440ml coke bottles, we have built a new 
            board out of strings and integers for you to do battle on, as your forefathers once
            did.                                     
                    
            Those who can harness the hyper-agressive and murderous nature of cows will quickly 
            master the game, and those who cannot will be served as the thin cardboard stuff 
            they put between a Mcdonalds bun. 
           
            Mills will be formed. Bovine blood will be spilled. Cows will fly.
               
            Friendships will be ruined. 
          
            If you are ready to go to cow war. Type: Begin.
            
            If you wish to learn the rules of the game I speak of. Type: Rules. 
          
            If you want to leave because this is weird. Type: Flee!.
            "  // Print out a welcome for Morabaraba 
           

let rec beginGame () = 
    startMessage ()
    let ans = System.Console.ReadLine()
    match ans.ToLower() with 
    | "begin" -> gameController ()
    | "rules" -> rules ()
                 beginGame ()
    | "flee!" | "flee" -> printfn "That's cowardice! THIS IS COW WAR!!!!!!!!" 
                          beginGame ()
    | _ -> printfn "What words are these?"
           beginGame  ()


[<EntryPoint>]
let main argv =
    beginGame () 
    printfn "-=-=-=-=-=-=-=- END GAME -=-=-=-=-=-=-=-=-=-" 
    let halt = System.Console.ReadLine ()
    printfn "%A" argv
    0 // return an integer exit code
