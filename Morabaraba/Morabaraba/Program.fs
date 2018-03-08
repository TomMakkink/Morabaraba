// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
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
// this creates the players at the begining of the game
let createPlayer (name:string) team = 
     let player1 = {Name = name; cowsLeft = givePlayerCows ([]) team ; isTurn = false; cowsOnField = []; Team = team}
     player1
// this creates each new node and returns them in a giant list.
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
     neighbours = []
     number = node.number + 1
     cow = {Name = "[]";Position = "NP"; isOP = false; isOnBoard = false; isAlive = false; inMill = 0 }
     }
// this function returns the nieghbours of the node name put in it returns a string list though.
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
// this method takes in a node name and returns the actual node. 
let GetNodeFromName nodeName nodeList =
 let rec getNode (tempNodeList: node List) = 
  match tempNodeList with 
  | [] -> failwith "This is not the node your looking for"
  | h::rest -> 
   match h.Name = nodeName with  
   | true -> h
   | false -> getNode rest
 getNode nodeList
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
// this method will check each cows mill number in the given row to see if they are allowed to form a mill
let CheckMillNumber listOfRows cowsToCheck = 
  let h::m::t::_ = listOfRows
  let cow1 = List.find (fun (x:Cow) -> x.Position = h ) cowsToCheck
  let cow2 = List.find (fun (x:Cow) -> x.Position = m ) cowsToCheck
  let cow3 = List.find (fun (x:Cow) -> x.Position = t ) cowsToCheck
  match (cow1.inMill = 0 && cow2.inMill = 0) && cow3.inMill =0 with 
  | true -> true, listOfRows
  | _ -> false,listOfRows
// this method will return a (bool*string List) which is a bool to show if a mill formed or not and the row.
let checkCowsInRow millNodeLists mainNodeList cowsOnField =
 let rec check (nodeList: string List List)  =
  match nodeList with 
  | [] -> false,[]
  | h::rest ->
   let fir::sec::third::_ = h
   let firNode = GetNodeFromName fir mainNodeList
   let secNode = GetNodeFromName sec mainNodeList
   let thirdNode = GetNodeFromName third mainNodeList
   match firNode.Occupied && secNode.Occupied && thirdNode.Occupied with 
   | false -> check rest 
   | true ->
    match firNode.team = secNode.team && thirdNode.team = secNode.team with 
    | false ->  check rest 
    | true -> 
     match CheckMillNumber h cowsOnField with 
     | false,_ -> check rest
     | true,x -> true,x
 check millNodeLists 
// CHECK TO SEE IF I DON"T NEED THIS METHOD
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
     | false,_ -> 0,[]
     | true,millrow ->
      printfn "A mill was formed \n"
      1,millrow
// this is the method that returns the updated lists.      
let shooting deadCow herdLeft =
 let rec check inList outList = 
  match inList with 
  | [] -> List.rev outList 
  | h::rest -> 
   match h=deadCow with 
   | true -> check rest outList
   | false -> check rest (h::outList)
 check herdLeft []
// this will change the node of the now dead cow to unOccupied and also the name of the cow to default
let updateSadNode newNodes oldNodes = 
 let rec check inList outList =
  match inList with 
  | [] -> List.rev outList
  | h::rest -> 
   match h.Name = newNodes.Name with 
   | true -> 
    let newNode = {newNodes with Occupied =  false; cow =  {Name = "[]";Position = "NP"; isOP = false; isOnBoard = false; isAlive = false; inMill = 0 }}
    check rest (newNode::outList)
   | false -> check rest (h::outList)
 check oldNodes []  
// This will return an updated enemy cowsleft and cowsOnField and will not allow friendly fire also returns an updated nodeList
// still need to call the board update as well as make sure the cow is dead
let shootCow nodeName mainNodeList enemy = 
 let actualNode = GetNodeFromName nodeName mainNodeList
 match actualNode.Occupied && actualNode.team<>enemy.Team with 
 | false -> enemy,mainNodeList
 | true -> 
  match actualNode.cow.inMill <> 1 with 
  | false -> enemy,mainNodeList
  | true ->
   let newCowsLeft = shooting actualNode.cow enemy.cowsLeft
   let newCowsOnField = shooting actualNode.cow enemy.cowsOnField
   let updateEnemy = {enemy with cowsLeft = newCowsLeft; cowsOnField = newCowsOnField}
   let NewMainNode = updateSadNode actualNode mainNodeList
   updateEnemy,NewMainNode
// this creates all the nodes and puts them all into one list.
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

    let rec createField node listNode acc =
        match acc with 
        | 23 -> listNode
        | _ -> 
            let newNode = createNewNode node
            createField newNode (newNode::listNode) (acc + 1)
    
    createField initNode [initNode] 0
// this just prints the game boards.
let printField (nodeList: node List) =
    //printfn "0  1    2       3       4  5   6"
    printf "a  "
    List.iteri ( fun i x -> 
            match i with 
            | 0 | 1 -> printf "%s------------" x.cow.Name
            | 2 -> printfn "%s   " x.cow.Name 
                   printfn "   | \            |           / |"
            | 3 -> printf "b  |  %s---------" x.cow.Name
            | 4 -> printf "%s---------" x.cow.Name 
            | 5 -> printfn "%s  |" x.cow.Name 
                   printfn "   |  | \         |        / |  |"
            | 6 -> printf "c  |  |  %s------" x.cow.Name
            | 7 -> printf "%s------" x.cow.Name 
            | 8 -> printfn "%s  |  |" x.cow.Name 
                   printfn "   |  |  |                |  |  |"
                   printf "d  "
            | 9 | 10 -> printf "%s-" x.cow.Name 
            | 11 -> printf "%s              " x.cow.Name 
            | 12 | 13 -> printf "%s-" x.cow.Name 
            | 14 -> printfn "%s" x.cow.Name 
                    printfn "   |  |  |                |  |  |"
            | 15 -> printf "e  |  |  %s------" x.cow.Name 
            | 16 -> printf "%s------" x.cow.Name 
            | 17 -> printfn "%s  |  |" x.cow.Name 
                    printfn "   |  | /         |        \ |  |"
            | 18 -> printf "f  |  %s---------" x.cow.Name 
            | 19 -> printf "%s---------" x.cow.Name 
            | 20 -> printfn "%s  |" x.cow.Name 
                    printfn "   | /            |           \ |"
                    printf "g  "
            | 21 | 22 -> printf "%s------------" x.cow.Name 
            | 23 -> printfn "%s" x.cow.Name 
    ) nodeList
// this is where each node.
let updateList tNode inList outList player =
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
let placeCow position fieldList player =
    let targetNode = List.find (fun x -> x.Name = position) fieldList
    match targetNode.team with
    | 0 -> updateList targetNode fieldList [] player
    | _ -> failwith "invalid move"
// this will take in a string List (cow List) and also the players filedcows and the new mill value and return a list of cows
let changeCowMill millRow fieldCows newMill = 
 let rec check inList (cowList: Cow List) outList = 
  match inList with 
  | [] -> List.rev outList
  | h::rest ->
   let head::tail = cowList
   match h = head.Position with 
   | false -> check inList tail (head::outList)
   | true -> 
    let newCow = {head with inMill = newMill}
    check rest tail (newCow::outList)
 check millRow fieldCows []

 // Print out rules and a welcome for Morabaraba 
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
            "

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
If either player has only three cows and neither player shoots a cow within ten moves, the game is drawn."


let printRules ans = 
    match ans with 
    | "Begin" -> ()
    | "Rules" -> rules () 
    | "Flee!" -> printfn "You cannot flee coward! THIS IS COW WAR!!!!!!!!" 
    | _ -> printfn "What words are these?" 

   
// this is our main game controller
let gameController () =
    let player1 = createPlayer "Power Rangers" 1
    let player2 = createPlayer  "Avengers" 2
    let fieldList = List.rev createNodeList
    printfn " "
    let print = printField fieldList

    let rec stateMachine state p1 p2 field turns = 
        match state with 
        | 0 ->
            match turns < 13 with
            | false -> stateMachine (state+1) p1 p2 field turns
            | _ ->
                printfn "place cow on which node.      turn = %d" turns
                let place = System.Console.ReadLine()
                let playerPlace = placeCow place field p1
                let h::p1CowsLeft = p1.cowsLeft
                let newCow = {Name =h.Name; Position = place; isAlive = h.isAlive; isOnBoard = true; isOP=h.isOP; inMill = h.inMill}
                let updatedPlayer1 = {p1 with cowsLeft = p1CowsLeft; cowsOnField = newCow::p1.cowsOnField}
                printField playerPlace
                // check with jeff when the node list occupied is updated cause mill method is failing due to that.
                // THIS AS ONLY BEEN MADE FOR PLAYER 1 SO FAR
                let q =CheckinMill (List.tryHead updatedPlayer1.cowsOnField) (field) (updatedPlayer1) 
                match q with 
                | 1,millRow -> 
                 printfn "Can shoot: Which enemy cow do you choose write the node /n"
                 let chosen = System.Console.ReadLine()
                 let newHerd = changeCowMill millRow updatedPlayer1.cowsOnField 1
                 let newPLayer1 = {updatedPlayer1 with cowsOnField = newHerd}
                 let updateEnemy,newfield = shootCow chosen field p2
                 printfn " "
                 stateMachine state newPLayer1 updateEnemy newfield (turns+1)
                | 0,_ -> 
                 printfn " "
                 stateMachine state updatedPlayer1 p2 playerPlace (turns + 1)
        | 1 -> printfn "2nd stage"
        | 2 -> printfn "super stage"
        | _ -> printfn "something went wrong with the game states"
    stateMachine 0 player1 player2 fieldList 1


[<EntryPoint>]
let main argv =
    startMessage ()
    let ans = System.Console.ReadLine()
    printRules ans 
    let game = gameController ()
    printfn "-=-=-=-=-=-=-=- END GAME -=-=-=-=-=-=-=-=-=-" 
    let halt = System.Console.ReadLine ()
    printfn "%A" argv
    0 // return an integer exit code
