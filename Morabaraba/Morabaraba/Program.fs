﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
type Cow = {
    Name: string
    Position: string
    isOP : bool
    isOnBoard : bool
    }

type Player = 
     {
      Name: string
      cowsLeft : Cow List
      isTurn: bool
      cowsOnField: Cow List
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
    neighbours: node list
    number: int
    }




let givePlayerCows  (myList: Cow list) team=
    let rec giveCows (playerFieldCows: Cow list) acc =
        match playerFieldCows.Length with
        | 12 ->List.rev playerFieldCows
        | _ -> 
            match team with 
            | 1 ->
                let newCow = {Name = ("R" + (string acc));Position = "NP"; isOP = false; isOnBoard = false }
                let name = newCow.Name
                let fieldCows = newCow::playerFieldCows
                giveCows fieldCows (acc+1)
            | 2 ->
                let newCow = {Name = ("B" + (string acc));Position = "NP"; isOP = false; isOnBoard=false}
                let name = newCow.Name
                let fieldCows = newCow::playerFieldCows
                giveCows fieldCows (acc+1)
            | _ -> failwith "lol thats not going to work"
    giveCows myList 0

let createPlayer (name:string) team = 
     let player1 = {Name = name; cowsLeft = givePlayerCows ([]) team ; isTurn = false; cowsOnField = []}
     player1

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
     }

// this function returns the nieghbours of the node name put in it returns a string list though.
let createNodeNieghbours (nodeName:string) =
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
let GetNodeFromName (nodeName:string) (nodeList: node List) =
 let rec getNode (tempNodeList: node List) = 
  match tempNodeList with 
  | [] -> failwith "This is not the node your looking for"
  | h::rest -> 
   match h.Name = nodeName with  
   | true -> h
   | false -> getNode rest
 getNode nodeList
(*
        0   1   2   3   4   5   6
    a   x-----------x-----------x
    b   |   x-------x-------x   |
    c   |   |   x---x---x   |   |
    d   x---x---x       x---x---x
    e   |   |   x---x---x   |   |
    f   |   x-------x-------x   |
    g   x-----------x-----------x

*)

let createNodeList =
    let initPoint = {xCoord=0; yCoord="a";}
    let initName = initPoint.yCoord + string initPoint.xCoord

    let initNode = {
        Name = initName
        Position = initPoint
        Occupied = false
        team = 0
        neighbours = []
        number = 0
        }

    let rec createField node listNode acc =
        match acc with 
        | 23 -> listNode
        | _ -> 
            let newNode = createNewNode node
            createField newNode (newNode::listNode) (acc + 1)
    
    createField initNode [initNode] 0

let printField (nodeList: node List) =
    //printfn "0  1    2       3       4  5   6"
    List.iteri ( fun i x -> 
            match i with 
            | 0 | 1 -> printf "%s------------" x.Name
            | 2 -> printfn "%s   " x.Name
            | 3 -> printf "|  %s---------" x.Name
            | 4 -> printf "%s---------" x.Name
            | 5 -> printfn "%s  |" x.Name
            | 6 -> printf "|  |  %s------" x.Name
            | 7 -> printf "%s------" x.Name
            | 8 -> printfn "%s  |  |" x.Name
            | 9 | 10 -> printf "%s-" x.Name
            | 11 -> printf "%s              " x.Name
            | 12 | 13 -> printf "%s-" x.Name
            | 14 -> printfn "%s" x.Name
            | 15 -> printf "|  |  %s------" x.Name
            | 16 -> printf "%s------" x.Name
            | 17 -> printfn "%s  |  |" x.Name
            | 18 -> printf "|  %s---------" x.Name
            | 19 -> printf "%s---------" x.Name
            | 20 -> printfn "%s  |" x.Name
            | 21 | 22 -> printf "%s------------" x.Name
            | 23 -> printfn "%s" x.Name
    ) nodeList

[<EntryPoint>]
let main argv =
    System.Console.ForegroundColor<-System.ConsoleColor.Green
    let field = List.rev createNodeList
    printfn " "
    let print = printField field
    let player1 = createPlayer "Power Rangers" 1
    let player2 = createPlayer  "Avengers" 2
    printfn " "
    System.Console.ForegroundColor<-System.ConsoleColor.Red
    printf "%A :   " player1.Name
    List.iter ( fun (c:Cow) -> printf " %s" c.Name) player1.cowsLeft
    printfn ""
    System.Console.ForegroundColor<-System.ConsoleColor.Cyan
    printf "%A :   " player2.Name
    List.iter ( fun (c:Cow) -> printf " %s" c.Name) player2.cowsLeft
    let halt = System.Console.ReadLine()
    printfn "%A" argv
    0 // return an integer exit code