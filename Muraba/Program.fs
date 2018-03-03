open System.Collections.Specialized
open System.Security.Policy

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
type Point =
     {row : char 
      collumn: int        
      }

type Node =
     {Place : Point 
      nieghbours: Node List  
      }

type Cow =
     { Name:string
       isOnNode: bool  
       isInMill: int
       WhichNode:  Node
      }

type Player = 
     {Name: string
      cowsLeft : Cow List
      isTurn: bool
      cowsOnField: Cow List
      }

// this will find if a player as a certain cow just from the cows name and return the full cow if it does exist.
let findCow (randCowName:string) (cowList: Cow List) = 
 let rec getCow (tempCowList: Cow List) = 
  match tempCowList with 
  | [] -> failwith "No cow found"
  | h::rest ->
   let tempCowName = h.Name
   match tempCowName = randCowName with 
   | true -> h
   |_ -> getCow rest
 getCow cowList  


// this function returns a bool value when seeing if a cow name is in a players cow list.
let checkCowIsReal (randCow: string) (playerCowList:Cow List) = 
 let rec CowBool (tempPlayerCowList: Cow List) =
  match tempPlayerCowList with 
  | [] -> false
  | h::rest -> 
   let q = h.Name
   match q =randCow with 
   | false -> CowBool rest
   | true -> true
 CowBool playerCowList

let checkifWon (player1:Player) (player2:Player) =
 match player1.cowsLeft.Length =2 || player2.cowsLeft.Length =2 with
 |false -> 0
 |true -> 
  match player1.cowsLeft.Length =2 with 
  | true -> 1
  | false -> 2

//create a new list of default cows
// cow = {Name = givenName; isOnNode = false; isInMill = 0; WhichNode ={place={row='z'; collumn=0}; nieghbours = []}}
let createCow (names: string List) = 
 let rec makeCows (nameList: string List) (newCows: Cow List) =
  match nameList with
  | [] -> newCows
  | h::rest -> 
   let newPoint = {row = 'z';collumn = 0}
   let newNode = {Place = newPoint; nieghbours = []}
   let newCow = {Name = h; isOnNode = false; isInMill = 0; WhichNode = newNode}
   makeCows rest (newCow::newCows)
 makeCows names []
   
// create a player class
// player = {Name =can be chosen; cowsLeft = 1-12; isTurn = false; cowOnField = []}
let createPlayer (name:string) = 
 let nameList = ["1";"2";"3";"4";"5";"6";"7";"8";"9";"10";"11";"12"]
 let player1 = {Name = name; cowsLeft = createCow (nameList) ; isTurn = false; cowsOnField = []}
 player1

 // put in a string to get a position
let place (p:string) = 
 let charList = Seq.toList p
 let col = List.last charList
 let point = {row = List.head charList; collumn = int col}
 point

let checkIsMoveableCoOrds (placetoMove: Point) (cow:Cow) = 
 match cow.WhichNode.Place <> placetoMove with 
 | false -> false
 | _ -> 
  match 

let turn (play:Player) =
 match play.isTurn with 
 | false -> ()
 | _ -> 
  System.Console.WriteLine("Please enter a cow name and node you want to move to. Seperate these by a space.")
  let newCoOrds = System.Console.ReadLine()
  let NameA = newCoOrds.Split([|' '|])
  
  match NameA.Length = 2 with 
  | false -> failwith "fuck a duck"
  | _ -> 
   let cowName = Array.head NameA
   let placeToMove = place (Array.last NameA) 

   match checkCowIsReal cowName play.cowsLeft with
   | false ->failwith "you Lied about your cow"
   | _ -> 
    match checkIsMoveableCoOrds placeToMove (findCow cowName play.cowsLeft) with
    
    

[<EntryPoint>]
let main _ = 
_  // return an integer exit code
