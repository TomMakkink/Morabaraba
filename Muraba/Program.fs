open System.Collections.Specialized

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
       WhichNode: Node
      }

type Player = 
     {Name: string
      cowsLeft : Cow List
      isTurn: bool
      cowsOnField: Cow List
      }

let checkCow (randCow: Cow) (player:Player) = 
 match player.isTurn with 
 | false -> failwith "this cow is a lie"
 | _ -> __

let createPlayer (name:string) = 
 let player1 = {Name = name; cowsLeft = []; isTurn=false; cowsOnField=[]}
 player1

let place (p:string) = 
 let charList = Seq.toList p
 let col = List.last charList
 let point = {row = List.head charList; collumn = int col}
 point

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
   match checkCow cowName play with
   | __



[<EntryPoint>]
let main _ = 
_  // return an integer exit code
