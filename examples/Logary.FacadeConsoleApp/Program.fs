
open System
open Libryy.Logging
open Libryy.Logging.Message

let logger = Targets.create Info

[<EntryPoint>]
let main argv = 
  let myEvent = Message.templateEvent<string> (Debug, "Hello {name}")
  let logArgs = Message.templateEvent<string[]> (Debug, "Recieved {args}")
  logger.info (logArgs argv)
  
  0 // return an integer exit code
