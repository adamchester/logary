
open System
open Libryy.Logging
open Libryy.Logging.Message

let logger = Targets.create Verbose

type MyRecord = { id: string; text: string }
  with override x.ToString() = sprintf "(ID=%s) \"%s\"" x.id x.text

type MyDu =
  | Case1 of int * decimal
  | Case2 of string * DateTime
  override x.ToString() =
    match x with
    | Case1 (i, d) -> sprintf "Case1 (%i, %M) (via ToString())" i d
    | Case2 (s, dt) -> sprintf "Case2 (%s, %A) (via ToString())" s dt

[<EntryPoint>]
let main argv = 

  let myString = Message.templateEvent<string> "My string is {string}"
  let receivedMyDuDefault = Message.templateEvent<MyDu> "Received MyDu itself as {duDefaultCapture}"
  let receivedMyDuStringified = Message.templateEvent<MyDu> "Received MyDu stringified as {$duStringified}"
  let receivedMyDuStructured = Message.templateEvent<MyDu> "Received MyDu structured as {@duStructured}"

  let receivedMyRecordDefault = Message.templateEvent<MyRecord> "Received MyRecord itself as {record}"
  let receivedMyRecordStringified = Message.templateEvent<MyRecord> "Received MyRecord stringified as {$recordStringified}"
  let receivedMyRecordStructured = Message.templateEvent<MyRecord> "Received MyRecord structured as {@recordStructured}"

  logger.verbose (myString "test")
  let myDuCase1_99 = Case1 (99, 99M)
  logger.debug (receivedMyDuDefault myDuCase1_99)
  logger.debug (receivedMyDuStringified myDuCase1_99)
  logger.debug (receivedMyDuStructured myDuCase1_99)

  let myRecordValue9 = {id="9"; text="nine"}
  logger.info (receivedMyRecordDefault myRecordValue9)
  logger.info (receivedMyRecordStructured myRecordValue9)
  logger.info (receivedMyRecordStringified myRecordValue9)
  
  0 // return an integer exit code
