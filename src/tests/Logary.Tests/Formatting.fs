module Logary.Tests.Formatting

open System

open Fuchu
open NodaTime

open Logary
open Logary.Formatting

open Logary.Tests.TestDSL

type SampleDuOverridingToString =
  | SampleCase1 of sc1Int : int
  | SampleCase2
  | SampleCase3 of sc3String : string * sc3Int : int
  override x.ToString() = sprintf "%A" x

type SampleRecord =
  { Field1 : string
    Field2 : int option }

let private sampleMessage : Message =
  { name      = PointName.ofList ["a"; "b"; "c"; "d"]
    value     = Event "this is bad"
    fields    = Map.empty
    context   = Map.empty
    timestamp = Instant.FromSecondsSinceUnixEpoch(3L).PlusTicks(1234567L).Ticks * 100L
    level     = LogLevel.Error }

let extractFormatFields msg =
  let template =
    match msg.value with
    | Event template ->
      template

    | x -> Tests.failtestf "unexpected %A" x

  let fields =
    msg.fields
    |> Seq.map (fun (KeyValue (key, value)) -> PointName.format key, value)

  template, Set.ofSeq fields

[<Tests>]
let tests =
  testList "formatting" [
    testCase "StringFormatter.Verbatim" <| fun _ ->
      (because "formatting the message verbatim" <| fun _ ->
        Message.eventError "hello world"
        |> StringFormatter.verbatim.format)
      |> should equal "hello world"
      |> thatsIt

    testCase "StringFormatter.VerbatimNewline" <| fun _ ->
      (because "logging verbatim with newline" <| fun () ->
        Message.eventInfo "hi there"
        |> StringFormatter.verbatimNewLine.format)
      |> should equal (sprintf "hi there%s" Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.VerbatimNewlineTemplated" <| fun _ ->
      (because "logging verbatim with newline, templated" <| fun () ->
        {(Message.eventInfo "what's {@direction}") with fields = [(PointName.ofSingle "direction", Field (String "up", None))] |> Map.ofList}
        |> StringFormatter.verbatimNewLine.format)
      |> should equal (sprintf "what's up%s" Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        sampleMessage |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
          sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]%s"
            Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, data" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with
            fields = Map [PointName.ofSingle "a", Field (String "b", None); PointName.ofSingle "a2", Field (Int64 24L, None) ]
            context = Map ["a", String "b"]}
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
          sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]%s  a => \"b\"%s  a2 => 24%s  Context:%s    a => \"b\"%s"
            Environment.NewLine Environment.NewLine Environment.NewLine Environment.NewLine Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, data, list with map in it" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with
            fields = [ PointName.ofSingle "a",  (Field (String "b", None))
                       PointName.ofSingle "a2", (Field (Int64 24L, None))
                       PointName.ofSingle "things",
                         (Field (Array
                           [ Int64 1L
                             Int64 2L
                             Object <| Map ["1", String "hello"] ], None))
                     ] |> Map.ofList
        }
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
          String.Format("E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]" +
                        "{0}  a => \"b\"{0}  a2 => 24{0}  things => {0}    - 1{0}    - 2{0}    - {0}      1 => \"hello\"{0}",
                        Environment.NewLine))
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, nested data" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with
            fields =
              [ PointName.ofSingle "a", (Field (["b", Int64 1L] |> Map.ofList |> Object, None))
                PointName.ofSingle "c", (Field (Int64 2L, None))
              ] |> Map.ofList
        }
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
          String.Format("E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]" +
                        "{0}  a => {0}    b => 1{0}  c => 2{0}", Environment.NewLine))
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl with exception" <| fun _ ->
      let e = new Exception("Gremlings in the machinery")
      (because "logging with exception attached" <| fun () ->
        sampleMessage
        |> Message.addExn e
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
        String.Format("E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]" +
                      "{0}  errors => {0}    - {0}      hResult => -2146233088{0}      message => \"{1}\"{0}      type => \"{2}\"{0}",
                      Environment.NewLine, e.Message, (e.GetType ()).FullName))
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl with exception, data" <| fun _ ->
      let e = new Exception("Gremlings in the machinery")
      (because "logging with exception attached" <| fun () ->
        { sampleMessage with fields = [PointName.ofSingle "a", Field (String "b", None); PointName.ofSingle "a2", Field (Int64 24L, None) ] |> Map.ofList }
        |> Message.addExn e
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
        String.Format("E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]"+
                      "{0}  a => \"b\"{0}  a2 => 24{0}" +
                      "  errors => {0}    - {0}      hResult => -2146233088{0}      message => \"{1}\"{0}      type => \"{2}\"{0}",
                      Environment.NewLine, e.Message, (e.GetType ()).FullName))
      |> thatsIt

    testCase "``JsonFormatter has no newline characters``" <| fun _ ->
      (because "logging message with newline in it" <| fun () ->
          { sampleMessage with value = Event "here\n  we\ngo!" } |> JsonFormatter.Default.format)
      |> should equal ("""{"context":{},"fields":{},"level":"error","name":["a","b","c","d"],""" +
                       """"timestamp":3123456700,"value":{"event":"here\n  we\ngo!"}}""")
      |> thatsIt

    testCase "Formatting.templateFormat, simple case" <| fun _ ->
      let format = "This {0} contains {1} words."
      let args : obj[] = [|"sentence"; 4|]
      (because "converting a String.Format into a message template" <| fun () ->
        extractFormatFields (Message.templateFormat format args))
      |> should equal ("This {0} contains {1} words.",
                       Set [ "0", Field (String "sentence", None)
                             "1", Field (Int64 4L, None) ])
      |> thatsIt
      
    testCase "Formatting.templateFormat, named and positional fields" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} {0}."
      let args : obj[] = [|"sentence"; 4; "words"|]
      (because "fields are matched left-to-right when any fields are named" <| fun () ->
        extractFormatFields (Message.templateFormat format args))
      |> should equal ("This {gramaticalStructure} contains {wordCount} {0}.",
                       Set [ "gramaticalStructure", Field (String "sentence", None)
                             "wordCount", Field (Int64 4L, None)
                             "0", Field (String "words", None) ])
      |> thatsIt

    testCase "Formatting.templateFormat, positional fields" <| fun _ ->
      let format = "Positionally - two {2} . {2} . zero {0} . {0}"
      let args : obj[] = [|0;1;2;3|] 
      (because "fields are matched positionally when all are numbered" <| fun () ->
        extractFormatFields (Message.templateFormat format args))
      |> should equal ("Positionally - two {2} . {2} . zero {0} . {0}",
                       Set [ ("0", Field (Int64 0L, None))
                             ("2", Field (Int64 2L, None)) ])
      |> thatsIt

    testCase "Formatting.templateFormat, named fields" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} words."
      let args : obj[] = [|"sentence"; 4|]
      (because "fields are matched left-to-right in message template" <| fun () ->
        extractFormatFields (Message.templateFormat format args))
      |> should equal ("This {gramaticalStructure} contains {wordCount} words.",
                       Set [ "gramaticalStructure", Field (String "sentence", None)
                             "wordCount", Field (Int64 4L, None) ])
      |> thatsIt
      

    testCase "Message.Capture can stringify objects (ignoring format) by prepending '$' to the property name" <| fun _ ->
      let format = "This {$stringifiedTimeSpanProperty:ignored} and {$stringifiedCustomObj:ignored} works."
      (because "message templates" <| fun () ->
        Message.Capture (format, TimeSpan.FromDays 1.0, SampleCase3("t", 9))
        |> Set.ofList)
      |> should equal (Set [ "stringifiedTimeSpanProperty", Field (String "1.00:00:00", None)
                             "stringifiedCustomObj",        Field (String "SampleCase3 (\"t\",9)", None) ])
      |> thatsIt


    testCase "Message.Capture can handle a Generic.Dictionary<Tkey,TValue>, which becomes an Object (map) in logary" <| fun _ ->
      let properDict list = System.Collections.Generic.Dictionary<'TKey, 'TValue>(dict list)
      let a99 = { Field1="A"; Field2=Some 99 }
      let format = "This {dictIntString} and {dictStringInt} and {dictGuidGuid} and {dictStringObj} works."
      (because "message templates" <| fun () ->
        Message.Capture (format,  properDict [1, "1"],
                                  properDict ["1", 1],
                                  properDict [Guid.Empty, Guid.Empty],
                                  properDict [ "o1", box (SampleCase1 1)
                                               "o2", box a99 ] // <-- Calls ToString() by default, without '@'
        )
        |> Set.ofList)
      |> should equal (Set [ "dictIntString", Field (Object (Map ["1",  String "1"]), None)
                             "dictStringInt", Field (Object (Map ["1",  Int64   1L]), None)
                             "dictGuidGuid",  Field (Object (Map ["00000000-0000-0000-0000-000000000000", String "00000000-0000-0000-0000-000000000000"]), None)
                             "dictStringObj", Field (Object (Map ["o1", String "SampleCase1 1"
                                                                  "o2", String "Logary.Tests.Formatting+SampleRecord" ]), None) ])
      |> thatsIt

    testCase "Message.Capture can extract the structure of objects by prepending '@' to the property name" <| fun _ ->
      let format = "This {@theVersion:#ignored} and {@theDuCase:#ignored} and {$theDuCaseStringified} works too."
      let expectedVersionProperties = Map [ "Build", Int64 0L
                                            "Major", Int64 1L
                                            "MajorRevision", Int64 0L
                                            "Minor", Int64 0L
                                            "MinorRevision", Int64 0L
                                            "Revision", Int64 0L
                                            "_typeTag", String "Version" ]

      let expectedDuSampleCase3Properties = Map [ "IsSampleCase1", Bool false
                                                  "IsSampleCase2", Bool false
                                                  "IsSampleCase3", Bool true
                                                  "sc3String", String "A"
                                                  "sc3Int", Int64 1L
                                                  "Tag", Int64 2L
                                                  "_typeTag", String "SampleCase3" ]

      (because "message templates" <| fun () ->
        Message.Capture (format, Version.Parse "1.0.0.0", SampleCase3("A", 1), SampleCase2)
        |> Set.ofList)
      |> should equal (Set [ "theVersion",            Field (Object expectedVersionProperties, None)
                             "theDuCase",             Field (Object expectedDuSampleCase3Properties, None)
                             "theDuCaseStringified",  Field (String "SampleCase2", None) ])
      |> thatsIt

    testCase "Formatting.templateFormat, named fields, missing last" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} words."
      let args : obj[] = [|"sentence"|]
      (because "fields are matched left-to-right in message template" <| fun () ->
        extractFormatFields (Message.templateFormat format args))
      |> should equal ("This {gramaticalStructure} contains {wordCount} words.",
                       Set [ "gramaticalStructure", Field (String "sentence", None) ])
      |> thatsIt

    testCase "Formatting.templateFormat, named fields, all missing" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} words."
      let args : obj[] = [||]
      (because "fields are matched left-to-right in message template" <| fun () ->
        extractFormatFields (Message.templateFormat format args))
      |> should equal ("This {gramaticalStructure} contains {wordCount} words.",
                       Set [ ])
      |> thatsIt

    ]
