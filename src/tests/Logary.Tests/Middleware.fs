﻿module Logary.Tests.Middleware

open Expecto
open Logary
open Hopac

type LoggerNext =
  Message -> Alt<Promise<Message>>

[<Tests>]
let middleware =
  testList "middleware" [
    let now = Message.setUTCTicks System.DateTime.UtcNow.Ticks

    let setUser next msg =
      msg
      |> Message.setContext "user" "haf12345678"
      |> next

    let setService next msg =
      msg
      |> Message.setContext "service" "app-service 123.1245"
      |> next

    let composed = Middleware.compose [ setUser; setService ]

    yield testCase "single" <| fun _ ->
      let msg = Middleware.identity id (Message.event Info "User logged in" |> now)
      Expect.equal msg (Message.eventInfo "User logged in" |> now) "identity"

    yield testCase "host" <| fun _ ->
      let msg = Message.event Debug "Hi!"
      let msg' = Middleware.host id msg
      Expect.isSome (msg'.context.TryFind "host") "Should have a host name"

    yield testCase "compose ordering" <| fun _ ->
      let calls = ref []
      let m1 next msg = calls := !calls @ ["m1"] ; next msg
      let m2 next msg = calls := !calls @ ["m2"] ; next msg
      let composed = Middleware.compose [ m1; m2 ]
      let res = composed (Message.eventInfo "compose" |> now)
      Expect.equal res (Message.eventInfo "compose" |> now) "should pass through"
      Expect.equal !calls [ "m1"; "m2" ] "calls in order"

    yield testCase "deep enriching" (* sounds good, doens't it? *) <| fun _ ->
      let msg = Message.eventInfo "App started"
      Expect.equal msg.context Map.empty "no context"

      let expected =
        { msg with
           context =
             Map.ofList [
               "user", String "haf12345678"
               "service", String "app-service 123.1245"
             ] }

      Expect.equal (composed msg) expected "adds to context like it should"

    yield testCase "compose twice" <| fun _ ->
      let msg = Message.eventInfo "App unload"
      let composed' = setService composed

      let expected =
        { msg with
           context =
             Map.ofList [
               "user", String "haf12345678"
               "service", String "app-service 123.1245"
             ] }

      Expect.equal (composed' msg) expected "adds to context like it should"

    yield testCase "compose three" <| fun _ ->
      let mids = [ Middleware.host; Middleware.service "abc"; Middleware.processName ]
      let composed = Middleware.compose mids
      let msg' = composed (Message.event Debug "Ran")
      Expect.isSome (msg'.context.TryFind "host") "has host"
      Expect.isSome (msg'.context.TryFind "processName") "has processName"
      Expect.isSome (msg'.context.TryFind "service") "has service"

  ]
