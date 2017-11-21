module Logary.Tests.Utils

open Hopac
open Logary
open Logary.Targets
open Logary.Configuration
open System.IO

let buildTextWriteTarget name =
  let (out, error) = (new StringWriter (), new StringWriter ())
  let twconf = TextWriter.TextWriterConf.create (out, error)
  let twTargetConf = TextWriter.create twconf name
  (out, error, twTargetConf)

let buildLogManager () = job {
  let svc = "svc"
  let host = "localhost"
  let tname = "4test"
  let (out, error, twTargetConf) = buildTextWriteTarget tname
  // let iloggerConf = ILogger.Targets [ twTargetConf ]
  let processing =
    Events.stream
    |> Events.subscribers [
      Events.events |> Events.sink [tname]
    ]
    |> Events.toProcessing


  let! registry =
    Config.create svc host
    // |> Config.ilogger iloggerConf
    // |> Config.ilogger (ILogger.Console Verbose)
    |> Config.target twTargetConf
    |> Config.processing processing
    // |> Config.disableGlobals
    |> Config.build
  let logm = Registry.toLogManager registry
  return (registry, logm, out, error)
}