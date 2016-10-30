/// The logging namespace, which contains the logging abstraction for this
/// library. See https://github.com/logary/logary for details. This module is
/// completely stand-alone in that it has no external references and its adapter
/// in Logary has been well tested.
namespace Logary.Facade

open System
open System.Runtime.CompilerServices

/// The log level denotes how 'important' the gauge or event message is.
[<CustomEquality; CustomComparison>]
type LogLevel =
  /// The log message is not that important; can be used for intricate debugging.
  | Verbose
  /// The log message is at a default level, debug level. Useful for shipping to
  /// infrastructure that further processes it, but not so useful for human
  /// inspection in its raw format, except during development.
  | Debug
  /// The log message is informational; e.g. the service started, stopped or
  /// some important business event occurred.
  | Info
  /// The log message is a warning; e.g. there was an unhandled exception or
  /// an even occurred which was unexpected. Sometimes human corrective action
  /// is needed.
  | Warn
  /// The log message is at an error level, meaning an unhandled exception
  /// occurred at a location where it is deemed important to keeping the service
  /// running. A human should take corrective action.
  | Error
  /// The log message denotes a fatal error which cannot be recovered from. The
  /// service should be shut down. Human corrective action is needed.
  | Fatal

  /// Converts the LogLevel to a string
  override x.ToString () =
    match x with
    | Verbose -> "verbose"
    | Debug   -> "debug"
    | Info    -> "info"
    | Warn    -> "warn"
    | Error   -> "error"
    | Fatal   -> "fatal"

  /// Converts the string passed to a Loglevel.
  static member ofString (str : string) =
    if str = null then invalidArg "str" "may not be null"
    match str.ToLowerInvariant() with
    | "verbose" -> Verbose
    | "debug"   -> Debug
    | "info"    -> Info
    | "warn"    -> Warn
    | "error"   -> Error
    | "fatal"   -> Fatal
    | _         -> Info

  /// Turn the LogLevel into an integer
  member x.toInt () =
    (function
    | Verbose -> 1
    | Debug   -> 2
    | Info    -> 3
    | Warn    -> 4
    | Error   -> 5
    | Fatal   -> 6) x

  /// Turn an integer into a LogLevel
  static member ofInt i =
    (function
    | 1 -> Verbose
    | 2 -> Debug
    | 3 -> Info
    | 4 -> Warn
    | 5 -> Error
    | 6 -> Fatal
    | _ as i -> failwithf "LogLevel matching integer %i is not available" i) i

  interface IComparable<LogLevel> with
    member x.CompareTo other =
      compare (x.toInt()) (other.toInt())

  static member op_LessThan (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) < 0
  static member op_LessThanOrEqual (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) <= 0
  static member op_GreaterThan (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) > 0
  static member op_GreaterThanOrEqual (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) >= 0

  override x.GetHashCode () =
    x.toInt ()

  interface IComparable with
    member x.CompareTo other =
      match other with
      | null ->
        1
      | :? LogLevel as tother ->
        (x :> IComparable<LogLevel>).CompareTo tother
      | _ ->
        failwithf "invalid comparison %A to %A" x other

  interface IEquatable<LogLevel> with
    member x.Equals other =
      x.toInt() = other.toInt()

  override x.Equals other =
    (x :> IComparable).CompareTo other = 0

/// Represents a logged value; either a Gauge or an Event.
type PointValue =
  /// An event is what it sounds like; something occurred and needs to be
  /// logged. Its field is named 'template' because it should not be interpolated
  /// with values; instead these values should be put in the 'fields' field of
  /// the Message.
  | Event of template:string
  /// This is as value for a metric, with a unit attached. The unit can be
  /// something like Seconds or Hz.
  | Gauge of value:int64 * units:string

/// The # of nanoseconds after 1970-01-01 00:00:00.
type EpochNanoSeconds = int64

/// Helper functions for transforming DateTime to timestamps in unix epoch.
module DateTime =

  /// Get the Logary timestamp off the DateTime.
  let timestamp (dt : DateTime) : EpochNanoSeconds =
    (dt.Ticks - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks)
    * 100L

  /// Get the DateTimeOffset ticks off from the EpochNanoSeconds.
  let ticksUTC (epoch : EpochNanoSeconds) : int64 =
    epoch / 100L
    + DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks

/// Helper functions for transforming DateTimeOffset to timestamps in unix epoch.
module DateTimeOffset =

  /// Get the Logary timestamp off the DateTimeOffset.
  let timestamp (dt : DateTimeOffset) : EpochNanoSeconds =
    (dt.Ticks - DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks)
    * 100L

  /// Get the DateTimeOffset ticks from EpochNanoSeconds
  let ticksUTC (epoch : EpochNanoSeconds) : int64 =
    epoch / 100L
    + DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks

/// This is record that is logged. It's capable of representing both metrics
/// (gauges) and events. See https://github.com/logary/logary for details.
type Message =
  { /// The 'path' or 'name' of this data point. Do not confuse template in
    /// (Event template) = message.value
    name      : string[]
    /// The main value for this metric or event. Either a Gauge or an Event. (A
    /// discriminated union type)
    value     : PointValue
    /// The semantic-logging data.
    fields    : Map<string, obj>
    /// When? nanoseconds since UNIX epoch.
    timestamp : EpochNanoSeconds
    /// How important? See the docs on the LogLevel type for details.
    level     : LogLevel }

  /// Gets the ticks for UTC since 0001-01-01 00:00:00 for this message. You
  /// can pass this value into a DateTimeOffset c'tor
  member x.utcTicks =
    DateTimeOffset.ticksUTC x.timestamp

  /// If you're looking for how to transform the Message's fields, then use the
  /// module methods rather than instance methods, since you'll be creating new
  /// values rather than changing an existing value.
  member x.README =
    ()

/// The logger is the interface for calling code to use for logging.
type Logger =
  /// Evaluates the callback if the log level is enabled. Returns an async that
  /// itself completes when the logging infrastructure has finished writing that
  /// Message. Completes directly if nothing is logged. What the ack means from
  /// a durability standpoint depends on the logging infrastructure you're using
  /// behind this facade. Will not block, besides doing the computation inside
  /// the callback. You should not do blocking operations in the callback.
  abstract member logWithAck : LogLevel -> (LogLevel -> Message) -> Async<unit>

  /// Evaluates the callback if the log level is enabled. Will not block,
  /// besides doing the computation inside the callback. You should not do
  /// blocking operations in the callback.
  abstract member log : LogLevel -> (LogLevel -> Message) -> unit

  /// Logs the message without awaiting the logging infrastructure's ack of
  /// having successfully written the log message. What the ack means from a
  /// durability standpoint depends on the logging infrastructure you're using
  /// behind this facade.
  abstract member logSimple : Message -> unit

/// Syntactic sugar on top of Logger for F# libraries.
[<AutoOpen>]
module internal LoggerEx =
  type Logger with
    member x.verbose (msgFactory : LogLevel -> Message) : unit =
      x.log Verbose msgFactory

    member x.debug (msgFactory : LogLevel -> Message) : unit =
      x.log Debug msgFactory

    member x.info msgFactory : unit =
      x.log Info msgFactory

    member x.warn msgFactory : unit =
      x.log Warn msgFactory

    member x.error msgFactory : unit =
      x.log Error msgFactory

    member x.fatal msgFactory : unit =
      x.log Fatal msgFactory

type LoggingConfig =
  { /// The `timestamp` function should preferably be monotonic and not 'jumpy'
    /// or take much time to call.
    timestamp        : unit -> int64
    /// The `getLogger` function returns a logger that directly can be logged to.
    getLogger        : string[] -> Logger
    /// When composing apps from the outside-in (rather than having a unified
    /// framework with static/global config) with libraries (again, rather than
    /// a unified framework) like is best-practice, there's not necessarily a
    /// way to coordinate around the STDOUT and STDERR streams between
    /// different libraries running things on different threads. Use Logary's
    /// adapter to replace this semaphore with a global semaphore.
    consoleSemaphore : obj }

module Literate =
  /// The output tokens, which can be potentially coloured.
  type LiterateToken =
    | Text | Subtext
    | Punctuation
    | LevelVerbose | LevelDebug | LevelInfo | LevelWarning | LevelError | LevelFatal
    | KeywordSymbol | NumericSymbol | StringSymbol | OtherSymbol | NameSymbol
    | MissingTemplateField

  type LiterateOptions =
    { formatProvider          : IFormatProvider
      theme                   : LiterateToken -> ConsoleColor
      getLogLevelText         : LogLevel -> string
      printTemplateFieldNames : bool }

    static member create ?formatProvider =
      // note: literate is meant for human consumption, and so the default
      // format provider of 'Current' is appropriate here. The reader expects
      // to see the dates, numbers, currency, etc formatted in the local culture
      { formatProvider = defaultArg formatProvider Globalization.CultureInfo.CurrentCulture
        getLogLevelText = function
                | Debug ->    "DBG"
                | Error ->    "ERR"
                | Fatal ->    "FTL"
                | Info ->     "INF"
                | Verbose ->  "VRB"
                | Warn ->     "WRN"
        theme = function
                | Text -> ConsoleColor.White
                | Subtext -> ConsoleColor.Gray
                | Punctuation -> ConsoleColor.DarkGray
                | LevelVerbose -> ConsoleColor.Gray
                | LevelDebug -> ConsoleColor.Gray
                | LevelInfo -> ConsoleColor.White
                | LevelWarning -> ConsoleColor.Yellow
                | LevelError -> ConsoleColor.Red
                | LevelFatal -> ConsoleColor.Red
                | KeywordSymbol -> ConsoleColor.Blue
                | NumericSymbol -> ConsoleColor.Magenta
                | StringSymbol -> ConsoleColor.Cyan
                | OtherSymbol -> ConsoleColor.Green
                | NameSymbol -> ConsoleColor.Gray
                | MissingTemplateField -> ConsoleColor.Red
        printTemplateFieldNames = false }

    static member createInvariant() =
      LiterateOptions.create Globalization.CultureInfo.InvariantCulture

/// Module that contains the 'known' keys of the Maps in the Message type's
/// fields/runtime data.
module Literals =

  [<Literal>]
  let FieldExnKey = "exn"

  [<Literal>]
  let FieldErrorsKey = "errors"

module internal FsMtParser =
  open System.Text

  type CaptureHint =
    | Default = 0
    | Structure = 1
    | Stringify = 2

  type AlignDirection = Right = 0 | Left = 1

  [<Struct>]
  type AlignInfo =
      new (direction : AlignDirection, width : int) = { _direction=direction; _width=width; }
      new (isValid : bool) = { _direction = AlignDirection.Right; _width = (if isValid then -1 else -2) }
      val private _direction : AlignDirection
      val private _width : int
      member this.direction with get() = this._direction
      member this.width with get() = this._width
      member this.isEmpty = this.width = -1
      member internal this.isValid = this.width <> -2
      static member empty = AlignInfo(isValid=true)
      static member invalid = AlignInfo(isValid=false)

  /// Represents a property; a placeholder in a message template which can be parsed
  /// and later replaced 
  type Property(name : string, format : string, captureHint : CaptureHint, align : AlignInfo) =
    static let emptyInstance = Property("", null, CaptureHint.Default, AlignInfo.empty)
    static member empty = emptyInstance
    member x.name = name
    member x.format = format
    member x.captureHint = captureHint
    member x.align = align
    /// Appends this property the string builder.
    member internal x.AppendPropertyString(sb : StringBuilder, ?replacementName, ?appendCaptureHint) =
      let appendCaptureHint = defaultArg appendCaptureHint true
      sb.Append("{") |> ignore

      if appendCaptureHint then
        match x.captureHint with
        | CaptureHint.Structure -> sb.Append "@" |> ignore
        | CaptureHint.Stringify -> sb.Append "$" |> ignore
        | _ -> ()

      sb.Append(defaultArg replacementName name)
        .Append(match x.format with null | "" -> "" | _ -> ":" + x.format) |> ignore

      if not (x.align.isEmpty) then
        sb.Append(if x.align.direction = AlignDirection.Right then ",-" else ",")
          .Append(string x.align.width)
          |> ignore

      sb.Append("}")
    override x.ToString() = x.AppendPropertyString(StringBuilder()).ToString()

  module internal ParserBits =
    let inline isNull o = match o with | null -> true | _ -> false
    let inline isLetterOrDigit c = System.Char.IsLetterOrDigit c
    let inline isValidInPropName c = c = '_' || System.Char.IsLetterOrDigit c
    let inline isValidInAlignment c = c = '-' || System.Char.IsDigit c
    let inline isValidInCaptureHint c = c = '@' || c = '$'
    let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
    let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c || isValidInCaptureHint c

    [<Struct>]
    type Range(startIndex : int, endIndex : int) =
      member inline x.start = startIndex
      member inline x.``end`` = endIndex
      member inline x.length = (endIndex - startIndex) + 1
      member inline x.getSubstring (s : string) = s.Substring(startIndex, x.length)
      member inline x.isEmpty = startIndex = -1 && endIndex = -1
      override x.ToString() = sprintf "(start=%i, end=%i)" x.start x.``end``
      static member inline substring (s : string, startIndex, endIndex) = s.Substring(startIndex, (endIndex - startIndex) + 1)
      static member inline empty = Range(-1, -1)

    let inline tryGetFirstCharInRange predicate (s : string) (range : Range) =
      let rec go i =
        if i > range.``end`` then -1
        else if not (predicate s.[i]) then go (i+1) else i
      go range.start

    let inline tryGetFirstChar predicate (s : string) first =
      tryGetFirstCharInRange predicate s (Range(first, s.Length - 1))

    let inline hasAnyInRange predicate (s : string) (range : Range) =
      match tryGetFirstChar (predicate) s range.start with
      | -1 ->
        false
      | i ->
        i <= range.``end``

    let inline hasAny predicate (s : string) = hasAnyInRange predicate s (Range(0, s.Length - 1))
    let inline indexOfInRange s range c = tryGetFirstCharInRange ((=) c) s range
  
    /// Attemps to parse an integer from the range within a string. Returns invalidValue if the
    /// string does not contain an integer. The '-' is allowed only as the first character.
    let inline tryParseIntFromRng (invalidValue : int) (s : string) (range : Range) =
      if range.length = 1 && '0' <= s.[0] && s.[0] <= '9' then
        int (s.[range.start]) - 48
      else
        let indexOfLastCharPlus1 = range.``end``+1
        let rec inside isNeg numSoFar i =
          if i = indexOfLastCharPlus1 then
            if isNeg then -numSoFar else numSoFar
          else
            let c = s.[i]
            if c = '-' then
              if i = range.start then inside true (numSoFar) (i+1)
              else invalidValue // no '-' character allowed other than first char
            elif '0' <= c && c <= '9' then
              inside isNeg (10*numSoFar + int c - 48) (i+1)
            else invalidValue

        inside false 0 range.start

    /// Will parse ",-10"   as AlignInfo(direction=Left, width=10).
    /// Will parse ",5"     as AlignInfo(direction=Right, width=5).
    /// Will parse ",0"     as AlignInfo(isValid=false) because 0 is not a valid alignment.
    /// Will parse ",-0"    as AlignInfo(isValid=false) because 0 is not a valid alignment.
    /// Will parse ",,5"    as AlignInfo(isValid=false) because ',5' is not an int.
    /// Will parse ",5-"    as AlignInfo(isValid=false) because '-' is in the wrong place.
    /// Will parse ",asdf"  as AlignInfo(isValid=false) because 'asdf' is not an int.
    let inline tryParseAlignInfoRng (s:string) (rng:Range) =
      match s, rng with
      | s, rng when (rng.start > rng.``end``) || (hasAnyInRange (not << isValidInAlignment) s rng) ->
        AlignInfo.invalid

      | s, rng ->
        let width =
          match tryParseIntFromRng (System.Int32.MinValue) s rng with
          | System.Int32.MinValue -> 0 // not a valid align number (e.g. dash in wrong spot)
          | n -> n

        if width = 0 then AlignInfo.invalid
        else
          let isNegativeAlignWidth = width < 0
          let direction = if isNegativeAlignWidth then AlignDirection.Left else AlignDirection.Right
          AlignInfo(direction, abs(width))

    /// Attempts to validate and parse a property token within the specified range. If the property
    /// insides contains any invalid characters, then the `Property.empty' instance is returned.
    let inline tryGetPropInRange (template : string) (within : Range) : Property =
      let nameRange, alignRange, formatRange =
        match indexOfInRange template within ',', indexOfInRange template within ':' with
        | -1, -1 ->
          // neither align nor format
          within, Range.empty, Range.empty

        | -1, fmtIdx ->
          // has format part, but does not have align part
          Range(within.start, fmtIdx-1), Range.empty, Range(fmtIdx+1, within.``end``)

        | alIdx, -1 ->
          // has align part, but does not have format part
          Range(within.start, alIdx-1), Range(alIdx+1, within.``end``), Range.empty

        | alIdx, fmtIdx when alIdx < fmtIdx && alIdx <> (fmtIdx-1) ->
          // has both align and format parts, in the correct order
          let align = Range(alIdx+1, fmtIdx-1)
          let fmt = Range(fmtIdx+1, within.``end``)
          Range(within.start, alIdx-1), align, fmt

        | alIdx, fmtIdx when alIdx > fmtIdx ->
          // has format part, no align (but one or more commas *inside* the format string)
          Range(within.start, fmtIdx-1), Range.empty, Range(fmtIdx+1, within.``end``)

        | _, _ ->
          Range.empty, Range.empty, Range.empty

      if nameRange.isEmpty then
        Property.empty // property name is empty
      else
        let propertyNameStartIndex, captureHint =
          match template.[nameRange.start] with
          | '@' -> nameRange.start + 1, CaptureHint.Structure
          | '$' -> nameRange.start + 1, CaptureHint.Stringify
          | _ -> nameRange.start, CaptureHint.Default

        let propertyName = Range.substring (template, propertyNameStartIndex, nameRange.``end``)
        if propertyName = "" || (hasAny (not<<isValidInPropName) propertyName) then
          // property name has invalid characters
          Property.empty
        else
          if (not formatRange.isEmpty) && (hasAnyInRange (not<<isValidInFormat) template formatRange) then
            Property.empty // format range has invalid characters
          else
            match alignRange.isEmpty, formatRange.isEmpty with
            | true, true -> Property(propertyName, null, captureHint, AlignInfo.empty)
            | true, false -> Property(propertyName, formatRange.getSubstring template, captureHint, AlignInfo.empty)
            | false, _ ->
              let formatString = if formatRange.isEmpty then null else formatRange.getSubstring template
              match tryParseAlignInfoRng template alignRange with
              | ai when ai.isValid -> Property(propertyName, formatString, captureHint, ai)
              | _ -> Property.empty // align has invalid characters

    let inline findNextNonPropText (startAt : int) (template : string) (foundText : string->unit) : int =
      // Finds the next text token (starting from the 'startAt' index) and returns the next character
      // index within the template string. If the end of the template string is reached, or the start
      // of a property token is found (i.e. a single { character), then the 'consumed' text is passed
      // to the 'foundText' method, and index of the next character is returned.
      let mutable escapedBuilder = Unchecked.defaultof<StringBuilder> // don't create one until it's needed
      let inline append (ch : char) = if not (isNull escapedBuilder) then escapedBuilder.Append(ch) |> ignore
      let inline createStringBuilderAndPopulate i =
        if isNull escapedBuilder then
          escapedBuilder <- StringBuilder() // found escaped open-brace, take the slow path
          for chIndex = startAt to i-1 do append template.[chIndex] // append all existing chars
      let rec go i =
        if i >= template.Length then
          template.Length // bail out at the end of the string
        else
          let ch = template.[i]
          match ch with
          | '{' ->
            if (i+1) < template.Length && template.[i+1] = '{' then
              createStringBuilderAndPopulate i
              append ch; go (i+2)
            else i // found an open brace (potentially a property), so bail out
          | '}' when (i+1) < template.Length && template.[i+1] = '}' ->
            createStringBuilderAndPopulate i
            append ch; go (i+2)
          | _ ->
            append ch; go (i+1)

      let nextIndex = go startAt
      if (nextIndex > startAt) then // if we 'consumed' any characters, signal that we 'foundText'
        if isNull escapedBuilder then
          foundText (Range.substring(template, startAt, nextIndex - 1))
        else
          foundText (escapedBuilder.ToString())
      nextIndex

    let findPropOrText (start : int) (template : string)
                        (foundText : string -> unit)
                        (foundProp : Property -> unit) : int =
      // Attempts to find the indices of the next property in the template
      // string (starting from the 'start' index). Once the start and end of
      // the property token is known, it will be further validated (by the
      // tryGetPropInRange method). If the range turns out to be invalid, it's
      // not a property token, and we return it as text instead. We also need
      // to handle some special case here: if the end of the string is reached,
      // without finding the close brace (we just signal 'foundText' in that case).
      let nextInvalidCharIndex =
        match tryGetFirstChar (not << isValidCharInPropTag) template (start+1) with
        | -1 ->
          template.Length
        | idx ->
          idx

      if nextInvalidCharIndex = template.Length || template.[nextInvalidCharIndex] <> '}' then
        foundText (Range.substring(template, start, (nextInvalidCharIndex - 1)))
        nextInvalidCharIndex
      else
        let nextIndex = nextInvalidCharIndex + 1
        let propInsidesRng = Range(start + 1, nextIndex - 2)
        match tryGetPropInRange template propInsidesRng with
        | prop when not (obj.ReferenceEquals(prop, Property.empty)) ->
          foundProp prop
        | _ ->
          foundText (Range.substring(template, start, (nextIndex - 1)))
        nextIndex

  /// Parses template strings such as "Hello, {@PropertyWithFormat:##.##}"
  /// and calls the 'foundTextF' or 'foundPropF' functions as the text or
  /// property tokens are encountered.
  let parseParts (template : string) foundTextF foundPropF =
    let tlen = template.Length
    let rec go start =
      if start >= tlen then ()
      else match ParserBits.findNextNonPropText start template foundTextF with
            | next when next <> start ->
              go next
            | _ ->
              go (ParserBits.findPropOrText start template foundTextF foundPropF)
    go 0

/// Internal module for formatting text for printing to the console.
module internal Formatting =
  open System.Text
  open Literals
  open Literate

  let rec appendTokenParts options
                           (prop: FsMtParser.Property)
                           propValue
                           (tokenParts : ResizeArray<string * LiterateToken>) =

    let propValueToFormattedString () =
      // render using string.Format, so the formatting is applied
      let stringFormatTemplate =
        prop.AppendPropertyString(StringBuilder(),
                                  replacementName="0",
                                  appendCaptureHint=false).ToString()

      String.Format (options.formatProvider, stringFormatTemplate, [| propValue |])

    match propValue with
    | :? bool ->
      tokenParts.Add (propValueToFormattedString(), KeywordSymbol)
    | :? int16 | :? int32 | :? int64 | :? decimal | :? float | :? double ->
      tokenParts.Add (propValueToFormattedString(), NumericSymbol)
    | :? string | :? char ->
      tokenParts.Add (propValueToFormattedString(), StringSymbol)
    | :? System.Collections.Generic.IEnumerable<_> ->
      tokenParts.Add ("[", Punctuation)
      tokenParts.Add ("TODO", Text)
      tokenParts.Add ("]", Punctuation)
    | :? System.Collections.Generic.IDictionary<_, _> -> 
      tokenParts.Add ("[", Punctuation)
      tokenParts.Add ("TODO", Text)
      tokenParts.Add ("]", Punctuation)
    | _ ->
      tokenParts.Add (propValueToFormattedString(), OtherSymbol)

  let literateFormatValue (options : LiterateOptions) (fields : Map<string, obj>) = function
    | Event template ->
      let tokenParts = ResizeArray<string * LiterateToken>()
      let matchedFields = ResizeArray<string>()
      let foundText (text: string) = tokenParts.Add (text, Text)
      let foundProp (prop: FsMtParser.Property) =
        match Map.tryFind prop.name fields with
        | Some propValue ->
          appendTokenParts options prop propValue tokenParts
          matchedFields.Add prop.name
        | None ->
          tokenParts.Add (prop.ToString(), MissingTemplateField)

      FsMtParser.parseParts template foundText foundProp
      Set.ofSeq matchedFields, List.ofSeq tokenParts

    | Gauge (value, units) ->
      Set.empty, [ sprintf "%i" value, NumericSymbol
                   sprintf "%s" units, KeywordSymbol ]

  let formatValue (fields : Map<string, obj>) (pv : PointValue) =
    let matchedFields, themedParts =
      literateFormatValue (LiterateOptions.createInvariant()) fields pv
    matchedFields, System.String.Concat(themedParts |> List.map fst)

  let literateExceptionColouriser (options : LiterateOptions) (ex : exn) =
    let stackFrameLinePrefix = "   at" // 3 spaces
    let monoStackFrameLinePrefix = "  at" // 2 spaces
    use exnLines = new System.IO.StringReader(ex.ToString())
    let rec go lines =
      match exnLines.ReadLine() with
      | null ->
        List.rev lines // finished reading
      | line ->
        if line.StartsWith(stackFrameLinePrefix) || line.StartsWith(monoStackFrameLinePrefix) then
          // subtext
          go ((Environment.NewLine, Text) :: ((line, Subtext) :: lines))
        else
          // regular text
          go ((Environment.NewLine, Text) :: ((line, Text) :: lines))
    go []

  let literateColouriseExceptions (context : LiterateOptions) message =
    let exnExceptionParts =
      match message.fields.TryFind FieldExnKey with
      | Some (:? Exception as ex) ->
        literateExceptionColouriser context ex
        @ [ Environment.NewLine, Text ]
      | _ ->
        [] // there is no spoon
    let errorsExceptionParts =
      match message.fields.TryFind FieldErrorsKey with
      | Some (:? List<obj> as exnListAsObjList) ->
        exnListAsObjList |> List.collect (function
          | :? exn as ex ->
            literateExceptionColouriser context ex
            @ [ Environment.NewLine, Text ]
          | _ ->
            [])
      | _ ->
        []

    exnExceptionParts @ errorsExceptionParts

  /// Split a structured message up into theme-able parts (tokens), allowing the
  /// final output to display to a user with colours to enhance readability.
  let literateDefaultTokeniser (options : LiterateOptions) (message : Message) : (string * LiterateToken) list =
    let formatLocalTime (utcTicks : int64) =
      DateTimeOffset(utcTicks, TimeSpan.Zero).LocalDateTime.ToString("HH:mm:ss", options.formatProvider),
      Subtext

    let themedMessageParts =
      message.value |> literateFormatValue options message.fields |> snd

    let themedExceptionParts =
      let exnParts = literateColouriseExceptions options message
      if not exnParts.IsEmpty then
        [ Environment.NewLine, Text ]
        @ exnParts
        @ [ Environment.NewLine, Text ]
      else []

    let getLogLevelToken = function
      | Verbose -> LevelVerbose
      | Debug -> LevelDebug
      | Info -> LevelInfo
      | Warn -> LevelWarning
      | Error -> LevelError
      | Fatal -> LevelFatal

    [ "[", Punctuation
      formatLocalTime message.utcTicks
      " ", Subtext
      options.getLogLevelText message.level, getLogLevelToken message.level
      "] ", Punctuation ]
    @ themedMessageParts
    @ themedExceptionParts

  let literateDefaultColourWriter sem (parts : (string * ConsoleColor) list) =
    lock sem <| fun _ ->
      let originalColour = Console.ForegroundColor
      let mutable currentColour = originalColour
      parts |> List.iter (fun (text, colour) ->
        if currentColour <> colour then
          Console.ForegroundColor <- colour
          currentColour <- colour
        Console.Write(text)
      )
      if currentColour <> originalColour then
        Console.ForegroundColor <- originalColour

  /// let the ISO8601 love flow
  let defaultFormatter (message : Message) =
    let app (x : obj) (sb : StringBuilder) =
      sb.Append x |> ignore

    let formatLevel (level : LogLevel) =
      "[" + Char.ToUpperInvariant(level.ToString().[0]).ToString() + "] "

    let formatInstant (utcTicks : int64) =
      (DateTimeOffset(utcTicks, TimeSpan.Zero).ToString("o")) + ": "

    let formatName (name : string[]) =
      " [" + String.concat "." name + "]"

    let formatExn (fields : Map<string, obj>) =
      match fields |> Map.tryFind FieldExnKey with
      | None ->
        String.Empty
      | Some ex ->
        " exn:\n" + ex.ToString()

    let formatFields (ignored : Set<string>) (fields : Map<string, obj>) =
      if not (Map.isEmpty fields) then
        fields
        |> Seq.filter (fun (KeyValue (k, _)) ->
          not (ignored |> Set.contains k))
        |> Seq.map (fun (KeyValue (k, v)) ->
          sprintf "\n - %s: %O" k v)
        |> String.concat ""
      else
        ""

    let matchedFields, valueString =
      formatValue message.fields message.value

    // [I] 2014-04-05T12:34:56Z: Hello World! [my.sample.app]
    formatLevel message.level +
    formatInstant message.utcTicks +
    valueString +
    formatName message.name +
    formatExn message.fields +
    formatFields matchedFields message.fields

/// Logs a line in a format that is great for human consumption,
/// using console colours to enhance readability.
/// Sample: [10:30:49 INF] User "AdamC" began the "checkout" process with 100 cart items
type LiterateConsoleTarget(minLevel, ?options, ?literateTokeniser, ?outputWriter, ?consoleSemaphore) =
  let sem          = defaultArg consoleSemaphore (obj())
  let options      = defaultArg options (Literate.LiterateOptions.create())
  let tokenise     = defaultArg literateTokeniser Formatting.literateDefaultTokeniser
  let colourWriter = defaultArg outputWriter Formatting.literateDefaultColourWriter sem

  let colouriseThenNewLine message =
    (tokenise options message) @ [Environment.NewLine, Literate.Text]
    |> List.map (fun (s, t) ->
      s, options.theme(t))

  interface Logger with
    member x.logWithAck level msgFactory =
      if level >= minLevel then
        colourWriter (colouriseThenNewLine (msgFactory level))
      async.Return ()
    member x.log level msgFactory =
      if level >= minLevel then
        colourWriter (colouriseThenNewLine (msgFactory level))
    member x.logSimple msg =
      if msg.level >= minLevel then
        colourWriter (colouriseThenNewLine msg)

type TextWriterTarget(minLevel, writer : System.IO.TextWriter, ?formatter) =
  let formatter = defaultArg formatter Formatting.defaultFormatter
  let log msg = writer.WriteLine(formatter msg)

  interface Logger with
    member x.log level msgFactory =
      if level >= minLevel then log (msgFactory level)

    member x.logWithAck level msgFactory =
      if level >= minLevel then log (msgFactory level)
      async.Return ()

    member x.logSimple msg =
      if msg.level >= minLevel then log msg

type OutputWindowTarget(minLevel, ?formatter) =
  let formatter = defaultArg formatter Formatting.defaultFormatter
  let log msg = System.Diagnostics.Debug.WriteLine(formatter msg)

  interface Logger with
    member x.log level msgFactory =
      if level >= minLevel then log (msgFactory level)

    member x.logWithAck level msgFactory =
      if level >= minLevel then log (msgFactory level)
      async.Return ()

    member x.logSimple msg =
      if msg.level >= minLevel then log msg

/// A logger to use for combining a number of other loggers
type CombiningTarget(otherLoggers : Logger list) =
  let sendToAll level msgFactory =
    otherLoggers
    |> List.map (fun l ->
      l.logWithAck level msgFactory)
    |> Async.Parallel
    |> Async.Ignore // Async<unit>

  interface Logger with
    member x.logWithAck level msgFactory =
      sendToAll level msgFactory

    member x.log level msgFactory =
      for logger in otherLoggers do
        logger.log level msgFactory

    member x.logSimple msg =
      sendToAll msg.level (fun _ -> msg)
      |> Async.Start

module Global =
  /// This is the global semaphore for colourising the console output. Ensure
  /// that the same semaphore is used across libraries by using the Logary
  /// Facade Adapter in the final composing app/service.
  let internal consoleSemaphore = obj ()

  /// The global default configuration, which logs to Console at Info level.
  let DefaultConfig =
    { timestamp        = fun () -> DateTimeOffset.timestamp DateTimeOffset.UtcNow
      getLogger        = fun _ -> LiterateConsoleTarget(Info) :> Logger
      consoleSemaphore = consoleSemaphore }

  let private config =
    ref (DefaultConfig, (* logical clock *) 1u)

  /// The flyweight just references the current configuration. If you want
  /// multiple per-process logging setups, then don't use the static methods,
  /// but instead pass a Logger instance around, setting the name field of the
  /// Message value you pass into the logger.
  type internal Flyweight(name : string[]) =
    let updating = obj()
    let mutable fwClock : uint32 = snd !config
    let mutable logger : Logger = (fst !config).getLogger name
    let rec withLogger action =
      let cfg, cfgClock = !config // copy to local
      let fwCurr = fwClock // copy to local
      if cfgClock <> fwCurr then
        lock updating <| fun _ ->
          logger <- cfg.getLogger name
          let c = fwCurr + 1u
          fwClock <- c
      action logger

    let ensureName (m : Message) =
      if Array.isEmpty m.name then { m with name = name } else m

    interface Logger with
      member x.log level msgFactory =
        withLogger (fun logger -> logger.log level (msgFactory >> ensureName))

      member x.logWithAck level msgFactory =
        withLogger (fun logger -> logger.logWithAck level (msgFactory >> ensureName))

      member x.logSimple message =
        withLogger (fun logger -> logger.logSimple (ensureName message))

  let internal getStaticLogger (name : string []) =
    Flyweight name

  let timestamp () : EpochNanoSeconds =
    (fst !config).timestamp ()

  /// Call from the initialisation of your library. Initialises the
  /// Logary.Facade globally/per process.
  let initialise cfg =
    config := (cfg, snd !config + 1u)

  let initialiseIfDefault cfg =
    if snd !config = 1u then initialise cfg

/// "Shortcut" for creating targets; useful at the top-level configuration point of
/// your library.
module Targets =
  /// Create a new target. Prefer `Log.create` in your own libraries, or let the
  /// composing app replace your target instance through your configuration.
  ///
  /// Will log to console (colourised) by default, and also to the output window
  /// in your IDE if you specify a level below Info.
  let create level =
    if level >= LogLevel.Info then
      LiterateConsoleTarget(level, consoleSemaphore = Global.consoleSemaphore) :> Logger
    else
      CombiningTarget(
        [ LiterateConsoleTarget(level, consoleSemaphore = Global.consoleSemaphore)
          OutputWindowTarget(level) ])
      :> Logger

/// Module for acquiring static loggers (when you don't want or can't)
/// pass loggers as values.
module Log =

  /// Create a named logger. Full stop (.) acts as segment delimiter in the
  /// hierachy of namespaces and loggers.
  let create (name : string) =
    match name with null -> invalidArg "name" "name is null" | _ -> ()
    Global.getStaticLogger (name.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries))
    :> Logger

  /// Create an hierarchically named logger
  let createHiera (name : string[]) =
    match name with null -> invalidArg "name" "name is null" | _ -> ()
    if name.Length = 0 then invalidArg "name" "must have >0 segments"
    Global.getStaticLogger name
    :> Logger

/// The Message module contains functions that can help callers compose messages. This
/// module is especially helpful to open to make calls into Logary's facade small.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
  open Literals

  /// Create a new event log message.
  let event level template =
    { name      = [||]
      value     = Event template
      fields    = Map.empty
      timestamp = Global.timestamp ()
      level     = level }

  /// Create a new event log message – like `event` but with parameters flipped.
  /// Useful to use with `Logger.log` with point-free style, to reduce the
  /// noise. E.g. `logger.logVerbose (eventX "Returned {code}" >> setField "code" 24)`
  let eventX template level =
    event level template

  /// Create a new instantaneous value in a log message.
  let gauge value units =
    { name      = [||]
      value     = Gauge (value, units)
      fields    = Map.empty
      timestamp = Global.timestamp ()
      level     = Debug }

  /// Sets the name/path of the log message.
  let setName (name : string[]) (x : Message) =
    { x with name = name }

  /// Sets the name as a single string; if this string contains dots, the string
  /// will be split on these dots.
  let setSingleName (name : string) (x : Message) =
    match name with null -> invalidArg "name" "may not be null" | _ -> ()

    let name' =
      name.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries)

    x |> setName name'

  /// Sets the value of the field on the log message.
  let setField (key : string) (value : obj) (x : Message) =
    { x with fields = x.fields |> Map.add key value }

  /// Alias to `setField`
  let setFieldValue = setField

  /// Sets the timestamp on the log message.
  let setTimestamp (ts : EpochNanoSeconds) (x : Message) =
    { x with timestamp = ts }

  /// Sets the level on the log message.
  let setLevel (level : LogLevel) (x : Message) =
    { x with level = level }

  /// Adds an exception to the Message, to the 'errors' field, inside a list.
  let addExn ex (x : Message) =
    let fields' =
      match Map.tryFind FieldErrorsKey x.fields with
      | None ->
        x.fields |> Map.add FieldErrorsKey (box [ box ex ])

      | Some errors ->
        let arr : obj list = unbox errors
        x.fields |> Map.add FieldErrorsKey (box (box ex :: arr))

    { x with fields = fields' }

[<AutoOpen>]
module TemplateEvent =
  open FsMtParser

  [<AutoOpen>]
  module internal Capturing =
    open System.Text
    open System.Collections.Generic

    type Capturer = obj -> obj

    let stringifyValue (property : Property) (value : obj) = 
      // Use String.Format to apply the formatting rules
      let formatString = property.AppendPropertyString(
                                      sb = StringBuilder(),
                                      replacementName = "0",
                                      appendCaptureHint = false)

      box (String.Format (formatString.ToString(), value))

    let createPublicPropertyCapturer<'TSource> () =
      let typ = typeof<'TSource>
      let publicReadProps =
        typ.GetProperties() |> Array.filter (fun p -> p.CanRead)

      if publicReadProps.Length = 0 then
        failwithf "Type %s has no public readable properties to capture" typ.FullName

      fun (value : obj) ->
        let nameValueDict = Dictionary<string, obj>()
        for i = 0 to publicReadProps.Length - 1 do
          let prop = publicReadProps.[i]
          try
            let propValue = prop.GetValue value
            nameValueDict.Add (prop.Name, propValue)
          with e ->
            nameValueDict.Add (prop.Name, sprintf "Property accessor threw exception: %s" e.Message)

        box nameValueDict

    let createCapturer<'TSource> (property : Property) =
      match property.captureHint with
      | CaptureHint.Stringify -> stringifyValue property
      | CaptureHint.Structure -> createPublicPropertyCapturer<'TSource>()
      | _ -> fun (value : 'TSource) -> box value

    let propsOrThrow1<'TValue1> format =
      let mutable prop : Property option = None
      let foundProp p = 
        match prop with
        | None -> prop <- Some p
        | _ -> failwithf "template format must have exactly 1 named property: '%s'" format
      parseParts format ignore foundProp
      match prop with
      | Some p -> p, createCapturer<'TValue1> p
      | None -> failwithf "template format must have exactly 1 named property: '%s'" format

    let propsOrThrow2<'T1, 'T2> format =
      let props = ResizeArray<Property>()
      let foundProp p = props.Add p
      parseParts format ignore foundProp
      if props.Count <> 2 then
        failwithf "template format must have exactly 2 named properties: '%s'" format
      
      (props.[0], createCapturer<'T1> props.[0])
      , (props.[1], createCapturer<'T2> props.[1])

    let propsOrThrow3<'T1, 'T2, 'T3> format =
      let props = ResizeArray<Property>()
      let foundProp p = props.Add p
      parseParts format ignore foundProp
      if props.Count <> 3 then
        failwithf "template format must have exactly 3 named properties: '%s'" format
      
      (props.[0], createCapturer<'T1> props.[0])
      , (props.[1], createCapturer<'T2> props.[1])
      , (props.[2], createCapturer<'T3> props.[2])

    let propsOrThrow4<'T1, 'T2, 'T3, 'T4> format =
      let props = ResizeArray<Property>()
      let foundProp p = props.Add p
      parseParts format ignore foundProp
      if props.Count <> 4 then
        failwithf "template format must have exactly 4 named properties: '%s'" format
      
      (props.[0], createCapturer<'T1> props.[0])
      , (props.[1], createCapturer<'T2> props.[1])
      , (props.[2], createCapturer<'T3> props.[2])
      , (props.[3], createCapturer<'T4> props.[3])

  type Message with
    /// Allows creating a predefined event using a format with 1 named
    /// property (e.g. "Hello {name1}"). The returned function accepts
    /// the the property value (typed 'T), followed by a log level, and
    /// finally returns the generated Message with the structured values
    /// attached as fields.
    /// Usage:
    /// let myEvent = Message.TemplateEvent<string> "Hello {name}";
    /// logger.verbose (myEvent "Adam")
    static member templateEvent<'T> (format : string) : ('T -> LogLevel -> Message) =
      let field, capturer = propsOrThrow1<'T> format
      fun (v : 'T) level ->
        Message.event level format
        |> Message.setFieldValue field.name (capturer v)

    /// Allows creating a predefined event using a format with 1 named
    /// properties (e.g. "Hello {name1} and {name2}"). The returned function
    /// accepts the the property values, followed by a log level, and
    /// finally returns the generated Message with the structured values attached
    /// as fields.
    /// Usage:
    /// let myEvent = Message.TemplateEvent<string, string> "Hello {name1} and {name2}";
    /// logger.verbose (myEvent "Adam" "Haf")
    static member templateEvent<'T1, 'T2> (format : string) : ('T1 -> 'T2 -> LogLevel -> Message) =
      let (field1, capturer1), (field2, capturer2) = propsOrThrow2 format
      fun (v1 : 'T1) (v2 : 'T2) level ->
        Message.event level format
        |> Message.setFieldValue field1.name (capturer1 v1)
        |> Message.setFieldValue field2.name (capturer2 v2)

    static member templateEvent<'T1, 'T2, 'T3> (format : string) : ('T1 -> 'T2 -> 'T3 -> LogLevel -> Message) =
      let pc1, pc2, pc3 = propsOrThrow3 format
      let field1, capturer1 = pc1
      let field2, capturer2 = pc2
      let field3, capturer3 = pc3
      fun (v1 : 'T1) (v2 : 'T2) (v3 : 'T3) level ->
        Message.event level format
        |> Message.setFieldValue field1.name (capturer1 v1)
        |> Message.setFieldValue field2.name (capturer2 v2)
        |> Message.setFieldValue field3.name (capturer3 v3)

    static member templateEvent<'T1, 'T2, 'T3, 'T4> (format : string) : ('T1 -> 'T2 -> 'T3 -> 'T4 -> LogLevel -> Message) =
      let pc1, pc2, pc3, pc4 = propsOrThrow4 format
      let field1, capturer1 = pc1
      let field2, capturer2 = pc2
      let field3, capturer3 = pc3
      let field4, capturer4 = pc4
      fun (v1 : 'T1) (v2 : 'T2) (v3 : 'T3) (v4 : 'T4) level ->
        Message.event level format
        |> Message.setFieldValue field1.name (capturer1 v1)
        |> Message.setFieldValue field2.name (capturer2 v2)
        |> Message.setFieldValue field3.name (capturer3 v3)
        |> Message.setFieldValue field4.name (capturer4 v4)

