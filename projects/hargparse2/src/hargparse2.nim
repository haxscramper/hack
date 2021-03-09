import macros
import re
import strformat
import result
import hmisc/helpers
import sequtils
import strutils
import options
import deques
import typeinfo
import typetraits
import tables
import macroutils
import hmisc/termformat
import ast_pattern_matching
import colechopkg/types
import gara
import packages/docutils/[rstgen, rst, rstast]

template setOk[V, E](res: var Result[V, E], val: V): untyped =
  ## Set ok value for result type and immediately return
  res.ok val
  return res

template setErr[V, E](res: var Result[V, E], val: E): untyped =
  ## Set error value for result type and immediately return
  res.err val
  return res


type
  OptDescBase* = ref object of RootObj
    ## Data structure that will be used to generate command/options
    ## tree. Configuration is first parsed into this structure and
    ## only then used to generate tree. This part of the type is
    ## avalable both at runtime and compile time.
    name*: string ## name of the command, used when acessing it in code
    help*: string ## Help string
    docstr*: string ## Longer string, used to generate manpages or html
                   ## documentation.
    pfix*: seq[string] ## Possible prefixes to set given option (--opt)

    maxValues*: int ## Maximum number of times option can be set. If
                   ## this equals zero option type will be `bool`. One
                   ## for `T`. Two or more will create `seq[T]`.

  OptDesc* = ref object of OptDescBase
    exclRules*: Option[NimNode] ## Boolean expression to check whether
                               ## or not it is possible to use command
                               ## (is it mutally exclisive with some
                               ## other command or not)

    parsecheck*: Option[NimNode] ## Code that will be evaluated to
                                ## check validity of the argument.

    parseto*: Option[NimNode] ## Type that *single* use of the option
                              ## can be parsed to. This is `T` in
                              ## `maxValues`

  ArgDescBase* = ref object of RootObj
    ## Description of the command argument.
    name*: string ## name of the command, used when acessing it in code
    variadic*: bool ## Whether or not argument might accept unlimited
                   ## number of arguments. Variadic options **must**
                   ## be placed last.
    help*: string ## Help string, displayed when `--help` is invoked.
    docstr*: string ## Longer string, used to generate manpages or html
                   ## documentation.

  ArgDesc* = ref object of ArgDescBase
    parsecheck*: Option[NimNode] ## Code that will be evaluated to
                                ## check validty of the argument.
    parseto*: Option[NimNode] ## Type that *single* use of the
                          ## argument can be parsed into. If
                          ## argument is `variadic` than each
                          ## entry will be checked.

  CommandDescrBase* = ref object of RootObj
    ## Description of the utility command. This is a base type,
    ## available at both runtime and compile-time. Due to limitations
    ## imposed by use of `NimNode` in `AragDesc` some fields (list of
    ## subcommands, arguments and options) have to be copied only as
    ## `*Base` types: `CommandDescr` uses `args: seq[ArgDesc]` at
    ## compile-time, but only `argsbase: seq[ArgDescBase]` at runtime

    name*: string ## Name of the command
    help*: string ## Help message to be displayed in help page
    dosctr*: string ## Longer string, used to generate manpages or html
                     ## documentation.

    argsbase*: seq[ArgDescBase] ## Descriptions of the command argument
                               ## that is available at both runtime
                               ## and compile-time

    subsbase*: seq[CommandDescrBase] ## Description of the subcommand
                                    ## that are available at both
                                    ## runtime and compile-time.

    optsbase*: seq[OptDescBase] ## Options for the command that are
                                ## available at both runtime and
                                ## compile-time.

  CommandDescr* = ref object of CommandDescrBase
    args*: seq[ArgDesc] ## Arguments for the command. Arguments are
                       ## tried after subcommands - in case of
                       ## non-disjoint sets of values subcommand
                       ## always takes precedence.
    subs*: seq[CommandDescr] ## Subcommands.
    opts*: seq[OptDesc] ## Option related to the command.
    onSelect*: Option[NimNode] ## Code will be executed if command is selected

func toBase(sub: ArgDesc): ArgDescBase = sub.ArgDescBase
func toBase(sub: OptDesc): OptDescBase = sub.OptDescBase

func toBase(sub: CommandDescr): CommandDescrBase =
  ## Convert command description to base recursively. Map all `*Desc`
  ## fields to `*DescBase` (OptDesc -> OptDescBase)
  result = sub.CommandDescrBase
  result.argsbase = sub.args.map(toBase)
  result.optsbase = sub.opts.map(toBase)
  result.subsbase = sub.subs.map(toBase)

var descriptionsComptime {.compileTime.}: Table[string, CommandDescr]

func toBase(tbl: Table[string, CommandDescr]
           ): Table[string, CommandDescrBase] =
  ## Convert table of compile-time descriptions to runtime command
  ## descriptions.
  toSeq(pairs(tbl)).mapIt((it[0], it[1].toBase())).toTable()

type
  CodegenConfig = object
    enableLogging: bool
    verboseLogging: bool
    rootFirst: bool

type
  ParseErrorKind* = enum
    ## Kind of command parse error
    pekUnknownOption
    pekUnknownSubcommand
    pekInvalidValue
    pekTooMuchValues
    pekMissingArgument
    pekTokenParseError
    pekInvalidType
    pekValueNotAllowed

  ParseError* = object
    ## Command parse error
    message*: string
    kind*: ParseErrorKind
    errComm*: CommandDescrBase

type
  CommandTokenKind* = enum
    ## Type of command line token
    ctkLongFlag
    ctkShortFlag

    ctkLongOption
    ctkShortOption

    ctkArgument
    ctkCommand
    ctkInvalid

  PossibleParse* = seq[CommandTokenKind]
    ## Possible parses for ambigout command line options (i.e.
    ## `-hello` might be a mistyped long flag, short flag with option
    ## or multiple short flags with last one havin argumnet (`-h`
    ## `-e=llo` is a possibilty))

  ParsedToken* = object
    ## Single, unambigously parsed token entry
    kind: CommandTokenKind
    key: string
    value: string


func isSubcommandFor(tok: string, comm: CommandDescrBase): bool =
  ## Return true if token is can be recognized as subcommand for
  ## command description
  comm.subsbase.anyofIt(it.name == tok)

func isSubcommandFor(tok: ParsedToken, comm: CommandDescrBase): bool =
  ## Return true if token is can be recognized as subcommand for
  ## command description
  tok.key.isSubcommandFor(comm)

# TODO replace using `rx`
func longOptRegex(): Regex = re"--((?:\w|-|_){2,})[:=](.*)"
func longFlagRegex(): Regex = re"--((?:\w|-|_){2,})$"
func shortOptRegex(): Regex = re"-(\w[_?+=])[:=](.*)"
func shortFlagRegex(): Regex = re"-(\w[_?+=])"

func classifyToken(tok: string, comm: CommandDescrBase): PossibleParse =
  debugEcho tok
  if tok =~ longFlagRegex():
    @[ctkLongFlag]
  elif tok =~ longOptRegex():
    @[ctkLongOption]
  elif tok =~ shortOptRegex():
    @[ctkShortOption]
  elif tok =~ shortFlagRegex():
    @[ctkShortFlag]
  elif tok.isSubcommandFor(comm):
    @[ctkCommand]
  else:
    @[ctkArgument]

func isUnambiguous(optParse: PossibleParse): bool =
  optParse.len == 1

func parseToken(tok: string, kind: CommandTokenKind): Result[ParsedToken, ParseError] =
  ## Parse single unambigous token
  ## :tok: is a token to parse
  ## :kind: is expected kind for a token
  debugEcho "Parsing token: ", tok, " as ", kind
  var res = ParsedToken(kind: kind)
  case kind:
    of ctkArgument: res.value = tok
    of ctkLongFlag:
      if tok.startsWith("--"):
        res.key = tok[2..^1]
        result.setOk res
      else:
        result.setErr ParseError(
          kind: pekTokenParseError,
          message: &"Cannot parse '{tok}' as long flag, missing '--'"
        )
    of ctkShortFlag:
      if tok.startsWith("-") and tok.len == 2:
        res.key = tok[1..^1]
        result.setOk res
      else:
        result.setErr ParseError(
          kind: pekTokenParseError,
          message: &"Cannot parse '{tok}' as short flag")
    of ctkLongOption:
      if tok =~ longOptRegex():
        let key = matches[0]
        let val = matches[1]
        result.setOk ParsedToken(
          kind: ctkLongOption,
          key: key,
          value: val
        )
    of ctkCommand:
      result.setOk ParsedToken(
        kind: ctkCommand,
        key: tok,
        value: ""
      )
    else:
      result.setErr ParseError(
        kind: pekTokenParseError,
        message: &"Unhandled token kind: {kind}"
      )

func separateTokens(
  tok: string, comm: CommandDescrBase
     ): Result[seq[ParsedToken], ParseError] =
  ## Separate token into set of parsed tokens according to
  ## specification. Token without leading dashes is classified as
  ## subcommand if `comm` has subcommand with the same name. Otherwise
  ## it is classified as argument.
  let classification = tok.classifyToken(comm)
  if classification.isUnambiguous():
    let res = tok.parseToken(classification[0])
    if res.isOk():
      result.ok @[res.get]
    else:
      result.err res.error
  else:
    if tok.startsWith("-"):
      discard

func toNimIdentName(name: string): string =
  ## Convert name of the command/option/argument to string that might
  ## be used as nim identifier name (remote dashes, whitespaces and
  ## any other characters not allowed in nim variable names)
  name.replace("-", "_")

func makeNimType(name: string, flds: seq[NimNode]): NimNode =
  ## Create new nim object type
  nnkStmtList.newTree(
    nnkTypeSection.newTree(
      nnkTypeDef.newTree(
        newIdentNode(name),
        newEmptyNode(),
        nnkObjectTy.newTree(
          newEmptyNode(),
          newEmptyNode(),
          nnkRecList.newTree(flds)))))

func joinTypeNames*(
  comm: CommandDescr, names: seq[string], noLast: bool = false
     ): string =
  ## Join type names to use in generated types
  (names & noLast.tern(
    @[], @[comm.name]
  )).map(toNimIdentName).map(capitalizeAscii).join("")

func makeOptTypeName(comm: CommandDescr, names: seq[string]): string =
  ## Get name of the option type for given list of parent command
  ## `names`
  "Opts" & comm.joinTypeNames(names)

func makeCommandTypeName(comm: CommandDescr, names: seq[string]): string =
  ## Get name of the command type for given list of parent command
  ## `names` (should include final command)
  "Command" & comm.joinTypeNames(names)

func makeCommandParentTypeName(comm: CommandDescr, names: seq[string]): string =
  ## Get name of the parent command
  "Command" & comm.joinTypeNames(names, noLast = true)

func makeCommandParserName(comm: CommandDescr, names: seq[string]): string =
  ## Get name of the command type for given list of parent command
  ## `names` (should include final command)
  "parse" & comm.joinTypeNames(names)

func makeCommandSetterName(comm: CommandDescr, names: seq[string]): string =
  ## Get command value setter proc name
  "setvalFor" & comm.joinTypeNames(names)

func makeSubcommandEnumName(comm: CommandDescr, names: seq[string]): string =
  ## Get subcommand selector enum name
  "Sub" & comm.joinTypeNames(names)

func makeSubcommandFieldEnumName(comm: CommandDescr, names: seq[string]): string =
  ## Get subcommand selector enum filed name
  "sub" & names.mapIt(($it[0]).toLower()).join("") & comm.name.capitalizeAscii()


func makeSubcommandFieldName(comm: CommandDescr, names: seq[string]): string =
  ## Get name of the subcommand field
  comm.name

func makeCommandOptionsType(
  comm: CommandDescr, parent: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate type describind possible options for the command
  ## description. This is what CLI options will be parsed **into**.
  let name = comm.makeOptTypeName(parent)
  let flds = comm.opts.mapIt(
    block:
      nnkIdentDefs.newTree(
        newIdentNode(it.name.toNimIdentName()),
        it.parseto.get(newIdentNode("string")),
        newEmptyNode()
      )
  )
  result = makeNimType(name, flds)

func makeSubcommandEnum(
  comm: CommandDescr, parent: seq[string]): NimNode =
  ## Generate enum used for selecting subcommands
  let enumname = comm.makeSubcommandEnumName(parent)
  TypeSection(
    TypeDef(
      Ident(enumname),
      Empty(),
      EnumTy(
        @[Empty()] &
          comm.subs.mapIt(it.makeSubcommandFieldEnumName(parent).ident())
      )))

func hasSubcommands(comm: CommandDescr): bool = comm.subs.len > 0

func makeCommandType(
  comm: CommandDescr, parent: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate type describing cli command. Subcommands are selected
  ## using 'kind' field. Enumeration with possible options is
  ## generated too.
  let name = comm.makeCommandTypeName(parent)
  let optName = comm.makeOptTypeName(parent)
  let optsfield = @[
    nnkIdentDefs.newTree(
      ident("options"),
      ident(optName),
      newEmptyNode()
    ),
    IdentDefs(
      "subcSelected",
      Ident("bool"),
      newEmptyNode()
    )
  ]



  if comm.hasSubcommands():
    let subcEnumType = comm.makeSubcommandEnumName(parent)
    let commandTypeDecl = makeNimType(
      name, optsField & @[
        IdentDefs(
          "activeSubc",
          Ident(subcEnumType),
          newEmptyNode()
        ),
      ] & comm.subs.mapIt(
        block:
          let subcType = it.makeCommandTypeName(parent & @[comm.name])
          let subcName = it.makeSubcommandFieldName(parent)
          IdentDefs(subcName, Ident(subcType), newEmptyNode())
      )
    )

    let enumTypeDecl = makeSubcommandEnum(comm, parent)
    result = quote do:
      `enumTypeDecl`
      `commandTypeDecl`
  else:
    let commandTypeDecl = makeNimType(name, optsField)
    result = quote do:
      `commandTypeDecl`

template makeVerboseLog(body: untyped): untyped =
  ## Return body if logging is enable and verblose logging is enabled.
  ## Otherwise return `discard` WARNING: expects to find variable
  ## `conf` of type `CodegenConfig` in surrounding scope.
  static:
    assert declared conf
    assert conf is CodegenConfig

  if conf.enableLogging and conf.verboseLogging:
    superQuote do:
      body
  else:
    quote do:
      discard

template makeRegularLog(body: untyped): untyped =
  ## Return body if logging is enabled. Otherwise return `discard`.
  ## WARNING: expects to find variable `conf` of type `CodegenConfig`
  ## in surrounding scope.
  static:
    assert declared conf
    assert conf is CodegenConfig

  if conf.enableLogging:
    superQuote do:
      body
  else:
    quote do:
      discard


proc echoMulti(id: string, padTo: int, m: varargs[string, `$`]): void =
  ## Print list of strings on multiple lines
  echo id.alignLeft(padTo, '_'), ": ", m[0]
  if m.len > 1:
    echo m[1..^1].mapIt(" ".repeat(padTo + 2) & it).join("\n")

proc printError(err: ParseError): void =
  ## Print parse error
  let msgWidth = 10
  echoMulti("error", msgWidth, err.kind)
  echoMulti("message", msgWidth, err.message.justifyFitTerminal((msgWidth, 10)))


func parseTokenImpl(val: string, res: string): string = val
func parseTokenImpl(val: string, res: int): int = result = val.parseInt()

proc parseTokenVal[T](val: string): Result[T, ParseError] =
  try:
    var deflt: T
    result.setOk parseTokenImpl(val, deflt)
  except:
    result.setErr ParseError(
      kind: pekInvalidType,
      message: getCurrentExceptionMsg()
    )

type
  OkOrMsg = Result[bool, string]


func makeResult[V, E](val: V): Result[V, E] = result.ok val
func makeResult[V, E](val: E): Result[V, E] = result.err val

# func makeResult[V, E](val: V | E): Result[V, E] =
#   when val is V:
#     result.ok val
#   else:
#     result.err val

func makeOptionCheck(opt: OptDesc, conf: CodegenConfig): NimNode =
  ## Create code for parsing option and checking it's validity
  let targettype =
    if opt.parseto.isSome(): opt.parseto.get()
    else: quote do: string

  let checkCode =
    if opt.parsecheck.isSome(): opt.parsecheck.get()
    else: quote do: true

  let optIdent = newIdentNode(opt.name)

  let valparseOk = makeVerboseLog:
    echo "value parsing is ok"

  result = superquote do:
    if tok.key == `newStrLitNode(opt.name)`:
      # Attempt to parse input token value to necesary type
      let parseResult: Result[`targettype`, ParseError] =
        parseTokenVal[`targettype`](tok.value)

      echo "Parsed ", parseResult

      # If parse is ok check for it's validity using additional code
      if parseResult.isOk():
        let parsedval {.inject.}: `targettype` = parseResult.get()
        let checkResult: OkOrMsg = `checkCode`
        if checkResult.isOk():
          # Check successfull, set value in the output TODO
          comm.options.`optIdent` = parsedval
          `valparseOk`
          result.setOk true
        else:
          result.setErr ParseError(
            kind: pekValueNotAllowed,
            message: checkResult.error
          )

func makeSubcommandSelector(
  comm: CommandDescr, parent: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate code for handling selection of the subcommands for
  ## command described in `comm`
  var subcHandlers: seq[NimNode]
  for subc in comm.subs:
    let commType = subc.makeCommandTypeName(
      parent & @[subc.name]).ident()

    let parser = subc.makeCommandParserName(
      parent & @[comm.name]).ident()

    let makeSubcSelector = subc.makeSubcommandFieldEnumName(
      parent
    ).ident()

    let subcField = subc.makeSubcommandFieldName(parent).ident()

    let commandActivatedLog = makeRegularLog:
      if parsed.isOk():
        echo "Activated command ", targetSubname
      else:
        echo "Failed to activate command ", targetSubname
        printError parsed.error

    let wrongCommandLog = makeVerboseLog:
      echo "tried command ", commname, " didn't match with ", targetSubname

    subcHandlers.add superquote do:
      let targetSubname {.inject.} = `subc.name.newStrLitNode()`
      if commname == targetSubname:
        # TODO add access to configuration options in all parent
        # commands
        let parsed {.inject.} = `parser`(tokens[(idx + 1)..^1])
        `commandActivatedLog`
        if parsed.isOk():
          res.activeSubc = `makeSubcSelector`
          res.subcSelected = true
          res.`subcField` = parsed.get()
        else:
          result.setErr parsed.error
      else:
        `wrongCommandLog`

  let selectorLogic = newStmtList(subcHandlers)

  result = superquote do:
    assert declared descr, "Missing command description from scope"
    assert descr is CommandDescrBase, "Command description has invalid type"
    if stok.isSubcommandFor(descr):
      let commname {.inject.} = stok.key
      `selectorLogic`


func makeCommandSetter(
  comm: CommandDescr, parent: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Create procedure for settings arguments to the command. The
  ## procedure modifies value of the command (`comm` argument)
  ## in-place. Second argument is single, unambigously parsed token
  ## entry. Generated procedure performs necessary checks and might
  ## generate error in parsing. Error returned as `err` value for
  ## `Result`.
  let name = comm.makeCommandSetterName(parent)
  let commPref = comm.makeCommandTypeName(parent)

  let settingTokLog = makeVerboseLog:
    echo "setting value of '", tok, "' for command ", commname

  let setterLogic = nnkStmtList.newTree(
    comm.opts.mapIt(makeOptionCheck(it, conf))
  )

  let postSet = makeRegularLog:
    defer:
      if result.isOk:
        echo "Succesfully set value from token ", tok
      else:
        echo "Failed to set value from toke"
        printError(result.error)

  let commArgIdent = ident("comm")
  result = superquote do:
    proc `name.ident()`(
      `commArgIdent`: var `commPref.ident()`, tok: ParsedToken
                     ): Result[bool, ParseError] =
      let tok {.inject.} = tok
      let commname {.inject.} = `newStrLitNode(commPref)`

      `postSet`

      `settingTokLog`
      if descriptions.commHasOpt(commname, tok.key):
        `setterLogic`
      else:
        result.setErr ParseError(
          kind: pekUnknownOption,
          message: &"No such option for {commname}: {tok.key}"
        )

func makeSubcommandTriggers(
  comm: CommandDescr, parent: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate code that will trigger on subcommand activation. Right
  ## now defaulting to root-first iteration.
  let optsTypeName = comm.makeOptTypeName(parent)
  let optsVariable = ("opts" & optsTypeName).ident()
  let commandTypeName = comm.makeCommandTypeName(parent)
  let commandVariable = ("parsed" & commandTypeName).ident()

  let caseTriggers =
    if comm.hasSubcommands():
      let ofBranches = comm.subs.mapIt(
        OfBranch(
          [ it.makeSubcommandFieldEnumName(parent).ident() ],
          it.makeSubcommandTriggers(parent & @[comm.name], conf)
        )
      )

      CaseStmt(
        (quote do: `commandVariable`.activeSubc) ,
        ofBranches
      )
    else:
      quote do:
        discard

  let parsedGetter =
    if parent.len == 0:
      quote do:
        result.get()
    else:
      # Name of the parent command variable
      let parentVariable = ("parsed" & comm.makeCommandParentTypeName(parent)).ident()
      # Name of the field in parent command variable
      let currentVariable = (comm.makeSubcommandFieldName(parent)).ident()
      quote do:
        `parentVariable`.`currentVariable`

  let triggerAction =
    block:
      iflet (onsel = comm.onSelect):
        onsel
      else:
        quote do:
          discard

  let actions =
    block:
      let tmp =
        if conf.rootFirst:
          quote do:
            `triggerAction`
            `caseTriggers`
        else:
          quote do:
            `caseTriggers`
            `triggerAction`

      if comm.hasSubcommands():
        quote do:
          if `commandVariable`.subcSelected:
            `tmp`
      else:
        tmp

  superquote do:
    # Selected command instance
    let `commandVariable`: `commandTypeName.ident()` = `parsedGetter`
    # Options for selected command
    let `optsVariable`: `optsTypeName.ident()` = `commandVariable`.options
    `actions`


func makeOnSelectTriggers(
  comm: CommandDescr, parent: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate 'on selection' code - it will run if all parsing has
  ## been completed succesfully for toplevel command.
  if parent.len() == 0:
    # IDEA TODO add support for leaf-first and root-first trigger
    # evaluation. Right now code will trigger in root-major mode.
    result = comm.makeSubcommandTriggers(@[], conf)
  else:
    result = quote do:
      discard

func makeCommandParser(
  comm: CommandDescr, parent: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate function for parsing input string to command
  let parserStrName = comm.makeCommandParserName(parent)
  let commResType = newIdentNode(comm.makeCommandTypeName(parent))
  let parserName = newIdentNode(parserStrName)
  let setterName = newIdentNode(comm.makeCommandSetterName(parent))
  let commName = comm.makeCommandTypeName(parent)
  let commLit = newStrLitNode(commName)

  let parseLogStart = makeRegularLog:
    echo "Parsing command '", `newStrLitNode(parserStrName)`, "'"

  let tokenFound = makeVerboseLog:
    echo "Found token '", tok, "'"

  let tokenSepLog = makeVerboseLog:
    echo "token separation result is ok: ", separate.isOk()
    if not separate.isOk():
      printError(separate.error)

  let setresErr = makeRegularLog:
    echo "failed to set option for token ", stok
    printError(setres.error)

  let subcHandlers = comm.makeSubcommandSelector(parent, conf)
  let tokensIdent = "tokens".ident()

  let onSelect = comm.makeOnSelectTriggers(parent, conf)

  result = superquote do:
    proc `parserName`(`tokensIdent`: seq[string]): Result[`commResType`, ParseError] =
      defer:
        if result.isOk():
          # Code to run if all parsing has been completed succesfully. It
          # will be called when parser is completed.
          `onSelect`


      `parseLogStart`
      let descr {.inject.} = descriptions[`commLit`]
      var res {.inject.}: `commResType`
      for idx {.inject.}, tok {.inject.} in tokens:
        `tokenFound`
        let separate {.inject.} = tok.separateTokens(descr)
        `tokenSepLog`
        if separate.isOk():
          for stok {.inject.} in separate.get():
            # If token is command type select it as active subcommand
            case stok.kind:
              of ctkCommand:
                if not stok.isSubcommandFor(descr):
                  result.setErr ParseError(
                    kind: pekUnknownSubcommand,
                    message: "Unknown subcommand " &
                      stok.key & " for command " & descr.name,
                    errComm: descr
                  )

                `subcHandlers`
              of ctkShortFlag, ctkLongFlag, ctkShortOption, ctkLongOption:
                let setres {.inject.} = `setterName`(res, stok)
                if setres.isErr():
                  `setresErr`
                  result.setErr setres.error
              of ctkInvalid:
                result.setErr ParseError(
                  message: "Attempt to set token of type ctkInvalid",
                  kind: pekTokenParseError
                )
              of ctkArgument:
                # TODO add argument command
                discard

        else:
          result.setErr separate.error

      result.setOk res

func makeCommandAccess(
  comm: CommandDescr, parent: seq[string], conf: CodegenConfig): NimNode =
  ## Create functions to simplify access to active fields in command.
  var procs: seq[NimNode]
  let commTypeName: NimNode = comm.makeCommandTypeName(parent).ident()
  let commArgName: NimNode = "comm".ident()


  for sub in comm.subs:
    let subcFieldName: string = sub.makeSubcommandFieldName(parent)
    let funcName: NimNode = ("get" & subcFieldName.capitalizeAscii()).ident()
    let subTypeName: string = sub.makeCommandTypeName(parent & @[comm.name])
    let activeField: string = sub.makeSubcommandFieldEnumName(parent)
    procs.add superquote do:
      func `funcName`(`commArgName`: `commTypeName`): `subTypeName.ident()` =
        if comm.activeSubc != `activeField.ident()`:
          raise newException(
            AssertionError,
            "Cannot access subcommand '" &
              `newStrLitNode(activeField)` &
              "' for command with '" &
              $comm.activeSubc & "' as active one."
          )
        else:
          comm.`subcFieldName.ident()`

  if comm.hasSubcommands():
    let enumTypeName: NimNode = comm.makeSubcommandEnumName(parent).ident()
    procs.add superquote do:
      func isActiveSubc(`commArgName`: `commTypeName`, kind: `enumTypeName`): bool =
        comm.activeSubc == kind

  result = StmtList(procs)


proc makeCommandDeclaration(
  comm: CommandDescr, parent: seq[string], conf: CodegenConfig
     ): NimNode =
  ## Generate types for command `comm`
  let subcommandDeclarations: seq[NimNode] =
    comm.subs.mapIt(makeCommandDeclaration(
      it, parent & @[comm.name], conf)
    )

  let subdeclarations = StmtList(subcommandDeclarations)

  let optType = comm.makeCommandOptionsType(parent, conf)
  let commType = comm.makeCommandType(parent, conf)
  let parser = comm.makeCommandParser(parent, conf)
  let setter = comm.makeCommandSetter(parent, conf)
  let access = comm.makeCommandAccess(parent, conf)
  descriptionsComptime[comm.makeCommandTypeName(parent)] = comm
  result = quote do:
    # First leaf commands need to be created
    `subdeclarations`
    # Declare type used for setting options
    `optType`
    # Declare type for command itself
    `commType`
    # Declare proc for setting values into options
    `setter`
    # Parser for options into command
    `parser`
    # Functions to access fields in generated commands
    `access`


# [T:
# # char   | int     | int8   | int16 |
# # int32  | int64   | uint   | uint8 |
# # uint16 | uint32  | uint64 | bool  |
# # string | float32 | float64
#              ]

proc initCodegen[T](opt: Option[T]): NimNode {.discardable.}
proc initCodegen[T](arr: seq[T]): NimNode {.discardable.}
proc initCodegen(val: char | int | bool | string): NimNode {.discardable.}

proc initCodegenObject[T: object | tuple | ref object](obj: T): NimNode {.discardable.} =
  let isObj = (obj is object) or (obj is ref object)
  var fieldInit: seq[NimNode]
  if isObj:
    fieldInit.add newIdentNode($typeof(T))

  # NOTE using operator [] allows to treat reference object as regular one
  for name, value in (when obj is ref object: obj[] else: obj).fieldPairs:
    when isNamedTuple(typeof obj):
      fieldInit.add initCodegen(value)
    else:
      fieldInit.add nnkExprColonExpr.newTree(
        ident(name), initCodegen(value)
      )

  if isObj:
    nnkObjConstr.newTree(fieldInit)
  else:
    nnkPar.newTree(fieldInit)

proc initCodegen(obj: object | tuple | ref object): NimNode {.discardable.} =
  initCodegenObject(obj)

proc initCodegen(val: char | int | bool | string): NimNode {.discardable.} =
  when val is object:
    initCodegenObject(val)
  else:
    when val is string: newStrLitNode(val)
    elif val is int: newIntLitNode(val)
    elif val is bool: ident(val.tern("true", "false"))
    else: initCodegenObject(val)



proc initCodegen(nd: NimNode): NimNode {.discardable.} =
  discard

proc initCodegen[T](arr: seq[T]): NimNode {.discardable.} =
  nnkPrefix.newTree(
    newIdentNode("@"),
    nnkBracket.newTree(arr.mapIt(it.initCodegen())))

proc initCodegen[T](opt: Option[T]): NimNode {.discardable.} =
  if opt.isSome(): initCodegen(opt.get())
  else: quote do: none(`T`)

proc initCodegen[K, V](tbl: Table[K, V]): NimNode {.discardable.} =
  var fieldInit: seq[NimNode]
  for key, val in tbl:
    fieldInit.add nnkPar.newTree(
      initCodegen[K](key),
      initCodegen[V](val)
    )

  nnkCall.newTree(
    newIdentNode("toTable"),
    nnkBracket.newTree(fieldInit)
  )


type
  TokenKind = enum
    tkKeyword
    tkVarname
    tkType

proc colorize(node: string, kind: TokenKind): ColoredString =
  var fg = fgDefault
  var bg = bgDefault
  var style: set[Style]

  case kind:
    of tkKeyword: fg = fgBlue
    of tkVarname: style.incl styleDim
    of tkType: fg = fgRed; style.incl styleDim

  ColoredString(fg: fg, bg: bg, str: node, style: style)

proc getColored(node: NimNode): ColoredString =
  ColoredString(str: $node.toStrLit())

proc colorPrint(node: NimNode, ind: int = 0): void =
  proc pr(msg: varargs[string, `$`]) = echo "  ".repeat(ind), msg.join(" ")

  matchAst(node, matchErrors):
  of nnkProcDef(
    `name`,
    _,
    `typeParam`,
    `paramList`,
    `pragmas`,
    _,
    `body`
  ):
    pr "matches body"
  of nnkStmtList:
    for sub in node:
      sub.colorPrint(ind + 1)

  of nnkLetSection(nnkIdentDefs(`varname`, `vartype`, `varrhs`)):
    pr colorize("let", tkKeyword),
     colorize($varname.toStrLit(), tkVarname),
     ":", colorize($vartype.toStrLit(), tkType),
     "=", getColored(varrhs)

  else:
    echo "ast had mo match ", node.kind
    # for err in matchErrors:
    #   echo err

func commHasOpt(
  desc: Table[string, CommandDescrBase],
  commName: string, key: string): bool =
  ## Return true if command `commName` has option `key`
  for opt in desc[commName].optsbase:
    if opt.name == key:
      return true

  return false

proc print(node: PRstNode, ind: int = 0): void =
  ## Print rst node in tree formatt
  let indent = "  ".repeat(ind)
  echo indent, node.kind, " ", node.text
  for sub in node.sons:
    print(sub, ind + 1)

proc renderHtml(node: PRstNode): string =
  ## Render rst node as html string
  var gen: RstGenerator
  gen.initRstGenerator(outHtml, defaultConfig(), "hello.html", {})

  var outhtml: string
  gen.renderRstToOut(node, outhtml)
  result = outhtml

func toRstNode(s: string): PRstNode =
  ## Convert string to `rnLeaf` node
  newRstNode(rnLeaf, s)

func toRstNode(n: PRstNode): PRstNode = n
func toRstNode(n: (string, string)): (PRstNode, PRstNode) =
  ## Convert pair of strings into pari of `rnLeaf` nodes
  (toRstNode(n[0]), toRstNode(n[1]))


func newRstTree(kind: RstNodeKind, children: varargs[PRstNode, toRstNode]): PRstNode =
  ## Create new rst tree with `kind` as kind and `children` as `sons` nodes.
  result = newRstNode(kind)
  for c in children:
    result.add(c)

func newRstParagraph(args: varargs[PRstNode, toRstNode]): PRstNode =
  ## Create new `rnParagraph` node with arguments as child nodes.
  newRstTree(rnParagraph, args)

func makeRstFieldList(flds: seq[(PRstNode, PRstNode)]): PRstNode =
  ## Create `rnFieldList` node with each pair in sequence being an
  ## `rnField` son for the list.
  newRstTree(
    rnFieldList,
    flds.mapIt(newRstTree(
      rnField,
      newRstTree(rnFieldName, it[0]),
      newRstTree(rnFieldBody, it[1])
  )))

func makeRstCodeBlock(code: string): PRstNode =
  ## Create `rnCodeBlock` node with `code` as body of code block.
  var blc = newRstNode(rnCodeBlock)
  var args = newRstNode(rnDirArg)
  blc.add args
  blc.add PRstNode(nil)

  var lb = newRstNode(rnLiteralBlock)
  var n = newRstNode(rnLeaf)
  n.text = code
  lb.add n
  blc.add lb

  result = blc


proc toRstDocs(comm: CommandDescr, parent: seq[string]): PRstNode =
  ## Generate rst nodes for creating documentation for the command description
  var top = newRstNode(rnHeadline)
  top.add("Command '" & parent.joinw() & " " & comm.name & "'")
  top.add(newRstTree(rnParagraph, &"""This command is represented as a type
'{comm.makeCommandTypeName(parent)}'.
"""))

  if comm.hasSubcommands():
    top.add newRstParagraph("It has following subcommands:")
    top.add(makeRstFieldList(comm.subs.mapIt((it.name, it.help).toRstNode())))
  else:
    top.add newRstParagraph("There are no subcommands")

  top.add(newRstTree(rnParagraph, &"""Options are represented as a field 'options' of type
'{comm.makeOptTypeName(parent)}'. This field has following fields:
"""))

  top.add(makeRstFieldList(comm.opts.mapIt((
    it.name & " (" & it.parseto.isSome().tern(
      $it.parseto.get().toStrLit(),
      "string"
    ) & ")", (it.help.len > 0).tern(
      it.help,
      "! THERE IS NO DOCUMENTATION FOR THIS OPTION !"
    )
  ).toRstNode())))

  if comm.onSelect.isSome():
    top.add newRstParagraph("When command is selected followind code will be executed:")
    top.add makeRstCodeBlock($comm.onSelect.get().toStrLit())

  for subc in comm.subs:
    top.add subc.toRstDocs(parent & @[comm.name])

  return top

proc generateDocumentation(comm: CommandDescr, basename: string): void =
  ## Generate documentation for types describing command `comm`

  let top = comm.toRstDocs(@[])

  (basename & ".html").writeFile(top.renderHtml())

macro testgen() =
  let conf = CodegenConfig(enableLogging: true, verboseLogging: true)
  let commTest = CommandDescr(
    name: "77",
    help: "There is not help",
    opts: @[OptDesc(
      name: "test",
      parseto: some(quote do: int),
      parsecheck: some(quote do: makeResult[bool, string](parsedval < 12))
    )],
    subs: @[
      CommandDescr(
        name: "kill",
        help: "Subcommand help",
        opts: @[OptDesc(
          name: "target",
          parsecheck: some(
            quote do:
              if parsedval.len < 5:
                makeResult[bool, string]("too short target for a kill")
              else:
                makeResult[bool, string](true)
          )
        )],
        onSelect: some quote do:
          echo "\e[32m@@@@@@@77 kill is selected\e[0m"
      )
    ],
  )

  generateDocumentation(commTest, "docs.tmp")
  let commDecls = makeCommandDeclaration(commTest, @[], conf)
  let descrInit = initCodegen(descriptionsComptime.toBase())
  result = quote do:
    let descriptions {.inject.}: Table[string, CommandDescrBase] = `descrInit`
    `commDecls`

  descriptionsComptime.clear()
  # echo result.toStrLit()
  "ast.tmp.nim".writeFile($result.toStrLit())
  # discard staticExec("nimpretty ast.tmp.nim")
  # echo staticExec("pygmentize -f terminal ast.tmp.nim ")
  # result.colorPrint()


testgen()

block:
  echo "============="
  let parsed = parse77(@["--test:12"])

  if parsed.isOk():
    echo "parse ok"
    let val = parsed.get()
    match (val.options):
      (test: 12):
        echo "test is 12"
      _:
        echo "different value"
        echo val.options.test
  else:
    echo "parsed failed"
    printError(parsed.error)

  echo "============"


block:
  echo "============="
  let parsed = parse77(@["--test:33", "kill"])

  if parsed.isOk():
    echo "parse ok"
    let val = parsed.get()
    match (val.options):
      (test: 33):
        echo "test is 12"
      _:
        echo "different value"
        echo val.options.test

    if val.isActiveSubc(subKill):
      let opts = val.getKill().options
      echo "Activated kill subcommand"
  else:
    echo "parsed failed"
    printError(parsed.error)

  echo "============"
