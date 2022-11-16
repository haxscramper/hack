include std/[options]


type
  Label[S] = object
    ## Single label for some span in the error message
    span*: S ## Unique identifier for the range in source code. Can be a
             ## simple `start..end` slice or a more complex type.
    # TEMP will be replaced by a colored string sequence, error messages
    # can have internal colorization
    msg*: Option[string] ## Error message to show in the output formatting
    color*: Option[int]
    order*: int ## Specify the order of this label relative to other labels.
    ##
    ## Lower values correspond to this label having an earlier order.
    ##
    ## If unspecified, labels default to an order of `0`.
    ##
    ## When labels are displayed after a line the crate needs to decide
    ## which labels should be displayed first. By Default, the orders
    ## labels based on where their associated line meets the text (see
    ## [`LabelAttach`]). Additionally, multi-line labels are ordered before
    ## inline labels. You can this this function to override this
    ## behaviour.

    priority*: int ## Specify the priority of this label relative to other
    ## labels.
    ##
    ## Higher values correspond to this label having a higher priority.
    ##
    ## If unspecified, labels default to a priority of `0`.
    ##
    ## Label spans can overlap. When this happens, the crate needs to
    ## decide which labels to prioritise for various purposes such as
    ## highlighting. By default, spans with a smaller length get a higher
    ## priority. You can this this function to override this behaviour.

func initLabel*[S](span: S): Label[S] =
  ## Create new error label with given span
  Label[S](span: span)

func setMessage*[S](label: var Label[S], msg: string) =
  label.msg = some msg

type
  LabelAttach = enum
    Start ## Arrows should attach to the start of the label span.
    Middle ## Arrows should attach to the middle of the label span (or as
           ## close to the middle as we can get).
    End ## Arrows should attach to the end of the label span.

  Config = object
    crossGap: bool ## When label lines cross one-another, should there be a
    ## gap?
    ##
    ## The alternative to this is to insert crossing characters. However,
    ## these interact poorly with label colours.
    labelAttach: LabelAttach ## Where should inline labels attach to their
                             ## spans?
    compact: bool ## Should the report remove gaps to minimise used space?
    underlines: bool ## Should underlines be used for label span where
                     ## possible?
    multilineArrows: bool ## Should arrows be used to point to the bounds
                          ## of multi-line spans?
    color: bool ## Should colored output should be enabled?
    tabWidth: int ## How many characters width should tab characters be?
    charSet: CharSet ## What character set should be used to display
                     ## dynamic elements such as boxes and arrows?

  CharSet = enum
    Unicode ## Unicode characters (an attempt is made to use only
            ## commonly-supported characters).
    Ascii ## ASCII-only characters.

  ReportKind = enum
    Error ## The report is an error and indicates a critical problem that
          ## prevents the program performing the requested action.
    Warning ## The report is a warning and indicates a likely problem, but
            ## not to the extent that the requested action cannot be
            ## performed.
    Advice ## The report is advice to the user about a potential
           ## anti-pattern of other benign issues.
    Custom ## The report is of a kind not built into Ariadne.

  Report[S] = object
    case kind*: ReportKind
      of Custom:
        customName: string
        customColor: int

      else:
        discard

    code: Option[string]
    msg: Option[string]
    note: Option[string]
    help: Option[string]
    location: (S, int)
    labels: seq[Label[S]]
    config: Config

type
  Characters = object
    hbar: char
    vbar: char
    xbar: char
    vbarBreak: char
    vbarGap: char

    uarrow: char
    rarrow: char

    ltop: char
    mtop: char
    rtop: char
    lbot: char
    rbot: char
    mbot: char

    lbox: char
    rbox: char

    lcross: char
    rcross: char

    underbar: char
    underline: char

func ascii(): Characters =
  Characters(
    hbar: '-',
    vbar: '|',
    xbar: '+',
    vbar_break: '*',
    vbar_gap: ':',
    uarrow: '^',
    rarrow: '>',
    ltop: ',',
    mtop: 'v',
    rtop: '.',
    lbot: '`',
    mbot: '^',
    rbot: '\'',
    lbox: '[',
    rbox: ']',
    lcross: '|',
    rcross: '|',
    underbar: '|',
    underline: '^',
  )

type
  LabelKind = enum
    Inline
    Multiline

  LabelInfo[S] = object
    kind: LabelKind
    label: Label[S]

  SourceGroup[S] = object
    src_id: S
    span: Slice[int]
    labels: seq[LabelInfo[S]]
