##
##
##    wn.h - header file needed to use WordNet Run Time Library
##
##    $Id: wn.h,v 1.61 2006/11/14 20:58:30 wn Exp $
##
##

##  Platform specific path and filename specifications


{.pragma: cvar, importc.}

when defined(windows):
  const
    DICTDIR* = "\\dict"
  when not defined(DEFAULTPATH):
    const
      DEFAULTPATH* = "C:\\Program Files\\WordNet\\3.0\\dict"
  const
    DATAFILE* = "%s\\data.%s"
    INDEXFILE* = "%s\\index.%s"
    SENSEIDXFILE* = "%s\\index.sense"
    KEYIDXFILE* = "%s\\index.key"
    REVKEYIDXFILE* = "%s\\index.key.rev"
    VRBSENTFILE* = "%s\\sents.vrb"
    VRBIDXFILE* = "%s\\sentidx.vrb"
    CNTLISTFILE* = "%s\\cntlist.rev"
else:
  const
    DICTDIR* = "/dict"
  when not defined(DEFAULTPATH):
    const
      DEFAULTPATH* = "/usr/local/WordNet-3.0/dict"
  const
    DATAFILE* = "%s/data.%s"
    INDEXFILE* = "%s/index.%s"
    SENSEIDXFILE* = "%s/index.sense"
    KEYIDXFILE* = "%s/index.key"
    REVKEYIDXFILE* = "%s/index.key.rev"
    VRBSENTFILE* = "%s/sents.vrb"
    VRBIDXFILE* = "%s/sentidx.vrb"
    CNTLISTFILE* = "%s/cntlist.rev"
##  Various buffer sizes

const
  SEARCHBUF* = ((clong)(200 * cast[clong](1024)))
  LINEBUF*   = (15 * 1024) ##  15K buffer to read index & data files
  SMLINEBUF* = (3 * 1024)  ##  small buffer for output lines
  WORDBUF*   = (256)       ##  buffer for one word or collocation
  ALLSENSES* = 0           ## pass to findtheinfo() if want all senses
  MAXID*     = 15          ##  maximum id number in lexicographer file
  MAXDEPTH*  = 20          ## maximum tree depth - used to find cycles
  MAXSENSE*  = 75          ## maximum number of senses in database
  MAX_FORMS* = 5           ## max # of different 'forms' word can have
  MAXFNUM*   = 44          ## maximum number of lexicographer files

##  Pointer type and search type counts
##  Pointers

type
  PTR_TYPES* = enum
    ANTPTR         = 1 ## `!`
    HYPERPTR       = 2 ## `@`
    HYPOPTR        = 3 ## `~`
    ENTAILPTR      = 4 ## `*`
    SIMPTR         = 5 ## `&`
    ISMEMBERPTR    = 6 ## `#m`
    ISSTUFFPTR     = 7 ## `#s`
    ISPARTPTR      = 8 ## `#p`
    HASMEMBERPTR   = 9 ## `%m`
    HASSTUFFPTR    = 10 ## `%s`
    HASPARTPTR     = 11 ## `%p`
    MERONYM        = 12
    HOLONYM        = 13
    CAUSETO        = 14
    PPLPTR         = 15
    SEEALSOPTR     = 16
    PERTPTR        = 17
    ATTRIBUTE      = 18
    VERBGROUP      = 19
    DERIVATION     = 20
    CLASSIFICATION = 21
    CLASS          = 22

const LASTTYPE       = CLASS

##  Misc searches

const
  SYNS*             = (LASTTYPE.cint + 1)
  FREQ*             = (LASTTYPE.cint + 2)
  FRAMES*           = (LASTTYPE.cint + 3)
  COORDS*           = (LASTTYPE.cint + 4)
  RELATIVES*        = (LASTTYPE.cint + 5)
  HMERONYM*         = (LASTTYPE.cint + 6)
  HHOLONYM*         = (LASTTYPE.cint + 7)
  WNGREP*           = (LASTTYPE.cint + 8)
  OVERVIEW*         = (LASTTYPE.cint + 9)
  MAXSEARCH*        = OVERVIEW
  CLASSIF_START*    = (MAXSEARCH + 1)
  CLASSIF_CATEGORY* = (CLASSIF_START) ##  ,c
  CLASSIF_USAGE*    = (CLASSIF_START + 1) ##  ,u
  CLASSIF_REGIONAL* = (CLASSIF_START + 2) ##  ,r
  CLASSIF_END*      = CLASSIF_REGIONAL
  CLASS_START*      = (CLASSIF_END + 1)
  CLASS_CATEGORY*   = (CLASS_START) ##  -c
  CLASS_USAGE*      = (CLASS_START + 1) ##  -u
  CLASS_REGIONAL*   = (CLASS_START + 2) ##  -r
  CLASS_END*        = CLASS_REGIONAL
  INSTANCE*         = (CLASS_END + 1)     ##  @i
  INSTANCES*        = (CLASS_END + 2)    ##  ~i
  MAXPTR*           = INSTANCES

##  WordNet part of speech stuff

const
  NUMPARTS* = 4
  NUMFRAMES* = 35

##  Generic names for part of speech

const
  NOUN*      = 1
  VERB*      = 2
  ADJ*       = 3
  ADV*       = 4
  SATELLITE* = 5
  ADJSAT*    = SATELLITE
  ALL_POS*   = 0

template bit*(n: untyped): untyped =
  (cast[cuint]((cast[cuint](1) shl (cast[cuint](n)))))

##  Adjective markers

const
  PADJ*              = 1
  NPADJ*             = 2
  IPADJ*             = 3
  UNKNOWN_MARKER*    = 0
  ATTRIBUTIVE*       = NPADJ
  PREDICATIVE*       = PADJ
  IMMED_POSTNOMINAL* = IPADJ

var wnrelease* {.cvar.}: cstring ##  WordNet release/version number
var lexfiles* {.cvar.}: ptr UncheckedArray[cstring] ##  names of
                                                    ## lexicographer files
var ptrtyp* {.cvar.}: ptr UncheckedArray[cstring] ##  pointer characters
var partnames* {.cvar.}: ptr UncheckedArray[cstring] ##  POS strings
var partchars* {.cvar.}: ptr UncheckedArray[char] ##  single chars for each POS
var adjclass* {.cvar.}: ptr UncheckedArray[cstring] ##  adjective class strings
var frametext* {.cvar.}: ptr UncheckedArray[cstring] ##  text of verb frames

##  Data structures used by search code functions.

type
  Index* {.bycopy.} = object
    ## Structure for index file entry
    idxoffset*: clong          ##  byte offset of entry in index file
    wd*: cstring               ##  word string
    pos*: cstring              ##  part of speech
    sense_cnt*: cint           ##  sense (collins) count
    off_cnt*: cint             ##  number of offsets
    tagged_cnt*: cint          ##  number senses that are tagged
    offset*: ptr culong         ##  offsets of synsets containing word
    ptruse_cnt*: cint          ##  number of pointers used
    ptruse*: ptr cint           ##  pointers used

  IndexPtr* = ptr Index


  Synset* {.bycopy.} = object
    ##  Structure for data file synset
    hereiam*: clong            ##  current file position
    sstype*: cint              ##  type of ADJ synset
    fnum*: cint                ##  file number that synset comes from
    pos*: cstring              ##  part of speech
    wcount*: cint              ##  number of words in synset
    words*: cstringArray       ##  words in synset
    lexid*: ptr cint            ##  unique id in lexicographer file
    wnsns*: ptr cint            ##  sense number in wordnet
    whichword*: cint           ##  which word in synset we're looking for
    ptrcount*: cint            ##  number of pointers
    ptrtyp*: ptr cint           ##  pointer types
    ptroff*: ptr clong          ##  pointer offsets
    ppos*: ptr cint             ##  pointer part of speech
    pto*: ptr cint              ##  pointer 'to' fields
    pfrm*: ptr cint             ##  pointer 'from' fields
    fcount*: cint              ##  number of verb frames
    frmid*: ptr cint            ##  frame numbers
    frmto*: ptr cint            ##  frame 'to' fields
    defn*: cstring             ##  synset gloss (definition)
    key*: cuint ##  unique synset key
              ##  these fields are used if a data structure is returned
              ##        instead of a text buffer
    nextss*: ptr Synset             ##  ptr to next synset containing searchword
    nextform*: ptr Synset           ##  ptr to list of synsets for alternate
                   ## 				   spelling of wordform
    searchtype*: cint          ##  type of search performed
    ptrlist*: ptr Synset            ##  ptr to synset list result of search
    headword*: cstring         ##  if pos is "s", this is cluster head word
    headsense*: cshort         ##  sense number of headword

  SynsetPtr* = ptr Synset
  SnsIndex* {.bycopy.} = object
    sensekey*: cstring         ##  sense key
    word*: cstring             ##  word string
    loc*: clong                ##  synset offset
    wnsense*: cint             ##  WordNet sense number
    tag_cnt*: cint             ##  number of semantic tags to sense
    nextsi*: ptr SnsIndex             ##  ptr to next sense index entry

  SnsIndexPtr* = ptr SnsIndex
  SearchResults* {.bycopy.} = object
    SenseCount*: array[MAX_FORMS, cint] ##  number of senses word form has
    OutSenseCount*: array[MAX_FORMS, cint] ##  number of senses printed for word form
    numforms*: cint            ##  number of word forms searchword has
    printcnt*: cint            ##  number of senses printed by search
    searchbuf*: cstring        ##  buffer containing formatted results
    searchds*: SynsetPtr       ##  data structure containing search results

  SearchResultsPtr* = ptr SearchResults

##  Global variables and flags

var wnresults* {.cvar.}: SearchResults ##  structure containing results of search
var fnflag* {.cvar.}: cint ##  if set, print lex filename after sense
var dflag* {.cvar.}: cint ##  if set, print definitional glosses
var saflag* {.cvar.}: cint ##  if set, print SEE ALSO pointers
var fileinfoflag* {.cvar.}: cint ##  if set, print lex file info on synsets
var frflag* {.cvar.}: cint ##  if set, print verb frames after synset
var abortsearch* {.cvar.}: cint ##  if set, stop search algorithm
var offsetflag* {.cvar.}: cint ##  if set, print byte offset of each synset
var wnsnsflag* {.cvar.}: cint ##  if set, print WN sense # for each word

##  File pointers for database files

var OpenDB*: cint

##  if non-zero, database file are open

var
  datafps* {.cvar.}: array[NUMPARTS + 1, ptr FILE]
  indexfps* {.cvar.}: array[NUMPARTS + 1, ptr FILE]
  sensefp* {.cvar.}: ptr FILE
  cntlistfp* {.cvar.}: ptr FILE
  keyindexfp* {.cvar.}: ptr FILE
  revkeyindexfp* {.cvar.}: ptr FILE
  vidxfilefp* {.cvar.}: ptr FILE
  vsentfilefp* {.cvar.}: ptr FILE

##  Method for interface to check for events while search is running

var interface_doevents_func*: proc ()

##  callback for interruptable searches in
##  single-threaded interfaces
##  General error message handler - can be defined by interface.
##    Default function provided in library returns -1

proc default_display_message*(a1: cstring): cint {.
    importc: "default_display_message".}
var display_message*: proc (a1: cstring): cint

##  Make all the functions compatible with c++ files

##  External library function prototypes
## ** Search and database functions (search.c) **

proc findtheinfo*(
    searchstr: cstring,
    dbase: cint,
    ptrtyp: cint,
    whichsense: cint
  ): cstring {.
    importc: "findtheinfo".}
  ## Immediately print search results into stdout. Formatting is not
  ## configurable, so most likely you would need to use `findtheinfo_ds`
  ## instead, since it returns a proper data structure.


proc findtheinfo_ds*(
    searchstr: cstring,
    dbase: cint,
    ptrtyp: cint,
    whichsense: cint): SynsetPtr {.
    importc: "findtheinfo_ds".}
  ## Return search results as a linked list of data instead of printing it
  ## immediately.

##  Set bit for each search type that is valid for the search word
##    passed and return bit mask.

proc is_defined*(a1: cstring, a2: cint): cuint {.importc: "is_defined".}
##  Set bit for each POS that search word is in.  0 returned if
##    word is not in WordNet.

proc in_wn*(a1: cstring, a2: cint): cuint {.importc: "in_wn".}
##  Find word in index file and return parsed entry in data structure.
##    Input word must be exact match of string in database.

proc index_lookup*(a1: cstring, a2: cint): IndexPtr {.importc: "index_lookup".}
##  'smart' search of index file.  Find word in index file, trying different
##    techniques - replace hyphens with underscores, replace underscores with
##    hyphens, strip hyphens and underscores, strip periods.

proc getindex*(a1: cstring, a2: cint): IndexPtr {.importc: "getindex".}
proc parse_index*(a1: clong, a2: cint, a3: cstring): IndexPtr {.importc: "parse_index".}
##  Read synset from data file at byte offset passed and return parsed
##    entry in data structure.

proc read_synset*(a1: cint, a2: clong, a3: cstring): SynsetPtr {.importc: "read_synset".}
##  Read synset at current byte offset in file and return parsed entry
##    in data structure.

proc parse_synset*(a1: ptr FILE, a2: cint, a3: cstring): SynsetPtr {.
    importc: "parse_synset".}
##  Free a synset linked list allocated by findtheinfo_ds()

proc free_syns*(a1: SynsetPtr) {.importc: "free_syns".}
##  Free a synset

proc free_synset*(a1: SynsetPtr) {.importc: "free_synset".}
##  Free an index structure

proc free_index*(a1: IndexPtr) {.importc: "free_index".}
##  Recursive search algorithm to trace a pointer tree and return results
##    in linked list of data structures.

proc traceptrs_ds*(a1: SynsetPtr, a2: cint, a3: cint, a4: cint): SynsetPtr {.
    importc: "traceptrs_ds".}
##  Do requested search on synset passed, returning output in buffer.

proc do_trace*(a1: SynsetPtr, a2: cint, a3: cint, a4: cint): cstring {.importc: "do_trace".}
## ** Morphology functions (morph.c) **
##  Open exception list files

proc morphinit*(): cint {.importc: "morphinit".}
##  Close exception list files and reopen

proc re_morphinit*(): cint {.importc: "re_morphinit".}
##  Try to find baseform (lemma) of word or collocation in POS.

proc morphstr*(a1: cstring, a2: cint): cstring {.importc: "morphstr".}
##  Try to find baseform (lemma) of individual word in POS.

proc morphword*(a1: cstring, a2: cint): cstring {.importc: "morphword".}
## ** Utility functions (wnutil.c) **
##  Top level function to open database files, initialize wn_filenames,
##    and open exeception lists.

proc wninit*(): cint {.importc: "wninit".}
##  Top level function to close and reopen database files, initialize
##    wn_filenames and open exception lists.

proc re_wninit*(): cint {.importc: "re_wninit".}
##  Count the number of underscore or space separated words in a string.

proc cntwords*(a1: cstring, a2: char): cint {.importc: "cntwords".}
##  Convert string to lower case remove trailing adjective marker if found

proc strtolower*(a1: cstring): cstring {.importc: "strtolower".}
##  Convert string passed to lower case

proc ToLowerCase*(a1: cstring): cstring {.importc: "ToLowerCase".}
##  Replace all occurrences of 'from' with 'to' in 'str'

proc strsubst*(a1: cstring, a2: char, a3: char): cstring {.importc: "strsubst".}
##  Return pointer code for pointer type characer passed.

proc getptrtype*(a1: cstring): cint {.importc: "getptrtype".}
##  Return part of speech code for string passed

proc getpos*(a1: cstring): cint {.importc: "getpos".}
##  Return synset type code for string passed.

proc getsstype*(a1: cstring): cint {.importc: "getsstype".}
##  Reconstruct synset from synset pointer and return ptr to buffer

proc FmtSynset*(a1: SynsetPtr, a2: cint): cstring {.importc: "FmtSynset".}
##  Find string for 'searchstr' as it is in index file

proc GetWNStr*(a1: cstring, a2: cint): cstring {.importc: "GetWNStr".}
##  Pass in string for POS, return corresponding integer value

proc StrToPos*(a1: cstring): cint {.importc: "StrToPos".}
##  Return synset for sense key passed.

proc GetSynsetForSense*(a1: cstring): SynsetPtr {.importc: "GetSynsetForSense".}
##  Find offset of sense key in data file

proc GetDataOffset*(a1: cstring): clong {.importc: "GetDataOffset".}
##  Find polysemy (collins) count for sense key passed.

proc GetPolyCount*(a1: cstring): cint {.importc: "GetPolyCount".}
##  Return word part of sense key

proc GetWORD*(a1: cstring): cstring {.importc: "GetWORD".}
##  Return POS code for sense key passed.

proc GetPOS*(a1: cstring): cint {.importc: "GetPOS".}
##  Convert WordNet sense number passed of IndexPtr entry to sense key.

proc WNSnsToStr*(a1: IndexPtr, a2: cint): cstring {.importc: "WNSnsToStr".}
##  Search for string and/or baseform of word in database and return
##    index structure for word if found in database.

proc GetValidIndexPointer*(a1: cstring, a2: cint): IndexPtr {.
    importc: "GetValidIndexPointer".}
##  Return sense number in database for word and lexsn passed.

proc GetWNSense*(a1: cstring, a2: cstring): cint {.importc: "GetWNSense".}
proc GetSenseIndex*(a1: cstring): SnsIndexPtr {.importc: "GetSenseIndex".}
proc GetOffsetForKey*(a1: cuint): cstring {.importc: "GetOffsetForKey".}
proc GetKeyForOffset*(a1: cstring): cuint {.importc: "GetKeyForOffset".}
proc SetSearchdir*(): cstring {.importc: "SetSearchdir".}
##  Return number of times sense is tagged

proc GetTagcnt*(a1: IndexPtr, a2: cint): cint {.importc: "GetTagcnt".}
##
## * Wrapper functions for strstr that allow you to retrieve each
## * occurance of a word within a longer string, not just the first.
## *
## * strstr_init is called with the same arguments as normal strstr,
## * but does not return any value.
## *
## * strstr_getnext returns the position offset (not a pointer, as does
## * normal strstr) of the next occurance, or -1 if none remain.
##

proc strstr_init*(a1: cstring, a2: cstring) {.importc: "strstr_init".}
proc strstr_getnext*(): cint {.importc: "strstr_getnext".}

## ** Binary search functions (binsearch.c) **
##  General purpose binary search function to search for key as first
##    item on line in open file.  Item is delimited by space.

proc bin_search*(a1: cstring, a2: ptr FILE): cstring {.importc: "bin_search".}
proc read_index*(a1: clong, a2: ptr FILE): cstring {.importc: "read_index".}
  ##  Copy contents from one file to another.

proc copyfile*(a1: ptr FILE, a2: ptr FILE) {.importc: "copyfile".}
  ##  Function to replace a line in a file.  Returns the original line,
  ##    or NULL in case of error.

proc replace_line*(a1: cstring, a2: cstring, a3: ptr FILE): cstring {.
    importc: "replace_line".}
  ##  Find location to insert line at in file.  If line with this
  ##    key is already in file, return NULL.

proc insert_line*(a1: cstring, a2: cstring, a3: ptr FILE): cstring {.
    importc: "insert_line".}
var helptext*: array[NUMPARTS + 1, cstringArray]

var license*: cstring = """This software and database is being provided to you, the LICENSEE, by
Princeton University under the following license.  By obtaining, using
and/or copying this software and database, you agree that you have
read, understood, and will comply with these terms and conditions.:

Permission to use, copy, modify and distribute this software and
database and its documentation for any purpose and without fee or
royalty is hereby granted, provided that you agree to comply with
the following copyright notice and statements, including the disclaimer,
and that the same appear on ALL copies of the software, database and
documentation, including modifications that you make for internal
use or for distribution.

WordNet 3.0 Copyright 2006 by Princeton University.  All rights reserved.

THIS SOFTWARE AND DATABASE IS PROVIDED \"AS IS\" AND PRINCETON
UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR
IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PRINCETON
UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES OF MERCHANT-
ABILITY OR FITNESS FOR ANY PARTICULAR PURPOSE OR THAT THE USE
OF THE LICENSED SOFTWARE, DATABASE OR DOCUMENTATION WILL NOT
INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR
OTHER RIGHTS.

The name of Princeton University or Princeton may not be used in
advertising or publicity pertaining to distribution of the software
and/or database.  Title to copyright in this software, database and
any associated documentation shall at all times remain with
Princeton University and LICENSEE agrees to preserve same.
"""

var dblicense*: cstring = """
1  This software and database is being provided to you, the LICENSEE, by
2  Princeton University under the following license.  By obtaining, using
3  and/or copying this software and database, you agree that you have
4  read, understood, and will comply with these terms and conditions.:
5
6  Permission to use, copy, modify and distribute this software and
7  database and its documentation for any purpose and without fee or
8  royalty is hereby granted, provided that you agree to comply with
9  the following copyright notice and statements, including the disclaimer,
10 and that the same appear on ALL copies of the software, database and
11 documentation, including modifications that you make for internal
12 use or for distribution.
13
14 WordNet 3.0 Copyright 2006 by Princeton University.  All rights reserved.
15
16 THIS SOFTWARE AND DATABASE IS PROVIDED \"AS IS\" AND PRINCETON
17 UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR
18 IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PRINCETON
19 UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES OF MERCHANT-
20 ABILITY OR FITNESS FOR ANY PARTICULAR PURPOSE OR THAT THE USE
21 OF THE LICENSED SOFTWARE, DATABASE OR DOCUMENTATION WILL NOT
22 INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR
23 OTHER RIGHTS.
24
25 The name of Princeton University or Princeton may not be used in
26 advertising or publicity pertaining to distribution of the software
27 and/or database.  Title to copyright in this software, database and
28 any associated documentation shall at all times remain with
29 Princeton University and LICENSEE agrees to preserve same.
"""

const
  DBLICENSE_SIZE* = (sizeof((dblicense)))
