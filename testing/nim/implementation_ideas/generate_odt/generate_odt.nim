import
  hmisc/core/all,
  hmisc/other/[oswrap, hshell],
  hmisc/algo/hstring_algo,
  hmisc/hasts/xml_ast

startHax()

let contentXml: string = readFile(relToSource"content.xml")

type
  OdtWriter = object
    writer: XmlWriter
    resDir: AbsDir

using
  writer: var OdtWriter
  params: openarray[(string, string)]

proc newOdtWriter*(dir: AbsDir): OdtWriter =
  OdtWriter(writer: newXmlWriter(), resDir: dir)

proc readAll*(writer): string = writer.writer.readAll()

proc raw(writer; text: string) = writer.writer.writeRaw(text)

proc eopen(writer; elem: string, params; indent: bool = false) =
  writer.writer.xmlStart(elem, params, indent)

proc esingle(writer; elem: string, params; indent: bool = false) =
  writer.writer.xmlSingle(elem, params, indent)

proc eclose(writer; elem: string; indent: bool = true) =
  writer.writer.xmlEnd(elem, indent)

template ewrap(writer; name: string; params; body: untyped): untyped =
  eopen(writer, name, params, false)
  body
  eclose(writer, name)

template ewrapl(writer; name: string; params; body: untyped): untyped =
  eopen(writer, name, params, true)
  body
  eclose(writer, name, true)

template ewrapl1(writer; name: string; params; body: untyped): untyped =
  eopen(writer, name, params, false)
  body
  eclose(writer, name, true)

proc cpFile(writer; file: AbsFile, sub: string) =
  let dir = writer.resDir / RelDir(sub)
  mkDir(dir)
  cpFile(file, dir /. file.splitFile2().file)

proc line(writer) = writer.writer.line()

proc image(writer; file: AbsFile) =
  assertExists file
  writer.ewrapl("draw:frame", {
    "text:anchor-type": "as-char",
    "svg:width": "6.9252in",
    "svg:height": "3.8972in",
    "draw:z-index": "0"
  }):
    writer.esingle("draw:image", {
      "xlink:href": "Pictures/" & file.name(),
      "xlink:type": "simple",
      "xlink:show": "embed",
      "xlink:actuate": "onLoad",
      "draw:mime-type": "image/jpeg"
    })

    writer.cpFile(file, "Pictures")


template p(writer; params; body: untyped): untyped =
  ewrap(writer, "text:p", params, body)
  # eopen(writer, "text:p", params)
  # body
  # eclose(writer, "text:p")

template span(writer; params; body: untyped): untyped =
  ewrap(writer, "text:span", params, body)
  # eopen(writer, "text:span", params, false)
  # body
  # eclose(writer, "text:span", false)

let dir = getAppTempDir()
rmDir dir


var w = newOdtWriter(dir)
for (kind, value) in interpolatedExprs(contentXml):
  case kind:
    of iekVar:
      case value:
        of "document_body":
          w.p({"style-name": "P1"}):
            w.raw("test12")

            w.span({"style-name": "T2"}):
              w.raw("123")

          w.p({"style-name": "P1"}):
            w.raw("Secon paragrasfdasdfin the document")

          # w.p({"style-name": "P1"}):
          #   w.span({"style-name": "T1"}):
          #     w.image(AbsFile"/tmp/file.jpg")


        else:
          raise newUnexpectedKindError(value)


#       w.raw """
# <text:p text:style-name="P1">tes12<text:span text:style-name="T2">123</text:span></text:p>"""

    else:
      w.raw value


mkWithDirStructure dir:
  dir "META-INF":
    file "manifest.xml":
      readFile(relToSource"manifest.xml")

  file "content.xml": readAll(w)
  file "styles.xml": readFile(relToSource"styles.xml")
  file "manifest.rdf": readFile(relToSource"manifest.rdf")
  file "meta.xml": readFile(relToSource"meta.xml")
  file "mimetype": "application/vnd.oasis.opendocument.text"
  file "settings.xml": readFile(relToSource"settings.xml")

  dir "Thumbnails":
    file "thumbnail.png":
      readFile(relToSource"thumbnail.png")

  dir "Configurations2":
    dir "accelerator"
    dir "floater"
    dir "images":
      dir "Bitmaps"

    dir "menubar"
    dir "popupmenu"
    dir "progressbar"
    dir "statusbar"
    dir "toolbar"
    dir "toolpanel"

let res = dir /. "res.odt"

withDir dir:
  discard runShell shellCmd(zip).withIt do:
    it - "r"
    it.arg res
    it.arg "."

let final = AbsFile("/tmp/res.odt")
rmFile final
mvFile res, final

# echov "opening", final
# execShell shellCmd(soffice, $final)
# echov "done"
