import std/[parsexml, streams]
import hmisc/other/oswrap
import hmisc/hdebug_misc
import hpprint
import hmisc/hasts/xml_ast


#=============================  ^^^ --- ^^^  =============================#

type
  MenuItem = object
    value: string
    onclick: string

  Popup = object
    menuitem: seq[MenuItem]

  Menu = object
    popup: Popup
    id: string
    value: string

proc parseMenuItem(parser: var XmlParser): MenuItem =
  assert parser.elementName == "menuitem"
  while true:
    parser.next()
    case parser.kind():
      of xmlAttribute:
        case parser.attrKey:
          of "value":
            result.value = parser.attrValue()

          of "onclick":
            result.onclick = parser.attrValue()

      of xmlEof: break
      of xmlElementClose: discard

      of xmlElementEnd:
        if parser.elementName() == "menuitem":
          parser.next()
          break

        else:
          echov parser.elementName()

      else:
        echov parser.kind()


proc parsePopup(parser: var XmlParser): Popup =
  assert parser.elementName == "popup"
  parser.next()
  while true:
    case parser.kind():
      of xmlElementStart, xmlElementOpen:
        case parser.elementName:
          of "menuitem":
            result.menuitem.add parseMenuItem(parser)


      of xmlEof: break
      of xmlElementClose: parser.next()

      of xmlElementEnd:
        if parser.elementName() == "popup":
          parser.next()
          break

        else:
          echov parser.elementName()

      else:
        echov parser.displayAt()
        echov parser.errorMsg("")


proc parseMenu(parser: var XmlParser): Menu =
  assert parser.elementName == "menu"
  while true:
    case parser.kind():
      of xmlAttribute:
        case parser.attrKey:
          of "id":
            result.id = parser.attrValue

          of "value":
            result.value = parser.attrValue

          else:
            echo parser.errorMsg(
              "Unexpected attribute for menuo " &
              parser.attrKey()
            )

        parser.next()

      of xmlElementStart, xmlElementOpen:
        case parser.elementName:
          of "popup":
            result.popup = parsePopup(parser)

        parser.next()

      of xmlEof:
        break

      else:
        parser.next()
        echov parser.kind()

proc main() =
  block:
    var parser = newXmlParser("<a b=\"1\"><b/></a>")
    while parser.kind() != xmlEof:
      parser.next()
      echo displayAt(parser)


  var parser = newXmlParser(cwd() /. "menu_input.xml")


  var menu: Menu
  while true:
    case parser.kind():
      of xmlElementStart, xmlElementOpen:
        case parser.elementName:
          of "menu":
            menu = parseMenu(parser)

          else:
            echov parser.elementName

        parser.next()

      of xmlEof:
        break

      else:
        parser.next()
        echov parser.kind()

  pprint menu


when isMainModule:
  startHax()
  main()
