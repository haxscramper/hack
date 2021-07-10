import std/[parsexml, streams]
import hmisc/other/oswrap
import hmisc/hdebug_misc
import hpprint
import hmisc/hasts/xml_ast

var parser = newHXmlParser("""
<file path="/tmp/from_nim_code2/a.nim">
  <body>
    <codeLines>
      <text><![CDATA[type]]></text>
      <parts line="0" column="0:3"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[  MalTypeKind* = enum Nil, True, False, Number, Symbol, String,]]></text>
      <parts line="1" column="0:21"/>
      <parts line="1" column="22:24" kind="EnumFieldDeclare" refid="0"/>
      <parts line="1" column="25:26"/>
      <parts line="1" column="27:30" kind="EnumFieldDeclare" refid="0"/>
      <parts line="1" column="31:32"/>
      <parts line="1" column="33:37" kind="EnumFieldDeclare" refid="0"/>
      <parts line="1" column="38:39"/>
      <parts line="1" column="40:45" kind="EnumFieldDeclare" refid="0"/>
      <parts line="1" column="46:47"/>
      <parts line="1" column="48:53" kind="EnumFieldDeclare" refid="0"/>
      <parts line="1" column="54:55"/>
      <parts line="1" column="56:61" kind="EnumFieldDeclare" refid="0"/>
      <parts line="1" column="62:62"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    List, Vector, HashMap, Fun, MalFun, Atom]]></text>
      <parts line="2" column="0:3"/>
      <parts line="2" column="4:7" kind="EnumFieldDeclare" refid="0"/>
      <parts line="2" column="8:9"/>
      <parts line="2" column="10:15" kind="EnumFieldDeclare" refid="0"/>
      <parts line="2" column="16:17"/>
      <parts line="2" column="18:24" kind="EnumFieldDeclare" refid="0"/>
      <parts line="2" column="25:26"/>
      <parts line="2" column="27:29" kind="EnumFieldDeclare" refid="0"/>
      <parts line="2" column="30:31"/>
      <parts line="2" column="32:37" kind="EnumFieldDeclare" refid="0"/>
      <parts line="2" column="38:39"/>
      <parts line="2" column="40:43" kind="EnumFieldDeclare" refid="0"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[]]></text>
      <parts line="3" column="-1:-1"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[type]]></text>
      <parts line="4" column="0:3"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[  FunType = proc(a: varargs[MalType]): MalType]]></text>
      <parts line="5" column="0:19"/>
      <parts line="5" column="20:26" kind="TypeSpecializationUse" refid="8544611809851626116"/>
      <parts line="5" column="27:27"/>
      <parts line="5" column="28:34" kind="TypeAsParameterUse" refid="7998879053558142527"/>
      <parts line="5" column="35:38"/>
      <parts line="5" column="39:45" kind="TypeDirectUse" refid="7998879053558142527"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[]]></text>
      <parts line="6" column="-1:-1"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[  MalFunType* = ref object]]></text>
      <parts line="7" column="0:25"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    fn*:       FunType]]></text>
      <parts line="8" column="0:14"/>
      <parts line="8" column="15:21" kind="TypeAsFieldUse" refid="-5067292736480289045"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    ast*:      MalType]]></text>
      <parts line="9" column="0:14"/>
      <parts line="9" column="15:21" kind="TypeAsFieldUse" refid="7998879053558142527"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    params*:   MalType]]></text>
      <parts line="10" column="0:14"/>
      <parts line="10" column="15:21" kind="TypeAsFieldUse" refid="7998879053558142527"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    is_macro*: bool]]></text>
      <parts line="11" column="0:14"/>
      <parts line="11" column="15:18" kind="TypeAsFieldUse" refid="5957158107991303717"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[]]></text>
      <parts line="12" column="-1:-1"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[  MalType* = ref object]]></text>
      <parts line="13" column="0:22"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    case kind*: MalTypeKind]]></text>
      <parts line="14" column="0:15"/>
      <parts line="14" column="16:26" kind="TypeAsFieldUse" refid="-8215668461364889329"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    of Nil, True, False: nil]]></text>
      <parts line="15" column="0:6"/>
      <parts line="15" column="7:9" kind="EnumFieldUse" refid="1863338922225267404"/>
      <parts line="15" column="10:11"/>
      <parts line="15" column="12:15" kind="EnumFieldUse" refid="245394008149229720"/>
      <parts line="15" column="16:17"/>
      <parts line="15" column="18:22" kind="EnumFieldUse" refid="2188396163466961961"/>
      <parts line="15" column="23:27"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    of Number:           number*:   int]]></text>
      <parts line="16" column="0:6"/>
      <parts line="16" column="7:12" kind="EnumFieldUse" refid="1600743038486634963"/>
      <parts line="16" column="13:35"/>
      <parts line="16" column="36:38" kind="TypeAsFieldUse" refid="3880518432700973504"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    of String, Symbol:   str*:      string]]></text>
      <parts line="17" column="0:6"/>
      <parts line="17" column="7:12" kind="EnumFieldUse" refid="7637025750429875697"/>
      <parts line="17" column="13:14"/>
      <parts line="17" column="15:20" kind="EnumFieldUse" refid="-210396880536877950"/>
      <parts line="17" column="21:35"/>
      <parts line="17" column="36:41" kind="TypeAsFieldUse" refid="1555827476273608127"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    of List, Vector:     list*:     seq[MalType]]]></text>
      <parts line="18" column="0:6"/>
      <parts line="18" column="7:10" kind="EnumFieldUse" refid="-8137316290788161419"/>
      <parts line="18" column="11:12"/>
      <parts line="18" column="13:18" kind="EnumFieldUse" refid="5577498862607451609"/>
      <parts line="18" column="19:35"/>
      <parts line="18" column="36:38" kind="TypeSpecializationUse" refid="5871924509106298836"/>
      <parts line="18" column="39:39"/>
      <parts line="18" column="40:46" kind="TypeAsParameterUse" refid="7998879053558142527"/>
      <parts line="18" column="47:47"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    of HashMap:          hash_map*: int]]></text>
      <parts line="19" column="0:6"/>
      <parts line="19" column="7:13" kind="EnumFieldUse" refid="9103505853010781225"/>
      <parts line="19" column="14:35"/>
      <parts line="19" column="36:38" kind="TypeAsFieldUse" refid="3880518432700973504"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    of Fun:]]></text>
      <parts line="20" column="0:6"/>
      <parts line="20" column="7:9" kind="EnumFieldUse" refid="-512057380347105288"/>
      <parts line="20" column="10:10"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[                         fun*:      FunType]]></text>
      <parts line="21" column="0:35"/>
      <parts line="21" column="36:42" kind="TypeAsFieldUse" refid="-5067292736480289045"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[                         is_macro*: bool]]></text>
      <parts line="22" column="0:35"/>
      <parts line="22" column="36:39" kind="TypeAsFieldUse" refid="5957158107991303717"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    of MalFun:           malfun*:   MalFunType]]></text>
      <parts line="23" column="0:6"/>
      <parts line="23" column="7:12" kind="EnumFieldUse" refid="-7001560519834143299"/>
      <parts line="23" column="13:35"/>
      <parts line="23" column="36:45" kind="TypeAsFieldUse" refid="8431100336313392925"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    of Atom:             val*:      MalType]]></text>
      <parts line="24" column="0:6"/>
      <parts line="24" column="7:10" kind="EnumFieldUse" refid="-5270006993801921104"/>
      <parts line="24" column="11:35"/>
      <parts line="24" column="36:42" kind="TypeAsFieldUse" refid="7998879053558142527"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[]]></text>
      <parts line="25" column="-1:-1"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    meta*: MalType]]></text>
      <parts line="26" column="0:10"/>
      <parts line="26" column="11:17" kind="TypeAsFieldUse" refid="7998879053558142527"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[]]></text>
      <parts line="27" column="-1:-1"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[type]]></text>
      <parts line="28" column="0:3"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[  Base = ref object of RootObj]]></text>
      <parts line="29" column="0:22"/>
      <parts line="29" column="23:29" kind="InheritFrom" refid="-3159662375592596541"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[]]></text>
      <parts line="30" column="-1:-1"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[  A = ref object of Base]]></text>
      <parts line="31" column="0:19"/>
      <parts line="31" column="20:23" kind="InheritFrom" refid="8031974234092824431"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    b: B]]></text>
      <parts line="32" column="0:6"/>
      <parts line="32" column="7:7" kind="TypeAsFieldUse" refid="-5368761830207146680"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[]]></text>
      <parts line="33" column="-1:-1"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[  B = ref object of Base]]></text>
      <parts line="34" column="0:19"/>
      <parts line="34" column="20:23" kind="InheritFrom" refid="8031974234092824431"/>
    </codeLines>
    <codeLines>
      <text><![CDATA[    a: A]]></text>
      <parts line="35" column="0:6"/>
      <parts line="35" column="7:7" kind="TypeAsFieldUse" refid="5739734122633823589"/>
    </codeLines>
  </body>
</file>
""", true)

var msg = ""


while parser.kind() != xmlEOF:
  msg.add parser.displayAt()
  msg.add "\n"
  parser.next()

"/tmp/from-string".writeFile(msg)

msg = ""
parser = newHXmlParser(AbsFile("/tmp/from_nim_code2/a.xml"), true)
while parser.kind() != xmlEOF:
  msg.add parser.displayAt()
  msg.add "\n"
  parser.next()


"/tmp/from-file".writeFile(msg)

echo "done"
