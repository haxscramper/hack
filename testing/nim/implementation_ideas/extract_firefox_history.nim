import std/[db_sqlite, uri, strutils, tables, re, httpclient, strformat]
import std/[htmlparser, xmltree, net]
from std/os import sleep
import hmisc/hasts/json_serde
import hmisc/other/hpprint
import hmisc/core/all
import hmisc/other/oswrap

startHax()

proc findPlaces(): AbsFile =
  for dir in walkDir(~".mozilla/firefox", AbsDir):
    let file = dir /. "places.sqlite"
    if "default-release" in dir and exists(file):
      return file

let places = getAppTempDir().getTempFile("places.sqlite")
cpFile findPlaces(), places

let db = open(places.string, "", "", "")

proc writeJson*(writer: var JsonSerializer, tab: Table[string, string]) =
  writeJsonPairs(writer, tab, true, false)

proc loadJson*(reader: var JsonDeserializer, tab: var Table[string, string]) =
  loadJsonPairs[string, string, Table[string, string]](reader, tab, false)


let resfile = getAppTempDir().getTempFile("links.json")
var links: Table[string, string]
if exists(resfile):
  links = fromJson(resfile, Table[string, string])



for row in db.fastRows(sql"select url from moz_places"):
  var url = parseUri(row[0])
  if url.hostname in [
    "forums.spacebattles.com",
    "forums.sufficientvelocity.com",
    "fanfiction.net",
    "fanfics.me",
    "m.fanfiction.net",
    "ficbook.net",
    "archiveofourown.org"
  ]:
    url.anchor = ""
    let ur = replace($url, re"(page-\d+)|(threadmarks)|(chapters\/\d+)", "")
    if ur notin links:
      links[ur] = ""

var client = newHttpClient(timeout = 15_000)

proc getDesc(link: string): string =
  echov "getting description for", link
  try:
    let text = client.getContent(link)
    let html = parseHtml(text)
    for title in findAll(html, "title"):
      return title.innerText()

  except TimeoutError:
    return "<timeout>"

  except HttpRequestError as err:
    return err.msg


for url, desc in links:
  if desc.len == 0:
    links[url] = getDesc(url)
    if globalTick() mod 100 == 0:
      echov "Write buffer"
      resfile.writeFile(toJson links)

resfile.writeFile(toJson links)

var orgfile = open(getAppTempDir().getTempFile("links.org"), fmWrite)

for url, desc in links:
  orgfile.writeLine &"""
** [[{url}]["{desc}"]]
"""

close(orgfile)

# ~/.mozilla/firefox/z2lpbw70.default-release/places.sqlite
