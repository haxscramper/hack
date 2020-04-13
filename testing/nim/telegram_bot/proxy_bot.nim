import telebot, asyncdispatch, logging, options, httpclient
import hmisc/[hjson, defensive]
from strutils import strip
import strformat
import os
import sequtils

initDefense()


const API_KEY = "sdfsf" #slurp("secret.key").strip()

let
  urlGetfile = &"https://api.telegram.org/bot{API_KEY}/getFile?file_id="
  apiFile = &"https://api.telegram.org/file/bot{API_KEY}/"

var isWaitingForNotebook = false
const (proxyaddr, testchat) =
  static:
    # let jsConf = "secret.json".readFile().parseJson()
    # let ip = jsConf["ip"].asStr()
    # let port = jsConf["port"].asStr()
    # (&"http://{ip}:{port}", jsConf["chatid"].asInt())
    ("sdf", 12)


proc updateHandler(bot: Telebot, e: Update): Future[bool] {.async, gcsafe.} =
  echo "Got update"
  if isWaitingForNotebook:
    let
      url_getfile = fmt"https://api.telegram.org/bot{API_KEY}/getFile?file_id="
      api_file = fmt"https://api.telegram.org/file/bot{API_KEY}/"

    var response = e.message.get
    if response.document.isSome:
      let
        document = response.document.get
        file_name = document.file_name.get
        mime_type = document.mime_type.get
        file_id = document.file_id
        file_size = document.file_size.get
        responz = await newAsyncHttpClient(
          proxy = newProxy(proxyaddr)
        ).get(url_getfile & file_id) # file_id > file_path
        responz_body = await responz.body
        file_path = parseJson(responz_body)["result"]["file_path"].getStr()
        responx = await newAsyncHttpClient(
          proxy = newProxy(proxyaddr)
        ).get(api_file & file_path)  # file_path > file
        file_content = await responx.body
        msg0_text = fmt"file_name: {file_name}, mime_type: {mime_type}, file_id: {file_id}, file_size: {file_size}, file_path: {file_path}"

      discard await bot.sendMessage(response.chat.id, msg0_text)

      let (_, _, ext) = fileName.splitFile()

      template msg(text: string): untyped =
        discard await bot.sendMessage(response.chat.id, text)

      if ext == ".ipynb":
        msg("Recieved .ipynb file, converting")
        echo("Recieved .ipynb file. Saving it into /tmp/bot/file.ipynb")
        createDir("/tmp/bot")

        "/tmp/bot/file.ipynb".writeFile(fileContent)

        shell:
          "ipynb-exporter" "--file:/tmp/bot/file.ipynb" "--temp-dir:/tmp/bot"

        let files: seq[string] = toSeq("/tmp/bot/list.txt".lines())
        for f in files:
          discard await bot.sendDocument(
            e.message.get.chat.id,
            &"file:///tmp/bot/{f}"
          )

        msg("Here is your shit")

        # processNotebook("/tmp/bot/file.ipynb", "/tmp/bot/tmpdir", false)



let bot = newTeleBot(API_KEY)

showLog(&"Using {proxyaddr} as proxy")
bot.setProxy(proxyaddr)
showInfo("Configured proxy")

bot.onCommand(
  "echo",
  proc(b: Telebot, c: Command): Future[bool] {.async.} =
    discard b.sendMessage(c.message.chat.id, c.message.text.get())
  )

# let prxy {.threadvar.} = "sdf"
let prxy = "sdf"

proc convertCallback(b: Telebot, c: Command): Future[bool] {.async, gcsafe.} =
  echo prxy
  discard b.sendMessage(c.message.chat.id, "Send me `.ipynb` file to convert")
  isWaitingForNotebook = true


block:
  bot.onCommand("convert", convertCallback)

  bot.onUpdate(updateHandler)



# discard bot.sendMessage(
#   testchat,
#   "bot started"
# )

bot.poll(timeout=300)
