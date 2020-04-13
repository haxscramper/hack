import asyncdispatch, os, telebot

var isWaitingForNotebook = false
# let prxy {.threadvar.} = "sdf"
let prxy = "sdf"

proc convertCallback(): Future[bool] {.async, gcsafe.} =
  echo prxy
  isWaitingForNotebook = true
