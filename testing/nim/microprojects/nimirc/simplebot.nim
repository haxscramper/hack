import irc
import std/[asyncdispatch, strutils, os, tables, strtabs]
import hpprint

proc onIrcEvent(client: AsyncIrc, event: IrcEvent) {.async.} =
  case event.typ
    of EvConnected:
      discard

    of EvDisconnected, EvTimeout:
      await client.reconnect()

    of EvMsg:
      if event.cmd == MPrivMsg:
        pprint event

        let msg = event.params[min(1, event.params.high)]
        case msg:
          of "!test":
            await client.privmsg(event.origin, "test response")

          else:
            discard



var client = newAsyncIrc(
  "localhost",
  nick = "haxbot",
  user = "haxbot",
  joinChans = @["#test"],
  callback = onIrcEvent
)

sleep(1000)

asyncCheck client.run()

runForever()
