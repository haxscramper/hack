𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:07]
Write meta RFC about cleaning up different RFCs, what should be accounted

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:07]
Minimum duality

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:07]
Quality

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:08]
Closing old RFC

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:08]
We have shit that is a year old

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:08]
Several years

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:09]
Timmy has like a trillion RFC open

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:09]
Versioned containers

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:09]
Other crap

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:09]
Or my error rfc

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:09]
"inactive, waiting for input"

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:10]
"inactive, seems like nobody is interested"

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:10]
"stalled"

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:10]
Other shit

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:10]
Also tag syntax, API etc.

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:10]
Tooling

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:10]
Require people to add tags for RFC

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:10]
At the section top

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:11]
And things like that

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 13:11]
Also tag as "minor" or "major"

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 14:49]
Every new module in the stdlib has to be accompanied by an RFC

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 14:50]
I'm not really happy when we try to pretend to have some vision about stdlib evolution, fusion and all that kind of stuff, that talks about reducing stdlib bloat on one way or another, and then we get std/genast with completely incomprehensible example, that didn't go though any RFC.

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 14:51]
And this is supposed to be a replacement for quote do, correct? Well, maybe we should have an RFC that discusses issues with it, maybe someone has an input on that one?

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 14:52]
Another issue is modules like setutils, enumerate and other XXXutils the thave almost no business being a separate module

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 14:53]
I've heard arguments about enumerate and why it could be a separate module - because it could break people's code if they had enumerate implementation already. Fair enough, that can be called a suitable justification

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 14:53]
So we could put this into setutils in 2.0 and before that it would be in enumerate

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 15:02]
Another explanation - if module is so tied into stdlib dependencies that it can't be added anywhere, maybe it should be moved somewhere else, like fusion

𝖍𝖆𝖝𝖘𝖈𝖗𝖆𝖒𝖕𝖊𝖗, [05.07.21 15:10]
https://github.com/nim-lang/Nim/blob/devel/lib/std/sums.nim why this couldn't be added to msth?