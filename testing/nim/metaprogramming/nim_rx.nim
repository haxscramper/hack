import macros

dumpTree:
  (rx \
     (group \
      "#" (eval tag_body_regex) \
      (0+ \
       (and "##" \
            (or (eval tag_body_regex) \
                (and  \
                 "[" \
                 (0+ (and (eval tag_body_regex) ",")) \
                 (eval tag_body_regex) \
                 "]"))))))

macro rx(arg: untyped): untyped = discard

discard rx"""
(rx
   (group
    "#" (eval tag_body_regex)
    (0+
     (and "##"
          (or (eval tag_body_regex)
              (and
               "["
               (0+ (and (eval tag_body_regex) ","))
               (eval tag_body_regex)
               "]"))))))
"""
