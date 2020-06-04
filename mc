FAILAT 21
assign >nil: EXISTS env:
IF WARN
  makedir ram:env
  assign env: RAM:env
  echo vga8 >env:FPC_VIDEO_BUILTINFONT
ENDIF
assign >nil: EXISTS t:
IF WARN
  makedir RAM:T
  assign T: RAM:
ENDIF

run Work:MCAmiga/MCAmiga


