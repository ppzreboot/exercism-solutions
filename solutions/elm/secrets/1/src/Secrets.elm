module Secrets exposing (clearBits, decrypt, flipBits, setBits, shiftBack)
import Bitwise


shiftBack = Bitwise.shiftRightZfBy

setBits = Bitwise.or


flipBits = Bitwise.xor


clearBits mask value =
    Bitwise.complement value
    |> Bitwise.or mask
    |> Bitwise.complement

decrypt =
    setBits 1996
    >> flipBits 2009
    >> shiftBack 5
    >> clearBits 0x10001
