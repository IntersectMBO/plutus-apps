Troubleshooting
===============

Error codes
-----------

To reduce code size, on-chain errors only output codes. Here's what they mean:

..
  This list can be generated with:
  grep -rEoh "\btrace\w*\s+\"[^\"]{1,5}\"\s+(--.*|\{-\".*\"-\})" *

- Prelude errors

  - ``P0: PlutusTx.Enum.().succ: bad argument``
  - ``P1: PlutusTx.Enum.().pred: bad argument``
  - ``P2: PlutusTx.Enum.().toEnum: bad argument``
  - ``P3: PlutusTx.Enum.Bool.succ: bad argument``
  - ``P4: PlutusTx.Enum.Bool.pred: bad argument``
  - ``P5: PlutusTx.Enum.Bool.toEnum: bad argument``
  - ``P6: PlutusTx.Enum.Ordering.succ: bad argument``
  - ``P7: PlutusTx.Enum.Ordering.pred: bad argument``
  - ``P8: PlutusTx.Enum.Ordering.toEnum: bad argument``
  - ``P9: PlutusTx.List.!!: negative index``
  - ``Pa: PlutusTx.List.!!: index too large``
  - ``Pb: PlutusTx.List.head: empty list``
  - ``Pc: PlutusTx.List.tail: empty list``
  - ``Pd: Check has failed``
  - ``Pe: Ratio has zero denominator``
  - ``Pf: round default defn: Bad value``
  - ``Pg: unsafeFromBuiltinData: Void is not supported``
