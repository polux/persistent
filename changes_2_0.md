- memory footprint reduced with a factor of 15 (wait what? Was the old implementation so
  ineffective? Or the new one is so cool? The truth is: both. Check out benchmarks)

- changes in API, most notably PersistentMap -> PMap, PersistentVector -> PVec

- more effective == and != on PMap

- deleted several classes, the whole class/interface hierarchy becomes much simpler (although little bit dirtier; some performance-motivated compromises were introduced)

