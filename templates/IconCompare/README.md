## IconCompare

#### Extends [IconStandalone](../IconStandalone/IconStandalone.tmpl)

This is first template set that decouples the *Replay Code* from the *Capture Code*, so the latter can be removed if no longer needed.
The generated test program compares the output data directly with the stored values from the Capture run and no longer writes the results to disc, so they don't need to be checked with the Serializer2 compare tool.

Nevertheless, the [Replay template](replay.test.tmpl) still contains two dependencies to the *Capture Code*:

```fortran
  USE ${module.name}, ONLY: ftg_${subroutine.name}_capture_input_enabled, ftg_${subroutine.name}_capture_output_enabled

[...]

  ! Remove if capture code already deleted
  ftg_${subroutine.name}_capture_input_enabled = .FALSE. 
  ftg_${subroutine.name}_capture_output_enabled = .FALSE.
```

This is just to make sure that capturing is disabled if the *Capture Code* still exists.
If the *Capture Code* is already removed, those lines need to be deleted from the generated test program.