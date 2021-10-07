open Bap.Std
open Core_kernel
open Bap_plugins.Std
open Format
open Or_error
open Bap_main
open Bap_knowledge
open Bap_core_theory


let man = {| 
           An example to demontrate the running of superset
           disassembly on a target binary.
           |}
   
let () =
  try Dynlink.loadfile "" with _ -> ()
                                  

let input = Extension.Command.argument
    ~doc:"The input file" Extension.Type.("FILE" %: string =? "a.out" )

let features_used = [
    "disassembler";
    "lifter";
  ]

let main bin =
  let backend = "llvm" in
  Superset.superset_disasm_of_file ~backend bin

let _demo_command : unit =
  let args =
    let open Extension.Command in
    args $input in
  Extension.Command.declare ~doc:man "demo"
    ~requires:features_used args @@
    fun target ctxt ->
    let _ = main target in
    Ok ()
