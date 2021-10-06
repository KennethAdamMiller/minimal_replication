open Cmdliner
open Bap.Std
open Core_kernel
open Bap_plugins.Std
open Format
open Or_error

exception Bad_user_input
exception No_input

let () =
  try Dynlink.loadfile "" with _ -> ()

   
let requires = ["llvm"; "disassemble"; "disassembler";]
let () = match Bap_main.init ~requires () with
  | Ok () -> ()
  | Error err -> 
     let open Bap_main in
     Bap_main.Extension.Error.pp Format.std_formatter err;
     exit 1
   
let parse_input f = 
  if Sys.file_exists f then (
    `Ok (f)
  ) else `Error "does not exist"
let input_type_printer p
  = Format.fprintf p "%s"
let target = 
  let doc = "Specify target binary or corpora folder." in
  Cmdliner.Arg.(
    required & opt (some (parse_input, input_type_printer)) None
    & info ["target"] ~docv:"Target" ~doc
  )

let create = ident
  
let program () =
  let doc = "Extended superset disassembler for constructing decision trees" in
  let man = [
      `S "SYNOPSIS";
      `Pre "$(b,$mname) [FORMAT/METRICS/DISASM_METHOD OPTION]
            [--ground_truth=FILE] [--phases=TRIM_PHASES] --target=FILE ";
      `S "DESCRIPTION";
      `P
        "Given a binary, this utility will
         disassemble, trim, and format as requested and finally
         output the results.";
      `S "OPTIONS";
    ] in
  let open Cmdliner in
  Term.(const create $target),
  Term.info "raw_superset" ~doc ~man

let parse argv =
  let open Cmdliner in
  match Term.eval ~argv (program ()) ~catch:false with
  | `Ok opts -> Ok opts
  | `Error `Parse -> exit 64
  | `Error _ -> exit 2
  | _ -> exit 1
  
let main bin =
  let backend = "llvm" in
  Superset.superset_disasm_of_file ~backend bin

let start options =
  return @@ main options

let exitf n =
  kfprintf (fun ppf -> pp_print_newline ppf (); exit n) err_formatter
  
let _main = 
  try match parse Sys.argv >>= start with
    | Ok _ -> exit 0
    | Error err -> exitf (-1) "%s\n" Error.(to_string_hum err)
  with
  | Bad_user_input -> 
    exitf (-2) "Could not parse: malformed input"
  | No_input -> exitf (-2) "Could not read from stdin"
