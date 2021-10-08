open Bap.Std
open Regular.Std
open Core_kernel
open Or_error
open Graphlib.Std
open Bap_knowledge
open Bap_core_theory

module Dis = Disasm_expert.Basic
open Superset_impl
type elem = Superset_impl.elem
type t = Superset_impl.t

(* private accessors *)
let get_map superset = superset.insn_map
let get_graph superset = superset.insn_risg

(* private modifiers *)
let add_to_map superset mem insn = 
  let insn_map = get_map superset in
  let addr = (Memory.min_addr mem) in
  let insn_map = Addr.Map.set insn_map addr (mem, insn) in
  { superset with insn_map }

module OG = Graphlib.To_ocamlgraph(G)
  
let add_to_graph superset mem insn =
  let addr = Memory.min_addr mem in
  let insn_risg = G.Node.insert addr superset.insn_risg in
  { superset with insn_risg }

module Core = struct
  let add superset mem insn =
    let superset = add_to_graph superset mem insn in
    let superset = add_to_map superset mem insn in
    superset

  let empty arch =
    let brancher = Brancher.of_bil arch in
    let module Target = (val target_of_arch arch) in
    let lifter = Target.lift in
    {
      arch;
      filename = None;
      main_entry = None;
      sections = Memmap.empty;
      brancher;
      endianness= None;
      lifter;
      insn_map = Addr.Map.empty;
      lifted   = Addr.Table.create ();
      insn_risg = Graphlib.create (module G) ();
      bad         = Addr.Hash_set.create ();
      keep        = Addr.Hash_set.create ();
    }

  let next_chunk mem ~addr =
    let next_addr = Addr.succ addr in
    Memory.view ~from:next_addr mem

  (** This builds the disasm type, and runs it on the memory. *)
  let disasm ?(backend="llvm") ~accu ~f arch memry =
    print_endline @@ sprintf "Superset.Core.disasm from %s to %s"
                       Addr.(to_string Memory.(min_addr memry))
                       Addr.(to_string Memory.(max_addr memry));
    let r = (Dis.with_disasm ~backend (Arch.to_string arch)
               ~f:(fun d ->
                 let rec next state accu addr =
                   print_endline @@ sprintf "next at %s" Addr.(to_string addr);                   
                   match next_chunk memry ~addr with
                   | Error(_) -> Dis.stop state accu
                   | Ok(jtgt) -> Dis.jump state jtgt accu in
                 let invalid state m accu =
                   let a = Memory.min_addr m in
                   print_endline @@ sprintf "invalid at %s" Addr.(to_string a);
                   let accu = f (m, None) accu in
                   next state accu Memory.(min_addr m) in
                 let hit state m insn accu =
                   let a = Memory.min_addr m in
                   print_endline @@ sprintf "hit at %s" Addr.(to_string a);
                   let accu = f (m, (Some insn)) accu in 
                   next state accu Memory.(min_addr m) in
                 Ok(Dis.run ~backlog:1 ~stop_on:[`Valid] ~invalid
                      ~hit d ~init:accu ~return:ident memry)
            )) in print_endline "disasm finished";
                  r

  let lookup superset addr =
    Map.find superset.insn_map addr

  let lift_at superset addr =
    match Addr.Table.find superset.lifted addr with
    | Some (bil) -> Some (bil)
    | None -> (
      match lookup superset addr with
      | Some (mem, insn) -> (
        let bil =
          Option.value_map insn ~default:[] ~f:(fun insn ->
              match (superset.lifter mem insn) with
              | Ok bil ->
                 let bil = Three_address_code.map_bil bil in
                 Addr.Table.set superset.lifted ~key:addr ~data:bil;
                 bil
              | _ -> []
            ) in
        Some (bil)
      )
      | None -> None
    )

  let update_with_mem ?backend ?f superset mem =
    let f = Option.value f ~default:(fun (m, i) a -> a) in
    let f (mem, insn) superset =
      let superset = add superset mem insn in
      f (mem, insn) superset in
    disasm ?backend ~accu:superset ~f superset.arch mem |> ok_exn
end

module Inspection = struct
  let contains_addr superset addr =
    Memmap.contains superset.sections addr

end

let with_img ~accu img ~f =
  let segments = Table.to_sequence @@ Image.segments img in
  Seq.fold segments ~init:accu ~f:(fun accu (mem, segment) ->
      if Image.Segment.is_executable segment then
        f ~accu mem
      else accu 
    )
  
let superset_of_img ?f ~backend img =
  let arch = Image.arch img in
  let segments =   Image.memory img in
  let main_entry = Image.entry_point img in
  let filename = Image.filename img in
  let f = Option.value f ~default:(fun (m, i) a -> a) in
  let superset =
    of_components ~main_entry ?filename ~segments arch in
  with_img ~accu:superset img
    ~f:(fun ~accu mem ->
      let a = Addr.to_string @@ Memory.min_addr mem in
      print_endline @@ sprintf "superset_of_img starting with mem at %s" a;
      let b = Core.update_with_mem ~backend accu mem ~f in
      let a = Addr.to_string @@ Memory.min_addr mem in
      print_endline
      @@ sprintf "superset_of_img finished with mem at %s" a;
      b
    )

let superset_disasm_of_file ?(backend="llvm") ?f binary =
  let img, errs = Image.create ~backend binary |> ok_exn in
  (*List.iter errs ~f:(fun err ->
      Format.fprintf Format.std_formatter "%s - \n%a\n"
        binary Error.pp err;
    );*)
  superset_of_img ~backend img ?f
    
