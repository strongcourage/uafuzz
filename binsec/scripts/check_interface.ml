open Format

module String_set = Set.Make(String)

let umsg = sprintf "%s [directories]" Sys.argv.(0)
let rec argspec : (Arg.key * Arg.spec * Arg.doc) list = []
and print_usage () =
  Arg.usage (Arg.align argspec) umsg;
  exit 0

let filter_to_list p a =
  Array.fold_left (fun acc e -> if p e then e :: acc else acc) [] a

let list_directories dir =
  let files = Sys.readdir dir in
  filter_to_list
    (fun f -> let f = Filename.concat dir f in Sys.is_directory f) files

let is_file f = not (Sys.is_directory f)


let is_ml file = Filename.check_suffix file ".ml"
let is_mli file = Filename.check_suffix file ".mli"

let list_files p dir =
  let files = Sys.readdir dir in
  filter_to_list
    (fun f ->
       let f = Filename.concat dir f in
       is_file f && p f) files

let list_caml_source_files  = list_files (fun f -> is_ml f || is_mli f)

let check_dir dir =
  let open String_set in
  let files = list_caml_source_files dir in
  let ml_files, mli_files = List.partition is_ml files in
  List.iter
    (fun ml_file ->
       let interface = ml_file^"i" in
       if List.mem interface mli_files |> not
       then printf "%s has no interface@." (Filename.concat dir ml_file)
    ) ml_files

let pp_dirset ppf dirset =
  printf "@[<hov 0>";
  String_set.iter (fun dir -> printf "%s;@ " dir) dirset;
  printf "@]"

let gather_subdirectories dirset =
  let open String_set in
  printf "Gathering subdirectories ...@.";
  let rec loop worklist dirset =
    printf "Current worklist: %a@." pp_dirset worklist;
    if is_empty worklist then dirset
    else
      let dir = choose worklist in
      let subdirs =
        List.map (fun subdir -> Filename.concat dir subdir) (list_directories dir) in
      let worklist' = union worklist (of_list subdirs) in
      loop (remove dir worklist') (add dir dirset)
  in loop dirset empty


let main () =
  let directories = ref ["."] in
  Arg.parse (Arg.align argspec)
    (fun dirname -> directories := dirname :: !directories) umsg;
  printf "Listing directories ...@.";
  let dirset = String_set.of_list !directories in
  gather_subdirectories dirset |> String_set.iter check_dir
;;

main ()
