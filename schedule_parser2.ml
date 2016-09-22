(* Parser to build ICFP program *)
(* Uses Xml-Light *)

open StdLabels
open Printf
open Xml

module SMap = Map.Make(String)

let make_map lst =
  let map = Hashtbl.create 7 in
  List.iter ~f:(fun (key, data) -> Hashtbl.add map key data) lst;
  map
let try_find map s =
  try Hashtbl.find map s with Not_found -> s

(* Configuration *)

(* One page per track and per day *)
let pages = [
  "TyDe", "2016/09/18", "Workshop on Type-Driven Development";
  "Scheme", "2016/09/18", "Scheme and Functional Programming Workshop";
  "HOPE", "2016/09/18", "Workshop on Higher-Order Programming with Effects";
  "PLMW", "2016/09/18", "Programming Languages Mentoring Workshop";
  "ICFP", "2016/09/19", "ICFP -- Day 1";
  "ICFP", "2016/09/20", "ICFP -- Day 2";
  "ICFP", "2016/09/21", "ICFP -- Day 3";
  "Haskell", "2016/09/22", "Haskell Symposium -- Day 1";
  "ML", "2016/09/22", "ML Family Workshop";
  "FHPC", "2016/09/22", "Workshop on Functional High-Performance Computing";
  "Tutorials", "2016/09/22", "CUFP Tutorials on Thursday";
  "Haskell", "2016/09/23", "Haskell Symposium -- Day 2";
  "OCaml", "2016/09/23", "OCaml Users and Developers Workshop";
  "Erlang", "2016/09/23", "Erlang Workshop";
  "Tutorials", "2016/09/23", "CUFP Tutorials on Friday";
  "CUFP", "2016/09/24", "Commercial Users of Functional Programming";
  "HIW", "2016/09/24", "Haskell Implementors Workshop";
  "FARM", "2016/09/24", "Functional Art, Modeling and Design";
]

let session_chairs = []
(*
  let c1 = ref 0 and c2 = ref 0 in
  List.map
    (fun name -> incr c1; sprintf "Keynote Talks: Keynote %d" !c1, name)
    ["Eijiro Sumii"; "Gabriele Keller"; "Jacques Garrigue"] @
  List.map
    (fun name -> incr c2; sprintf "Research Papers: Session %d" !c2, name)
    ["Akimasa Morihata";
     "Kathleen Fisher";
     "Neel Krishnaswami";
     "Tom Schrijvers";
     "Robby Findler";
     "Johan Jeuring";
     "Andres Löh";
     "Scott Owens";
     "Sam Lindley";
     "John Reppy";
     "Alejandro Russo";
     "Jeremy Gibbons"]
*)

(* Dates are mapped to weekdays, in the order they appear in the above list *)
let days =
  [ "Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday";
    "Saturday"; "Week" ]
let day_map = make_map []

(* Remap some track names, to allow printing on the same page *)
let track_map = make_map
    [ "ICFP 2016 Keynote Talks", "ICFP";
      "Research Papers", "ICFP";
      "Reports", "ICFP";
      "Receptions", "ICFP";
      "Type-driven Development", "TyDe" ]
let map_track ?(date="") track =
  if track = "Receptions" && date = "2016/09/22" then "Tutorials" else
  try_find track_map track

(* Do not print event with these titles *)
let omit_event = ["Reception"]

(* Night sessions start after *)
let night = "18:00"
let night_track = "Nights"

(* In these tracks, print the room for each sessions,
   rather than at the top of the page *)
let parallel_sessions = ["Tutorials"; night_track]

(* Do not print time for this session title *)
let poster_sessions = ["OCaml: Break and Poster Session"]

(* More compact format for these tracks *)
let compact_tracks = ["OCaml"]

(* How to print affiliations, and for which tracks *)
let affiliation_map = make_map []
let map_affiliation = try_find affiliation_map
let use_affiliations = []

(* Generic? code *)

(* Escaping and cleaning *)
let replacements = [
  Str.regexp "ễ", "e";           (* UTF8 characters not supported by TeX *)
  Str.regexp "[#%&]", "\\\\\\0"; (* require escaping in TeX *)
]
let protect s =
  List.fold_left ~init:s ~f:(fun s (r, n) -> Str.global_replace r n s)
    replacements

(* Remove track name from session name *)
let track_header = Str.regexp ".*: "
let remove_track s =
  Str.replace_first track_header "" s

(* Do not print session name if it is just a short number *)
let short_number = Str.regexp " *[0-9][0-9]? *"
let omit_session_title s =
  Str.string_match short_number s 0 && Str.match_end () = String.length s

(* The real processing starts here *)

(* Parsing *)

let rec map_concat_rev acc ~f = function
    [] -> acc
  | x :: l ->
      map_concat_rev (List.rev_append (f x) acc) ~f l

let map_concat ~f l =
  List.rev (map_concat_rev [] ~f l)

let rec last = function
    [] -> failwith "last"
  | [a] -> a
  | _::l -> last l

let element_contents = function
    Element (_, _, contents) -> contents
  | _ -> []

let is_element ~key = function
    Element (k, _, _) -> k = key
  | PCData _ -> false

let extract_element ~key = function
    Element (k, _, contents) when k = key -> [contents]
  | _ -> []

let find_elements ~key lst =
  map_concat (extract_element ~key) lst

let extract_subevents t =
  find_elements ~key:"subevent" (element_contents t)

let find_str_element ~key ?(default="") lst =
  match find_elements ~key lst with
    [PCData s] :: _ -> s
  | _ -> default

let find_title = find_str_element ~key:"title" ~default:"no title"
let find_date = find_str_element ~key:"date" ~default:"no date"

let sort_by_track subevs =
  let sorted = ref SMap.empty in
  let add_event track ev =
    let date = find_date ev in
    let track = map_track ~date track in
    let evs = try SMap.find track !sorted with Not_found -> [] in
    sorted := SMap.add track (ev :: evs) !sorted
  in
  List.iter subevs ~f:
    begin fun contents ->
      let tracks_ct = List.concat (find_elements ~key:"tracks" contents) in
      let track = find_str_element ~key:"track" tracks_ct in
      add_event track contents
    end;
  !sorted

type session =
    {s_title: string; s_date: string; s_room: string;
     s_time: string; s_slots: timeslot list}
and timeslot =
    {t_title: string; t_start: string; t_end: string;
     t_authors: author list; t_track: string}
and author = {first: string; last: string; affiliation: string}
and event =
    {e_title: string; e_acronym: string; e_start: string; e_end: string;
     e_location: string}

let parse_author lst =
  {first = find_str_element ~key:"first_name" lst;
   last  = find_str_element ~key:"last_name" lst;
   affiliation = find_str_element ~key:"affiliation" lst}

let parse_timeslot lst =
  let track =
    find_str_element ~key:"track"
      (List.concat (find_elements ~key:"tracks" lst))
  and t_authors =
    let persons = List.concat (find_elements ~key:"persons" lst) in
    let persons = find_elements ~key:"person" persons in
    List.map ~f:parse_author persons
  in
  let t_start = find_str_element ~key:"start_time" lst
  and t_end   = find_str_element ~key:"end_time" lst in
  (* Hack for HOPE *)
  let t_start =
    if t_start <= "09:10" && t_end >= "09:15" then "09:10" else t_start in
  {t_title = find_title lst;
   t_track = map_track track;
   t_start; t_end; t_authors}

let sort_by ~prj l = List.sort (fun a b -> compare (prj a) (prj b)) l

let parse_subevent lst =
  let slots = List.map parse_timeslot (find_elements ~key:"timeslot" lst) in
  let slots = sort_by (fun t -> t.t_start) slots in
  let time =
    if slots = [] then "" else
    sprintf "%s -- %s" (List.hd slots).t_start (last slots).t_end
  in
  {s_title = find_title lst;
   s_date = find_date lst;
   s_room = find_str_element ~key:"room" lst;
   s_time = time; s_slots = slots}

let parse_event_details t =
  let lst = element_contents t in
  let details = List.concat (find_elements ~key:"event_details" lst) in
  let location = List.concat (find_elements ~key:"location" details) in
  {e_title = find_str_element ~key:"title" ~default:"no title" details;
   e_acronym = find_str_element ~key:"acronym" ~default:"no acronym" details;
   e_start = find_str_element ~key:"start_date" details;
   e_end = find_str_element ~key:"end_date" details;
   e_location = find_str_element ~key:"facility_name" location}

(* Printing *)

let output_authors ~track oc lst =
  let lst = List.map lst ~f:
      (fun auth -> {auth with affiliation = map_affiliation auth.affiliation})
  in
  let pr_auth oc au =
    fprintf oc "%s %s" (protect au.first) (protect au.last);
    let affi = au.affiliation in
    if List.mem track use_affiliations && affi <> "" && affi <> "undefined"
    then fprintf oc " (%s)" (protect affi)
  in
  match List.rev lst with
    [] -> ()
  | [au] -> fprintf oc "\\author{%a}\n" pr_auth au
  | au1 :: au2 :: prev ->
      List.iter
        (fun au -> fprintf oc "\\author{%a,}\n" pr_auth au)
        (List.rev prev);
      fprintf oc "\\author{%a and %a}\n" pr_auth au2 pr_auth au1

let output_slot ~track ~poster oc sl =
  let time = if poster then "" else sl.t_start in
  match sl.t_authors with
    (* Hack for a more compact format *)
    [au] when String.length sl.t_title < 20 && List.mem track compact_tracks ->
      fprintf oc "\\slot{%s}{\\makebox[20ex][l]{%s} \\textit{%s %s}}\n"
        time (protect sl.t_title) (protect au.first) (protect au.last)
  | _ ->
      fprintf oc "\\slot{%s}{%s%s}\n" time (protect sl.t_title)
        (if sl.t_authors = [] then "\\vspace{1mm}" else "");
      if sl.t_authors <> [] && track <> night_track then
        fprintf oc "\\authors{%%\n%a}\n"
          (output_authors ~track) sl.t_authors

let output_session ~track ~room oc ss =
  let chair =
    try List.assoc ss.s_title session_chairs with Not_found -> "" in
  let poster = List.mem ss.s_title poster_sessions in
  let title = protect (remove_track ss.s_title) in
  let title = if omit_session_title title then "" else title in
  let slots =
    List.filter (fun t -> not (List.mem t.t_title omit_event)) ss.s_slots in
  (* Hack to push receptions to bottom *)
  if ss.s_time > night then () else
  if ss.s_room <> room && track <> "Tutorials"  then () else
  let title =
    if ss.s_room <> room then title ^ " \\hfill " ^ ss.s_room else
    if chair <> "" then title ^ " \\hfill \\normalfont \\small \\sf " ^ chair
    else title in
  if slots = [] then
    fprintf oc "\\emptysession{%s}{%s}\n" ss.s_time title
  else begin
    fprintf oc "\\session{%s}{%s}\n" ss.s_time title;
    List.iter (fun sl -> output_slot ~track ~poster oc sl) slots;
    fprintf oc "\\closesession\n"
  end

let date_day date =
  let pos = try String.rindex date '/' + 1 with Not_found -> 0 in
  String.sub ~pos ~len:(String.length date - pos) date

let get_sessions ~track sorted =
    try SMap.find track sorted with Not_found -> []

let output_page ~track ?(date="") ?(title=track) oc sessions =
  let sessions =
    if date = "" then sessions else
    List.filter (fun s -> s.s_date = date) sessions
  in
  if sessions = [] then () else
  let sessions =
    sort_by sessions ~prj:(fun s -> s.s_date, s.s_time, s.s_title)
  in
  let first = List.hd sessions in
  let date, multi =
    if date <> "" then (date, false) else
    let last_date = (last sessions).s_date in
    if first.s_date = last_date then (last_date, false) else
    (sprintf "%s -- %s" first.s_date last_date, true)
  in
  let room = if List.mem track parallel_sessions then "" else first.s_room in
  fprintf oc "\\header{%s}{%s}{%s}{%s}\n%s"
    track room date title
    (if List.mem track compact_tracks then "\\vspace{-1ex}\n" else "");
  fprintf oc "\\label{%s-%s}\n\n" track (if multi then "" else date_day date);
  let prev_date = ref "" in
  List.iter sessions ~f:
    begin fun ss ->
      if multi && ss.s_date <> !prev_date then begin
        fprintf oc "\\weekday{%s %s}\n"
          (Hashtbl.find day_map ss.s_date) (date_day ss.s_date);
        prev_date := ss.s_date
      end;
      output_session ~track ~room oc ss
    end;
  if track = night_track then fprintf oc "\\outputmap{}\n";
  fprintf oc "\\vfill \\newpage\n\n"

let extract_night_events s =
  SMap.fold
    (fun track se l -> List.filter se ~f:(fun s -> s.s_time > night) @ l)
      s []

let lookup_title ~track ~date pages =
  match List.filter pages ~f:(fun (t,d,_) -> t = track && d = date) with
    [_,_,s] -> s
  | _ -> failwith "lookup_title"

let snd3 (_,x,_) = x

let output_overview ~days ~pages ~details oc =
  let days = ref days and ddate = ref "" in
  fprintf oc "\\header{%s}{%s}{%s -- %s}{Overview}\n\n"
    details.e_acronym details.e_location details.e_start details.e_end;
  List.iter pages ~f:begin
    fun (name, date, title) ->
      if date <> !ddate then begin
        match !days with [] -> ()
        | day :: rem ->
            if !ddate <> "" then fprintf oc "\\closeday\n\n";
            days := rem; ddate := date;
            Hashtbl.add day_map date day;
            fprintf oc "\\weekday{%s %s}\n" day (date_day date)
      end;
      fprintf oc "\\overview{%s}{%s}{\\pageref{%s-%s}}\n"
        name title name (date_day date)
  end;
  fprintf oc "\\newpage\n\n"

let output_all ~days ~pages ~details oc sorted =
  let night_events = extract_night_events sorted in
  let pages =
    if night_events = [] then pages else
    (night_track, "", "Table of Night Events")::pages
  in
  (*output_overview ~days ~pages ~details oc;
  output_page ~track:night_track ~title:"Night Events" oc night_events;*)
  List.iter pages ~f:begin
    fun (track, date, title) ->
      output_page ~track ~date ~title oc (get_sessions ~track sorted)
  end

(* Full processing *)

let process ?out xml =
  let schedule = parse_file xml in
  let details = parse_event_details schedule in
  let subevs = extract_subevents schedule in
  let sorted = sort_by_track subevs in
  let sorted_sessions = SMap.map (List.map ~f:parse_subevent) sorted in
  let oc = match out with None -> stdout | Some f -> open_out f in
  output_all ~days ~pages ~details oc sorted_sessions;
  if out <> None then close_out oc;;

let () =
  if Array.length Sys.argv >= 2
  && Filename.check_suffix Sys.argv.(1) ".xml" then
    (process Sys.argv.(1); exit 0)
  else
    (prerr_endline "Usage: schedule_parser schedule.xml > schedule.tex"; exit 1)
;;

(* Code after this line is not executed when compiled *)

process "acmdlxml.xml" ~out:"schedule.tex";;

let schedule = parse_file "acmdlxml.xml"
let subevs = extract_subevents schedule
let sorted = sort_by_track subevs
let keys = List.map fst (SMap.bindings sorted)
let sorted_sessions = SMap.map (List.map ~f:parse_subevent) sorted;;
output_page ~track:"HOPE" stdout (get_sessions "HOPE" sorted_sessions);;
output_overview ~days ~pages stdout;;
output_all ~days ~pages stdout sorted_sessions;;
