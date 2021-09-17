open Lwt
open Cohttp_lwt_unix

let () = 
  Fmt.set_utf_8 Fmt.stdout true;
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty

let red = Fmt.(styled `Red (styled `Bold string))
let green = Fmt.(styled `Green (styled `Bold string))
let tmp_dir = "/tmp/ocaml-tempmail"
let tmpmail_address = tmp_dir ^ "/email_adress.txt"

let save_email email =
  match () with
  | _ when Sys.file_exists tmp_dir -> 
    let oc = open_out tmpmail_address in
    Printf.fprintf oc "%s\n" email;
    close_out oc
  | _ -> 
    Sys.mkdir tmp_dir 0o777;
    let oc = open_out tmpmail_address in
    Printf.fprintf oc "%s\n" email;
    close_out oc

let return_body uri =
  Client.get(Uri.of_string uri) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body -> body

let generate value =
  let uri = "https://www.1secmail.com/api/v1/?action=genRandomMailbox&count=1" in

  match value with
  | Some value -> ignore value
  | None -> 
    let body = Lwt_main.run (return_body uri) in 
    let json = Yojson.Basic.from_string body in
    let email = 
      json
      |> Yojson.Basic.Util.index 0
      |> Yojson.Basic.Util.to_string
    in
      save_email email;
      Fmt.pr "%a %s\n" green "[EMAIL]:" email

let read_file channel =
  let size = in_channel_length channel in
  let buf = Bytes.create size in
  really_input channel buf 0 size;
  Bytes.unsafe_to_string buf

let get_email () =
  print_endline "sex";
  if Sys.file_exists tmpmail_address then
    let x = tmpmail_address
          |> open_in
          |> read_file
          |> Str.split (Str.regexp "@")
        in
          let y = match x with
            | [x; y] -> (x, y)
            | _ -> failwith "too bad"
          in
          `Tuple y
  else `Unit (generate None)

let access value =   
  let tuple = 
    match get_email () with
    | `Tuple (x, y) -> (x, y)
    | `Unit _ -> failwith "too bad"
  in

  let fst (x, _) = x
  and scd (_, x) = x
  in
  match value with
  | Some value -> 
    let uri = 
      "https://www.1secmail.com/api/v1/?action=readMessage&login=" ^ fst tuple ^ "&domain=" ^ scd tuple ^ "&id=" ^ value 
      |> Str.replace_first (Str.regexp "\n") ""
    in
    let json_pos json p =
      json
      |> Yojson.Basic.Util.member p
      |> Yojson.Basic.Util.to_string
    in

    let print_email x y =
      Fmt.pr "%a%s\n" green x y
    in

    let body = Lwt_main.run (return_body uri) in
    let json = Yojson.Basic.from_string body in 
      print_email "[DATE]: " (json_pos json "date"); 
      print_email "[FROM]: " (json_pos json "from"); 
      print_email "[SUBJECT]: " (json_pos json "subject"); 
      print_email "\n" (json_pos json "textBody");
  | None ->
    Fmt.pr "%a%s\n" red "[ERROR]: " "enter the ID to access the email"

let refresh value =
  let tuple = 
    match get_email () with
    | `Tuple (x, y) -> (x, y)
    | `Unit _ -> failwith "too bad"
  in

  let fst (x, _) = x
  and scd (_, x) = x
  in
  let uri = "https://www.1secmail.com/api/v1/?action=getMessages&login=" ^ fst tuple ^ "&domain=" ^ scd tuple in

  let print x y =
    Fmt.pr "%a %s\t\t" green x y
  in

  let show j =
    j |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_int |> string_of_int |> print "[ID]:"; 
    j |> Yojson.Basic.Util.member "from" |> Yojson.Basic.Util.to_string |> print "[FROM]:";
    j |> Yojson.Basic.Util.member "subject" |> Yojson.Basic.Util.to_string |> print "[SUBJECT]:";
    print_endline ""
  in

  match value with
  | Some value -> print_endline value
  | None -> 
    let body = Lwt_main.run (return_body uri) in
    let json = Yojson.Basic.from_string body in
    json |> Yojson.Basic.Util.to_list |> List.iter show