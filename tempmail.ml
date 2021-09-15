open Lwt
open Cohttp_lwt_unix

let () = 
  Fmt.set_utf_8 Fmt.stdout true;
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty

let red = Fmt.(styled `Red (styled `Bold string))
let green = Fmt.(styled `Green (styled `Bold string))
let print_green x y =
  Fmt.pr "%a %s\t\t" green x y

let generate value =
  let fetch = Client.get(Uri.of_string "https://www.1secmail.com/api/v1/?action=genRandomMailbox&count=1") >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body -> body
  in  

  match value with
  | Some value -> print_endline value
  | None -> 
    let body = Lwt_main.run fetch in 
    let json = Yojson.Basic.from_string body in
    let email = 
      json
      |> Yojson.Basic.Util.index 0
      |> Yojson.Basic.Util.to_string
    in
      Printf.printf "your email is => %s\n" email

let refresh value =
  let user = "1olnfk0d" 
  and domain = "wwjmp" in 
  let uri = "https://www.1secmail.com/api/v1/?action=getMessages&login=" ^ user ^ "&domain=" ^ domain ^ ".com" in
  let fetch = Client.get(Uri.of_string uri) >>= fun (_, body) -> 
    body |> Cohttp_lwt.Body.to_string >|= fun body -> body
  in

  let show j =
    j |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_int |> string_of_int |> print_green "[ID]:"; 
    j |> Yojson.Basic.Util.member "from" |> Yojson.Basic.Util.to_string |> print_green "[FROM]:";
    j |> Yojson.Basic.Util.member "subject" |> Yojson.Basic.Util.to_string |> print_green "[SUBJECT]:";
    print_endline "\n"
  in

  match value with
  | Some value -> print_endline value
  | None -> 
    let body = Lwt_main.run fetch in
    let json = Yojson.Basic.from_string body in
    json |> Yojson.Basic.Util.to_list |> List.iter show

let access value = 
  let user = "1olnfk0d" 
  and domain = "wwjmp" in 

  match value with
  | Some value -> 
    let uri = "https://www.1secmail.com/api/v1/?action=readMessage&login=" ^ user ^ "&domain=" ^ domain ^ ".com&id=" ^ value in
    let fetch = Client.get(Uri.of_string uri) >>= fun (_, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body -> body
    in

    let json_pos json p =
      json
      |> Yojson.Basic.Util.member p
      |> Yojson.Basic.Util.to_string
    in

    let print_email x y =
      Fmt.pr "%a%s\n" green x y
    in

    let body = Lwt_main.run fetch in
    let json = Yojson.Basic.from_string body in 
    print_email "[DATE]: " (json_pos json "date"); 
    print_email "[FROM]: " (json_pos json "from"); 
    print_email "[SUBJECT]: " (json_pos json "subject"); 
    print_email "\n" (json_pos json "textBody");
  | None ->
    Fmt.pr "%a%s\n" red "[Error]: " "enter the ID to access the email"