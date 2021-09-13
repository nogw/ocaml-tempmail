open Lwt
(* open Cohttp *)
open Cohttp_lwt_unix

let print_t v = 
  match v with
  | Some v -> print_endline v
  | None -> print_endline "none"

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
    j |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_int |> string_of_int |> Printf.printf "id - %s   ";
    j |> Yojson.Basic.Util.member "from" |> Yojson.Basic.Util.to_string |> Printf.printf "from - %s   ";
    j |> Yojson.Basic.Util.member "subject" |> Yojson.Basic.Util.to_string |> Printf.printf "subject - %s\n";
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

    let body = Lwt_main.run fetch in
    let json = Yojson.Basic.from_string body in
    Format.printf 
      "date: %s\nfrom: %s\nsubject: %s\n\n%s\n" 
      (json_pos json "date") 
      (json_pos json "from") 
      (json_pos json "subject")
      (json_pos json "textBody")
  | None ->
    print_endline "id?"