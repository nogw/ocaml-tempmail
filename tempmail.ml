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

  match value with
  | Some value -> print_endline value
  | None -> 
    let body = Lwt_main.run fetch in
    let json = Yojson.Safe.from_string body in
    Format.printf "Parsed to %a" Yojson.Safe.pp json

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