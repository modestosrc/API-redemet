open Lwt
open Cohttp_lwt_unix
open Yojson.Basic

[@@@warning "-69-34-32"]

type mensagem = {
  id : string;
  val_inicial : string;
  mens : string;
  recebimento : string;
}

let remove_quotes s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then String.sub s 1 (len - 2)
  else s

let get_metar_from_data data =
  let l = String.split_on_char ',' data in
  let rec aux l =
    match l with
    | [] -> ""
    | h :: t ->
        let ld = String.split_on_char ':' h in
        if List.hd ld = "\"mens\"" then List.nth ld 1 else aux t
  in

  aux l |> remove_quotes

let parse_json_string s =
  (* Divide uma string do formato "chave":"valor" em um par (chave, valor) *)
  let parse_key_value_pair s =
    match String.split_on_char ':' s with
    | [ key; value ] ->
        (remove_quotes (String.trim key), remove_quotes (String.trim value))
    | _ -> failwith "Formato de par chave-valor inválido"
  in

  (* Remove as chaves {} e divide a string restante em pares chave-valor *)
  let trimmed = String.trim s in
  let without_brackets = String.sub trimmed 1 (String.length trimmed - 2) in
  let items = String.split_on_char ',' without_brackets in
  List.map parse_key_value_pair items

let get_redemet icao key =
  Client.get
    (Uri.of_string ("https://api-redemet.decea.mil.br/mensagens/metar/" ^ icao))
    ~headers:(Cohttp.Header.of_list [ ("X-Api-Key", key) ])
  >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body -> body

let run icao key =
  let body = get_redemet icao key |> Lwt_main.run in
  let json = from_string body in
  let data =
    Util.member "data" json |> Util.member "data" |> to_string
    |> get_metar_from_data
  in
  print_endline data

let error_handler e =
  print_endline ("Erro" ^ e);
  print_endline ("Uso: " ^ Sys.argv.(0));
  print_endline "\t-i <ICAO>\tCódigo ICAO do aeroporto";
  print_endline "\t-k <API_KEY>\tChave de acesso à API da REDEMET";
  print_endline "\tOu defina as variáveis de ambiente ICAO e REDEMET_API_KEY";
  print_endline "\t-h\t\tMostra esta mensagem";
  exit 1

let () =
  let icao = ref "" in
  let key = ref "" in
  let speclist =
    [
      ("-i", Arg.Set_string icao, "Código ICAO do aeroporto");
      ("-k", Arg.Set_string key, "Chave de acesso à API da REDEMET");
      ("-h", Arg.Unit (fun _ -> error_handler ""), "Mostra esta mensagem");
    ]
  in
  if Array.length Sys.argv = 1 then (
    icao := Sys.getenv "ICAO";
    key := Sys.getenv "REDEMET_API_KEY")
  else Arg.parse speclist error_handler "";
  if !icao = "" || !key = "" then error_handler "";
  run !icao !key
