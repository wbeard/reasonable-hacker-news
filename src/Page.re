/* open Bs_promise; */
/* open Bs_fetch; */
let unwrapUnsafely v =>
  switch v {
  | Some v => v
  | None => raise (Invalid_argument "unwrapUnsafely called on None")
  };

module Page = {
  include ReactRe.Component;
  type props = {message: string};
  let name = "Page";
  let componentDidMount _ => {
    let _ =
      Js.Promise.(
        Bs_fetch.fetch "https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty" |>
        then_ Bs_fetch.Response.json |>
        then_ (fun json => Js.Json.decodeArray json |> resolve) |>
        then_ (fun opt => unwrapUnsafely opt |> resolve) |>
        then_ (
          fun items =>
            items |>
            Js.Array.map (
              fun item => {
                let safe_item = item |> Js.Json.decodeString |> unwrapUnsafely;
                "https://hacker-news.firebaseio.com/v0/item/" ^ safe_item ^ ".json?print=pretty"
              }
            ) |> resolve
        ) |>
        then_ (
          fun item_urls => {
            Js.log item_urls;
            item_urls |> resolve
          }
        )
      );
    None
  };
  let render {props} => <div> (ReactRe.stringToElement props.message) </div>;
};

include ReactRe.CreateComponent Page;

let createElement ::message => wrapProps {message: message};
