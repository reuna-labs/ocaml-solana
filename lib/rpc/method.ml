type 'a t = {
  name : string;
  params : Yojson.Safe.t list;
  decode : Yojson.Safe.t -> ('a, string) result;
}

let make ~name ?(params = []) decode = { name; params; decode }
let name method_ = method_.name
let params method_ = method_.params
let decode method_ value = method_.decode value
