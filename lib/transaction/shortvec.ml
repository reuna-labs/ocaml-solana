let encode value =
  if value < 0 || value > 0xffff then Error "shortvec: value outside [0, 65535]"
  else
    let buffer = Buffer.create 3 in
    let rec loop remaining =
      let byte = remaining land 0x7f in
      let rest = remaining lsr 7 in
      Buffer.add_char buffer (Char.chr (if rest = 0 then byte else byte lor 0x80));
      if rest <> 0 then loop rest
    in
    loop value;
    Ok (Buffer.contents buffer)

let decode source offset =
  let length = String.length source in
  let rec loop index value =
    if offset + index >= length then Error "shortvec: truncated input"
    else if index >= 3 then Error "shortvec: more than three bytes"
    else
      let byte = Char.code source.[offset + index] in
      if index > 0 && byte = 0 then Error "shortvec: alias encoding"
      else if index = 2 && byte land 0x80 <> 0 then Error "shortvec: third byte continues"
      else
        let value = value lor ((byte land 0x7f) lsl (7 * index)) in
        if value > 0xffff then Error "shortvec: u16 overflow"
        else if byte land 0x80 = 0 then Ok (value, offset + index + 1)
        else loop (index + 1) value
  in
  loop 0 0
