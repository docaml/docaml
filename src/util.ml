module OptionMonad = struct

  let (>>=) opt f =
    match opt with
    | Some x -> f x
    | None -> None

  let (>>>=) opt f =
    match opt with
    | Some x -> Some (f x)
    | None -> None

  let return x = Some x

end
