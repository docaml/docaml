type attr = {
  name : string ;
  value : string
}

type t = {
  tag : string ;
  attributes : attr ;
  children : t list
}
