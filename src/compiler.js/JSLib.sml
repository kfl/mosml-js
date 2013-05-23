structure JSLib :> JSLib = struct
  val set = false
  val web_includejslib = "<script src=\"jslib.js\" type=\"text/javascript\"></script>\n"
  val node_includejslib = "require(\"jslib.js\")\n"
  (*val includejslib = if set then web_includejslib else node_includejslib*)
  fun includejslib jsmode = case jsmode of
    "node" => node_includejslib
  | "web"  => web_includejslib
  | "none" => ""
  prim_val jsdebug_ : 'a -> unit = 1 "jsdebug"
  fun jsdebug x = jsdebug_ x
end