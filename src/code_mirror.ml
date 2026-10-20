module Types = Types
module Tjv = Tjv
module Extension = Extension
module State = State
module View = View
module Keymap = Keymap

let basic_setup : Extension.t =
  Extension.of_jv (Jv.get Jv.global "__CM__basic_setup")
