open Extraction_plugin
open Pp
open CErrors
open Util
open Names
open Table
open Miniml
open Mlutil
open Common

let vernac_simple_extraction ~opaque_access qualids =
    CErrors.user_err
      Pp.(str "Not Implemented")
