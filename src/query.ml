open List
open Options

let options_of_uri uri = options_of_alist (Uri.query (Uri.of_string uri))

let uri_of_options options = Uri.with_query options.base (alist_of_options options)

